#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defun parse-constraint (constraint)
  (with-input-from-string (in constraint)
    (let ((buffer (make-string-output-stream))
          (mode :include)
          (groups ())
          (tokens ()))
      (labels ((end-token ()
                 (let ((token (get-output-stream-string buffer)))
                   (when (string/= "" token)
                     (push (cons mode token) tokens))
                   (setf mode :include)))
               (end-group ()
                 (end-token)
                 (when tokens (push (nreverse tokens) groups))
                 (setf tokens ())))
        (loop for char = (read-char in NIL)
              while char
              do (case char
                   (#\+ (end-token))
                   (#\- (end-token) (setf mode :exclude))
                   (#\  (end-token))
                   (#\| (end-group))
                   (#\\ (write-char (read-char in) buffer))
                   (#\" (loop for char = (read-char in NIL)
                              do (case char
                                   (#\\ (write-char (read-char in) buffer))
                                   (#\" (return))
                                   ((NIL) (return))
                                   (T (write-char char buffer))))
                    (end-token))
                   (T (write-char char buffer)))
              finally (end-group))
        (nreverse groups)))))

(defun normalize-constraint (campaign constraint)
  (with-output-to-string (out)
    (loop for group in (parse-constraint constraint)
          do (loop for (mode . tag-name) in group
                   for tag = (dm:get-one 'tag (db:query (:and (:= 'campaign (ensure-id campaign))
                                                              (:= 'title tag-name))))
                   do (cond (tag
                             (write-char (ecase mode
                                           (:include #\+)
                                           (:exclude #\-))
                                         out)
                             (princ (dm:id tag) out))
                            (T
                             (cerror "Ignore the tag" "No such tag ~s" tag-name))))
             (write-char #\| out))))

(defun tag-constraint-applicable-p (normalized-constraint subscriber)
  (let ((tags (db:iterate 'tag-table (db:query (:= 'subscriber (ensure-id subscriber)))
                          (lambda (r) (gethash "tag" r)) :fields '("tag") :accumulate T)))
    (loop with start = 0
          with mode = :inclusive
          with passed = T
          for i from 0 below (length normalized-constraint)
          do (flet ((check-tag ()
                      (let ((id (db:ensure-id (subseq normalized-constraint start i))))
                        (setf start (1+ i))
                        (if (find id tags :test #'equal)
                            (when (eq :exclude mode) (setf passed NIL))
                            (when (eq :include mode) (setf passed NIL))))))
               (case (char normalized-constraint i)
                 (#\+ (check-tag) (setf mode :include))
                 (#\- (check-tag) (setf mode :exclude))
                 (#\| (when passed (return T)))))
          finally (return passed))))

(defun process-trigger (subscriber trigger &key force)
  (when (or force
            (tag-constraint-applicable-p (dm:field trigger "normalized-constraint") subscriber))
    (l:info :courier.trigger "Running trigger ~a for ~a" trigger subscriber)
    (let ((target (resolve-typed (dm:field trigger "target-type")
                                 (dm:field trigger "target-id"))))
      (ecase (dm:collection target)
        (mail
         (unless (mail-sent-p target subscriber)
           (case (dm:field trigger "target-type")
             (10 (mark-mail-sent target subscriber :unlocked))
             (0 (enqueue-mail target
                              :target subscriber
                              :time (+ (get-universal-time)
                                       (dm:field trigger "delay")))))))
        (tag
         (tag subscriber target))
        (campaign
         (delete-subscriber subscriber))))))

(defun process-triggers (subscriber triggers)
  (l:debug :courier.trigger "Processing triggers ~a for ~a" triggers subscriber)
  (dolist (trigger (etypecase triggers
                     (list triggers)
                     (dm:data-model (list-triggers triggers))))
    (process-trigger subscriber trigger)))
