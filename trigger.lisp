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
          (tokens ()))
      (flet ((end-token ()
               (let ((token (get-output-stream-string buffer)))
                 (when (string/= "" token)
                   (push (cons mode token) tokens))
                 (setf mode :include))))
        (loop for char = (read-char in NIL)
              while char
              do (case char
                   (#\+ (end-token))
                   (#\- (end-token) (setf mode :exclude))
                   (#\  (end-token))
                   (#\\ (write-char (read-char in) buffer))
                   (#\" (loop for char = (read-char in NIL)
                              do (case char
                                   (#\\ (write-char (read-char in) buffer))
                                   (#\" (return))
                                   ((NIL) (return))
                                   (T (write-char char buffer))))
                    (end-token))
                   (T (write-char char buffer)))
              finally (end-token))
        (nreverse tokens)))))

(defun normalize-constraint (campaign constraint)
  (with-output-to-string (out)
    (loop for (mode . tag-name) in (parse-constraint constraint)
          for tag = (dm:get-one 'tag (db:query (:and (:= 'campaign (ensure-id campaign))
                                                     (:= 'title tag-name))))
          do (cond (tag
                    (write-char (ecase mode
                                  (:include #\+)
                                  (:exclude #\-))
                                out)
                    (princ (dm:id tag) out))
                   (T
                    (cerror "Ignore the tag" "No such tag ~s" tag-name))))))

(defun tag-constraint-applicable-p (normalized-constraint subscriber)
  (let ((tags (db:iterate 'tag-table (db:query (:= 'subscriber (ensure-id subscriber)))
                          (lambda (r) (gethash "tag" r)) :fields '("tag") :accumulate T)))
    (loop with start = 0
          with mode = :inclusive
          for i from 0 below (length normalized-constraint)
          do (flet ((check-tag ()
                      (let ((id (db:ensure-id (subseq normalized-constraint start i))))
                        (setf start (1+ i))
                        (if (find id tags :test #'equal)
                            (when (eq :exclude mode) (return NIL))
                            (when (eq :include mode) (return NIL))))))
               (case (char normalized-constraint i)
                 (#\+ (check-tag) (setf mode :include))
                 (#\- (check-tag) (setf mode :exclude))))
          finally (return T))))

(defun process-trigger (subscriber trigger &key force)
  (when (or force
            (tag-constraint-applicable-p (dm:field trigger "normalized-constraint") subscriber))
    (v:info :courier.trigger "Running trigger ~a for ~a" trigger subscriber)
    (let ((target (resolve-typed (dm:field trigger "target-type")
                                 (dm:field trigger "target-id"))))
      (ecase (dm:collection target)
        (mail
         (unless (mail-sent-p target subscriber)
           (case (dm:field trigger "target-type")
             (10 (mark-mail-sent target subscriber :unlocked))
             (0 (enqueue-email target
                               :target subscriber
                               :time (+ (get-universal-time)
                                        (dm:field trigger "time-offset")))))))
        (tag
         (tag subscriber target))))))

(defun process-triggers (subscriber triggers)
  (v:debug :courier.trigger "Processing triggers ~a for ~a" triggers subscriber)
  (dolist (trigger (etypecase triggers
                     (list triggers)
                     (dm:data-model (triggers triggers))))
    (process-trigger subscriber trigger)))
