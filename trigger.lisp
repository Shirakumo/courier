#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defun ensure-trigger (trigger-ish)
  (or
   (etypecase trigger-ish
     (dm:data-model trigger-ish)
     (db:id (dm:get-one 'trigger (db:query (:= '_id trigger-ish))))
     (T (ensure-trigger (db:ensure-id trigger-ish))))
   (error 'request-not-found :message "No such trigger.")))

(defun list-triggers (thing &key amount (skip 0))
  (ecase (dm:collection thing)
    (campaign
     (dm:get 'trigger (db:query (:= 'campaign (dm:id thing)))
             :amount amount :skip skip))
    (sequence
     (dm:get (rdb:join (trigger _id) (sequence-trigger trigger))
             (db:query (:= 'sequence (dm:id thing)))
             :amount amount :skip skip :hull 'trigger))
    ((link mail tag)
     (dm:get 'trigger (db:query (:and (:= 'target-id (dm:id thing))
                                      (:= 'target-type (collection-type thing))))
             :amount amount :skip skip))))

(defun list-source-triggers (thing &key amount (skip 0))
  (dm:get 'trigger (db:query (:and (:= 'source-id (dm:id thing))
                                   (:= 'source-type (collection-type thing))))
          :amount amount :skip skip))

(defun make-trigger (campaign source target &key description (delay 0) tag-constraint (save T))
  (dm:with-model trigger ('trigger NIL)
    (setf-dm-fields trigger campaign description delay tag-constraint)
    (setf (dm:field trigger "normalized-constraint") (normalize-constraint campaign (or tag-constraint "")))
    (setf (dm:field trigger "source-id") (dm:id source))
    (setf (dm:field trigger "source-type") (collection-type source))
    (setf (dm:field trigger "target-id") (dm:id target))
    (setf (dm:field trigger "target-type") (collection-type target))
    (when save (dm:insert trigger))
    trigger))

(defun edit-trigger (trigger &key description source target delay tag-constraint (save T))
  (setf-dm-fields trigger description delay tag-constraint)
  (when tag-constraint
    (setf (dm:field trigger "normalized-constraint") (normalize-constraint (dm:field trigger "campaign") tag-constraint)))
  (when source
    (setf (dm:field trigger "source-id") (dm:id source))
    (setf (dm:field trigger "source-type") (collection-type source)))
  (when target
    (setf (dm:field trigger "target-id") (dm:id target))
    (setf (dm:field trigger "target-type") (collection-type target)))
  (when save (dm:save trigger))
  trigger)

(defun delete-trigger (trigger)
  (let ((trigger (ensure-trigger trigger)))
    (db:with-transaction ()
      (db:remove 'sequence-trigger (db:query (:= 'trigger (dm:id trigger))))
      (db:remove 'trigger-receipt (db:query (:= 'trigger (dm:id trigger))))
      (dm:delete trigger))))

(defun delete-triggers-for (thing)
  (db:with-transaction ()
    (mapcar #'delete-trigger (dm:get 'trigger (db:query (:or (:and (:= 'target-id (dm:id thing))
                                                                   (:= 'target-type (collection-type thing)))
                                                             (:and (:= 'source-id (dm:id thing))
                                                                   (:= 'source-type (collection-type thing)))))))))

(defun trigger-triggered-p (trigger subscriber)
  (< 0 (db:count 'trigger-receipt (db:query (:and (:= 'trigger (ensure-id trigger))
                                                  (:= 'subscriber (ensure-id subscriber)))))))

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

(defun process-trigger (trigger subscriber &key force)
  (when (or force
            (and (not (trigger-triggered-p trigger subscriber))
                 (tag-constraint-applicable-p (dm:field trigger "normalized-constraint") subscriber)))
    (l:info :courier.trigger "Running trigger ~a for ~a" trigger subscriber)
    (with-simple-restart (continue "Skip running the trigger.")
      (let ((target (resolve-typed (dm:field trigger "target-type")
                                   (dm:field trigger "target-id"))))
        (unless target
          (error "Trigger ~a does not have an existing target!" trigger))
        (ecase (dm:collection target)
          (mail
           (unless (or (mail-sent-p target subscriber)
                       (mail-in-queue-p target subscriber))
             (case (dm:field trigger "target-type")
               (10 (mark-mail-sent target subscriber :unlocked))
               (0 (enqueue-mail target
                                :target subscriber
                                :time (+ (get-universal-time)
                                         (dm:field trigger "delay")))))))
          (tag
           (tag subscriber target))
          (campaign
           (delete-subscriber subscriber))))
      (unless (trigger-triggered-p trigger subscriber)
        (db:insert 'trigger-receipt `((subscriber . ,(dm:id subscriber))
                                      (trigger . ,(dm:id trigger))))))))

(defun process-triggers (subscriber triggers)
  (let ((triggers (etypecase triggers
                    (list triggers)
                    (dm:data-model (list-source-triggers triggers)))))
    (l:debug :courier.trigger "Processing triggers ~a for ~a" triggers subscriber)
    (dolist (trigger triggers)
      (process-trigger trigger subscriber))))
