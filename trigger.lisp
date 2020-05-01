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

(defun list-triggers (thing &key amount (skip 0) (type (collection-type thing)) query)
  (with-query (query description)
    (ecase (dm:collection thing)
      (campaign
       (dm:get 'trigger (query (:= 'campaign (dm:id thing)))
               :amount amount :skip skip))
      (sequence
       (fixup-ids (dm:get (rdb:join (trigger _id) (sequence-trigger trigger))
                          (query (:= 'sequence (dm:id thing)))
                          :amount amount :skip skip :hull 'trigger)
                  "trigger"))
      ((link mail tag)
       (dm:get 'trigger (query (:and (:= 'target-id (dm:id thing))
                                     (:= 'target-type type)))
               :amount amount :skip skip)))))

(defun list-source-triggers (thing &key amount (skip 0) (type (collection-type thing)))
  (dm:get 'trigger (db:query (:and (:= 'source-id (dm:id thing))
                                   (:= 'source-type type)))
          :amount amount :skip skip))

(defun make-trigger (campaign source target &key description (delay 0) tag-constraint rule (save T))
  (dm:with-model trigger ('trigger NIL)
    (setf-dm-fields trigger campaign description delay tag-constraint rule)
    (setf (dm:field trigger "normalized-constraint") (normalize-constraint campaign (or tag-constraint "")))
    (setf (dm:field trigger "source-id") (dm:id source))
    (setf (dm:field trigger "source-type") (collection-type source))
    (setf (dm:field trigger "target-id") (dm:id target))
    (setf (dm:field trigger "target-type") (collection-type target))
    (when save
      (dm:insert trigger)
      (when rule ;; Make sure we process the rule immediately.
        (process-rule trigger campaign)))
    trigger))

(defun edit-trigger (trigger &key description source target delay tag-constraint (rule NIL rule-p) (save T))
  (db:with-transaction ()
    (setf-dm-fields trigger description delay tag-constraint)
    (when rule-p
      (setf (dm:field trigger "rule") rule))
    (when tag-constraint
      (setf (dm:field trigger "normalized-constraint") (normalize-constraint (dm:field trigger "campaign") tag-constraint)))
    (when source
      (setf (dm:field trigger "source-id") (dm:id source))
      (setf (dm:field trigger "source-type") (collection-type source)))
    (when target
      (setf (dm:field trigger "target-id") (dm:id target))
      (setf (dm:field trigger "target-type") (collection-type target))
      ;; Our action changed, clear trigger receipt table to ensure the rule stays consistent
      ;; and performs its action again for all applicable. This is fine even if it would cause
      ;; duplicate fires since we prevent double-tagging or double-mailing already anyway.
      (when (dm:field trigger "rule")
        (db:remove 'trigger-receipt (db:query (:= 'trigger (dm:id trigger))))))
    (when save
      (dm:save trigger)
      (when (dm:field trigger "rule") ;; Make sure we process the rule immediately.
        (process-rule trigger T))))
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
    (loop for cons on (parse-constraint constraint)
          for group = (car cons)
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
                             (cerror "Ignore the tag" 'api-argument-invalid :argument 'constraint
                                                                            :message (format NIL "The tag ~s does not exist." tag-name)))))
             (when (cdr cons)
               (write-char #\| out)))))

(defun tag-constraint-applicable-p (normalized-constraint subscriber)
  (or (string= "" normalized-constraint)
      (let ((tags (db:iterate 'tag-table (db:query (:= 'subscriber (ensure-id subscriber)))
                    (lambda (r) (gethash "tag" r)) :fields '("tag") :accumulate T))
            (mode (ecase (char normalized-constraint 0)
                    (#\+ :include)
                    (#\- :exclude)))
            (start 1) (i 1) (passed T))
        (flet ((check-tag ()
                 (when (< start i)
                   (let ((id (db:ensure-id (subseq normalized-constraint start i))))
                     (if (find id tags :test #'equal)
                         (when (eq :exclude mode) (setf passed NIL))
                         (when (eq :include mode) (setf passed NIL)))))
                 (setf start (1+ i))))
          (loop while (< i (length normalized-constraint))
                do (case (char normalized-constraint i)
                     (#\+ (check-tag) (setf mode :include))
                     (#\- (check-tag) (setf mode :exclude))
                     (#\| (check-tag) (if passed (return T) (setf passed T))))
                   (incf i)
                finally (progn (check-tag) (return passed)))))))

(defun trigger-satisfied-p (subscriber trigger)
  (and (tag-constraint-applicable-p (dm:field trigger "normalized-constraint") subscriber)
       (let ((source (resolve-typed (dm:field trigger "source-type")
                                    (dm:field trigger "source-id"))))
         (ecase (dm:collection source)
           (campaign
            (equal (dm:field subscriber "campaign")
                   (dm:field trigger "campaign")))
           (mail
            (mail-received-p source subscriber))
           (link
            (link-received-p source subscriber))
           (tag
            (tagged-p source subscriber))))))

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
             (ecase (dm:field trigger "target-type")
               (10 (mark-mail-sent target subscriber :unlocked))
               (0 (enqueue-mail target
                                :target subscriber
                                :time (+ (get-universal-time)
                                         (dm:field trigger "delay")))))))
          (tag
           (ecase (dm:field trigger "target-type")
             (2 (tag subscriber target))
             (20 (untag subscriber target))))
          (campaign
           (edit-subscriber subscriber :status :deactivated))))
      (unless (trigger-triggered-p trigger subscriber)
        (db:insert 'trigger-receipt `((subscriber . ,(dm:id subscriber))
                                      (trigger . ,(dm:id trigger))))))))

(defun process-triggers (subscriber triggers)
  (let ((triggers (etypecase triggers
                    (list triggers)
                    (dm:data-model (list-source-triggers triggers)))))
    (l:debug :courier.trigger "Processing triggers ~a for ~a" triggers subscriber)
    (dolist (trigger triggers)
      (unless (dm:field trigger "rule")
        (process-trigger trigger subscriber)))
    ;; A trigger condition might have changed, always also run rules.
    (process-rules subscriber)))

(defun process-rule (trigger target)
  (etypecase target
    ((eql T)
     (process-rule trigger (ensure-campaign (dm:field trigger "campaign"))))
    (dm:data-model
     (ecase (dm:collection target)
       (subscriber
        (when (and (not (trigger-triggered-p trigger target))
                   (trigger-satisfied-p target trigger))
          ;; Set force to T so we bypass the double tests.
          (process-trigger trigger target :force T)))
       (campaign
        (dolist (subscriber (list-subscribers target))
          (process-rule trigger subscriber)))))))

(defun process-rules (subscriber)
  (let* ((subscriber (ensure-subscriber subscriber))
         (campaign (dm:field subscriber "campaign"))
         (triggers (dm:get 'trigger (db:query (:and (:= 'campaign campaign)
                                                    (:= 'rule T))))))
    (l:debug :courier "Processing rules for ~a" subscriber)
    (dolist (trigger triggers)
      (process-rule trigger subscriber))))
