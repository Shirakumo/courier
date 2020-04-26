#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defun ensure-sequence (sequence-ish)
  (or
   (etypecase sequence-ish
     (dm:data-model sequence-ish)
     (T (dm:get-one 'sequence (db:query (:= '_id (db:ensure-id sequence-ish))))))
   (error 'request-not-found :message "No such sequence.")))

(defun list-sequences (campaign &key amount (skip 0))
  (dm:get 'sequence (db:query (:= 'campaign (ensure-id campaign)))
          :amount amount :skip skip))

(defun make-sequence (campaign title &key triggers (save T))
  (let ((sequence (dm:hull 'sequence)))
    (setf-dm-fields sequence campaign title)
    (when save
      (db:with-transaction ()
        (dm:insert sequence)
        (loop for trigger in triggers
              for i from 1
              do (etypecase trigger
                   (dm:data-model
                    (db:insert 'sequence-trigger `(("sequence" . ,(dm:id sequence))
                                                   ("trigger" . ,(dm:id trigger)))))
                   (cons
                    (destructuring-bind (delay subject) trigger
                      (let* ((title (format NIL "~a - ~a" title i))
                             (mail (make-mail campaign :title title :subject subject :body "; TODO: Write mail"))
                             (trigger (make-trigger campaign campaign mail :description title :delay delay)))
                        (db:insert 'sequence-trigger `(("sequence" . ,(dm:id sequence))
                                                       ("trigger" . ,(dm:id trigger)))))))))))
    sequence))

(defun edit-sequence (sequence-ish &key title triggers (save T))
  (let ((sequence (ensure-sequence sequence-ish)))
    (setf-dm-fields sequence title)
    (when save
      (db:with-transaction ()
        (dm:save sequence)
        (db:remove 'sequence-trigger (db:query (:= 'sequence (dm:id sequence))))
        (loop with campaign = (ensure-campaign (dm:field sequence "campaign"))
              for (id delay subject) in triggers
              for i from 0
              for trigger = (cond (id
                                   (let ((trigger (ensure-trigger id)))
                                     (edit-trigger trigger :delay delay)
                                     (edit-mail (dm:field trigger "target-id") :subject subject)
                                     trigger))
                                  (T
                                   (let* ((title (format NIL "~a - ~a" title i))
                                          (mail (make-mail campaign :title title :subject subject :body "; TODO: Write mail")))
                                     (make-trigger campaign campaign mail :description title :delay delay))))
              do (db:insert 'sequence-trigger `(("sequence" . ,(dm:id sequence))
                                                ("trigger" . ,(dm:id trigger)))))))
    sequence))

(defun delete-sequence (sequence-ish)
  (let ((sequence (ensure-sequence sequence-ish)))
    (db:with-transaction ()
      (mapcar #'delete-trigger (list-triggers sequence))
      (dm:delete sequence))))
