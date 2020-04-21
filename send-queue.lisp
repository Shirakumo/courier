#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defun enqueue-mail (mail &key target time)
  (let* ((time (or time 0))
         (campaign (dm:get-one 'campaign (db:query (:= '_id (dm:field mail "campaign")))))
         (host (dm:field campaign "host")))
    (flet ((send (subscriber-id)
             (db:insert 'mail-queue `(("host" . ,host)
                                      ("subscriber" . ,subscriber-id)
                                      ("mail" . ,(dm:id mail))
                                      ("send-time" . ,time)
                                      ("attempts" . 0)))))
      (unless target
        (setf target campaign))
      (etypecase target
        (dm:data-model
         (ecase (dm:collection target)
           (subscriber
            (if (eql :active (id-user-status (dm:field target "status")))
                (send (dm:id target))
                (error 'api-argument-invalid
                       :argument 'target
                       :message (format NIL "Subscriber ~a is not an active subscription" (dm:field target "address")))))
           (tag
            (mapcar #'send (db:iterate (rdb:join (tab-table subscriber) (subscriber _id))
                             (db:query (:and (:= 'tag (dm:id target))
                                             (:= 'status (user-status-id :active))))
                             (lambda (r) (gethash "subscriber" r) :fields '("subscriber") :accumulate T))))
           (campaign
            (mapcar #'send (db:iterate 'subscriber (db:query (:and (:= 'campaign (dm:id target))
                                                                   (:= 'status (user-status-id :active))))
                                       (lambda (r) (gethash "_id" r)) :fields '("_id") :accumulate T)))))
        (db:id
         (send target))))
    (notify-task 'send-queue time)))

(defun send-queue (queue)
  (send-mail (dm:get-one 'mail (db:query (:= '_id (dm:field queue "mail"))))
             (dm:get-one 'subscriber (db:query (:= '_id (dm:field queue "subscriber")))))
  (dm:delete queue))

(defun process-send-queue-for-host (host)
  (l:trace :courier.send-queue "Processing ~a" host)
  (let* ((host (ensure-host host))
         (timeout (- (+ (dm:field host "last-send-time")
                        (dm:field host "batch-cooldown"))
                     (get-universal-time))))
    (when (< timeout 0)
      (let ((queued (dm:get 'mail-queue (db:query (:and (:= 'host (dm:id host))
                                                        (:<= 'send-time (get-universal-time))))
                            :amount (dm:field host "batch-size") :sort '((send-time :asc)))))
        (dolist (queue queued)
          (restart-case
              (handler-bind ((error (lambda (e)
                                      (l:error :courier.send-queue "Failed to send queued mail.")
                                      (l:trace :courier.send-queue e)
                                      (cond (radiance:*debugger*
                                             (invoke-debugger e))
                                            ((< (dm:field queue "attempts") (config :send-queue :retry-attempts))
                                             (l:debug :courier.send-queue "Rescheduling to try again later.")
                                             (invoke-restart 'ignore))
                                            (T
                                             (l:debug :courier.send-queue "Exceeded max send attempts, dropping mail.")
                                             (invoke-restart 'forget))))))
                (send-queue queue))
            (ignore ()
              :report "Ignore the send and retry later."
              (setf (dm:field queued "time") (expt (dm:field queued "attempts") (config :send-queue :retry-backoff-exponent)))
              (incf (dm:field queued "attempts"))
              (dm:save queued))
            (forget ()
              :report "Give up trying to send the queued mail."
              (dm:delete queue))))
        (when queued
          (setf (dm:field host "last-send-time") (get-universal-time))
          (dm:save host))))
    (+ (dm:field host "last-send-time")
       (dm:field host "batch-cooldown"))))

(defun process-send-queue ()
  (loop for host in (dm:get 'host (db:query :all))
        maximize (process-send-queue-for-host host)))

(define-task send-queue ()
  (setf (due-time task) (process-send-queue)))
