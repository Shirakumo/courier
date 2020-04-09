#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defvar *mail-queue-thread* NIL)

(defun enqueue-email (mail &key target time)
  (let* ((time (or time 0))
         (campaign (dm:get-one 'campaign (db:query (:= '_id (dm:field mail "campaign")))))
         (host (dm:field campaign "host")))
    (flet ((send (subscriber-id)
             (db:insert 'mail-queue `(("host" . ,host)
                                      ("subscriber" . ,subscriber-id)
                                      ("mail" . ,(dm:id mail))
                                      ("time" . ,time)))))
      (unless target
        (setf target campaign))
      (etypecase target
        (dm:data-model
         (ecase (dm:collection target)
           (subscriber
            (if (dm:field target "confirmed")
                (send (dm:id target))
                (error "Subscriber ~a has not yet confirmed their subscription" (dm:field target "address"))))
           (tag
            (mapcar #'send (db:iterate (rdb:join (tab-table subscriber) (subscriber _id))
                             (db:query (:and (:= 'tag (dm:id target))
                                             (:= 'confirmed T)))
                             (lambda (r) (gethash "subscriber" r) :fields '("subscriber") :accumulate T))))
           (campaign
            (mapcar #'send (db:iterate 'subscriber (db:query (:and (:= 'campaign (dm:id target))
                                                                   (:= 'confirmed T)))
                                       (lambda (r) (gethash "_id" r)) :fields '("_id") :accumulate T)))))
        (db:id
         (send target))))))

(defun send-queue (queue)
  (send-email (dm:get-one 'mail (db:query (:= '_id (dm:field queue "mail"))))
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
                                                        (:<= 'time (get-universal-time))))
                            :amount (dm:field host "batch-size") :sort '((time :asc)))))
        (dolist (queue queued)
          (restart-case
              (handler-bind ((error (lambda (e)
                                      (v:error :courier.send-queue "Failed to send queued mail.")
                                      (v:trace :courier.send-queue e)
                                      (if radiance:*debugger*
                                          (invoke-debugger e)
                                          (invoke-restart 'ignore)))))
                (send-queue queue))
            (ignore ()
              :report "Ignore the send and retry later."
              (setf (dm:field queued "time") (+ (get-universal-time) 60))
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
  ;; Return next time we can send stuff again.
  (loop for host in (dm:get 'host (db:query :all))
        maximize (process-send-queue-for-host host)))

(defun send-queue-loop ()
  (loop for next = (process-send-queue)
        for now = (get-universal-time)
        for interval = (config :send-queue-poll-interval)
        do (cond ((< next now)
                  (sleep interval))
                 ((< (- next now) interval)
                  (sleep interval))
                 (T ;; We have work to do soon. Do it early.
                  (sleep (- next now))))
        while (started-p)))

(defun start-send-queue ()
  (when (and *mail-queue-thread*
             (bt:thread-alive-p *mail-queue-thread*))
    (error "The send queue is already running.")) 
  (flet ((send-queue-thunk ()
           (l:info :courier.send-queue "Starting send queue.")
           (unwind-protect (send-queue-loop)
             (l:info :courier.send-queue "Stopping send queue.")
             (setf *mail-queue-thread* NIL))))
    (setf *mail-queue-thread* (bt:make-thread #'send-queue-thunk :name "courier send queue"))))

(define-trigger (server-start start-send-queue) ()
  (unless (and *mail-queue-thread*
               (bt:thread-alive-p *mail-queue-thread*))
    (start-send-queue)))
