#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defvar *prune-thread* NIL)

(defun prune-loop ()
  (loop with false = NIL
        for expiry-time = (- (get-universal-time) (* 60 60 24))
        for next = (dm:get 'subscriber (db:query (:and (:= 'confirmed false)
                                                       (:< 'signup-time expiry-time)))
                       :amount 10)
        do (if next
               (dolist (subscriber next)
                 (l:info :courier.prune "Pruning unconfirmed subscriber ~a" (dm:field subscriber "address"))
                 (delete-subscriber subscriber))
               (sleep (* 60 60)))
        while (started-p)))

(defun start-prune-thread ()
  (when (and *prune-thread*
             (bt:thread-alive-p *prune-thread*))
    (error "The prune thread is already running.")) 
  (flet ((prune-thunk ()
           (l:info :courier.prune "Starting prune thread.")
           (unwind-protect (prune-loop)
             (l:info :courier.prune "Stopping prune thread.")
             (setf *prune-thread* NIL))))
    (setf *prune-thread* (bt:make-thread #'prune-thunk :name "courier prune thread"))))

(define-trigger (server-start start-prune-thread) ()
  (unless (and *prune-thread*
               (bt:thread-alive-p *prune-thread*))
    (start-prune-thread)))
