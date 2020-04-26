#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defun make-host (&key author title display-name address hostname port username password (encryption 1) (batch-size 10) (batch-cooldown 60) (save T))
  (check-title-exists 'host title (db:query (:and (:= 'author author)
                                                  (:= 'title title))))
  (dm:with-model host ('host NIL)
    (setf-dm-fields host author title display-name address hostname port username encryption batch-size batch-cooldown)
    (when password (setf (dm:field host "password") (encrypt password)))
    (setf (dm:field host "confirmed") NIL)
    (setf (dm:field host "last-send-time") 0)
    (when save (dm:insert host))
    host))

(defun edit-host (host &key author title display-name address hostname port username password encryption batch-size batch-cooldown (confirmed NIL confirmed-p) save)
  (let ((host (ensure-host host)))
    (setf-dm-fields host author title display-name address hostname port username encryption batch-size batch-cooldown)
    (when confirmed-p (setf (dm:field host "confirmed") confirmed))
    (when password (setf (dm:field host "password") (encrypt password)))
    (when save (dm:save host))
    host))

(defun ensure-host (host-ish &optional (user (auth:current)))
  (or
   (etypecase host-ish
     (dm:data-model host-ish)
     (db:id (dm:get-one 'host (db:query (:= '_id host-ish))))
     (T (or (dm:get-one 'host (db:query (:and (:= 'author (user:id user))
                                              (:= 'title host-ish))))
            (dm:get-one 'host (db:query (:= '_id (db:ensure-id host-ish)))))))
   (error 'request-not-found :message "No such host.")))

(defun delete-host (host)
  (db:with-transaction ()
    ;; Don't delete campaigns, just set the host to NULL
    (db:update 'campaign (db:query (:= 'host (dm:id host))) `(("host" . NIL)))
    (db:remove 'mail-queue (db:query (:= 'host (dm:id host))))
    (dm:delete host)
    host))

(defun list-hosts (&optional user &key amount (skip 0))
  (if user
      (dm:get 'host (db:query (:= 'author (user:id user)))
              :sort '((title :asc)) :amount amount :skip skip)
      (dm:get 'host (db:query :all)
              :sort '((title :asc)) :amount amount :skip skip)))
