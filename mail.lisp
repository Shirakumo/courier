#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defun ensure-mail (mail-ish)
  (or
   (etypecase mail-ish
     (dm:data-model mail-ish)
     (db:id (dm:get-one 'mail (db:query (:= '_id mail-ish))))
     (string (ensure-mail (db:ensure-id mail-ish))))
   (error 'request-not-found :message "No such mail.")))

(defun list-mails (thing &key amount (skip 0) query)
  (macrolet ((query (clause)
               `(if query
                    (let ((query (prepare-query query)))
                      (db:query (:and ,clause
                                      (:or (:matches 'title query)
                                           (:matches 'subject query)
                                           (:matches 'body query)))))
                    (db:query ,clause))))
    (ecase (dm:collection thing)
      (campaign
       (dm:get 'mail (query (:= 'campaign (dm:id thing)))
               :sort '((time :desc)) :amount amount :skip skip))
      (subscriber
       (fixup-ids (dm:get (rdb:join (mail _id) (mail-log mail)) (query (:= 'subscriber (dm:id thing)))
                          :sort '(("send-time" :desc)) :hull 'mail)
                  "mail")))))

(defun make-mail (campaign &key title subject body (type :markless) (save T))
  (let ((campaign (ensure-campaign campaign)))
    (dm:with-model mail ('mail NIL)
      (setf-dm-fields mail title subject body campaign)
      (setf (dm:field mail "time") (get-universal-time))
      (setf (dm:field mail "type") (mail-type-id type))
      (when save
        (dm:insert mail))
      mail)))

(defun edit-mail (mail &key title subject body type (save T))
  (let ((mail (ensure-mail mail)))
    (setf-dm-fields mail title subject body)
    (when type
      (setf (dm:field mail "type") (mail-type-id type)))
    (when save (dm:save mail))
    mail))

(defun delete-mail (mail)
  (let ((mail (ensure-mail mail)))
    (db:with-transaction ()
      (db:update 'feed-entry (db:query (:= 'mail (dm:id mail))) '((mail . NIL)))
      (db:remove 'mail-queue (db:query (:= 'mail (dm:id mail))))
      (db:remove 'mail-log (db:query (:= 'mail (dm:id mail))))
      (db:remove 'mail-receipt (db:query (:= 'mail (dm:id mail))))
      (delete-triggers-for mail)
      (dm:delete mail))))

(defun mark-link-received (link subscriber)
  (db:with-transaction ()
    (unless (link-received-p link subscriber)
      (db:insert 'link-receipt `(("link" . ,(ensure-id link))
                                 ("subscriber" . ,(ensure-id subscriber))
                                 ("time" . ,(get-universal-time))))
      (process-triggers subscriber link))))

(defun mail-received-p (mail subscriber)
  (< 0 (db:count 'mail-receipt (db:query (:and (:= 'mail (ensure-id mail))
                                               (:= 'subscriber (ensure-id subscriber)))))))

(defun mail-sent-p (mail subscriber)
  (< 0 (db:count 'mail-log (db:query (:and (:= 'mail (ensure-id mail))
                                           (:= 'status (mail-status-id :success))
                                           (:= 'subscriber (ensure-id subscriber)))))))

(defun mail-in-queue-p (mail subscriber)
  (< 0 (db:count 'mail-queue (db:query (:and (:= 'mail (ensure-id mail))
                                             (:= 'subscriber (ensure-id subscriber)))))))

(defun mail-coverage (mail)
  (let ((sent (db:count 'mail-log (db:query (:= 'mail (dm:id mail)))))
        (read (db:count 'mail-receipt (db:query (:= 'mail (dm:id mail))))))
    (if (= 0 sent) 0
        (/ read sent))))

(defun mail-sent-count (thing)
  (ecase (dm:collection thing)
    (mail
     (db:count 'mail-log (db:query (:= 'mail (dm:id thing)))))
    (subscriber
     (db:count 'mail-log (db:query (:= 'subscriber (dm:id thing)))))))

(defun mark-mail-received (mail subscriber)
  (db:with-transaction ()
    (unless (mail-received-p mail subscriber)
      (db:insert 'mail-receipt `(("mail" . ,(ensure-id mail))
                                 ("subscriber" . ,(ensure-id subscriber))
                                 ("time" . ,(get-universal-time))))
      (process-triggers subscriber mail))))

(defun mark-mail-sent (mail subscriber &optional (status :success))
  (db:with-transaction ()
    (unless (mail-sent-p mail subscriber)
      (db:insert 'mail-log `(("mail" . ,(dm:id mail))
                             ("subscriber" . ,(dm:id subscriber))
                             ("send-time" . ,(get-universal-time))
                             ("status" . ,(mail-status-id status)))))))

(defun mail-count (thing)
  (ecase (dm:collection thing)
    (campaign (db:count 'mail (db:query (:= 'campaign (dm:id thing)))))))

(defun mail-status-id (status)
  (ecase status
    (:success 0)
    (:unlocked 1)
    (:failed 10)
    (:send-failed 11)
    (:compile-failed 12)
    ((0 1 10 11 12) status)))

(defun id-mail-status (id)
  (ecase id
    (0 :success)
    (1 :unlocked)
    (10 :failed)
    (11 :send-failed)
    (12 :compile-failed)))

(defun mail-type-id (type)
  (ecase type
    (:text 0)
    (:markless 1)
    (:ctml 2)
    ((0 1 2) type)))

(defun id-mail-type (id)
  (ecase id
    (0 :text)
    (1 :markless)
    (2 :ctml)))
