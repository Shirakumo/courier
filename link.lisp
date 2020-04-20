#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defun ensure-link (link-ish)
  (or
   (etypecase link-ish
     (dm:data-model link-ish)
     (db:id (dm:get-one 'link (db:query (:= '_id link-ish))))
     (T (ensure-link (db:ensure-id link-ish))))
   (error 'request-not-found :message "No such link.")))

(defun list-links (campaign &key amount (skip 0))
  (dm:get 'link (db:query (:= 'campaign (dm:id campaign)))
          :sort '((url :asc)) :amount amount :skip skip))

(defun make-link (campaign &key url (save T))
  (let ((hash (cryptos:sha256 url :to :base64)))
    (or (dm:get-one 'link (db:query (:and (:= 'campaign (dm:id campaign))
                                          (:= 'hash hash))))
        (dm:with-model link ('link NIL)
          (setf-dm-fields link url hash campaign)
          (when save (dm:insert link))
          link))))

(defun delete-link (link-ish)
  (db:with-transaction ()
    (let ((link (ensure-link link-ish)))
      (db:remove 'link-receipt (db:query (:= link (dm:id link))))
      (delete-triggers-for link)
      (dm:delete link))))

(defun link-received-p (link subscriber)
  (< 0 (db:count 'link-receipt (db:query (:and (:= 'link (ensure-id link))
                                               (:= 'subscriber (ensure-id subscriber)))))))

(defun link-coverage (link)
  (/ (db:count 'link-receipt (db:query (:= 'link (dm:id link))))
     (db:count 'subscriber (db:query (:= 'campaign (dm:field link "campaign"))))))
