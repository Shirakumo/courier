#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defun ensure-pool (pool-ish &optional campaign)
  (or
   (etypecase pool-ish
     (dm:data-model pool-ish)
     (db:id (dm:get-one 'pool (db:query (:= '_id pool-ish))))
     (string (or (when campaign
                   (dm:get-one 'pool (db:query (:and (:= 'campaign (dm:id campaign))
                                                     (:= 'title pool-ish)))))
                 (ignore-errors
                  (dm:get-one 'pool (db:query (:= '_id (db:ensure-id pool-ish))))))))
   (error 'request-not-found :message "No such pool.")))

(defun list-pools (campaign &key amount (skip 0) query)
  (with-query (query title url)
    (dm:get 'pool (query (:= 'campaign (ensure-id campaign)))
            :sort '((title :asc)) :skip skip :amount amount)))

(defun make-pool (campaign title &key description entries (save T))
  (let ((pool (dm:hull 'pool)))
    (setf-dm-fields pool campaign title description)
    (when save
      (db:with-transaction ()
        (dm:insert pool)
        (dolist (entry entries)
          (db:insert 'pool-entry `(("pool" . ,(dm:id pool))
                                   ("content" . ,entry))))))
    pool))

(defun edit-pool (pool &key title description entries)
  (db:with-transaction ()
    (let ((pool (ensure-pool pool)))
      (setf-dm-fields pool title description)
      (dm:save pool))
    (dolist (entry entries)
      (db:insert 'pool-entry `(("pool" . ,(dm:id pool))
                               ("content" . ,entry)))))
  pool)

(defun delete-pool (pool)
  (db:with-transaction ()
    (let ((pool (ensure-pool pool)))
      (db:remove 'pool-entry (db:query (:= 'pool (dm:id pool))))
      (dm:delete pool))))

(defun claim-pool-entry (pool subscriber &key entry default)
  (db:with-transaction ()
    (let* ((pool (ensure-pool pool))
           (subscriber (ensure-subscriber subscriber))
           (entry (if entry
                      (etypecase entry
                        (dm:data-model entry)
                        (db:id (dm:get-one 'pool-entry (db:query (:and (:= 'pool (dm:id pool)) (:= '_id entry))))))
                      (or (dm:get-one 'pool-entry (db:query (:and (:= 'pool (dm:id pool))
                                                                  (:= 'claimant (dm:id subscriber)))))
                          (dm:get-one 'pool-entry (db:query (:and (:= 'pool (dm:id pool))
                                                                  (:null 'claimant))))))))
      (cond (entry
             (setf (dm:field entry "claimant") (dm:id subscriber))
             (dm:save entry)
             entry)
            (T
             default)))))

(defun list-pool-entries (pool)
  (dm:get 'pool-entry (db:query (:= 'pool (dm:id (ensure-pool pool)))) :sort '((claimant :desc) (content :asc))))
