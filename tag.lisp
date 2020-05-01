#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defun make-tag (campaign &key title description (save T))
  (let ((campaign (ensure-campaign campaign)))
    (check-title-exists 'tag title (db:query (:and (:= 'campaign (dm:id campaign))
                                                   (:= 'title title))))
    (dm:with-model tag ('tag NIL)
      (setf-dm-fields tag campaign title description)
      (when save (dm:insert tag))
      tag)))

(defun edit-tag (tag-ish &key title description (save T))
  (let ((tag (ensure-tag tag-ish)))
    (setf-dm-fields tag title description)
    (when save (dm:save tag))
    tag))

(defun ensure-tag (tag-ish)
  (or
   (etypecase tag-ish
     (dm:data-model tag-ish)
     (db:id (dm:get-one 'tag (db:query (:= '_id tag-ish))))
     (string (ensure-tag (db:ensure-id tag-ish))))
   (error 'request-not-found :message "No such tag.")))

(defun delete-tag (tag)
  (db:with-transaction ()
    (db:remove 'tag-table (db:query (:= 'tag (dm:id tag))))
    (delete-triggers-for tag)
    (dm:delete tag)))

(defun list-tags (thing &key amount (skip 0) query)
  (with-query (query title description)
    (ecase (dm:collection thing)
      (campaign
       (dm:get 'tag (query (:= 'campaign (dm:id thing)))
               :sort '((title :asc)) :amount amount :skip skip))
      (subscriber
       (fixup-ids (dm:get (rdb:join (tag _id) (tag-table tag)) (query (:= 'subscriber (dm:id thing)))
                          :sort '((title :asc)) :amount amount :skip skip :hull 'tag)
                  "tag")))))

(defun tagged-p (tag subscriber)
  (< 0 (db:count 'tag-table (db:query (:and (:= 'subscriber (ensure-id subscriber))
                                            (:= 'tag (ensure-id tag)))))))

(defun tag (subscriber tag)
  (db:with-transaction ()
    (let ((tag (ensure-tag tag)))
      (unless (tagged-p tag subscriber)
        (db:insert 'tag-table `(("tag" . ,(ensure-id tag))
                                ("subscriber" . ,(ensure-id subscriber))))
        (process-triggers subscriber tag)))))

(defun untag (subscriber tag)
  (db:with-transaction ()
    (let ((tag (ensure-tag tag)))
      (when (tagged-p tag subscriber)
        (db:remove 'tag-table (db:query (:and (:= 'tag (ensure-id tag))
                                              (:= 'subscriber (ensure-id subscriber)))))
        (process-triggers subscriber (list-source-triggers tag :type 20))))))
