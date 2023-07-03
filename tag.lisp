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
    (db:remove 'mail-tag-table (db:query (:= 'tag (dm:id tag))))
    (delete-triggers-for tag)
    (dm:delete tag)))

(defun list-tags (thing &key amount (skip 0) query)
  (with-query (query title description)
    (ecase (dm:collection thing)
      (campaign
       (dm:get 'tag (query (:= 'campaign (dm:id thing)))
               :sort '((title :asc)) :amount amount :skip skip))
      (subscriber
       (fixup-ids (dm:get (rdb:join (tag _id) (tag-table tag)) (db:query (:= 'subscriber (dm:id thing)))
                          :sort '((title :asc)) :amount amount :skip skip :hull 'tag)
                  "tag"))
      (mail
       (fixup-ids (dm:get (rdb:join (tag _id) (mail-tag-table tag)) (db:query (:= 'mail (dm:id thing)))
                          :sort '((title :asc)) :amount amount :skip skip :hull 'tag)
                  "tag")))))

(defun list-tagged (tag type &key amount (skip 0))
  (ecase type
    (subscriber
     (fixup-ids
      (dm:get (rdb:join (subscriber _id) (tag-table subscriber)) (db:query (:= 'tag (dm:id tag)))
              :sort '((signup-time :DESC)) :amount amount :skip skip :hull 'subscriber)
      "subscriber"))
    (mail
     (fixup-ids
      (dm:get (rdb:join (mail _id) (mail-tag-table mail)) (db:query (:= 'tag (dm:id tag)))
              :sort '((time :DESC)) :amount amount :skip skip :hull 'mail)
      "mail"))))

(defun tagged-p (tag thing)
  (< 0 (ecase (dm:collection thing)
         (subscriber
          (db:count 'tag-table (db:query (:and (:= 'subscriber (ensure-id thing))
                                               (:= 'tag (ensure-id tag))))))
         (mail
          (db:count 'mail-tag-table (db:query (:and (:= 'mail (ensure-id thing))
                                                    (:= 'tag (ensure-id tag)))))))))

(defun tag (thing tag)
  (db:with-transaction ()
    (let ((tag (ensure-tag tag)))
      (unless (tagged-p tag thing)
        (ecase (dm:collection thing)
          (subscriber
           (db:insert 'tag-table `(("tag" . ,(ensure-id tag))
                                   ("subscriber" . ,(ensure-id thing))))
           (process-triggers thing tag))
          (mail
           (db:insert 'mail-tag-table `(("tag" . ,(ensure-id tag))
                                        ("mail" . ,(ensure-id thing))))))))))

(defun untag (thing tag)
  (db:with-transaction ()
    (let ((tag (ensure-tag tag)))
      (when (tagged-p tag thing)
        (ecase (dm:collection thing)
          (subscriber
           (db:remove 'tag-table (db:query (:and (:= 'tag (ensure-id tag))
                                                 (:= 'subscriber (ensure-id thing)))))
           (process-triggers thing (list-source-triggers tag :type 20)))
          (mail
           (db:remove 'mail-tag-table (db:query (:and (:= 'tag (ensure-id tag))
                                                      (:= 'mail (ensure-id thing)))))))))))
