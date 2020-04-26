#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defparameter *attribute-types*
  '((0 "text" "Free-Form Text")
    (1 "number" "Number")
    (2 "url" "URL")
    (3 "date" "Date")
    (4 "color" "Color")
    (5 "tel" "Telephone Number")))

(defun ensure-campaign (campaign-ish &optional (user (auth:current)))
  (or
   (etypecase campaign-ish
     (dm:data-model campaign-ish)
     (db:id (dm:get-one 'campaign (db:query (:= '_id campaign-ish))))
     (string (or (dm:get-one 'campaign (db:query (:and (:= 'author (user:id user))
                                                       (:= 'title campaign-ish))))
                 (dm:get-one 'campaign (db:query (:= '_id (db:ensure-id campaign-ish)))))))
   (error 'request-not-found :message "No such campaign.")))

(defun list-campaigns (&optional user &key amount (skip 0))
  (if user
      (append ;; KLUDGE: workaround for JOIN thrashing _id when not inner join
       (dm:get 'campaign (db:query (:= 'author (user:id user)))
               :sort '((title :asc)) :amount amount :skip skip)
       (fixup-ids (dm:get (rdb:join (campaign _id) (campaign-access campaign))
                          (db:query (:= 'user (user:id user)))
                          :sort '((title :asc)) :amount amount :skip skip :hull 'campaign)
                  "campaign"))
      (dm:get 'campaign (db:query :all)
              :sort '((title :asc)) :amount amount :skip skip)))

(defun make-campaign (author host title &key description reply-to template attributes address (report-interval (* 60 60 24)) (save T))
  (check-title-exists 'campaign title (db:query (:and (:= 'author author)
                                                      (:= 'title title))))
  (dm:with-model campaign ('campaign NIL)
    (setf-dm-fields campaign title host description reply-to template address report-interval)
    (setf (dm:field campaign "author") (user:id author))
    (setf (dm:field campaign "last-report") (get-universal-time))
    (when save
      (db:with-transaction ()
        (dm:insert campaign)
        (loop for (attribute type required) in attributes
              do (setf type (etypecase type
                              (string (parse-integer type))
                              (integer type)))
                 (unless (find type *attribute-types* :key #'car)
                   (error "Invalid attribute type ~s.~%Must be one of ~s." type (mapcar #'car *attribute-types*)))
                 (db:insert 'attribute `(("campaign" . ,(dm:id campaign))
                                         ("title" . ,attribute)
                                         ("type" . ,type)
                                         ("required" . ,required))))
        (make-subscriber campaign
                         (or (user:field "name" author) (user:username author))
                         reply-to
                         :status :active))
      (notify-task 'report-subscribers))
    campaign))

(defun edit-campaign (campaign &key host author title description reply-to template attributes address report-interval (save T))
  (let ((campaign (ensure-campaign campaign)))
    (setf-dm-fields campaign host author title description reply-to template address report-interval)
    (when save
      (db:with-transaction ()
        (dm:save campaign)
        (let ((existing (list-attributes campaign)))
          (loop for (attribute type required) in attributes
                for previous = (find attribute existing :key (lambda (dm) (dm:field dm "name")) :test #'string=)
                do (setf type (etypecase type
                                (string (parse-integer type))
                                (integer type)))
                   (unless (find type *attribute-types* :key #'car)
                     (error "Invalid attribute type ~s.~%Must be one of ~s." type (mapcar #'car *attribute-types*)))
                   (cond (previous
                          (setf existing (delete previous existing))
                          (setf (dm:field previous "title") attribute)
                          (setf (dm:field previous "type") type)
                          (setf (dm:field previous "required") required)
                          (dm:save previous))
                         (T
                          (db:insert 'attribute `(("campaign" . ,(dm:id campaign))
                                                  ("title" . ,attribute)
                                                  ("type" . ,type)
                                                  ("required" . ,required))))))
          (dolist (attribute existing)
            (db:remove 'attribute-value (db:query (:= 'attribute (dm:id attribute))))
            (dm:delete attribute))))
      (notify-task 'report-subscribers))
    campaign))

(defun campaign-author (campaign)
  (dm:get-one 'subscriber (db:query (:and (:= 'campaign (dm:id campaign))
                                          (:= 'address (dm:field campaign "reply-to"))))))

(defun delete-campaign (campaign)
  (db:with-transaction ()
    (let ((directory (campaign-file-directory campaign)))
      (mapcar #'delete-sequence (list-sequences campaign))
      (mapcar #'delete-subscriber (list-subscribers campaign))
      (mapcar #'delete-trigger (list-triggers campaign))
      (mapcar #'delete-mail (list-mails campaign))
      (mapcar #'delete-tag (list-tags campaign))
      (mapcar #'delete-link (list-links campaign))
      (db:remove 'file (db:query (:= 'campaign (dm:id campaign))))
      (db:remove 'attribute (db:query (:= 'campaign (dm:id campaign))))
      (dm:delete campaign)
      (uiop:delete-directory-tree directory :validate (constantly T) :if-does-not-exist :ignore))))

(defun list-attributes (campaign &key amount (skip 0))
  (when (dm:id campaign)
    (dm:get 'attribute (db:query (:= 'campaign (ensure-id campaign)))
            :sort '((title :asc)) :amount amount :skip skip)))

(defun set-access (user campaign &key access-field (access-level 0))
  (let ((access (or (dm:get-one 'campaign-access (db:query (:and (:= 'campaign (ensure-id campaign))
                                                                 (:= 'user (user:id user)))))
                    (dm:hull 'campaign-access)))
        (access-field (or access-field (access-field access-level))))
    (cond ((<= access-field 0)
           (unless (dm:hull-p access)
             (dm:delete access)))
          (T
           (setf-dm-fields access campaign access-field)
           (setf (dm:field access "user") (user:id user))
           (if (dm:hull-p access)
               (dm:insert access)
               (dm:save access))))))

(defun list-access (campaign &key amount (skip 0))
  (dm:get 'campaign-access (db:query (:= 'campaign (ensure-id campaign)))
          :sort `((user :asc)) :amount amount :skip skip))

(defun send-subscriber-update (campaign &optional since)
  (let* ((campaign (ensure-campaign campaign))
         (since (or since (dm:field campaign "last-report")))
         (new (new-subscribers-since campaign since)))
    (send-system-mail
     (@template "email/subscriber-report.mess")
     (dm:field campaign "reply-to")
     (dm:field campaign "host")
     campaign
     :subject (format NIL "Mailing list subscription report for ~a" (dm:field campaign "title"))
     :campaign (dm:field campaign "title")
     :count (length new)
     :subscribers new)
    (db:with-transaction ()
      ;; FIXME: rejigger this so that reports always happen at specified times of the day
      (setf (dm:field campaign "last-report") (get-universal-time))
      (dm:save campaign))))

(defun report-subscribers (&optional campaign)
  (if campaign
      (let ((campaign (ensure-campaign campaign))
            (next-report (+ (dm:field campaign "report-interval")
                            (dm:field campaign "last-report"))))
        (cond ((< 0 (dm:field campaign "report-interval"))
               (when (<= next-report (get-universal-time))
                 (send-subscriber-update campaign)
                 (setf next-report (+ (get-universal-time) (dm:field campaign "report-interval"))))
               next-report)
              (T
               (+ (get-universal-time) (* 60 60 24 365 100)))))
      (loop for campaign in (list-campaigns)
            minimize (report-subscribers campaign))))

(define-task report-subscribers ()
  (setf (due-time task) (report-subscribers)))
