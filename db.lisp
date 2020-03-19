#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defparameter *attribute-types*
  '(("name" . "Name")
    ("text" . "Free-Form Text")
    ("number" . "Number")
    ("url" . "URL")
    ("date" . "Date")
    ("color" . "Color")
    ("password" . "Password")
    ("tel" . "Telephone Number")))

(define-trigger db:connected ()
  (db:create 'host
             '((author :id)
               (title (:varchar 32))
               (address (:varchar 64))
               (hostname (:varchar 64))
               (port :integer)
               (username (:varchar 32))
               (password (:varchar 128))
               (encryption (:integer 1))
               (batch-size :integer)
               (batch-cooldown :integer)
               (last-send-time (:integer 5))
               (confirmed :boolean))
             :indices '(author title))

  (db:create 'campaign
             '((host (:id host))
               (author :id)
               (title (:varchar 32))
               (description :text)
               (time (:integer 5))
               (reply-to (:varchar 64))
               (template :text))
             :indices '(author title))

  (db:create 'subscriber
             '((campaign (:id campaign))
               (name (:varchar 64))
               (address (:varchar 64))
               (signup-time (:integer 5))
               (confirmed :boolean))
             :indices '(campaign confirmed))

  (db:create 'attribute
             '((campaign (:id campaign))
               (title (:varchar 32))
               (type (:varchar 16))
               (required :boolean))
             :indices '(campaign))

  (db:create 'attribute-value
             '((attribute (:id attribute))
               (subscriber (:id subscriber))
               (value (:varchar 64)))
             :indices '(attribute subscriber))

  (db:create 'mail
             '((campaign (:id campaign))
               (title (:varchar 32))
               (subject (:varchar 128))
               (body :text))
             :indices '(campaign title))

  (db:create 'mail-receipt
             '((mail (:id mail))
               (subscriber (:id subscriber))
               (time (:integer 5)))
             :indices '(mail subscriber))

  (db:create 'mail-log
             '((mail (:id mail))
               (subscriber (:id subscriber))
               (time (:integer 5)))
             :indices '(mail subscriber))

  (db:create 'mail-queue
             '((host (:id host))
               (subscriber (:id subscriber))
               (mail (:id mail))
               (time (:integer 5)))
             :indices '(host subscriber))

  (db:create 'tag
             '((campaign (:id campaign))
               (title (:varchar 32))
               (description :text))
             :indices '(campaign))

  (db:create 'tag-table
             '((tag (:id tag))
               (subscriber (:id subscriber)))
             :indices '(tag subscriber))

  (db:create 'link
             '((campaign (:id campaign))
               (hash (:varchar 44))
               (url (:varchar 256)))
             :indices '(campaign hash))

  (db:create 'link-receipt
             '((link (:id link))
               (subscriber (:id subscriber))
               (time (:integer 5)))
             :indices '(link subscriber))

  (db:create 'mail-trigger
             '((mail (:id mail))
               (to-mail (:id mail))
               (to-link (:id link))
               (time-offset (:integer 5)))
             :indices '(mail to-mail to-link))

  (db:create 'mail-trigger-tags
             '((mail-trigger (:id mail-trigger))
               (tag (:id tag))
               (inverted :boolean))
             :indices '(mail-trigger))

  (db:create 'tag-trigger
             '((tag (:id tag))
               (to-mail (:id mail))
               (to-link (:id link))
               (time-offset (:integer 5)))
             :indices '(tag to-mail to-link))

  (db:create 'tag-trigger-tags
             '((tag-trigger (:id tag-trigger))
               (tag (:id tag))
               (inverted :boolean))
             :indices '(tag-trigger)))

;; FIXME: check values

(defun ensure-id (id-ish)
  (etypecase id-ish
    (db:id id-ish)
    (dm:data-model (dm:id id-ish))))

(defun make-host (&key author title address hostname port username password (encryption 1) (batch-size 100) (batch-cooldown 60) (save T))
  (check-title-exists 'host title (db:query (:and (:= 'author author)
                                                  (:= 'title title))))
  (dm:with-model host ('host NIL)
    (setf-dm-fields host author title address hostname port username encryption batch-size batch-cooldown)
    (when password (setf (dm:field host "password") (encrypt password)))
    (setf (dm:field host "confirmed") NIL)
    (setf (dm:field host "last-send-time") 0)
    (when save (dm:insert host))
    host))

(defun edit-host (host &key author title address hostname port username password encryption batch-size batch-cooldown confirmed save)
  (let ((host (ensure-host host)))
    (setf-dm-fields host author title address hostname port username password encryption batch-size batch-cooldown confirmed)
    (when save (dm:save host))
    host))

(defun ensure-host (host-ish &optional (user (auth:current)))
  (or
   (etypecase host-ish
     (dm:data-model host-ish)
     (db:id (dm:get-one 'host (db:query (:= '_id host-ish))))
     (string (dm:get-one 'host (db:query (:and (:= 'author (user:id user))
                                               (:= 'title host-ish))))))
   (error 'request-not-found :message "No such host.")))

(defun delete-host (host-ish &optional (user (auth:current)))
  (db:with-transaction ()
    (let ((host (ensure-host host-ish user)))
      ;; FIXME: cascade
      (dm:delete host)
      host)))

(defun list-hosts (&optional (user (auth:current)))
  (dm:get 'host (db:query (:= 'author (user:id user))) :sort '((title :asc))))

(defun make-campaign (&key host author title description reply-to template attributes (save T))
  (check-title-exists 'campaign title (db:query (:and (:= 'author author)
                                                      (:= 'title title))))
  (dm:with-model campaign ('campaign NIL)
    (setf-dm-fields campaign author title description reply-to template)
    (when host (setf (dm:field campaign "host") (dm:id (ensure-host host))))
    (when save
      (dm:insert campaign)
      (loop for (attribute type required) in attributes
            do (unless (find type *attribute-types* :key #'car :test #'string=)
                 (error "Invalid attribute type ~s.~%Must be one of ~s." type (mapcar #'car *attribute-types*)))
               (db:insert 'attribute `(("campaign" . ,(dm:id campaign))
                                       ("name" . ,attribute)
                                       ("type" . ,type)
                                       ("required" . ,required)))))
    campaign))

(defun edit-campaign (campaign &key host author title description reply-to template attributes (save T))
  (let ((campaign (ensure-campaign campaign)))
    (setf-dm-fields campaign host author title description reply-to template)
    (db:with-transaction ()
      (let ((existing (list-attributes campaign)))
        (loop for (attribute type required) in attributes
              for previous = (find attribute existing :key (lambda (dm) (dm:field dm "name")))
              do (unless (find type *attribute-types* :key #'car :test #'string=)
                   (error "Invalid attribute type ~s.~%Must be one of ~s." type (mapcar #'car *attribute-types*)))
                 (cond (previous
                        (setf existing (delete previous existing))
                        (setf (dm:field previous "name") attribute)
                        (setf (dm:field previous "type") type)
                        (setf (dm:field previous "required") required)
                        (dm:save previous))
                       (T
                        (db:insert 'attribute `(("campaign" . ,(dm:id campaign))
                                                ("name" . ,attribute)
                                                ("type" . ,type)
                                                ("required" . ,required))))))
        (dolist (attribute existing)
          (db:remove 'attribute-value (db:query (:= 'attribute (dm:id attribute))))
          (dm:delete attribute))))
    (when save (dm:save campaign))
    campaign))

(defun ensure-campaign (campaign-ish &optional (user (auth:current)))
  (or
   (etypecase campaign-ish
     (dm:data-model campaign-ish)
     (db:id (dm:get-one 'campaign (db:query (:= '_id campaign-ish))))
     (string (or (dm:get-one 'campaign (db:query (:= '_id (db:ensure-id campaign-ish))))
                 (dm:get-one 'campaign (db:query (:and (:= 'author (user:id user))
                                                       (:= 'title campaign-ish)))))))
   (error 'request-not-found :message "No such campaign.")))

(defun delete-campaign (campaign-ish &optional (user (auth:current)))
  (db:with-transaction ()
    (let ((campaign (ensure-campaign campaign-ish user)))
      ;; FIXME: cascade
      (dm:delete campaign))))

(defun list-campaigns (&optional (user (auth:current)))
  (dm:get 'campaign (db:query (:= 'author (user:id user))) :sort '((title :asc))))

(defun list-attributes (campaign)
  (dm:get 'attribute (db:query (:= 'campaign (ensure-id campaign))) :sort '((name :asc))))

(defun make-subscriber (campaign name address attributes &optional confirmed)
  (db:with-transaction ()
    (when (< 0 (db:count 'subscriber (db:query (:and (:= 'campaign (dm:id campaign))
                                                     (:= 'address address)))))
      (error "You are already subscribed!"))
    (dm:with-model subscriber ('subscriber NIL)
      (setf (dm:field subscriber "campaign") (dm:id campaign))
      (setf (dm:field subscriber "name") name)
      (setf (dm:field subscriber "address") address)
      (setf (dm:field subscriber "signup-time") (get-universal-time))
      (setf (dm:field subscriber "confirmed") confirmed)
      (dm:insert subscriber)
      
      (loop for (attribute . value) in attributes
            do (dm:with-model attribute-value ('attribute-value NIL)
                 (setf (dm:field attribute-value "attribute") (ensure-id attribute))
                 (setf (dm:field attribute-value "subscriber") (print (dm:id subscriber)))
                 (setf (dm:field attribute-value "value") value)
                 (dm:insert attribute-value)))
      subscriber)))

(defun ensure-subscriber (subscriber-ish)
  (or
   (etypecase subscriber-ish
     (dm:data-model subscriber-ish)
     (T (dm:get-one 'subscriber (db:query (:= '_id (db:ensure-id subscriber-ish))))))
   (error 'request-not-found :message "No such subscriber.")))

(defun delete-subscriber (subscriber)
  (db:with-transaction ()
    ;; FIXME: cascade
    (dm:delete subscriber)))

(defun subscriber-attributes (subscriber)
  (loop for attribute in (db:select (rdb:join (attribute _id) (attribute-value attribute))
                                    (db:query (:= 'subscriber (ensure-id subscriber))))
        collect (gethash "title" attribute)
        collect (gethash "value" attribute)))

(defun subscriber-tags (subscriber)
  (loop for tag in (db:select (rdb:join (tag _id) (tag-table tag))
                              (db:query (:= 'subscriber (ensure-id subscriber))))
        collect (gethash "title" tag)))

(defun make-mail (campaign &key title subject body (save T))
  (let ((campaign (ensure-campaign campaign)))
    (check-title-exists 'mail title (db:query (:and (:= 'campaign (dm:id campaign))
                                                    (:= 'title title))))
    (dm:with-model mail ('mail NIL)
      (setf-dm-fields mail title subject body)
      (setf (dm:field mail "campaign") (dm:id campaign))
      (when save (dm:insert mail))
      mail)))

(defun edit-mail (mail &key title subject body (save T))
  (setf-dm-fields mail title subject body)
  (when save (dm:save mail))
  mail)

(defun ensure-mail (mail-ish)
  (or
   (etypecase mail-ish
     (dm:data-model mail-ish)
     (db:id (dm:get-one 'mail (db:query (:= '_id mail-ish))))
     (string (ensure-mail (db:ensure-id mail-ish))))
   (error 'request-not-found :message "No such mail.")))

(defun delete-mail (mail)
  (db:with-transaction ()
    ;; FIXME: cascade
    (dm:delete mail)))

(defun list-mails (campaign-ish &optional (user (auth:current)))
  (let ((campaign (ensure-campaign campaign-ish user)))
    (dm:get 'mail (db:query (:= 'campaign (dm:id campaign))) :sort '((title :asc)))))

(defun make-mail-trigger (mail &key to-mail to-link (time-offset 0) constraints (save T))
  (dm:with-model trigger ('mail-trigger NIL)
    (setf-dm-fields trigger time-offset)
    (setf (dm:field trigger "mail") (dm:id mail))
    (when to-mail (setf (dm:field trigger "to-mail") (dm:id to-mail)))
    (when to-link (setf (dm:field trigger "to-link") (dm:id to-link)))
    (setf (dm:field trigger "time-offset") time-offset)
    (when save
      (db:with-transaction ()
        (dm:insert trigger)
        (loop for (tag inverted) in constraints
              do (db:insert 'mail-trigger-tags `(("mail-trigger" . ,(dm:id trigger))
                                                 ("tag" . ,(dm:id tag))
                                                 ("inverted" . ,inverted))))))
    trigger))

(defun edit-mail-trigger (mail-trigger &key (to-mail NIL to-mail-p) (to-link NIL to-link-p) time-offset constraints (save T))
  (setf-dm-fields mail-trigger time-offset)
  (when to-mail-p (setf (dm:field mail-trigger "to-mail") (dm:id to-mail)))
  (when to-link-p (setf (dm:field mail-trigger "to-link") (dm:id to-link)))
  (when save
    (db:with-transaction ()
      (dm:save mail-trigger)
      (db:remove 'mail-trigger-tags (db:query (:= 'mail-trigger (dm:id mail-trigger))))
      (loop for (tag inverted) in constraints
            do (db:insert 'mail-trigger-tags `(("mail-trigger" . ,(dm:id mail-trigger))
                                               ("tag" . ,(dm:id tag))
                                               ("inverted" . ,inverted))))))
  mail-trigger)

(defun delete-mail-trigger (mail-trigger)
  (db:with-transaction ()
    (db:remove 'mail-trigger-tags (db:query (:= 'mail-trigger (dm:id mail-trigger))))
    (dm:delete mail-trigger)))

(defun ensure-mail-trigger (mail-trigger-ish)
  (or
   (etypecase mail-trigger-ish
     (dm:data-model mail-trigger-ish)
     (db:id (dm:get-one 'mail-trigger (db:query (:= '_id mail-trigger-ish))))
     (T (ensure-mail-trigger (db:ensure-id mail-trigger-ish))))
   (error 'request-not-found :message "No such mail-trigger.")))

(defun make-tag (campaign &key title description triggers (save T))
  (let ((campaign (ensure-campaign campaign)))
    (check-title-exists 'tag title (db:query (:and (:= 'campaign (dm:id campaign))
                                                   (:= 'title title))))
    (dm:with-model tag ('tag NIL)
      (setf-dm-fields tag title description)
      (setf (dm:field tag "campaign") (dm:id campaign))
      (when save
        (db:with-transaction ()
          (dm:insert tag)
          (loop for args in triggers
                do (apply #'make-tag-trigger tag args))))
      tag)))

(defun edit-tag (tag-ish &key title description triggers (save T))
  (let ((tag (ensure-tag tag-ish)))
    (setf-dm-fields tag title description)
    (when save
      (db:with-transaction ()
        (dm:save tag)
        ;; FIXME: suboptimal...
        (db:remove (rdb:join (tag-trigger-tags tag-trigger) (tag-trigger _id)) (db:query (:= 'tag (dm:id tag))))
        (db:remove 'tag-trigger (db:query (:= 'tag (dm:id tag))))
        (loop for args in triggers
              do (apply #'make-tag-trigger tag args))))
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
    ;; FIXME: cascade
    (dm:delete tag)))

(defun list-tags (campaign-ish &optional (user (auth:current)))
  (let ((campaign (ensure-campaign campaign-ish user)))
    (dm:get 'tag (db:query (:= 'campaign (dm:id campaign))) :sort '((title :asc)))))

(defun make-tag-trigger (tag &key to-mail to-link (time-offset 0) constraints (save T))
  (dm:with-model trigger ('tag-trigger NIL)
    (setf-dm-fields trigger time-offset)
    (setf (dm:field trigger "tag") (dm:id tag))
    (when to-mail (setf (dm:field trigger "to-mail") (dm:id to-mail)))
    (when to-link (setf (dm:field trigger "to-link") (dm:id to-link)))
    (setf (dm:field trigger "time-offset") time-offset)
    (when save
      (db:with-transaction ()
        (dm:insert trigger)
        (loop for (tag inverted) in constraints
              do (db:insert 'tag-trigger-tags `(("tag-trigger" . ,(dm:id trigger))
                                                ("tag" . ,(dm:id tag))
                                                ("inverted" . ,inverted))))))
    trigger))

(defun edit-tag-trigger (tag-trigger &key (to-mail NIL to-mail-p) (to-link NIL to-link-p) time-offset constraints (save T))
  (setf-dm-fields tag-trigger time-offset)
  (when to-mail-p (setf (dm:field tag-trigger "to-mail") (dm:id to-mail)))
  (when to-link-p (setf (dm:field tag-trigger "to-link") (dm:id to-link)))
  (when save
    (db:with-transaction ()
      (dm:save tag-trigger)
      (db:remove 'tag-trigger-tags (db:query (:= 'tag-trigger (dm:id tag-trigger))))
      (loop for (tag inverted) in constraints
            do (db:insert 'tag-trigger-tags `(("tag-trigger" . ,(dm:id tag-trigger))
                                               ("tag" . ,(dm:id tag))
                                               ("inverted" . ,inverted))))))
  tag-trigger)

(defun ensure-tag-trigger (tag-trigger-ish)
  (or
   (etypecase tag-trigger-ish
     (dm:data-model tag-trigger-ish)
     (db:id (dm:get-one 'tag-trigger (db:query (:= '_id tag-trigger-ish))))
     (T (ensure-tag-trigger (db:ensure-id tag-trigger-ish))))
   (error 'request-not-found :message "No such tag-trigger.")))

(defun triggers (thing)
  (when (dm:id thing)
    (ecase (dm:collection thing)
      (tag (dm:get 'tag-trigger (db:query (:= 'tag (dm:id thing)))))
      (mail (dm:get 'mail-trigger (db:query (:= 'mail (dm:id thing))))))))

(defun trigger-tags (thing)
  (when (dm:id thing)
    (ecase (dm:collection thing)
      (tag-trigger (dm:get (rdb:join (tag-trigger-tags tag) (tag _id)) (db:query (:= 'tag-trigger (dm:id thing)))))
      (mail-trigger (dm:get (rdb:join (mail-trigger-tags tag) (tag _id)) (db:query (:= 'mail-trigger (dm:id thing))))))))

(defun make-link (campaign &key url (save T))
  (let ((hash (cryptos:sha256 url :to :base64)))
    (or (dm:get-one 'link (db:query (:and (:= 'campaign (dm:id campaign))
                                          (:= 'hash hash))))
        (dm:with-model link ('link NIL)
          (setf (dm:field link "url") url)
          (setf (dm:field link "hash") hash)
          (setf (dm:field link "campaign") (dm:id campaign))
          (when save (dm:insert link))
          link))))

(defun ensure-link (link-ish)
  (or
   (etypecase link-ish
     (dm:data-model link-ish)
     (db:id (dm:get-one 'link (db:query (:= '_id link-ish))))
     (T (ensure-link (db:ensure-id link-ish))))
   (error 'request-not-found :message "No such link.")))

(defun delete-link (link-ish)
  (db:with-transaction ()
    (let ((link (ensure-link link-ish)))
      ;; FIXME: cascade
      (dm:delete link))))

(defun list-links (campaign-ish &optional (user (auth:current)))
  (let ((campaign (ensure-campaign campaign-ish user)))
    (dm:get 'link (db:query (:= 'campaign (dm:id campaign))) :sort '((title :asc)))))

(defun link-received-p (link subscriber)
  (< 0 (db:count 'link-receipt (db:query (:and (:= 'link (ensure-id link))
                                               (:= 'subscriber (ensure-id subscriber)))))))

(defun link-coverage (link)
  (/ (db:count 'link-receipt (db:query (:= 'link (dm:id link))))
     (db:count 'subscriber (db:query (:= 'campaign (dm:field link "campaign"))))))

(defun mark-link-received (link subscriber)
  (db:with-transaction ()
    (unless (link-received-p link subscriber)
      (db:insert 'link-receipt `(("link" . ,(ensure-id link))
                                 ("subscriber" . ,(ensure-id subscriber))
                                 ("time" . ,(get-universal-time)))))))

(defun mail-received-p (mail subscriber)
  (< 0 (db:count 'mail-receipt (db:query (:and (:= 'mail (ensure-id mail))
                                               (:= 'subscriber (ensure-id subscriber)))))))

(defun mail-coverage (mail)
  (/ (db:count 'mail-receipt (db:query (:= 'mail (dm:id mail))))
     (db:count 'subscriber (db:query (:= 'campaign (dm:field mail "campaign"))))))

(defun mark-mail-received (mail subscriber)
  (db:with-transaction ()
    (unless (mail-received-p mail subscriber)
      (db:insert 'mail-receipt `(("mail" . ,(ensure-id mail))
                                 ("subscriber" . ,(ensure-id subscriber))
                                 ("time" . ,(get-universal-time)))))))
