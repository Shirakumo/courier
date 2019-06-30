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
               (address (:varchar 64))
               (signup-time (:integer 5))
               (confirmed :boolean))
             :indices '(campaign confirmed))

  (db:create 'attribute
             '((campaign (:id campaign))
               (title (:varchar 32))
               (type (:varchar 16)))
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

  (db:create 'tag
             '((campaign (:id campaign))
               (title (:varchar 32))
               (description :text))
             :indices '(campaign))

  (db:create 'tag-table
             '((tag (:id tag))
               (subscriber (:id subscriber)))
             :indices '(tag subscriber))

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
             :indices '(tag-trigger))

  (db:create 'link
             '((campaign (:id campaign))
               (title (:varchar 32))
               (url (:varchar 256)))
             :indices '(campaign mail))

  (db:create 'link-receipt
             '((link (:id link))
               (subscriber (:id subscriber))
               (time (:integer 5)))
             :indices '(link subscriber)))

(define-trigger startup-done ()
  (defaulted-config "Courier Mailing" :title)
  (defaulted-config "Shirakumo" :copyright)
  (defaulted-config NIL :registration-open)
  (defaulted-config (make-random-string 32) :private-key))

;; FIXME: check values

(defun make-host (&key author title address hostname port username password (encryption 1) (batch-size 100) (batch-cooldown 60) confirmed (save T))
  (check-title-exists 'host title (db:query (:and (:= 'author author)
                                                  (:= 'title title))))
  (dm:with-model host ('host NIL)
    (setf-dm-fields host author title address hostname port username encryption batch-size batch-cooldown)
    (when password (setf (dm:field host "password") (encrypt password)))
    (setf (dm:field host "confirmed") NIL)
    (when save (dm:insert host))
    host))

(defun edit-host (host)
  ;; TODO: this
  )

(defun ensure-host (host-ish &optional (user (auth:current)))
  (or
   (etypecase host-ish
     (dm:data-model host-ish)
     (string (dm:get-one 'host (db:query (:and (:= 'author (user:id user))
                                               (:= 'title host-ish)))))
     (db:id (dm:get-one 'host (db:query (:= '_id host-ish)))))
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
      (loop for (attribute . type) in attributes
            do (unless (find type *attribute-types* :key #'car :test #'string=)
                 (error "Invalid attribute type ~s.~%Must be one of ~s." type (mapcar #'car *attribute-types*)))
               (db:insert 'attribute `(("campaign" . ,(dm:id campaign))
                                       ("name" . ,attribute)
                                       ("type" . ,type)))))
    campaign))

(defun edit-campaign (campaign)
  ;; TODO: this
  (db:with-transaction ()
    (db:remove 'attribute-value (db:query (:= 'attribute (dm:id attribute))))
    (dm:delete attribute)))

(defun ensure-campaign (campaign-ish &optional (user (auth:current)))
  (or
   (etypecase campaign-ish
     (dm:data-model campaign-ish)
     (string (dm:get-one 'campaign (db:query (:and (:= 'author (user:id user))
                                                   (:= 'title campaign-ish)))))
     (db:id (dm:get-one 'campaign (db:query (:= '_id campaign-ish)))))
   (error 'request-not-found :message "No such campaign.")))

(defun delete-campaign (campaign-ish &optional (user (auth:current)))
  (db:with-transaction ()
    (let ((campaign (ensure-campaign campaign-ish user)))
      ;; FIXME: cascade
      (dm:delete campaign))))

(defun list-campaigns (&optional (user (auth:current)))
  (dm:get 'campaign (db:query (:= 'author (user:id user))) :sort '((title :asc))))

(defun list-attributes (campaign-ish &optional (user (auth:current)))
  (let ((campaign (ensure-campaign campaign-ish user)))
    (dm:get 'attribute (db:query (:= 'campaign (dm:id campaign))) :sort '((name :asc)))))

(defun make-mail (campaign &key title subject body (save T))
  (let ((campaign (ensure-campaign campaign)))
    (check-title-exists 'mail title (db:query (:and (:= 'campaign (dm:id campaign))
                                                    (:= 'title title))))
    (dm:with-model mail ('mail NIL)
      (setf-dm-fields mail title subject body)
      (setf (dm:field mail "campaign") (dm:id campaign))
      (when save (dm:insert mail))
      mail)))

(defun edit-mail (mail)
  ;; TODO: this
  )

(defun ensure-mail (campaign mail-ish &optional (user (auth:current)))
  (or
   (etypecase mail-ish
     (dm:data-model mail-ish)
     (string (dm:get-one 'mail (db:query (:and (:= 'campaign (dm:id (ensure-campaign campaign user)))
                                               (:= 'title mail-ish)))))
     (db:id (dm:get-one 'mail (db:query (:= '_id mail-ish)))))
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
      (dm:insert trigger)
      (loop for (tag . inverted) in constraints
            do (db:insert 'mail-trigger-tags `(("mail-trigger" . ,(dm:id trigger))
                                               ("tag" . ,(dm:id tag))
                                               ("inverted" . ,inverted)))))
    trigger))

(defun edit-mail-trigger (mail-trigger)
  ;; TODO: this
  )

(defun delete-mail-trigger (mail-trigger)
  (db:with-transaction ()
    (db:remove 'mail-trigger-tags (db:query (:= 'mail-trigger (dm:id mail-trigger))))
    (dm:delete mail-trigger)))

(defun list-mail-triggers (mail)
  (let ((id (etypecase mail
              (dm:data-model (dm:id mail))
              (db:id mail))))
    (dm:get 'mail-trigger (db:query (:= 'mail id)) :sort '((time-offset :asc)))))

(defun list-mail-trigger-tags (trigger)
  (let ((id (etypecase trigger
              (dm:data-model (dm:id trigger))
              (db:id trigger))))
    (dm:get 'mail-trigger-tags (db:query (:= 'mail-trigger id)))))

(defun make-tag (campaign &key title description (save T))
  (let ((campaign (ensure-campaign campaign)))
    (check-title-exists 'tag title (db:query (:and (:= 'campaign (dm:id campaign))
                                                   (:= 'title title))))
    (dm:with-model tag ('tag NIL)
      (setf-dm-fields tag title description)
      (setf (dm:field tag "campaign") (dm:id campaign))
      (when save (dm:insert tag))
      tag)))

(defun ensure-tag (campaign tag-ish &optional (user (auth:current)))
  (or
   (etypecase tag-ish
     (dm:data-model tag-ish)
     (string (dm:get-one 'tag (db:query (:and (:= 'campaign (dm:id (ensure-campaign campaign user)))
                                              (:= 'title tag-ish)))))
     (db:id (dm:get-one 'tag (db:query (:= '_id tag-ish)))))
   (error 'request-not-found :message "No such tag.")))

(defun delete-tag (tag)
  (db:with-transaction ()
    ;; FIXME: cascade
    (dm:delete tag)))

(defun list-tags (campaign-ish &optional (user (auth:current)))
  (let ((campaign (ensure-campaign campaign-ish user)))
    (dm:get 'tag (db:query (:= 'campaign (dm:id campaign))) :sort '((title :asc)))))

(defun make-link (campaign &key title url (save T))
  (dm:with-model link ('link NIL)
    (setf-dm-fields link title url)
    (setf (dm:field link "campaign") (dm:id campaign))
    (when save (dm:insert link))
    link))

(defun ensure-link (link-ish)
  (or
   (etypecase link-ish
     (dm:data-model link-ish)
     (db:id (dm:get-one 'link (db:query (:= '_id link-ish)))))
   (error 'request-not-found :message "No such link.")))

(defun delete-link (link-ish)
  (db:with-transaction ()
    (let ((link (ensure-link link-ish)))
      ;; FIXME: cascade
      (dm:delete link))))

(defun list-links (campaign-ish &optional (user (auth:current)))
  (let ((campaign (ensure-campaign campaign-ish user)))
    (dm:get 'link (db:query (:= 'campaign (dm:id campaign))) :sort '((title :asc)))))
