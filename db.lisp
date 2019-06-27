#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

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
               (name (:varchar 32))
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
               (mail (:id mail))
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

(defmacro setf-dm-fields (model &rest vars)
  (let ((modelg (gensym "MODEL")))
    `(let ((,modelg ,model))
       ,@(loop for var in vars
               collect (destructuring-bind (var field) (radiance::enlist var (string-downcase var))
                         `(when ,var (setf (dm:field ,modelg ,field) ,var))))
       ,modelg)))

(defun make-host (&key author title address hostname port username password (encryption 1) (batch-size 100) (batch-cooldown 60) confirmed (save T))
  (dm:with-model host ('host NIL)
    (when (and title (< 0 (db:count 'host (db:query (:and (:= 'author author)
                                                          (:= 'title title))))))
      (error 'api-argument-invalid
             :argument 'title
             :message "A host with that title already exists."))
    (setf-dm-fields host author title address hostname port username encryption batch-size batch-cooldown)
    (when password (setf (dm:field host "password") (encrypt password)))
    (setf (dm:field host "confirmed") NIL)
    (when save (dm:insert host))
    host))

(defun ensure-host (host-ish &optional (user (auth:current)))
  (or
   (etypecase host-ish
     (dm:data-model host-ish)
     (string (dm:get-one 'host (db:query (:and (:= 'author (user:id user))
                                               (:= 'title host-ish)))))
     (integer (dm:get-one 'host (db:query (:and (:= 'author (user:id user))
                                                (:= '_id host-ish))))))
   (error "No such host.")))

(defun delete-host (host-ish &optional (user (auth:current)))
  (db:with-transaction ()
    (let ((host (ensure-host host-ish user)))
      ;; FIXME: cascade
      (dm:delete host)
      host)))

(defun list-hosts (&optional (user (auth:current)))
  (dm:get 'host (db:query (:= 'author (user:id user))) :sort '((title :asc))))

(defun make-campaign (&key host author title description reply-to template (save T))
  (dm:with-model campaign ('campaign NIL)
    (when (and title (< 0 (db:count 'campaign (db:query (:and (:= 'author author)
                                                              (:= 'title title))))))
      (error 'api-argument-invalid
             :argument 'title
             :message "A campaign with that title already exists."))
    (setf-dm-fields campaign author title description reply-to template)
    (when host (setf (dm:field campaign "host") (dm:id (ensure-host host))))
    (when save (dm:insert campaign))
    campaign))

(defun ensure-campaign (campaign-ish &optional (user (auth:current)))
  (or
   (etypecase campaign-ish
     (dm:data-model campaign-ish)
     (string (dm:get-one 'campaign (db:query (:and (:= 'author (user:id user))
                                                   (:= 'title campaign-ish)))))
     (integer (dm:get-one 'campaign (db:query (:and (:= 'author (user:id user))
                                                    (:= '_id campaign-ish))))))
   (error "No such campaign.")))

(defun delete-campaign (campaign-ish &optional (user (auth:current)))
  (db:with-transaction ()
    (let ((campaign (ensure-campaign campaign-ish user)))
      ;; FIXME: cascade
      (dm:delete campaign)
      campaign)))

(defun list-campaigns (&optional (user (auth:current)))
  (dm:get 'campaign (db:query (:= 'author (user:id user))) :sort '((title :asc))))

(defun list-attributes (campaign-ish &optional (user (auth:current)))
  (let ((campaign (ensure-campaign campaign-ish user)))
    (dm:get 'attribute (db:query (:= 'campaign (dm:id campaign))) :sort '((name :asc)))))
