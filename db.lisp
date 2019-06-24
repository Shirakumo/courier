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
               (password (:varchar 32))
               (encryption (:integer 1))
               (batch-size :integer)
               (batch-cooldown :integer)
               (reply-to (:varchar 64))
               (confirmed :boolean))
             :indices '(author title))

  (db:create 'campaign
             '((host (:id host))
               (author :id)
               (title (:varchar 32))
               (description :text)
               (time (:integer 5))
               (reply-to (:varchar 64)))
             :indices '(author title))

  (db:create 'subscriber
             '((campaign (:id campaign))
               (address (:varchar 64))
               (signup-time (:integer 5))
               (confirmed :boolean))
             :indices '(campaign confirmed))

  (db:create 'attribute
             '((campaign (:id campaign))
               (name (:varchar 32)))
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
  (defaulted-config NIL :registration-open))

(defun make-host (&key author title address hostname port username password encryption batch-size batch-cooldown reply-to save)
  (dm:with-model host ('host NIL)
    (setf (dm:field host "author") author)
    (setf (dm:field host "title") title)
    (setf (dm:field host "address") address)
    (setf (dm:field host "hostname") hostname)
    (setf (dm:field host "port") (or port 25))
    (setf (dm:field host "username") username)
    (setf (dm:field host "password") password)
    (setf (dm:field host "encryption") (or encryption 1))
    (setf (dm:field host "batch-size") (or batch-size 100))
    (setf (dm:field host "batch-cooldown") (or batch-cooldown 60))
    (setf (dm:field host "reply-to") reply-to)
    (when save (dm:insert host))
    host))

(defun send-host (host to subject message)
  (cl-smtp:send-email
   (dm:field host "hostname")
   (dm:field host "address")
   to subject message
   :ssl (ecase (dm:field host "encryption")
          (0 NIL) (1 :starttls) (2 :tls))
   :port (dm:field host "port")
   :reply-to (dm:field host "reply-to")
   :authentication (list :plain
                         (dm:field host "username")
                         (dm:field host "password"))
   :display-name (dm:field host "title")))
