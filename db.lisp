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
               (port :integer)
               (username (:varchar 32))
               (password (:varchar 32))
               (encryption :integer)))

  (db:create 'campaign
             '((host (:id host))
               (author :id)
               (title (:varchar 32))
               (description :text)
               (time (:integer 5))
               (reply-to (:varchar 64))))

  (db:create 'subscriber
             '((campaign (:id campaign))
               (address (:varchar 64))
               (signup-time (:integer 5))
               (confirmed :boolean)))

  (db:create 'attribute
             '((campaign (:id campaign))
               (name (:varchar 32))))

  (db:create 'attribute-value
             '((attribute (:id attribute))
               (subscriber (:id subscriber))
               (value (:varchar 64))))

  (db:create 'mail
             '((campaign (:id campaign))
               (title (:varchar 32))
               (subject (:varchar 128))
               (body :text)))

  (db:create 'mail-receipt
             '((mail (:id mail))
               (subscriber (:id subscriber))
               (time (:integer 5))))

  (db:create 'mail-log
             '((mail (:id mail))
               (subscriber (:id subscriber))
               (time (:integer 5))))

  (db:create 'mail-queue
             '((host (:id host))
               (subscriber (:id subscriber))
               (mail (:id mail))
               (time (:integer 5))))

  (db:create 'mail-trigger
             '((mail (:id mail))
               (to-mail (:id mail))
               (to-link (:id link))
               (time-offset (:integer 5))))

  (db:create 'mail-trigger-tags
             '((mail-trigger (:id mail-trigger))
               (tag (:id tag))
               (inverted :boolean)))

  (db:create 'tag
             '((campaign (:id campaign))
               (title (:varchar 32))
               (description :text)))

  (db:create 'tag-table
             '((tag (:id tag))
               (subscriber (:id subscriber))))

  (db:create 'tag-trigger
             '((tag (:id tag))
               (to-mail (:id mail))
               (to-link (:id link))
               (time-offset (:integer 5))))

  (db:create 'tag-trigger-tags
             '((tag-trigger (:id tag-trigger))
               (tag (:id tag))
               (inverted :boolean)))

  (db:create 'link
             '((campaign (:id campaign))
               (mail (:id mail))
               (title (:varchar 32))
               (url (:varchar 256))))

  (db:create 'link-receipt
             '((link (:id link))
               (subscriber (:id subscriber))
               (time (:integer 5)))))

(define-trigger startup-done ())
