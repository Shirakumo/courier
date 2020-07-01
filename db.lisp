#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

;; TODO: move collection definitions into the respective files.
(define-trigger db:connected ()
  (db:create 'host
             '((author :id)
               (title (:varchar 32))
               (display-name (:varchar 32))
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
               (report-interval (:integer 4))
               (last-report (:integer 5))
               (template :text)
               (address :text))
             :indices '(author title))

  (db:create 'campaign-access
             '((campaign (:id campaign))
               (user :id)
               (access-field (:integer 2))))

  (db:create 'subscriber
             '((campaign (:id campaign))
               (name (:varchar 64))
               (address (:varchar 64))
               (signup-time (:integer 5))
               (status (:integer 1)))
             :indices '(campaign status))

  (db:create 'attribute
             '((campaign (:id campaign))
               (title (:varchar 32))
               (type (:integer 1))
               (qualifier (:integer 1)))
             :indices '(campaign))

  (db:create 'attribute-value
             '((attribute (:id attribute))
               (subscriber (:id subscriber))
               (value (:varchar 64)))
             :indices '(attribute subscriber))

  (db:create 'mail
             '((campaign (:id campaign))
               (title (:varchar 128))
               (subject (:varchar 128))
               (body :text)
               (type (:integer 1))
               (time (:integer 5)))
             :indices '(campaign title))

  (db:create 'mail-receipt
             '((mail (:id mail))
               (subscriber (:id subscriber))
               (time (:integer 5)))
             :indices '(mail subscriber))

  (db:create 'mail-log
             '((mail (:id mail))
               (subscriber (:id subscriber))
               (send-time (:integer 5))
               (status (:integer 1)))
             :indices '(mail subscriber))

  (db:create 'mail-queue
             '((host (:id host))
               (subscriber (:id subscriber))
               (mail (:id mail))
               (send-time (:integer 5))
               (attempts (:integer 1)))
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

  (db:create 'trigger
             '((campaign (:id campaign))
               (description :text)
               (source-type (:integer 1))
               (source-id :id)
               (target-type (:integer 1))
               (target-id :id)
               (delay (:integer 5))
               (tag-constraint (:varchar 64))
               (normalized-constraint :text)
               (rule :boolean))
             :indices '(campaign source-type source-id target-type target-id))

  (db:create 'trigger-receipt
             '((trigger (:id trigger))
               (subscriber (:id subscriber)))
             :indices '(trigger subscriber))

  (db:create 'sequence
             '((campaign (:id campaign))
               (title :text))
             :indices '(campaign))
  
  (db:create 'sequence-trigger
             '((sequence (:id sequence))
               (trigger (:id trigger)))
             :indices '(sequence))

  (db:create 'file
             '((campaign (:id campaign))
               (author :id)
               (filename (:varchar 64))
               (mime-type (:varchar 32)))
             :indices '(campaign))

  (db:create 'feed
             '((campaign (:id campaign))
               (title (:varchar 32))
               (url (:varchar 256))
               (last-update (:integer 5))
               (frequency (:integer 2))
               (send-new :boolean)
               (template :text)))

  (db:create 'feed-entry
             '((feed (:id feed))
               (mail (:id mail))
               (guid (:varchar 256)))))

(defun ensure-id (id-ish)
  (etypecase id-ish
    (db:id id-ish)
    (dm:data-model (dm:id id-ish))
    (T (db:ensure-id id-ish))))

(defun collection-type (collection)
  (ecase (etypecase collection
           (symbol collection)
           (dm:data-model (dm:collection collection)))
    (mail 0)
    (link 1)
    (tag 2)
    (subscriber 3)
    (campaign 4)
    (file 5)
    (sequence 6)
    (trigger 7)
    (host 8)))

(defun type-collection (type)
  (ecase type
    ((0 10 mail) 'mail)
    ((1 link) 'link)
    ((2 20 tag) 'tag)
    ((3 subscriber) 'subscriber)
    ((4 campaign) 'campaign)
    ((5 file) 'file)
    ((6 sequence) 'sequence)
    ((7 trigger) 'trigger)
    ((8 host) 'host)))

(defun resolve-typed (type id)
  (let ((id (db:ensure-id id)))
    (dm:get-one (type-collection
                 (etypecase type
                   ((or symbol integer) type)
                   (string (parse-integer type))))
                (db:query (:= '_id id)))))

(defun check-accessible (dm &key (target (dm:collection dm)) (user (auth:current)))
  (labels ((check (author)
             (unless (equal (user:id user) author)
               (error 'radiance:request-denied :message (format NIL "You do not own the ~a you were trying to access."
                                                                (dm:collection dm)))))
           (check-campaign (campaign)
             (let* ((campaign (ensure-campaign campaign))
                    (record (db:select 'campaign-access
                                       (db:query (:and (:= 'campaign (dm:id campaign))
                                                       (:= 'user (user:id user))))
                                       :amount 1 :fields '(access-field)))
                    (access-field (if record
                                      (gethash "access-field" (first record))
                                      0)))
               (unless (or (equal (user:id user) (dm:field campaign "author"))
                           (etypecase target
                             (symbol (logbitp (collection-type target) access-field))
                             (integer (< target (access-level access-field)))))
                 (error 'radiance:request-denied :message (format NIL "You do not have permission to access ~as." target))))))
    ;; TODO: extended author intent checks
    (ecase (dm:collection dm)
      (host
       (check (dm:field dm "author")))
      (campaign
       (check-campaign dm))
      ((mail tag trigger link subscriber file sequence feed)
       (check-campaign (dm:field dm "campaign"))))
    dm))

(defun access-level (access-field)
  (cond ((logbitp (collection-type 'campaign) access-field) 4)
        ((logbitp (collection-type 'subscriber) access-field) 3)
        ((logbitp (collection-type 'trigger) access-field) 2)
        ((logbitp (collection-type 'mail) access-field) 1)
        (T 0)))

(defun access-field (access-level)
  (let ((field 0))
    (macrolet ((set-bit (i)
                 `(setf field (logior field (ash 1 ,i)))))
      (when (< 0 access-level)
        (set-bit (collection-type 'mail))
        (set-bit (collection-type 'link))
        (set-bit (collection-type 'file)))
      (when (< 1 access-level)
        (set-bit (collection-type 'tag))
        (set-bit (collection-type 'trigger))
        (set-bit (collection-type 'sequence))
        (set-bit (collection-type 'feed)))
      (when (< 2 access-level)
        (set-bit (collection-type 'subscriber)))
      (when (< 3 access-level)
        (set-bit (collection-type 'campaign)))
      field)))
