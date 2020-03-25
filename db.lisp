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
               (type (:integer 1))
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
               (body :text)
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

  (db:create 'trigger
             '((campaign (:id campaign))
               (description :text)
               (source-type (:integer 1))
               (source-id :id)
               (target-type (:integer 1))
               (target-id :id)
               (time-offset (:integer 5))
               (tag-constraint :text))
             :indices '(campaign source-type source-id)))

;; FIXME: check values

(defun ensure-id (id-ish)
  (etypecase id-ish
    (db:id id-ish)
    (dm:data-model (dm:id id-ish))
    (T (db:ensure-id id-ish))))

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
                                         ("required" . ,required))))))
    campaign))

(defun edit-campaign (campaign &key host author title description reply-to template attributes (save T))
  (let ((campaign (ensure-campaign campaign)))
    (setf-dm-fields campaign host author title description reply-to template)
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
            (dm:delete attribute)))))
    campaign))

(defun ensure-campaign (campaign-ish &optional (user (auth:current)))
  (or
   (etypecase campaign-ish
     (dm:data-model campaign-ish)
     (db:id (dm:get-one 'campaign (db:query (:= '_id campaign-ish))))
     (string (or (dm:get-one 'campaign (db:query (:and (:= 'author (user:id user))
                                                       (:= 'title campaign-ish))))
                 (dm:get-one 'campaign (db:query (:= '_id (db:ensure-id campaign-ish)))))))
   (error 'request-not-found :message "No such campaign.")))

(defun delete-campaign (campaign-ish &optional (user (auth:current)))
  (db:with-transaction ()
    (let ((campaign (ensure-campaign campaign-ish user)))
      ;; FIXME: cascade
      (dm:delete campaign))))

(defun list-campaigns (&optional (user (auth:current)))
  (dm:get 'campaign (db:query (:= 'author (user:id user))) :sort '((title :asc))))

(defun list-attributes (campaign)
  (dm:get 'attribute (db:query (:= 'campaign (ensure-id campaign))) :sort '((title :asc))))

(defun make-subscriber (campaign name address &key attributes tags confirmed)
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
      (loop for tag in tags
            do (db:insert 'tag-table `(("subscriber" . ,(dm:id subscriber))
                                       ("tag" . ,(dm:id tag)))))
      (loop for (attribute . value) in attributes
            do (dm:with-model attribute-value ('attribute-value NIL)
                 (setf (dm:field attribute-value "attribute") (ensure-id attribute))
                 (setf (dm:field attribute-value "subscriber") (dm:id subscriber))
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

(defun list-subscribers (thing)
  (ecase (dm:collection thing)
    (campaign
     (dm:get 'subscriber (db:query (:= 'campaign (dm:id thing)))
             :sort '((signup-time . :DESC))))
    (tag
     (dm:get (rdb:join (subscriber _id) (tag-table subscriber)) (db:query (:= 'tag (dm:id thing)))
             :sort '((signup-time . :DESC))))
    (link
     (dm:get (rdb:join (subscriber _id) (link-receipt subscriber)) (db:query (:= 'link (dm:id thing)))
             :sort '((signup-time . :DESC))))))

(defun subscriber-attributes (subscriber)
  (loop for attribute in (db:select (rdb:join (attribute _id) (attribute-value attribute))
                                    (db:query (:= 'subscriber (ensure-id subscriber))))
        collect (gethash "title" attribute)
        collect (gethash "value" attribute)))

(defun make-mail (campaign &key title subject body (save T))
  (let ((campaign (ensure-campaign campaign)))
    (check-title-exists 'mail title (db:query (:and (:= 'campaign (dm:id campaign))
                                                    (:= 'title title))))
    (dm:with-model mail ('mail NIL)
      (setf-dm-fields mail title subject body campaign)
      (setf (dm:field mail "time") (get-universal-time))
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

(defun list-mails (campaign)
  (dm:get 'mail (db:query (:= 'campaign (dm:id campaign))) :sort '((title :asc))))

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
    ;; FIXME: cascade
    (dm:delete tag)))

(defun list-tags (thing)
  (ecase (dm:collection thing)
    (campaign
     (dm:get 'tag (db:query (:= 'campaign (dm:id thing))) :sort '((title :asc))))
    (subscriber
     (dm:get (rdb:join (tag _id) (tag-table tag)) (db:query (:= 'subscriber (dm:id thing))) :sort '((title :asc))))))

(defun make-trigger (campaign source target &key description (time-offset 0) tag-constraint (save T))
  (dm:with-model trigger ('trigger NIL)
    (setf-dm-fields trigger campaign description time-offset tag-constraint)
    (setf (dm:field trigger "source-id") (dm:id source))
    (setf (dm:field trigger "source-type") (ecase (dm:collection source)
                                             (mail 0)
                                             (link 1)))
    (setf (dm:field trigger "target-id") (dm:id target))
    (setf (dm:field trigger "target-type") (ecase (dm:collection target)
                                             (mail 0)
                                             (tag 2)))
    (when save (dm:insert trigger))
    trigger))

(defun edit-trigger (trigger &key description source target time-offset tag-constraint (save T))
  (setf-dm-fields trigger description time-offset tag-constraint)
  (when source
    (setf (dm:field trigger "source-id") (dm:id source))
    (setf (dm:field trigger "source-type") (ecase (dm:collection source)
                                             (mail 0)
                                             (link 1))))
  (when target
    (setf (dm:field trigger "target-id") (dm:id target))
    (setf (dm:field trigger "target-type") (ecase (dm:collection target)
                                             (mail 0)
                                             (tag 2))))
  (when save (dm:save trigger))
  trigger)

(defun ensure-trigger (trigger-ish)
  (or
   (etypecase trigger-ish
     (dm:data-model trigger-ish)
     (db:id (dm:get-one 'trigger (db:query (:= '_id trigger-ish))))
     (T (ensure-trigger (db:ensure-id trigger-ish))))
   (error 'request-not-found :message "No such trigger.")))

(defun list-triggers (campaign)
  (dm:get 'trigger (db:query (:= 'campaign (dm:id campaign)))))

(defun triggers (thing)
  (when (dm:id thing)
    (dm:get 'trigger (db:query (:and (:= 'target-id (dm:id thing))
                                     (:= 'target-type (ecase (dm:collection thing)
                                                        (mail 0)
                                                        (tag 2))))))))

(defun make-link (campaign &key url (save T))
  (let ((hash (cryptos:sha256 url :to :base64)))
    (or (dm:get-one 'link (db:query (:and (:= 'campaign (dm:id campaign))
                                          (:= 'hash hash))))
        (dm:with-model link ('link NIL)
          (setf-dm-fields link url hash campaign)
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

(defun list-links (campaign &optional (user (auth:current)))
  (dm:get 'link (db:query (:= 'campaign (dm:id campaign))) :sort '((title :asc))))

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

(defun resolve-typed (type id)
  (let ((id (db:ensure-id id)))
    (dm:get-one (ecase (etypecase type
                         ((or symbol integer) type)
                         (string (parse-integer type)))
                  ((0 mail) 'mail)
                  ((1 link) 'link)
                  ((2 tag) 'tag)
                  ((3 subscriber) 'subscriber)
                  ((4 campaign) 'campaign))
            (db:query (:= '_id id)))))

(defun check-accessible (dm &optional (user (auth:current)))
  (labels ((check (author)
             (unless (equal (user:id user) author)
               (error 'radiance:request-denied :message (format NIL "You do not own the ~a you were trying to access."
                                                                (dm:collection dm)))))
           (check-campaign (campaign)
             (db:iterate 'campaign (db:query (:= '_id campaign))
                         (lambda (r) (check (gethash "author" r)))
                         :fields '(author) :amount 1)))
    (ecase (dm:collection dm)
      (host
       (check (dm:field dm "author")))
      (campaign
       (check (dm:field dm "author")))
      (mail
       (check-campaign (dm:field dm "campaign")))
      (tag
       (check-campaign (dm:field dm "campaign")))
      (trigger
       (check-campaign (dm:field dm "campaign")))
      (link
       (check-campaign (dm:field dm "campaign")))
      (subscriber
       (check-campaign (dm:field dm "campaign"))))
    dm))
