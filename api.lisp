#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

;; FIXME: check access more thoroughly for objects that can't be checked against author immediately:
;;        mail, tag, triggers, subscribers
;; FIXME: prune unconfirmed subscribers after a day

(defun send-confirm-host (host &optional (user (auth:current)))
  (send-templated host (or (user:field "email" user)
                           (error "User must configure the email field!"))
                  (@template "email/confirm-host.ctml")
                  :host (dm:field host "title")
                  :recipient (or (user:field "displayname" user)
                                 (user:username user))
                  :link (url> "courier/api/courier/host/confirm"
                              :query `(("host" . ,(princ-to-string (dm:id host)))
                                       ("token" . ,(hash (dm:id host)))
                                       ("browser" . "true")))))

(defun send-confirm-subscription (campaign subscriber)
  (let ((host (dm:get-one 'host (db:query (:= '_id (dm:field campaign "host"))))))
    (send-templated host (dm:field subscriber "address")
                    (@template "email/confirm-subscription.ctml")
                    :campaign (dm:field campaign "title")
                    :recipient (dm:field subscriber "name")
                    :link (url> "courier/api/courier/subscription/confirm"
                                :query `(("id" . ,(generate-id subscriber))
                                         ("browser" . "true"))))))

(define-api courier/host/new (title address hostname &optional port username password encryption batch-size batch-cooldown) (:access (perm courier))
  (check-title title)
  (db:with-transaction ()
    (ratify:with-parsed-forms ((:email address)
                               (:host hostname)
                               (:port port)
                               (:integer encryption batch-size batch-cooldown))
      (let ((host (make-host :author (user:id (auth:current))
                             :title title
                             :address address
                             :hostname hostname
                             :port port
                             :username username
                             :password (or* password)
                             :encryption encryption
                             :batch-size batch-size
                             :batch-cooldown batch-cooldown)))
        (send-confirm-host host)
        (if (string= "true" (post/get "browser"))
            (redirect (url> "courier/host/" :query `(("message" . "Host created. Please check your emails to confirm."))))
            (api-output host))))))

(define-api courier/host/edit (host &optional title address hostname port username password encryption batch-size batch-cooldown) :access (perm courier)
  (when title (check-title title))
  (db:with-transaction ()
    (let ((host (ensure-host (db:ensure-id host))))
      (ratify:with-parsed-forms ((:email address)
                                 (:host hostname)
                                 (:port port)
                                 (:integer encryption batch-size batch-cooldown))
        (setf-dm-fields host title address hostname port username encryption batch-size batch-cooldown)
        (when (or* password) (setf (dm:field host "password") (encrypt password)))
        (dm:save host)
        (if (string= "true" (post/get "browser"))
            (redirect (url> "courier/host" :query `(("message" . "Host edited."))))
            (api-output host))))))

(define-api courier/host/delete (host) (:access (perm courier))
  (db:with-transaction ()
    (delete-host (db:ensure-id host))
    (if (string= "true" (post/get "browser"))
        (redirect (url> "courier/host" :query `(("message" . "Host deleted."))))
        (api-output NIL))))

(define-api courier/host/confirm (host token) (:access (perm courier))
  (db:with-transaction ()
    (let ((host (ensure-host (db:ensure-id host))))
      (check-hash token (dm:id host))
      (setf (dm:field host "confirmed") T)
      (dm:save host)
      (if (string= "true" (post/get "browser"))
          (redirect (url> "courier/host" :query `(("message" . "Host confirmed."))))
          (api-output host)))))

(define-api courier/host/test (host) (:access (perm courier))
  (send-confirm-host host))

(define-api courier/campaign/new (host title &optional description reply-to template attribute[] attribute-type[]) (:access (perm courier))
  (check-title title)
  (db:with-transaction ()
    (ratify:with-parsed-forms ((:email reply-to))
      (let ((campaign (make-campaign :author (user:id (auth:current))
                                     :host host
                                     :title title
                                     :description description
                                     :reply-to reply-to
                                     :template template)))
        (loop for title in attribute[]
              for type in attribute-type[]
              do (make-attribute campaign title :type type))
        (if (string= "true" (post/get "browser"))
            (redirect (url> "courier/campaign/" :query `(("message" . "Campaign created."))))
            (api-output campaign))))))

(define-api courier/campaign/edit (campaign &optional title host description reply-to template attribute[] attribute-type[]) :access (perm courier)
  (when title (check-title title))
  (db:with-transaction ()
    (let ((campaign (ensure-campaign (db:ensure-id campaign))))
      (ratify:with-parsed-forms ((:email reply-to)
                                 (:integer host))
        (setf-dm-fields campaign title host description reply-to template)
        (dm:save campaign)
        (loop with existing = (list-attributes campaign)
              for title in attribute[]
              for type in attribute-type[]
              for prev = (find title existing :key (lambda (d) (dm:field d "title")))
              do (if prev
                     (setf existing (delete (dm:save (setf-dm-fields prev title type)) existing))
                     (make-attribute campaign title :type type))
              finally (mapcar #'delete-attribute existing))
        (if (string= "true" (post/get "browser"))
            (redirect (url> "courier/campaign" :query `(("message" . "Campaign edited."))))
            (api-output campaign))))))

(define-api courier/campaign/delete (campaign) (:access (perm courier))
  (db:with-transaction ()
    (delete-campaign (db:ensure-id campaign))
    (if (string= "true" (post/get "browser"))
        (redirect (url> "courier/campaign" :query `(("message" . "Campaign deleted."))))
        (api-output NIL))))

(define-api courier/campaign/list () (:access (perm courier))
  (api-output (db:select 'campaign (db:query (:= 'author (user:id (auth:current)))) :sort '((title :asc)))))

(define-api courier/mail/new (campaign title subject body) (:access (perm courier))
  (check-title title)
  (db:with-transaction ()
    (let* ((campaign (ensure-campaign (db:ensure-id campaign)))
           (mail (make-mail campaign
                            :title title
                            :subject subject
                            :body body)))
      (if (string= "true" (post/get "browser"))
          (redirect (url> (format NIL "courier/campaign/~a/mail/" (dm:field campaign "title"))
                          :query `(("message" . "Mail created."))))
          (api-output mail)))))

(define-api courier/mail/edit (mail &optional title subject body) (:access (perm courier))
  (check-title title)
  (db:with-transaction ()
    (let* ((mail (ensure-mail mail))
           (campaign (ensure-campaign (dm:field mail "campaign"))))
      (setf-dm-fields mail title subject body)
      (dm:save mail)
      (if (string= "true" (post/get "browser"))
          (redirect (url> (format NIL "courier/campaign/~a/mail/" (dm:field campaign "title"))
                          :query `(("message" . "Mail edited."))))
          (api-output mail)))))

(define-api courier/mail/delete (mail) (:access (perm courier))
  (db:with-transaction ()
    (let* ((mail (ensure-mail mail))
           (campaign (ensure-campaign (dm:field mail "campaign"))))
      (delete-mail mail)
      (if (string= "true" (post/get "browser"))
          (redirect (url> (format NIL "courier/campaign/~a/mail/" (dm:field campaign "title"))
                          :query `(("message" . "Mail deleted."))))
          (api-output NIL)))))

(define-api courier/mail/test (mail) (:access (perm courier))
  (let ((mail (ensure-mail mail)))
    (enqueue-email mail :target  :time time)
    (if (string= "true" (post/get "browser"))
        (redirect (url> (format NIL "courier/campaign/~a/mail/~a/" (dm:field mail "campaign") (dm:id mail))
                        :query `(("message" . "Mail sent."))))
        (api-output NIL))))

(define-api courier/mail/send (mail &optional target-type target-id time) (:access (perm courier))
  (let ((mail (ensure-mail mail)))
    (when time
      (setf time (local-time:timestamp-to-universal (local-time:parse-timestring time))))
    (if (or* target-type)
        (enqueue-email mail :target (resolve-typed target-type target-id) :time time)
        (enqueue-email mail :time time))
    (if (string= "true" (post/get "browser"))
        (redirect (url> (format NIL "courier/campaign/~a/mail/~a/send" (dm:field mail "campaign") (dm:id mail))
                        :query `(("message" . "Mail sent."))))
        (api-output NIL))))

(define-api courier/mail/list (campaign) (:access (perm courier))
  (api-output (db:select 'mail (db:query (:= 'campaign (db:ensure-id campaign))))))

(define-api courier/tag/new (campaign title &optional description) (:access (perm courier))
  (let* ((campaign (ensure-campaign (db:ensure-id campaign)))
         (tag (make-tag campaign :title title :description description)))
    (if (string= "true" (post/get "browser"))
        (redirect (url> (format NIL "courier/campaign/~a/tag" (dm:field campaign "title"))
                        :query `(("message" . "Tag created."))))
        (api-output tag))))

(define-api courier/tag/edit (tag &optional title description) (:access (perm courier))
  (let ((tag (ensure-tag tag)))
    (edit-tag tag :title (or* title) :description (or* description))
    (if (string= "true" (post/get "browser"))
        (redirect (url> (format NIL "courier/campaign/~a/tag" (dm:field tag "campaign"))
                        :query `(("message" . "Tag edited."))))
        (api-output tag))))

(define-api courier/tag/delete (tag) (:access (perm courier))
  (let* ((tag (ensure-tag tag))
         (campaign (ensure-campaign (dm:field tag "campaign"))))
    (delete-tag tag)
    (if (string= "true" (post/get "browser"))
        (redirect (url> (format NIL "courier/campaign/~a/tag/" (dm:field campaign "title"))
                        :query `(("message" . "Tag deleted."))))
        (api-output NIL))))

(define-api courier/tag/list (campaign) (:access (perm courier))
  (api-output (db:select 'tag (db:query (:= 'campaign (db:ensure-id campaign))))))

(define-api courier/trigger/new (campaign source-type source-id target-type target-id &optional description time-offset tag-constraint) (:access (perm courier))
  (let* ((campaign (ensure-campaign campaign))
         (source (resolve-typed source-type source-id))
         (target (resolve-typed target-type target-id))
         (trigger (make-trigger campaign source target
                                :description description :time-offset time-offset
                                :tag-constraint tag-constraint)))
    (if (string= "true" (post/get "browser"))
        (redirect (url> (format NIL "courier/campaign/~a/trigger" (dm:field campaign "title"))
                        :query `(("message" . "Trigger created."))))
        (api-output trigger))))

(define-api courier/trigger/edit (trigger &optional source-type source-id target-type target-id description time-offset tag-constraint) (:access (perm courier))
  (let ((trigger (ensure-trigger trigger))
        (source (resolve-typed source-type source-id))
        (target (resolve-typed target-type target-id)))
    (edit-trigger trigger :description description :time-offset time-offset :tag-constraint tag-constraint
                          :source source :target target)
    (if (string= "true" (post/get "browser"))
        (redirect (url> (format NIL "courier/campaign/~a/trigger" (dm:field trigger "campaign"))
                        :query `(("message" . "Trigger edited."))))
        (api-output trigger))))

(define-api courier/trigger/list (campaign) (:access (perm courier))
  (api-output (db:select 'trigger (db:query (:= 'campaign (db:ensure-id campaign))))))

(define-api courier/link/list (campaign) (:access (perm courier))
  (api-output (db:select 'link (db:query (:= 'campaign (db:ensure-id campaign))))))

(define-api courier/subscriber/list (campaign) (:access (perm courier))
  (api-output (db:select 'subscriber (db:query (:= 'campaign (db:ensure-id campaign))) :sort '((signup-time :desc)))))

;; User sections
(defvar *tracker* (alexandria:read-file-into-byte-vector (@static "receipt.gif")))

(define-api courier/subscription/new (campaign name address &optional fields[] values[]) ()
  (db:with-transaction ()
    (let* ((campaign (dm:get-one 'campaign (db:query (:= '_id (db:ensure-id campaign)))))
           (attributes (loop for field in fields[]
                             for value in values[]
                             for attribute = (dm:get-one 'attribute (db:query (:= '_id (db:ensure-id field))))
                             do (unless (equal (dm:id campaign) (dm:field attribute "campaign"))
                                  (error "Invalid attribute field."))
                             collect (cons attribute value)))
           (subscriber (make-subscriber campaign name address attributes)))
      (send-confirm-subscription campaign subscriber)
      (if (string= "true" (post/get "browser"))
          (redirect (url> "courier/subscription"
                          :query `(("action" . "subscribed")
                                   ("campaign" . ,(princ-to-string (dm:id campaign))))))
          (api-output subscriber)))))

(define-api courier/subscription/confirm (id) ()
  (let ((subscriber (dm:get-one 'subscriber (db:query (:= '_id (first (decode-id id)))))))
    (setf (dm:field subscriber "confirmed") T)
    (dm:save subscriber)
    (if (string= "true" (post/get "browser"))
        (redirect (url> "courier/subscription"
                        :query `(("action" . "confirmed")
                                 ("campaign" . ,(princ-to-string (dm:field subscriber "campaign"))))))
        (api-output NIL))))

(define-api courier/subscription/delete (id) ()
  (let* ((subscriber (dm:get-one 'subscriber (db:query (:= '_id (first (decode-id id))))))
         (campaign (dm:get-one 'campaign (db:query (:= '_id (dm:field subscriber "campaign"))))))
    (delete-subscriber subscriber)
    (if (string= "true" (post/get "browser"))
        (redirect (url> (format NIL "courier/subscription")
                        :query `(("action" . "unsubscribed")
                                 ("campaign" . ,(princ-to-string (dm:id campaign))))))
        (api-output NIL))))

(defun unsubscribe-url (subscriber)
  (url> "courier/api/courier/subscription/delete" 
        :query `(("id" . ,(generate-id subscriber))
                 ("browser" . "true"))))

(define-api courier/mail/receipt (id) ()
  (destructuring-bind (subscriber mail) (decode-id id)
    (mark-mail-received mail subscriber)
    (setf (header "Cache-Control") "public, max-age=31536000")
    *tracker*))

(defun mail-receipt-url (subscriber mail)
  (url> "courier/api/courier/mail/receipt"
        :query `(("id" . ,(generate-id subscriber (ensure-id mail))))))

(define-api courier/link/resolve (id) ()
  (destructuring-bind (subscriber link &optional mail) (decode-id id)
    (mark-link-received link subscriber)
    (when mail (mark-mail-received mail subscriber))
    (let ((link (db:select 'link (db:query (:= '_id link)) :fields '("url"))))
      (redirect (gethash "url" link)))))

(defun link-receipt-url (subscriber link mail)
  (url> "courier/api/courier/link/resolve"
        :query `(("id" . ,(generate-id subscriber (ensure-id link) (ensure-id mail))))))
