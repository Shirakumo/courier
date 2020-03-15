#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

;; FIXME: check access more thoroughly for objects that can't be checked against author immediately:
;;        mail, tag, triggers, subscribers

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
    (let* ((mail (ensure-mail NIL (db:ensure-id mail)))
           (campaign (ensure-campaign (dm:field mail "campaign"))))
      (setf-dm-fields mail title subject body)
      (dm:save mail)
      (if (string= "true" (post/get "browser"))
          (redirect (url> (format NIL "courier/campaign/~a/mail/" (dm:field campaign "title"))
                          :query `(("message" . "Mail edited."))))
          (api-output mail)))))

(define-api courier/mail/delete (mail) (:access (perm courier))
  (db:with-transaction ()
    (let* ((mail (ensure-mail NIL (db:ensure-id mail)))
           (campaign (ensure-campaign (dm:field mail "campaign"))))
      (delete-mail mail)
      (if (string= "true" (post/get "browser"))
          (redirect (url> (format NIL "courier/campaign/~a/mail/" (dm:field campaign "title"))
                          :query `(("message" . "Mail deleted."))))
          (api-output NIL)))))


;; User sections
(defvar *tracker* (alexandria:read-file-into-byte-vector (@static "receipt.gif")))

(define-api courier/campaign/subscribe (campaign address fields[] values[]) ()
  (let* ((campaign (dm:get-one 'campaign (db:query (:= '_id (db:ensure-id campaign)))))
         (attributes (loop for field in fields[]
                           for value in values[]
                           for attribute = (dm:get-one 'attribute (db:query (:= '_id (db:ensure-id field))))
                           do (unless (equal (dm:id campaign) (dm:field attribute "campaign"))
                                (error "Invalid attribute field."))
                           collect (cons attribute value)))
         (subscriber (make-subscriber campaign address attributes)))
    (if (string= "true" (post/get "browser"))
        (redirect (url> "courier/subscribe/"
                        :query `(("action" . "subscribed")
                                 ("campaign" . ,(dm:id campaign)))))
        (api-output subscriber))))

(define-api courier/campaign/unsubscribe (id) ()
  (let* ((subscriber (dm:get-one 'subscriber (db:query (:= '_id (decode-id id)))))
         (campaign (dm:get-one 'campaign (db:query (:= '_id (dm:field subscriber "campaign"))))))
    (delete-subscriber subscriber)
    (if (string= "true" (post/get "browser"))
        (redirect (url> (format NIL "courier/unsubscribe")
                        :query `(("action" . "unsubscribed")
                                 ("campaign" . ,(dm:id campaign)))))
        (api-output "Ok."))))

(defun unsubscribe-url (subscriber)
  (url> "courier/unsubscribe" 
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
