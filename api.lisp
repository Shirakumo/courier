#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defun check-title (title)
  (when (string= title "")
    (error 'api-argument-invalid :argument 'title :message "The title cannot be empty."))
  (when (string-equal title "new")
    (error 'api-argument-invalid :argument 'title :message "The title cannot be \"new\"."))
  (when (find #\/ title)
    (error 'api-argument-invalid :argument 'title :message "The title cannot contain a slash.")))

(defun send-confirm-host (host &optional (user (auth:current)))
  (send-templated host (user:field "email" user)
                  (@template "email/confirm-host.ctml")
                  :recipient (or (user:field "displayname" user)
                                 (user:username user))
                  :link (url> "courier/api/courier/host/confirm"
                              :query `(("host" . ,(princ-to-string (dm:id host)))
                                       ("token" . ,(hash (dm:id host)))
                                       ("browser" . "true")))))

(define-api courier/host/new (title address hostname &optional port username password encryption batch-size batch-cooldown) (:access (perm courier))
  (db:with-transaction ()
    (ratify:with-parsed-forms ((:email address)
                               (:host hostname)
                               (:port port)
                               (:integer encryption batch-size batch-cooldown))
      (check-title title)
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
  (db:with-transaction ()
    (let ((host (ensure-host (parse-integer host))))
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
    (delete-host (parse-integer host))
    (if (string= "true" (post/get "browser"))
        (redirect (url> "courier/host" :query `(("message" . "Host deleted."))))
        (api-output NIL))))

(define-api courier/host/confirm (host token) (:access (perm courier))
  (db:with-transaction ()
    (let ((host (ensure-host (parse-integer host))))
      (check-hash token (dm:id host))
      (setf (dm:field host "confirmed") T)
      (dm:save host)
      (if (string= "true" (post/get "browser"))
          (redirect (url> "courier/host" :query `(("message" . "Host confirmed."))))
          (api-output host)))))

(define-api courier/host/test (host) (:access (perm courier))
  (send-confirm-host host))

(define-api courier/campaign/new (host title &optional description reply-to template attribute[] attribute-type[]) (:access (perm courier))
  ;; FIXME: attributes
  (db:with-transaction ()
    (ratify:with-parsed-forms ((:email reply-to))
      (check-title title)
      (let ((campaign (make-campaign :author (user:id (auth:current))
                                     :host host
                                     :title title
                                     :description description
                                     :reply-to reply-to
                                     :template template)))
        (if (string= "true" (post/get "browser"))
            (redirect (url> "courier/campaign/" :query `(("message" . "Campaign created."))))
            (api-output campaign))))))

(define-api courier/campaign/edit (campaign &optional title host description reply-to template attribute[] attribute-type[]) :access (perm courier)
  ;; FIXME: attributes
  (db:with-transaction ()
    (let ((campaign (ensure-campaign (parse-integer campaign))))
      (ratify:with-parsed-forms ((:email reply-to)
                                 (:integer host))
        (setf-dm-fields campaign title host description reply-to template)
        (dm:save campaign)
        (if (string= "true" (post/get "browser"))
            (redirect (url> "courier/campaign" :query `(("message" . "Campaign edited."))))
            (api-output campaign))))))

(define-api courier/campaign/delete (campaign) (:access (perm courier))
  (db:with-transaction ()
    (delete-campaign (parse-integer campaign))
    (if (string= "true" (post/get "browser"))
        (redirect (url> "courier/campaign" :query `(("message" . "Campaign deleted."))))
        (api-output NIL))))
