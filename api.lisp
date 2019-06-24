#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(define-api courier/host/new (title address hostname &optional port username password encryption batch-size batch-cooldown reply-to) (:access (perm courier))
  (db:with-transaction ()
    (when (< 0 (db:count 'host (db:query (:and (:= 'author (user:id (auth:current)))
                                               (:= 'title title)))))
      (error 'api-argument-invalid
             :argument 'title
             :message "A host with that title already exists."))
    (ratify:with-parsed-forms ((:email address reply-to)
                               (:host hostname)
                               (:port port)
                               (:integer encryption batch-size batch-cooldown))
      (let ((host (make-host :author (user:id (auth:current))
                             :title title
                             :address address
                             :hostname hostname
                             :port port
                             :username username
                             :password password
                             :encryption encryption
                             :batch-size batch-size
                             :batch-cooldown batch-cooldown
                             :reply-to reply-to
                             :save T)))
        (send-host host (user:field "email" (auth:current))
                   (format NIL "Confirm the Courier host settings")
                   (format NIL "Hello ~a,

Please visit the following URL to confirm that your email host settings are correct:

  ~a

Thank you."
                           (user:field "displayname" (auth:current))
                           (uri-to-url "courier/api/courier/host/confirm"
                                       :representation :external
                                       :query `(("id" . ,(dm:id host))))))
        (if (string= "true" (post/get "browser"))
            (redirect "courier/host/")
            (api-output host))))))

(define-api courier/host/edit (id &optional title address hostname port username password encryption batch-size batch-cooldown) :access (perm courier)
  )

(define-api courier/host/confirm (id) (:access (perm courier))
  )
