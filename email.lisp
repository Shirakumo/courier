#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defun compile-email (host template &rest args)
  ;; FIXME: softdrink apply stylesheet
  (apply #'r-clip:process
         (etypecase template
           (string template)
           (pathname (@template (namestring template))))
         :host (dm:field host "title")
         :title (config :title)
         :copyright (config :copyright)
         :version (asdf:component-version (asdf:find-system :courier))
         args))

(defun extract-plaintext (html)
  ;; KLUDGE: This is real dumb.
  (with-output-to-string (out)
    (lquery:$ html "body" (text) (each (lambda (text) (write-line text out))))))

(defun extract-subject (html)
  (lquery:$1 html "head title" (text)))

(defun format-email (text)
  (etypecase text
    (string
     (values text))
    (plump:node
     (values (extract-plaintext text)
             (plump:serialize text NIL)))))

(defun send (host to subject message &optional reply-to)
  (multiple-value-bind (plaintext html) (format-email message)
    (l:trace :courier.mail "Sending to ~a via ~a/~a:~a"
             to
             (dm:field host "address")
             (dm:field host "hostname")
             (dm:field host "port"))
    ;; FIXME: better timeouts on unreachable host
    (cl-smtp:send-email
     (dm:field host "hostname")
     (dm:field host "address")
     to subject plaintext
     :html-message html
     :ssl (ecase (dm:field host "encryption")
            (0 NIL) (1 :starttls) (2 :tls))
     :port (dm:field host "port")
     :reply-to reply-to
     :authentication (when (dm:field host "username")
                       (list :plain
                             (dm:field host "username")
                             (decrypt (dm:field host "password"))))
     :display-name (dm:field host "title"))))

(defun send-templated (host to template &rest args)
  (let ((html (apply #'compile-email host template args)))
    (send host to (extract-subject html) html)))

(defun generate-id (id subscriber)
  (cryptos:encrypt (format NIL "~a/~a ~a" (make-random-string 8) id (dm:id subscriber))
                   (config :private-key)))

(defun decode-id (thing)
  (let ((string (cryptos:decrypt thing (config :private-key))))
    (values (db:ensure-id (subseq string (1+ (position #\/ string)) (position #\Space string)))
            (db:ensure-id (subseq string (1+ (position #\Space string)))))))

(defun mail-receipt-url (mail subscriber)
  (uri-to-url "courier/api/courier/mail/receipt"
              :representation :external
              :query `(("id" . ,(generate-id (dm:id mail) subscriber)))))

(defun link-receipt-url (link subscriber)
  (uri-to-url "courier/api/courier/link/resolve"
              :representation :external
              :query `(("id" . ,(generate-id (dm:id link) subscriber)))))

(defun mail-template-args (mail subscriber)
  (list :mail-receipt (mail-receipt-url mail subscriber)
        ))

(defun send-mail (mail subscriber)
  (l:debug :courier.mail "Sending ~a to ~a" mail subscriber)
  (let* ((campaign (ensure-campaign (dm:field mail "campaign")))
         (host (ensure-campaign (dm:field campaign "host")))
         (html (apply #'compile-email host (dm:field campaign "template")
                      (mail-template-args mail subscriber))))
    (send host (dm:field subscriber "address")
          (extract-subject html)
          html
          (dm:field campaign "reply-to"))))
