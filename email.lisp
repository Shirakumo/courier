#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defun compile-email (host template &rest args)
  ;; FIXME: softdrink apply stylesheet
  (apply #'r-clip:process
         (@template template)
         :host (dm:field host "title")
         :title (config :title)
         :copyright (config :copyright)
         :version (asdf:component-version (asdf:find-system :courier))
         args))

(defun extract-plaintext (html)
  ;; KLUDGE: This is real dumb.
  (lquery:$ html "body" (text)))

(defun extract-subject (html)
  (lquery:$1 html "head title" (text)))

(defun format-email (text)
  (etypecase text
    (string
     (values text))
    (plump:node
     (values (extract-plaintext text)
             (plump:serialize text NIL)))))

(defun send (host to subject message)
  (multiple-value-bind (plaintext html) message
    (cl-smtp:send-email
     (dm:field host "hostname")
     (dm:field host "address")
     to subject plaintext
     :html-message html
     :ssl (ecase (dm:field host "encryption")
            (0 NIL) (1 :starttls) (2 :tls))
     :port (dm:field host "port")
     :reply-to (dm:field host "reply-to")
     :authentication (list :plain
                           (dm:field host "username")
                           (dm:field host "password"))
     :display-name (dm:field host "title"))))

(defun send-templated (host to template &rest args)
  (let ((html (apply #'compile-email host template args)))
    (send host to (extract-subject html) html)))

(defun hash (thing)
  (cryptos:pbkdf2-hash thing (config :private-key)))

(defun check-hash (hash thing &optional (argument 'token))
  (unless (equal hash (hash thing))
    (error 'api-argument-invalid :argument argument)))
