#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defun encrypt (thing)
  (cryptos:encrypt thing (config :private-key)))

(defun decrypt (thing)
  (cryptos:decrypt thing (config :private-key)))

(defun hash (thing)
  (let ((thing (etypecase thing
                 (string thing)
                 (integer (princ-to-string thing)))))
    (cryptos:pbkdf2-hash thing (config :private-key))))

(defun check-hash (hash thing &optional (argument 'token))
  (unless (equal hash (hash thing))
    (error 'api-argument-invalid :argument argument)))

(defun url> (uri &key query fragment)
  (uri-to-url uri :representation :external
                  :query query
                  :fragment fragment))
