#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:courier
  (:use #:cl #:radiance)
  (:shadow #:trigger #:delete-file #:file #:make-sequence #:sequence)
  (:export)
  (:local-nicknames
   (#:dns #:org.shirakumo.dns-client)
   (#:feeder #:org.shirakumo.feeder)
   (#:markless #:org.shirakumo.markless)
   (#:components #:org.shirakumo.markless.components)))
(in-package #:courier)

(define-trigger startup ()
  (defaulted-config "Courier Mailing" :title)
  (defaulted-config "Shirakumo" :copyright)
  (defaulted-config (make-random-string 32) :private-key)
  (defaulted-config (make-random-string 32) :salt)
  (defaulted-config 3 :send-queue :retry-backoff-exponent)
  (defaulted-config 10 :send-queue :retry-attempts)
  (defaulted-config 60 :send-queue :poll-interval))
