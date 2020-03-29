#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:courier
  (:use #:cl #:radiance)
  (:shadow #:trigger)
  (:export)
  (:local-nicknames
   (#:markless #:org.shirakumo.markless)
   (#:components #:org.shirakumo.markless.components)))
(in-package #:courier)

(define-trigger startup ()
  (defaulted-config "Courier Mailing" :title)
  (defaulted-config "Shirakumo" :copyright)
  (defaulted-config NIL :registration-open)
  (defaulted-config (make-random-string 32) :private-key)
  (defaulted-config 60 :send-queue-poll-interval))
