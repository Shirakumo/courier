#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem #:courier
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "0.0.0"
  :license "zlib"
  :description "An email marketing service for Radiance"
  :homepage "https://shirakumo.github.io/courier/"
  :bug-tracker "https://github.com/shirakumo/courier/issues"
  :source-control (:git "https://github.com/shirakumo/courier.git")
  :serial T
  :components ((:file "module")
               (:file "toolkit")
               (:file "email")
               (:file "db")
               (:file "send-queue")
               (:file "front")
               (:file "api"))
  :depends-on ((:interface :database)
               (:interface :auth)
               :r-data-model
               :r-clip
               :ratify
               :crypto-shortcuts
               :bordeaux-threads
               :softdrink
               :cl-markless-plump
               :cl-smtp))
