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
               (:file "task-runner")
               (:file "markless")
               (:file "db")
               (:file "host")
               (:file "campaign")
               (:file "subscriber")
               (:file "mail")
               (:file "tag")
               (:file "link")
               (:file "trigger")
               (:file "file")
               (:file "sequence")
               (:file "feed")
               (:file "send")
               (:file "send-queue")
               (:file "import")
               (:file "front")
               (:file "api"))
  :depends-on ((:interface :relational-database)
               (:interface :auth)
               :bordeaux-threads
               :r-data-model
               :r-clip
               :i-json
               :drakma
               :feeder
               :ratify
               :alexandria
               :crypto-shortcuts
               :bordeaux-threads
               :softdrink
               :cl-markless-plump
               :cl-smtp
               :cl-ppcre
               :cl-csv))
