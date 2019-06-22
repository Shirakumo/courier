#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defun render-page (page content &rest args)
  (apply #'r-clip:process (@template "front.ctml")
         :title (config :title)
         :page page
         :content (plump:parse content)
         :copyright (config :copyright)
         args))

(define-page frontpage "courier/^$" ()
  (if (user:check (auth:current "anonymous") (perm courier))
      (render-page "Dashboard" (@template "dashboard.ctml"))
      (render-page "Frontpage" (@template "frontpage.ctml"))))

(define-page campaigns "courier/^campaign/$" (:auth (perm courier))
  (render-page "Campaigns" (@template "campaigns.ctml")))

(define-page mail-log "courier/^log$" (:auth (perm courier))
  (render-page "Mail Log" (@template "mail-log.ctml")))
