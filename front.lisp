#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defun render-page (page content &rest args)
  (r-clip:with-clip-processing ("template.ctml")
    (apply #'r-clip:process T
           :title (config :title)
           :page page
           :content (plump:parse content)
           :copyright (config :copyright)
           :version (asdf:component-version (asdf:find-system :courier))
           :logged-in (user:check (auth:current "anonymous") (perm courier))
           :registration-open (config :registration-open)
           args)))

(define-page frontpage "courier/^$" ()
  (if (user:check (auth:current "anonymous") (perm courier))
      (render-page "Dashboard" (@template "dashboard.ctml"))
      (render-page "Frontpage" (@template "frontpage.ctml"))))

(define-page list-host "courier/^host/$" (:access (perm courier))
  (render-page "Configured Hosts" (@template "hosts.ctml")
               :hosts (dm:get 'host (db:query (:= 'author (user:id (auth:current)))) :sort '((title :asc)))))

(define-page edit-host "courier/^host/(.+)$" (:uri-groups (host) :access (perm courier))
  (if (string= host "new")
      (render-page "New Host" (@template "host.ctml")
                   :host (make-host))
      (render-page host (@template "host.ctml")
                   :host (or (dm:get-one 'host (db:query (:and (:= 'author (user:id (auth:current)))
                                                               (:= 'title host))))
                             (error 'request-not-found :messag "No such host.")))))

(define-page campaigns "courier/^campaign/$" (:access (perm courier))
  (render-page "Campaigns" (@template "campaigns.ctml")))

(define-page mail-log "courier/^log$" (:access (perm courier))
  (render-page "Mail Log" (@template "mail-log.ctml")))
