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

(define-page host-list "courier/^host/?$" (:access (perm courier))
  (render-page "Configured Hosts"
               (@template "host-list.ctml")
               :hosts (list-hosts)))

(define-page host-new ("courier/^host/new$" 1) (:uri-groups () :access (perm courier))
  (render-page "New Host"
               (@template "host-edit.ctml")
               :host (make-host :save NIL)))

(define-page host-edit "courier/^host/(.+)/edit$" (:uri-groups (host) :access (perm courier))
  (render-page (format NIL "Edit ~a" host)
               (@template "host-edit.ctml")
               :host (ensure-host host)))

(define-page campaign-list "courier/^campaign/?$" (:access (perm courier))
  (render-page "Campaigns"
               (@template "campaign-list.ctml")
               :campaigns (list-campaigns)))

(define-page campaign-overview "courier/^campaign/([^/]+)/?$" (:uri-groups (campaign) :access (perm courier))
  )

(define-page campaign-new ("courier/^campaign/new$" 1) (:uri-groups () :access (perm courier))
  (render-page "New Campaign"
               (@template "campaign-edit.ctml")
               :hosts (list-hosts)
               :campaign (make-campaign :save NIL)))

(define-page campaign-edit "courier/^campaign/([^/]+)/edit$" (:uri-groups (campaign) :access (perm courier))
  (render-page (format NIL "Edit ~a" campaign)
               (@template "campaign-edit.ctml")
               :hosts (list-hosts)
               :campaign (ensure-campaign campaign)))

(define-page mail-list "courier/^campaign/([^/]+)/mail/?$" (:uri-groups (campaign) :access (perm courier))
  (render-page "Mails"
               (@template "mail-list.ctml")
               :mails (list-mails campaign)
               :campaign campaign))

(define-page mail-overview "courier/^campaign/([^/]+)/mail/([^/]+)/?$" (:uri-groups (campaign mail) :access (perm courier))
  )

(define-page mail-new ("courier/^campaign/([^/]+)/mail/new$" 1) (:uri-groups (campaign) :access (perm courier))
  (render-page "New Mail"
               (@template "mail-edit.ctml")
               :mail (make-mail campaign :save NIL)))

(define-page mail-edit "courier/^campaign/([^/]+)/mail/([^/]+)/edit$" (:uri-groups (campaign mail) :access (perm courier))
  (render-page (format NIL "Edit ~a" mail)
               (@template "mail-edit.ctml")
               :mail (ensure-mail campaign mail)))

(define-page mail-triggers "courier/^campaign/([^/]+)/mail/([^/]+)/triggers$" (:uri-groups (campaign mail) :access (perm courier))
  (render-page (format NIL "~a Triggers" mail)
               (@template "mail-triggers.ctml")
               :triggers (list-mail-triggers (ensure-mail campaign mail))))

(define-page mail-send "courier/^campaign/([^/]+)/mail/([^/]+)/send$" (:uri-groups (campaign mail) :access (perm courier))
  (render-page (format NIL "Send ~a" (dm:field mail "title"))
               (@template "mail-send.ctml")
               :mail (ensure-mail campaign mail)))

(define-page tag-list "courier/^campaign/([^/]+)/tag/?$" (:uri-groups (campaign) :access (perm courier))
  (render-page "Tags"
               (@template "tag-list.ctml")
               :tags (list-tags campaign)
               :campaign campaign))

(define-page tag-overview "courier/^campaign/([^/]+)/tag/([^/]+)/?$" (:uri-groups (campaign tag) :access (perm courier))
  )

(define-page tag-new ("courier/^campaign/([^/]+)/tag/new" 1) (:uri-groups (campaign) :access (perm courier))
  (render-page "New Tag"
               (@template "tag-edit.ctml")
               :tag (make-tag campaign :save NIL)))

(define-page tag-edit "courier/^campaign/([^/]+)/tag/([^/]+)/edit$" (:uri-groups (campaign tag) :access (perm courier))
  (render-page (format NIL "Edit ~a" tag)
               (@template "tag-edit.ctml")
               :tag (ensure-tag campaign tag)))

(define-page tag-members "courier/^campaign/([^/]+)/tag/([^/]+)/members$" (:uri-groups (campaign tag) :access (perm courier))
  )

(define-page subscriber-overview "courier/^subscriber/(.+)" (:uri-groups (subscriber) :access (perm courier))
  )

(define-page mail-log "courier/^log$" (:access (perm courier))
  (render-page "Mail Log" (@template "mail-log.ctml")))
