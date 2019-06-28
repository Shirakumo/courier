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

(define-page list-host "courier/^host/?$" (:access (perm courier))
  (render-page "Configured Hosts"
               (@template "hosts.ctml")
               :hosts (list-hosts)))

(define-page edit-host "courier/^host/(.+)/edit$" (:uri-groups (host) :access (perm courier))
  (if (string= host "new")
      (render-page "New Host"
                   (@template "host.ctml")
                   :host (make-host :save NIL))
      (render-page (format NIL "Edit ~a" host)
                   (@template "host.ctml")
                   :host (ensure-host host))))

(define-page list-campaigns "courier/^campaign/?$" (:access (perm courier))
  (render-page "Campaigns"
               (@template "campaigns.ctml")
               :campaigns (list-campaigns)))

(define-page campaign-overview "courier/^campaign/([^/]+)/?$" (:uri-groups (campaign) :access (perm courier))
  )

(define-page edit-campaign "courier/^campaign/([^/]+)/edit$" (:uri-groups (campaign) :access (perm courier))
  (if (string= campaign "new")
      (render-page "New Campaign"
                   (@template "campaign.ctml")
                   :hosts (list-hosts)
                   :campaign (make-campaign :save NIL))
      (render-page (format NIL "Edit ~a" campaign)
                   (@template "campaign.ctml")
                   :hosts (list-hosts)
                   :campaign (ensure-campaign campaign))))

(define-page list-mails "courier/^campaign/([^/]+)/mail/?$" (:uri-groups (campaign) :access (perm courier))
  (render-page "Mails"
               (@template "mails.ctml")
               :mails (list-mails campaign)))

(define-page mail-overview "courier/^campaign/([^/]+)/mail/([^/]+)/?$" (:uri-groups (campaign mail) :access (perm courier))
  )

(define-page edit-mail "courier/^campaign/([^/]+)/mail/([^/]+)/edit$" (:uri-groups (campaign mail) :access (perm courier))
  (if (string= mail "new")
      (render-page "New Mail"
                   (@template "mail.ctml")
                   :mail (make-mail campaign :save NIL))
      (let ((mail (ensure-mail campaign mail)))
        (render-page (format NIL "Edit ~a" mail)
                     (@template "mail.ctml")
                     :mail mail))))

(define-page send-mail "courier/^campaign/([^/]+)/mail/([^/]+)/send$" (:uri-groups (campaign mail) :access (perm courier))
  (let ((mail (ensure-mail campaign mail)))
    (render-page (format NIL "Send ~a" (dm:field mail "title"))
                 (@template "mail-send.ctml")
                 :mail mail)))

(define-page list-tags "courier/^campaign/([^/]+)/tag/?$" (:uri-groups (campaign) :access (perm courier))
  (render-page "Tags"
               (@template "tags.ctml")
               :mails (list-tags campaign)))

(define-page tag-overview "courier/^campaign/([^/]+)/tag/([^/]+)/?$" (:uri-groups (campaign tag) :access (perm courier))
  )

(define-page edit-tag "courier/^campaign/([^/]+)/tag/([^/]+)/edit$" (:uri-groups (campaign tag) :access (perm courier))
  (if (string= tag "new")
      (render-page "New Tag"
                   (@template "tag.ctml")
                   :tag (make-tag :save NIL))
      (let ((tag (ensure-tag campaign tag)))
        (render-page (format NIL "Edit ~a" tag)
                     (@template "tag.ctml")
                     :tag tag))))

(define-page tag-members "courier/^campaign/([^/]+)/tag/([^/]+)/members$" (:uri-groups (campaign tag) :access (perm courier))
  )

(define-page subscriber-overview "courier/^subscriber/(.+)" (:uri-groups (subscriber) :access (perm courier))
  )

(define-page mail-log "courier/^log$" (:access (perm courier))
  (render-page "Mail Log" (@template "mail-log.ctml")))
