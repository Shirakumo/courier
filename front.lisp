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
  (render-page campaign
               (@template "campaign-overview.ctml")
               :campaign (ensure-campaign campaign)))

(define-page campaign-new ("courier/^campaign/new$" 1) (:uri-groups () :access (perm courier))
  (let ((campaign (make-campaign :save NIL)))
    (setf (dm:field campaign "template") (alexandria:read-file-into-string (@template "email/default-template.ctml")))
    (render-page "New Campaign"
                 (@template "campaign-edit.ctml")
                 :hosts (list-hosts)
                 :campaign campaign)))

(define-page campaign-edit "courier/^campaign/([^/]+)/edit$" (:uri-groups (campaign) :access (perm courier))
  (render-page (format NIL "Edit ~a" campaign)
               (@template "campaign-edit.ctml")
               :hosts (list-hosts)
               :campaign (ensure-campaign campaign)))

(define-page campaign-subscribers "courier/^campaign/([^/]+)/subscribers(?:/([^/]+))?" (:uri-groups (campaign page) :access (perm courier))
  (let ((campaign (ensure-campaign campaign))
        (page (or (ignore-errors  (parse-integer page)) 0)))
    (render-page (format NIL "~a Subscribers" (dm:field campaign "title"))
                 (@template "campaign-subscribers.ctml")
                 :campaign campaign
                 :subscribers (dm:get 'subscriber (db:query (:= 'campaign (dm:id campaign)))
                                      :skip (* 100 page) :amount 100 :sort '(("signup-time" :desc)))
                 :next-page (1+ page))))

(define-page mail-list "courier/^campaign/([^/]+)/mail/?$" (:uri-groups (campaign) :access (perm courier))
  (render-page "Mails"
               (@template "mail-list.ctml")
               :mails (list-mails campaign)
               :campaign campaign))

(define-page mail-overview "courier/^campaign/([^/]+)/mail/([^/]+)/?$" (:uri-groups (campaign mail) :access (perm courier))
  (let ((mail (ensure-mail mail)))
    (render-page (dm:field mail "title")
                 (@template "mail-overview.ctml")
                 :campaign (ensure-campaign (dm:field mail "campaign"))
                 :mail mail)))

(define-page mail-new ("courier/^campaign/([^/]+)/mail/new$" 1) (:uri-groups (campaign) :access (perm courier))
  (render-page "New Mail"
               (@template "mail-edit.ctml")
               :mail (make-mail campaign :save NIL)))

(define-page mail-edit "courier/^campaign/([^/]+)/mail/([^/]+)/edit$" (:uri-groups (campaign mail) :access (perm courier))
  (let ((mail (ensure-mail mail)))
    (render-page (format NIL "Edit ~a" (dm:field mail "title"))
                 (@template "mail-edit.ctml")
                 :mail mail)))

(define-page mail-send "courier/^campaign/([^/]+)/mail/([^/]+)/send$" (:uri-groups (campaign mail) :access (perm courier))
  (let ((mail (ensure-mail mail)))
    (render-page (format NIL "Send ~a" (dm:field mail "title"))
                 (@template "mail-send.ctml")
                 :mail mail)))

(define-page mail-trigger-list "courier/^campaign/([^/]+)/mail/([^/]+)/trigger/?$" (:uri-groups (campaign mail) :access (perm courier))
  (let ((mail (ensure-mail mail)))
    (render-page (format NIL "~a Triggers" (dm:field mail "title"))
                 (@template "mail-trigger-list.ctml")
                 :triggers (list-mail-triggers mail)
                 :campaign campaign
                 :mail mail)))

(define-page mail-trigger-new "courier/^campaign/([^/]+)/mail/([^/]+)/trigger/new$" (:uri-groups (campaign mail) :access (perm courier))
  (let ((mail (ensure-mail mail)))
    (render-page (format NIL "New Trigger for ~a" mail)
                 (@template "mail-trigger-edit.ctml")
                 :trigger (make-mail-trigger mail :save NIL)
                 :campaign campaign
                 :mail mail)))

(define-page mail-trigger-edit "courier/^campaign/([^/]+)/mail/([^/]+)/trigger/(\\d+)/edit$" (:uri-groups (campaign mail trigger) :access (perm courier))
  (let ((mail (ensure-mail mail)))
    (render-page (format NIL "Edit Trigger #~a for ~a" trigger mail)
                 (@template "mail-trigger-edit.ctml")
                 :trigger (ensure-mail-trigger trigger)
                 :campaign campaign
                 :mail mail)))

(define-page tag-list "courier/^campaign/([^/]+)/tag/?$" (:uri-groups (campaign) :access (perm courier))
  (render-page "Tags"
               (@template "tag-list.ctml")
               :tags (list-tags campaign)
               :campaign campaign))

(define-page tag-overview "courier/^campaign/([^/]+)/tag/([^/]+)/?$" (:uri-groups (campaign tag) :access (perm courier))
  (let ((tag (ensure-tag tag)))
    (render-page (dm:field tag "title")
                 (@template "tag-overview.ctml")
                 :campaign (ensure-campaign campaign)
                 :tag tag)))

(define-page tag-new ("courier/^campaign/([^/]+)/tag/new" 1) (:uri-groups (campaign) :access (perm courier))
  (render-page "New Tag"
               (@template "tag-edit.ctml")
               :tag (make-tag campaign :save NIL)))

(define-page tag-edit "courier/^campaign/([^/]+)/tag/([^/]+)/edit$" (:uri-groups (campaign tag) :access (perm courier))
  (let ((tag (ensure-tag tag)))
    (render-page (format NIL "Edit ~a" (dm:field tag "title"))
                 (@template "tag-edit.ctml")
                 :tag tag)))

(define-page tag-members "courier/^campaign/([^/]+)/tag/([^/]+)/members(?:/([^/]+))?$" (:uri-groups (campaign tag page) :access (perm courier))
  (let ((campaign (ensure-campaign campaign))
        (tag (ensure-tag tag))
        (page (or (ignore-errors (parse-integer page)) 0)))
    (render-page (format NIL "~a Members" (dm:field tag "title"))
                 (@template "campaign-subscribers.ctml")
                 :campaign campaign
                 :subscribers (dm:get (rdb:join (subscriber _id) (tag-table subscriber)) (db:query (:= 'tag (dm:id tag)))
                                      :skip (* 100 page) :amount 100 :sort '(("signup-time" :desc)))
                 :next-page (1+ page))))

(define-page trigger-list "courier/^campaign/([^/]+)/trigger/?$" (:uri-groups (campaign) :access (perm courier))
  (let ((campaign (ensure-campaign campaign)))
    (render-page "Triggers"
                 (@template "trigger-list.ctml")
                 :triggers (list-triggers campaign)
                 :campaign campaign)))

(define-page trigger-new ("courier/^campaign/([^/]+)/trigger/new" 1) (:uri-groups (campaign) :access (perm courier))
  (let ((campaign (ensure-campaign campaign))
        (source (cond ((string= "mail" (post/get "source-type"))
                       (ensure-mail (post/get "source-id")))
                      ((string= "link" (post/get "source-type"))
                       (ensure-link (post/get "source-id")))
                      (T (dm:hull 'mail))))
        (target (cond ((string= "mail" (post/get "target-type"))
                       (ensure-mail (post/get "target-id")))
                      ((string= "tag" (post/get "target-type"))
                       (ensure-tag (post/get "target-id")))
                      (T (dm:hull 'mail)))))
    (render-page "New Trigger"
                 (@template "trigger-edit.ctml")
                 :trigger (make-trigger campaign source target :save NIL))))

(define-page trigger-edit "courier/^campaign/([^/]+)/trigger/([^/]+)/edit$" (:uri-groups (campaign trigger) :access (perm courier))
  (let ((trigger (ensure-trigger trigger)))
    (render-page (format NIL "Edit ~a" (dm:id trigger))
                 (@template "trigger-edit.ctml")
                 :trigger trigger)))

(define-page mail-log "courier/^log/mail/([^/]+)" (:uri-groups (mail) :access (perm courier))
  (let ((mail (ensure-mail mail)))
    (render-page "Mail Log"
                 (@template "mail-log.ctml")
                 :log (dm:get 'mail-log (db:query (:= 'mail (dm:id mail))) :sort '(("time" :desc)) :amount 100))))

(define-page campaign-log "courier/^log/campaign/([^/]+)" (:uri-groups (campaign) :access (perm courier))
  (let ((campaign (ensure-campaign campaign)))
    (render-page "Mail Log"
                 (@template "mail-log.ctml")
                 :log (dm:get (rdb:join (mail-log mail) (mail _id)) (db:query (:= 'campaign (dm:id campaign)))
                              :sort '(("time" :desc)) :amount 100))))

;; User sections
(define-page campaign-subscription "courier/^subscription(:?/.*)?" ()
  (let ((campaign (ensure-campaign (db:ensure-id (post/get "campaign")))))
    (r-clip:with-clip-processing ("campaign-subscription.ctml")
      (r-clip:process T
                      :title (format NIL "Subscribe to ~a" (dm:field campaign "title"))
                      :description (dm:field campaign "description")
                      :campaign campaign
                      :action (or (post/get "action") "subscribe")))))

(define-page mail-view "courier/^view/(.+)" (:uri-groups (id))
  (destructuring-bind (subscriber mail) (decode-id id)
    (mark-mail-received mail subscriber)
    (let* ((mail (dm:get-one 'mail (db:query (:= '_id mail))))
           (subscriber (dm:get-one 'subscriber (db:query (:= '_id subscriber))))
           (campaign (ensure-campaign (dm:field mail "campaign"))))
      (r-clip:with-clip-processing ("view-mail.ctml")
        (apply #'r-clip:process T
               :mail mail
               :campaign campaign
               :subscriber subscriber
               :content (compile-email-content campaign mail subscriber))))))

(defun mail-url (mail subscriber)
  (uri-to-url (format NIL "courier/view/~a" (generate-id subscriber (ensure-id mail)))
              :representation :external))
