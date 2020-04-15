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

(defun render-public-page (page content &rest args)
  (r-clip:with-clip-processing ("public-template.ctml")
    (apply #'r-clip:process T
           :title (config :title)
           :page page
           :content (plump:parse content)
           :copyright (config :copyright)
           :version (asdf:component-version (asdf:find-system :courier))
           args)))

(define-page frontpage "courier/^$" ()
  (if (user:check (auth:current "anonymous") (perm courier))
      (render-page "Dashboard"
                   (@template "dashboard.ctml")
                   :hosts (list-hosts (auth:current))
                   :campaigns (list-campaigns (auth:current)))
      (render-page "Frontpage" (@template "frontpage.ctml"))))

(define-page host-list "courier/^host/?$" (:access (perm courier))
  (render-page "Configured Hosts"
               (@template "host-list.ctml")
               :hosts (list-hosts (auth:current))))

(define-page host-new ("courier/^host/new$" 1) (:uri-groups () :access (perm courier))
  (render-page "New Host"
               (@template "host-edit.ctml")
               :host (make-host :save NIL)))

(define-page host-edit "courier/^host/(.+)/edit$" (:uri-groups (host) :access (perm courier))
  (let ((host (check-accessible (ensure-host host))))
    (render-page (format NIL "Edit ~a" (dm:field host "title"))
                 (@template "host-edit.ctml")
                 :host host)))

(define-page campaign-list "courier/^campaign/?$" (:access (perm courier))
  (render-page "Campaigns"
               (@template "campaign-list.ctml")
               :campaigns (list-campaigns (auth:current))))

(define-page campaign-overview "courier/^campaign/([^/]+)/?$" (:uri-groups (campaign) :access (perm courier))
  (let ((campaign (check-accessible (ensure-campaign campaign))))
    (render-page (dm:field campaign "title")
                 (@template "campaign-overview.ctml")
                 :campaign campaign)))

(define-page campaign-new ("courier/^campaign/new$" 1) (:uri-groups () :access (perm courier))
  (let ((campaign (make-campaign (auth:current) NIL NIL
                                 :reply-to (or (user:field "email" (auth:current))
                                               (dm:field (first (list-hosts)) "address"))
                                 :save NIL)))
    (setf (dm:field campaign "template") (alexandria:read-file-into-string (@template "email/default-template.ctml")))
    (render-page "New Campaign"
                 (@template "campaign-edit.ctml")
                 :hosts (list-hosts (auth:current))
                 :campaign campaign)))

(define-page campaign-edit "courier/^campaign/([^/]+)/edit$" (:uri-groups (campaign) :access (perm courier))
  (let ((campaign (check-accessible (ensure-campaign campaign))))
    (render-page (format NIL "Edit ~a" (dm:field campaign "title"))
                 (@template "campaign-edit.ctml")
                 :hosts (list-hosts (auth:current))
                 :campaign campaign)))

(define-page mail-list "courier/^campaign/([^/]+)/mail/?$" (:uri-groups (campaign) :access (perm courier))
  (let ((campaign (check-accessible (ensure-campaign campaign)))
        (page (or (ignore-errors  (parse-integer (post/get "page"))) 0)))
    (render-page "Mails"
                 (@template "mail-list.ctml")
                 :mails (list-mails campaign :amount 100 :skip (* 100 page))
                 :campaign campaign
                 :next-page (url> (format NIL "courier/campaign/~a/mail" (dm:field campaign "title"))
                                  :query `(("page" . ,(princ-to-string (1+ page))))))))

(define-page mail-overview "courier/^campaign/([^/]+)/mail/([^/]+)/?$" (:uri-groups (campaign mail) :access (perm courier))
  (let ((mail (check-accessible (ensure-mail mail))))
    (render-page (dm:field mail "title")
                 (@template "mail-overview.ctml")
                 :campaign (ensure-campaign (dm:field mail "campaign"))
                 :mail mail)))

(define-page mail-new ("courier/^campaign/([^/]+)/mail/new$" 1) (:uri-groups (campaign) :access (perm courier))
  (render-page "New Mail"
               (@template "mail-edit.ctml")
               :mail (make-mail campaign :save NIL)))

(define-page mail-edit "courier/^campaign/([^/]+)/mail/([^/]+)/edit$" (:uri-groups (campaign mail) :access (perm courier))
  (let ((mail (check-accessible (ensure-mail mail))))
    (render-page (format NIL "Edit ~a" (dm:field mail "title"))
                 (@template "mail-edit.ctml")
                 :mail mail)))

(define-page mail-send "courier/^campaign/([^/]+)/mail/([^/]+)/send$" (:uri-groups (campaign mail) :access (perm courier))
  (let ((mail (check-accessible (ensure-mail mail))))
    (render-page (format NIL "Send ~a" (dm:field mail "title"))
                 (@template "mail-send.ctml")
                 :mail mail)))

(define-page tag-list "courier/^campaign/([^/]+)/tag/?$" (:uri-groups (campaign) :access (perm courier))
  (let ((campaign (check-accessible (ensure-campaign campaign))))
    (render-page "Tags"
                 (@template "tag-list.ctml")
                 :tags (list-tags campaign)
                 :campaign campaign)))

(define-page tag-overview "courier/^campaign/([^/]+)/tag/([^/]+)/?$" (:uri-groups (campaign tag) :access (perm courier))
  (let ((tag (check-accessible (ensure-tag tag))))
    (render-page (dm:field tag "title")
                 (@template "tag-overview.ctml")
                 :tag tag)))

(define-page tag-new ("courier/^campaign/([^/]+)/tag/new" 1) (:uri-groups (campaign) :access (perm courier))
  (render-page "New Tag"
               (@template "tag-edit.ctml")
               :tag (make-tag campaign :save NIL)))

(define-page tag-edit "courier/^campaign/([^/]+)/tag/([^/]+)/edit$" (:uri-groups (campaign tag) :access (perm courier))
  (let ((tag (check-accessible (ensure-tag tag))))
    (render-page (format NIL "Edit ~a" (dm:field tag "title"))
                 (@template "tag-edit.ctml")
                 :tag tag)))

(define-page tag-members "courier/^campaign/([^/]+)/tag/([^/]+)/members$" (:uri-groups (campaign tag) :access (perm courier))
  (let ((campaign (check-accessible (ensure-campaign campaign)))
        (tag (ensure-tag tag))
        (page (or (ignore-errors (parse-integer (post/get "page"))) 0)))
    (render-page (format NIL "~a Members" (dm:field tag "title"))
                 (@template "subscriber-list.ctml")
                 :campaign campaign
                 :subscribers (list-subscribers tag :amount 100 :skip (* 100 page))
                 :next-page (url> (format NIL "courier/campaign/~a/tag/~a/members" (dm:field campaign "title") (dm:id tag))
                                  :query `(("page" . ,(princ-to-string (1+ page))))))))

(define-page trigger-list "courier/^campaign/([^/]+)/trigger/?$" (:uri-groups (campaign) :access (perm courier))
  (let ((campaign (check-accessible (ensure-campaign campaign))))
    (render-page "Triggers"
                 (@template "trigger-list.ctml")
                 :triggers (list-triggers campaign)
                 :campaign campaign)))

(define-page trigger-new ("courier/^campaign/([^/]+)/trigger/new" 1) (:uri-groups (campaign) :access (perm courier))
  (let ((campaign (check-accessible (ensure-campaign campaign)))
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
  (let ((trigger (check-accessible (ensure-trigger trigger))))
    (render-page (format NIL "Edit ~a" (dm:id trigger))
                 (@template "trigger-edit.ctml")
                 :trigger trigger)))

(define-page subscriber-list "courier/^campaign/([^/]+)/subscriber$" (:uri-groups (campaign) :access (perm courier))
  (let ((campaign (check-accessible (ensure-campaign campaign)))
        (page (or (ignore-errors  (parse-integer (post/get "page"))) 0)))
    (render-page (format NIL "~a Subscribers" (dm:field campaign "title"))
                 (@template "subscriber-list.ctml")
                 :campaign campaign
                 :subscribers (list-subscribers campaign :amount 100 :skip (* 100 page))
                 :next-page (url> (format NIL "courier/campaign/~a/subscriber" (dm:field campaign "title"))
                                  :query `(("page" . ,(princ-to-string (1+ page))))))))

(define-page subscriber-overview "courier/^campaign/([^/]+)/subscriber/([^/]+)/?$" (:uri-groups (campaign subscriber) :access (perm courier))
  (let* ((campaign (ensure-campaign campaign))
         (subscriber (check-accessible (ensure-subscriber subscriber))))
    (render-page (format NIL (dm:field subscriber "address"))
                 (@template "subscriber-overview.ctml")
                 :campaign campaign
                 :subscriber subscriber
                 :fields (list-attributes campaign)
                 :field-values (subscriber-attributes subscriber)
                 :tags (list-tags subscriber))))

(define-page subscriber-new ("courier/^campaign/([^/]+)/subscriber/new" 1) (:uri-groups (campaign) :access (perm courier))
  (let* ((campaign (check-accessible (ensure-campaign campaign))))
    (render-page "New Subscriber"
                 (@template "subscriber-edit.ctml")
                 :campaign campaign
                 :fields (list-attributes campaign))))

(define-page subscriber-edit "courier/^campaign/([^/]+)/subscriber/([^/]+)/edit$" (:uri-groups (campaign subscriber) :access (perm courier))
  (let* ((campaign (ensure-campaign campaign))
         (subscriber (check-accessible (ensure-subscriber subscriber))))
    (render-page (format NIL "Edit ~a" (dm:field subscriber "address"))
                 (@template "subscriber-edit.ctml")
                 :campaign campaign
                 :subscriber subscriber
                 :fields (dm:get (rdb:join (attribute-value attribute) (attribute _id))
                                 (db:query (:= 'subscriber (dm:id subscriber))) :sort '(("title" :desc)))
                 :tags (list-tags subscriber)
                 :all-tags (list-tags campaign))))

(define-page subscriber-import ("courier/^campaign/([^/]+)/subscriber/import" 1) (:uri-groups (campaign) :access (perm courier))
  (let* ((campaign (check-accessible (ensure-campaign campaign))))
    (render-page "Import Subscribers"
                 (@template "subscriber-import.ctml")
                 :campaign campaign)))

(define-page sequence-list "courier/^campaign/([^/]+)/sequence/?" (:uri-groups (campaign) :access (perm courier))
  (let ((campaign (check-accessible (ensure-campaign campaign))))
    (render-page "Sequences"
                 (@template "sequence-list.ctml")
                 :campaign campaign
                 :sequences (list-sequences campaign))))

(define-page sequence-new "courier/^campaign/([^/]+)/sequence/new" (:uri-groups (campaign) :access (perm courier))
  (let ((campaign (check-accessible (ensure-campaign campaign))))
    (render-page "Create Sequence"
                 (@template "sequence-edit.ctml")
                 :sequence (make-sequence campaign "" :save NIL))))

(define-page sequence-edit "courier/^campaign/([^/]+)/sequence/([^/]+)/edit" (:uri-groups (campaign sequence) :access (perm courier))
  (let ((sequence (check-accessible (ensure-sequence sequence))))
    (render-page "Create Sequence"
                 (@template "sequence-edit.ctml")
                 :sequence sequence
                 :triggers (list-triggers sequence))))

(define-page file-list "courier/^campaign/([^/]+)/file/?" (:uri-groups (campaign) :access (perm courier))
  (let* ((campaign (check-accessible (ensure-campaign campaign)))
         (page (or (ignore-errors  (parse-integer (post/get "page"))) 0)))
    (render-page "File List"
                 (@template "file-list.ctml")
                 :campaign campaign
                 :files (list-files campaign :amount 100 :skip (* 100 page))
                 :next-page (url> (format NIL "courier/campaign/~a/file" (dm:field campaign "title"))
                                  :query `(("page" . ,(princ-to-string (1+ page))))))))

(define-page mail-log "courier/^log/mail/([^/]+)" (:uri-groups (mail) :access (perm courier))
  (let ((mail (check-accessible (ensure-mail mail))))
    (render-page "Mail Log"
                 (@template "mail-log.ctml")
                 :log (dm:get (rdb:join (((mail-log subscriber) (subscriber _id)) mail) (mail _id))
                              (db:query (:= 'mail (dm:id mail)))
                              :sort '(("send-time" :desc)) :amount 100))))

(define-page campaign-log "courier/^log/campaign/([^/]+)" (:uri-groups (campaign) :access (perm courier))
  (let ((campaign (check-accessible (ensure-campaign campaign))))
    (render-page "Mail Log"
                 (@template "mail-log.ctml")
                 ;; Does not work right, sigh.
                 :log (dm:get (rdb:join (((mail-log subscriber) (subscriber _id)) mail) (mail _id))
                              (db:query (:= 'campaign (dm:id campaign)))
                              :sort '(("send-time" :desc)) :amount 100))))

(define-page subscriber-log "courier/^log/subscriber/([^/]+)" (:uri-groups (subscriber) :access (perm courier))
  (let ((subscriber (check-accessible (ensure-subscriber subscriber))))
    (render-page "Mail Log"
                 (@template "mail-log.ctml")
                 :log (dm:get (rdb:join (((mail-log subscriber) (subscriber _id)) mail) (mail _id))
                              (db:query (:= 'subscriber (dm:id subscriber)))
                              :sort '(("send-time" :desc)) :amount 100))))

(define-page mail-queue "courier/^queue/" (:access (perm courier host))
  (render-page "Mail Queue"
               (@template "mail-queue.ctml")
               :queue (dm:get (rdb:join (((mail-queue subscriber) (subscriber _id)) mail) (mail _id))
                              (db:query :all)
                              :sort '(("send-time" :desc)) :amount 100)))

(define-page help "courier/^help(:?/([^/]+))?" (:uri-groups (page) :access (perm courier))
  (let ((content (plump:make-root)))
    (markless:output (markless:parse (@template (format NIL "help/~:[index~;~:*~a~].mess" page)) T)
                     :target content
                     :format 'org.shirakumo.markless.plump:plump)
    (render-page "Help"
                 (@template "help.ctml")
                 :text content)))

;; User sections
(define-page campaign-subscription "courier/^subscription/([^/]+)" (:uri-groups (campaign))
  (let ((campaign (ensure-campaign (db:ensure-id campaign))))
    (render-public-page (format NIL "Subscribe to ~a" (dm:field campaign "title"))
                        (@template "campaign-subscription.ctml")
                        :description (dm:field campaign "description")
                        :campaign campaign
                        :fields (list-attributes campaign)
                        :action (or (post/get "action") "subscribe"))))

(define-page mail-view "courier/^view/(.+)" (:uri-groups (id))
  (destructuring-bind (subscriber mail) (decode-id id)
    (let* ((mail (dm:get-one 'mail (db:query (:= '_id mail))))
           (subscriber (ensure-subscriber subscriber))
           (campaign (ensure-campaign (dm:field mail "campaign")))
           (content (getf (mail-template-args campaign mail subscriber) :body)))
      (mark-mail-received mail subscriber)
      (render-public-page (dm:field mail "subject")
                          (@template "mail-view.ctml")
                          :mail mail
                          :campaign campaign
                          :subscriber subscriber
                          :mail-content content))))

(defun mail-url (mail subscriber)
  (uri-to-url (format NIL "courier/view/~a" (generate-id subscriber (ensure-id mail)))
              :representation :external))

(define-page mail-archive "courier/^archive/(.+)" (:uri-groups (id))
  (destructuring-bind (subscriber) (decode-id id)
    (let* ((mails (dm:get (rdb:join (mail _id) (mail-log mail)) (db:query (:= 'subscriber subscriber))
                          :sort '(("send-time" :desc)) :hull 'mail))
           (subscriber (ensure-subscriber subscriber))
           (campaign (ensure-campaign (dm:field subscriber "campaign"))))
      (render-public-page (dm:field campaign "title")
                          (@template "mail-archive.ctml")
                          :mails mails
                          :campaign campaign
                          :subscriber subscriber))))

(defun archive-url (subscriber)
  (uri-to-url (format NIL "courier/archive/~a" (generate-id subscriber))
              :representation :external))
