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

(defun pageinated-args (uri list-fun arg &key (amount 100))
  (let ((page (int* (post/get "page") 0))
        (query (or* (post/get "query"))))
    (list :list (funcall list-fun arg :amount amount :skip (* amount page) :query query)
          :prev-page (when (< 0 page)
                       (url> uri
                             :query `(("page" . ,(princ-to-string (1- page)))
                                      ("query" . ,query))))
          :next-page (url> uri
                           :query `(("page" . ,(princ-to-string (1+ page)))
                                    ("query" . ,query))))))

(define-page frontpage "courier/^$" ()
  (if (user:check (auth:current "anonymous") (perm courier))
      (render-page "Dashboard"
                   (@template "dashboard.ctml")
                   :hosts (list-hosts (auth:current))
                   :campaigns (list-campaigns (auth:current)))
      (render-page "Frontpage" (@template "frontpage.ctml"))))

(define-page host-list "courier/^host/?$" (:access (perm courier user))
  (apply #'render-page "Configured Hosts"
         (@template "host-list.ctml")
         (pageinated-args "courier/host/" #'list-hosts (auth:current))))

(define-page host-new ("courier/^host/new$" 1) (:uri-groups () :access (perm courier host new))
  (render-page "New Host"
               (@template "host-edit.ctml")
               :up (url> "courier/host/") :up-text "Hosts"
               :host (make-host :save NIL)))

(define-page host-edit "courier/^host/(.+)/edit$" (:uri-groups (host) :access (perm courier host edit))
  (let ((host (check-accessible (ensure-host host))))
    (render-page "Edit"
                 (@template "host-edit.ctml")
                 :up (url> "courier/host/") :up-text (dm:field host "title")
                 :host host)))

(define-page campaign-list "courier/^campaign/?$" (:access (perm courier user))
  (apply #'render-page "Campaigns"
         (@template "campaign-list.ctml")
         (pageinated-args "courier/campaign/" #'list-campaigns (auth:current))))

(define-page campaign-overview "courier/^campaign/([^/]+)/?$" (:uri-groups (campaign) :access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 0)))
    (render-page (dm:field campaign "title")
                 (@template "campaign-overview.ctml")
                 :up (url> "courier/campaign/") :up-text "Campaigns"
                 :campaign campaign)))

(define-page campaign-new ("courier/^campaign/new$" 1) (:uri-groups () :access (perm courier campaign new))
  (let ((campaign (make-campaign (auth:current) NIL NIL
                                 :reply-to (or (user:field "email" (auth:current))
                                               (dm:field (first (list-hosts)) "address"))
                                 :save NIL)))
    (setf (dm:field campaign "template") (alexandria:read-file-into-string (@template "email/default-template.ctml")))
    (render-page "New"
                 (@template "campaign-edit.ctml")
                 :up (url> "courier/campaign/") :up-text "Campaigns"
                 :hosts (list-hosts (auth:current))
                 :campaign campaign)))

(define-page campaign-edit "courier/^campaign/([^/]+)/edit$" (:uri-groups (campaign) :access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign))))
    (render-page "Edit"
                 (@template "campaign-edit.ctml")
                 :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
                 :up-text (dm:field campaign "title")
                 :hosts (list-hosts (auth:current))
                 :campaign campaign)))

(define-page campaign-access "courier/^campaign/([^/]+)/access$" (:uri-groups (campaign) :access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign))))
    (render-page "Manage Access"
                 (@template "campaign-access.ctml")
                 :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
                 :up-text (dm:field campaign "title")
                 :campaign campaign
                 :access (list-access campaign))))

(define-page campaign-import ("courier/^campaign/([^/]+)/import$" 1) (:uri-groups (campaign) :access (perm courier user))
  (let* ((campaign (check-accessible (ensure-campaign campaign) :target 'campaign)))
    (render-page "CSV Import"
                 (@template "campaign-import.ctml")
                 :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
                 :up-text (dm:field campaign "title")
                 :campaign campaign)))

(define-page campaign-export ("courier/^campaign/([^/]+)/export$" 1) (:uri-groups (campaign) :access (perm courier user))
  (let* ((campaign (check-accessible (ensure-campaign campaign) :target 'campaign)))
    (render-page "CSV Export"
                 (@template "campaign-export.ctml")
                 :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
                 :up-text (dm:field campaign "title")
                 :campaign campaign)))

(define-page mail-list "courier/^campaign/([^/]+)/mail/?$" (:uri-groups (campaign) :access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 0)))
    (apply #'render-page "Mails"
           (@template "mail-list.ctml")
           :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
           :up-text (dm:field campaign "title")
           :campaign campaign
           (pageinated-args (format NIL "courier/campaign/~a/mail" (dm:field campaign "title")) #'list-mails campaign))))

(define-page mail-overview "courier/^campaign/([^/]+)/mail/([^/]+)/?$" (:uri-groups (campaign mail) :access (perm courier user))
  (let ((mail (check-accessible (ensure-mail mail) :target 0))
        (campaign (ensure-campaign campaign)))
    (render-page (dm:field mail "title")
                 (@template "mail-overview.ctml")
                 :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
                 :up-text (dm:field campaign "title")
                 :campaign (ensure-campaign (dm:field mail "campaign"))
                 :mail mail
                 :tags (list-tags mail))))

(define-page mail-new ("courier/^campaign/([^/]+)/mail/new$" 1) (:uri-groups (campaign) :access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 'mail)))
    (render-page "New Mail"
                 (@template "mail-edit.ctml")
                 :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
                 :up-text (dm:field campaign "title")
                 :mail (make-mail campaign :save NIL)
                 :all-tags (list-tags campaign))))

(define-page mail-edit "courier/^campaign/([^/]+)/mail/([^/]+)/edit$" (:uri-groups (campaign mail) :access (perm courier user))
  (let ((campaign (ensure-campaign campaign))
        (mail (check-accessible (ensure-mail mail))))
    (render-page "Edit"
                 (@template "mail-edit.ctml")
                 :up (url> (format NIL "courier/campaign/~a/mail/~a" (dm:field campaign "title") (dm:id mail)))
                 :up-text (dm:field mail "title")
                 :mail mail
                 :tags (list-tags mail)
                 :all-tags (list-tags campaign))))

(define-page mail-send "courier/^campaign/([^/]+)/mail/([^/]+)/send$" (:uri-groups (campaign mail) :access (perm courier user))
  (let ((mail (check-accessible (ensure-mail mail)))
        (campaign (ensure-campaign campaign)))
    (render-page "Send"
                 (@template "mail-send.ctml")
                 :up (url> (format NIL "courier/campaign/~a/mail/~a" (dm:field campaign "title") (dm:id mail)))
                 :up-text (dm:field mail "title")
                 :mail mail)))

(define-page mail-trend ("courier/^campaign/([^/]+)/mail/trend$" 1) (:uri-groups (campaign)) :access (perm courier user)
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 'mail)))
    (render-page "Mail trend"
                 (@template "mail-trend.ctml")
                 :campaign campaign
                 :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
                 :up-text (dm:field campaign "title"))))

(define-page tag-list "courier/^campaign/([^/]+)/tag/?$" (:uri-groups (campaign) :access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 0)))
    (apply #'render-page "Tags"
           (@template "tag-list.ctml")
           :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
           :up-text (dm:field campaign "title")
           :campaign campaign
           (pageinated-args (format NIL "courier/campaign/~a/tag" (dm:field campaign "title")) #'list-tags campaign))))

(define-page tag-overview "courier/^campaign/([^/]+)/tag/([^/]+)/?$" (:uri-groups (campaign tag) :access (perm courier user))
  (let ((campaign (ensure-campaign campaign))
        (tag (check-accessible (ensure-tag tag) :target 0)))
    (render-page (dm:field tag "title")
                 (@template "tag-overview.ctml")
                 :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
                 :up-text (dm:field campaign "title")
                 :tag tag)))

(define-page tag-new ("courier/^campaign/([^/]+)/tag/new" 1) (:uri-groups (campaign) :access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 'tag)))
    (render-page "New Tag"
                 (@template "tag-edit.ctml")
                 :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
                 :up-text (dm:field campaign "title")
                 :tag (make-tag campaign :save NIL))))

(define-page tag-edit "courier/^campaign/([^/]+)/tag/([^/]+)/edit$" (:uri-groups (campaign tag) :access (perm courier user))
  (let ((tag (check-accessible (ensure-tag tag)))
        (campaign (ensure-campaign campaign)))
    (render-page "Edit"
                 (@template "tag-edit.ctml")
                 :up (url> (format NIL "courier/campaign/~a/tag/~a" (dm:field campaign "title") (dm:id tag)))
                 :up-text (dm:field tag "title")
                 :tag tag)))

(define-page tag-members "courier/^campaign/([^/]+)/tag/([^/]+)/members$" (:uri-groups (campaign tag) :access (perm courier user))
  (let ((campaign (ensure-campaign campaign))
        (tag (check-accessible (ensure-tag tag) :target 0)))
    (apply #'render-page "Members"
           (@template "subscriber-list.ctml")
           :up (url> (format NIL "courier/campaign/~a/tag/~a" (dm:field campaign "title") (dm:id tag)))
           :up-text (dm:field tag "title")
           :campaign campaign
           (pageinated-args (format NIL "courier/campaign/~a/tag/~a/members" (dm:field campaign "title") (dm:id tag)) #'list-subscribers tag))))

(define-page trigger-list "courier/^campaign/([^/]+)/trigger/?$" (:uri-groups (campaign) :access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 0)))
    (apply #'render-page "Triggers"
           (@template "trigger-list.ctml")
           :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
           :up-text (dm:field campaign "title")
           :campaign campaign
           (pageinated-args (format NIL "courier/campaign/~a/trigger" (dm:field campaign "title")) #'list-triggers campaign))))

(define-page trigger-new ("courier/^campaign/([^/]+)/trigger/new" 1) (:uri-groups (campaign) :access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 'trigger))
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
                 :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
                 :up-text (dm:field campaign "title")
                 :trigger (make-trigger campaign source target :save NIL))))

(define-page trigger-edit "courier/^campaign/([^/]+)/trigger/([^/]+)/edit$" (:uri-groups (campaign trigger) :access (perm courier user))
  (let ((trigger (check-accessible (ensure-trigger trigger)))
        (campaign (ensure-campaign campaign)))
    (render-page (format NIL "Edit ~a" (dm:id trigger))
                 (@template "trigger-edit.ctml")
                 :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
                 :up-text (dm:field campaign "title")
                 :trigger trigger)))

(define-page subscriber-list "courier/^campaign/([^/]+)/subscriber/?$" (:uri-groups (campaign) :access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 0)))
    (apply #'render-page "Subscribers"
           (@template "subscriber-list.ctml")
           :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
           :up-text (dm:field campaign "title")
           :campaign campaign
           (pageinated-args (format NIL "courier/campaign/~a/subscriber" (dm:field campaign "title")) #'list-subscribers campaign))))

(define-page subscriber-overview "courier/^campaign/([^/]+)/subscriber/([^/]+)/?$" (:uri-groups (campaign subscriber) :access (perm courier user))
  (let* ((campaign (ensure-campaign campaign))
         (subscriber (check-accessible (ensure-subscriber subscriber) :target 0)))
    (render-page (dm:field subscriber "address")
                 (@template "subscriber-overview.ctml")
                 :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
                 :up-text (dm:field campaign "title")
                 :campaign campaign
                 :subscriber subscriber
                 :fields (list-attributes campaign)
                 :field-values (subscriber-attributes subscriber)
                 :tags (list-tags subscriber))))

(define-page subscriber-new ("courier/^campaign/([^/]+)/subscriber/new" 1) (:uri-groups (campaign) :access (perm courier user))
  (let* ((campaign (check-accessible (ensure-campaign campaign) :target 'subscriber)))
    (render-page "New Subscriber"
                 (@template "subscriber-edit.ctml")
                 :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
                 :up-text (dm:field campaign "title")
                 :campaign campaign
                 :fields (list-attributes campaign))))

(define-page subscriber-edit "courier/^campaign/([^/]+)/subscriber/([^/]+)/edit$" (:uri-groups (campaign subscriber) :access (perm courier user))
  (let* ((campaign (ensure-campaign campaign))
         (subscriber (check-accessible (ensure-subscriber subscriber))))
    (render-page "Edit"
                 (@template "subscriber-edit.ctml")
                 :up (url> (format NIL "courier/campaign/~a/subscriber/~a" (dm:field campaign "title") (dm:id subscriber)))
                 :up-text (dm:field subscriber "address")
                 :campaign campaign
                 :subscriber subscriber
                 ;; KLUDGE: Can't do this with a simple join since fields can be optional, so manually join here.
                 :fields (loop for attribute in (list-attributes campaign)
                               for val = (dm:get-one 'attribute-value (db:query (:and (:= 'attribute (dm:id attribute))
                                                                                      (:= 'subscriber (dm:id subscriber)))))
                               do (when val (setf (dm:field attribute "value") (dm:field val "value")))
                               collect attribute)
                 :tags (list-tags subscriber)
                 :all-tags (list-tags campaign))))

(define-page subscriber-compose "courier/^campaign/([^/]+)/subscriber/([^/]+)/compose$" (:uri-groups (campaign subscriber) :access (perm courier user))
  (let* ((campaign (ensure-campaign campaign))
         (subscriber (check-accessible (ensure-subscriber subscriber))))
    (render-page "Compose"
                 (@template "subscriber-compose.ctml")
                 :up (url> (format NIL "courier/campaign/~a/subscriber/~a" (dm:field campaign "title") (dm:id subscriber)))
                 :up-text (dm:field subscriber "address")
                 :subscriber subscriber
                 :mail (make-mail campaign :save NIL))))

(define-page sequence-list "courier/^campaign/([^/]+)/sequence/?" (:uri-groups (campaign) :access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 0)))
    (apply #'render-page "Sequences"
           (@template "sequence-list.ctml")
           :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
           :up-text (dm:field campaign "title")
           :campaign campaign
           (pageinated-args (format NIL "courier/campaign/~a/sequence" (dm:field campaign "title")) #'list-sequences campaign))))

(define-page sequence-new "courier/^campaign/([^/]+)/sequence/new" (:uri-groups (campaign) :access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 'sequence)))
    (render-page "New Sequence"
                 (@template "sequence-edit.ctml")
                 :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
                 :up-text (dm:field campaign "title")
                 :sequence (make-sequence campaign "" :save NIL))))

(define-page sequence-edit "courier/^campaign/([^/]+)/sequence/([^/]+)/edit" (:uri-groups (campaign sequence) :access (perm courier user))
  (let ((campaign (ensure-campaign campaign))
        (sequence (check-accessible (ensure-sequence sequence))))
    (render-page (dm:field sequence "title")
                 (@template "sequence-edit.ctml")
                 :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
                 :up-text (dm:field campaign "title")
                 :sequence sequence
                 :triggers (list-triggers sequence))))

(define-page file-list "courier/^campaign/([^/]+)/file/?" (:uri-groups (campaign) :access (perm courier user))
  (let* ((campaign (check-accessible (ensure-campaign campaign) :target 'file)))
    (apply #'render-page "Files"
           (@template "file-list.ctml")
           :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
           :up-text (dm:field campaign "title")
           :campaign campaign
           (pageinated-args (format NIL "courier/campaign/~a/file" (dm:field campaign "title")) #'list-files campaign))))

(define-page feed-list "courier/^campaign/([^/]+)/feed/?" (:uri-groups (campaign) :access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 'feed)))
    (apply #'render-page "Feeds"
           (@template "feed-list.ctml")
           :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
           :up-text (dm:field campaign "title")
           :campaign campaign
           (pageinated-args (format NIL "courier/campaign/~a/feed" (dm:field campaign "title")) #'list-feeds campaign))))

(define-page feed-overview "courier/^campaign/([^/]+)/feed/([^/]+)/?" (:uri-groups (campaign feed) :access (perm courier user))
  (let ((campaign (ensure-campaign campaign))
        (feed (check-accessible (ensure-feed feed))))
    (apply #'render-page (dm:field feed "title")
           (@template "mail-list.ctml")
           :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
           :up-text (dm:field campaign "title")
           :campaign campaign
           (pageinated-args (format NIL "courier/campaign/~a/feed/~a" (dm:field campaign "title") (dm:id feed)) #'list-mails feed))))

(define-page feed-new ("courier/^campaign/([^/]+)/feed/new" 1) (:uri-groups (campaign) :access (perm courier user))
  (let* ((campaign (check-accessible (ensure-campaign campaign) :target 'feed))
         (feed (make-feed campaign "" :save NIL)))
    (render-page "Create Feed"
                 (@template "feed-edit.ctml")
                 :feed feed)))

(define-page feed-edit "courier/^campaign/([^/]+)/feed/([^/+])/edit" (:uri-groups (campaign feed) :access (perm courier user))
  (let ((campaign (ensure-campaign campaign))
        (feed (check-accessible (ensure-feed feed))))
    (render-page "Edit"
                 (@template "feed-edit.ctml")
                 :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
                 :up-text (dm:field feed "title")
                 :feed feed)))

(define-page pool-list "courier/^campaign/([^/]+)/pool/?" (:uri-groups (campaign) :access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 'pool)))
    (apply #'render-page "Pools"
           (@template "pool-list.ctml")
           :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
           :up-text (dm:field campaign "title")
           :campaign campaign
           (pageinated-args (format NIL "courier/campaign/~a/pool" (dm:field campaign "title")) #'list-pools campaign))))

(define-page pool-overview "courier/^campaign/([^/]+)/pool/([^/]+)/?" (:uri-groups (campaign pool) :access (perm courier user))
  (let ((campaign (ensure-campaign campaign))
        (pool (check-accessible (ensure-pool pool))))
    (apply #'render-page (dm:field pool "title")
           (@template "pool-overview.ctml")
           :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
           :up-text (dm:field campaign "title")
           :campaign campaign
           :pool pool
           :entries (list-pool-entries pool)
           (pageinated-args (format NIL "courier/campaign/~a/pool/~a" (dm:field campaign "title") (dm:id pool)) #'list-pools pool))))

(define-page pool-new ("courier/^campaign/([^/]+)/pool/new" 1) (:uri-groups (campaign) :access (perm courier user))
  (let* ((campaign (check-accessible (ensure-campaign campaign) :target 'pool))
         (pool (make-pool campaign "" :save NIL)))
    (render-page "Create Pool"
                 (@template "pool-edit.ctml")
                 :pool pool)))

(define-page pool-edit "courier/^campaign/([^/]+)/pool/([^/+])/edit" (:uri-groups (campaign pool) :access (perm courier user))
  (let ((campaign (ensure-campaign campaign))
        (pool (check-accessible (ensure-pool pool))))
    (render-page "Edit"
                 (@template "pool-edit.ctml")
                 :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
                 :up-text (dm:field pool "title")
                 :pool pool)))

(define-page mail-log "courier/^log/mail/([^/]+)" (:uri-groups (mail) :access (perm courier user))
  (let ((mail (check-accessible (ensure-mail mail))))
    (render-page "Log"
                 (@template "mail-log.ctml")
                 :up (url> (format NIL "courier/campaign/~a/mail/~a" (dm:field mail "campaign") (dm:id mail)))
                 :up-text (dm:field mail "title")
                 :log (dm:get (rdb:join (((mail-log subscriber) (subscriber _id)) mail) (mail _id))
                              (db:query (:= 'mail (dm:id mail)))
                              :sort '(("send-time" :desc)) :amount 100))))

(define-page campaign-log "courier/^log/campaign/([^/]+)" (:uri-groups (campaign) :access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 'mail)))
    (render-page "Log"
                 (@template "mail-log.ctml")
                 :up (url> (format NIL "courier/campaign/~a" (dm:field campaign "title")))
                 :up-text (dm:field campaign "title")
                 ;; KLUDGE: Can't use the query to limit to campaign since the join messes things up.
                 :log (remove-if-not (lambda (dm) (equal (dm:id campaign) (dm:field dm "campaign")))
                                     (dm:get (rdb:join (((mail-log subscriber) (subscriber _id)) mail) (mail _id))
                                             (db:query :all)
                                             :sort '(("send-time" :desc)) :amount 100)))))

(define-page subscriber-log "courier/^log/subscriber/([^/]+)" (:uri-groups (subscriber) :access (perm courier user))
  (let ((subscriber (check-accessible (ensure-subscriber subscriber) :target 'mail)))
    (render-page "Log"
                 (@template "mail-log.ctml")
                 :up (url> (format NIL "courier/campaign/~a/subscriber/~a" (dm:field subscriber "campaign") (dm:id subscriber)))
                 :up-text (dm:field subscriber "address")
                 :log (dm:get (rdb:join (((mail-log subscriber) (subscriber _id)) mail) (mail _id))
                              (db:query (:= 'subscriber (dm:id subscriber)))
                              :sort '(("send-time" :desc)) :amount 100))))

(define-page mail-queue "courier/^queue/" (:access (perm courier host))
  (render-page "Mail Queue"
               (@template "mail-queue.ctml")
               :queue (dm:get (rdb:join (((mail-queue subscriber) (subscriber _id)) mail) (mail _id))
                              (db:query :all)
                              :sort '(("send-time" :desc)) :amount 100)))

(define-page help "courier/^help(:?/([^/]+))?" (:uri-groups (page) :access (perm courier user))
  (let ((content (plump:make-root)))
    (markless:output (markless:parse (@template (format NIL "help/~:[index~;~:*~a~].mess" page)) T)
                     :target content
                     :format 'org.shirakumo.markless.plump:plump)
    (render-page "Help"
                 (@template "help.ctml")
                 :text content)))

;; User sections
(define-page campaign-subscription "courier/^subscription/([^/]+)(?:/(.+))?" (:uri-groups (campaign action))
  (let ((campaign (ensure-campaign (db:ensure-id campaign))))
    (render-public-page (format NIL "Subscribe to ~a" (dm:field campaign "title"))
                        (@template "campaign-subscription.ctml")
                        :description (dm:field campaign "description")
                        :campaign campaign
                        :fields (list-attributes campaign)
                        :action (or action "subscribe"))))

(define-page tag-invite "courier/^invite/([^/]+)(?:/(.+))?" (:uri-groups (tag action))
  (declare (ignore tag))
  (destructuring-bind (subscriber tag) (decode-id (post/get "id"))
    (let* ((tag (ensure-tag tag))
           (campaign (ensure-campaign (dm:field tag "campaign")))
           (subscriber (ensure-subscriber subscriber)))
      (render-public-page (format NIL "Join ~a" (dm:field tag "title"))
                          (@template "tag-invite.ctml")
                          :action (or* action "join")
                          :tag tag
                          :campaign campaign
                          :subscriber subscriber))))

(defun tag-invite-url (tag subscriber)
  (uri-to-url (format NIL "courier/invite/~a" (dm:field tag "title"))
              :query `(("id" . ,(generate-id subscriber (ensure-id tag))))
              :representation :external))

(define-page mail-view "courier/^view/(.+)" (:uri-groups (id))
  (destructuring-bind (subscriber mail) (decode-id id)
    (let* ((mail (ensure-mail mail))
           (subscriber (ensure-subscriber subscriber))
           (campaign (ensure-campaign (dm:field mail "campaign")))
           (content (compile-mail-body (dm:field mail "body")
                                       (id-mail-type (dm:field mail "type"))
                                       :html
                                       :vars (mail-template-vars campaign mail subscriber)
                                       :campaign campaign
                                       :subscriber subscriber
                                       :mail mail)))
      (mark-mail-received mail subscriber)
      (render-public-page (dm:field mail "subject")
                          (@template "mail-view.ctml")
                          :mail mail
                          :campaign campaign
                          :subscriber subscriber
                          :mail-content content))))

(defun mail-url (mail subscriber)
  (when (or (typep mail 'db:id) (not (null (dm:id mail))))
    (uri-to-url (format NIL "courier/view/~a" (generate-id subscriber (ensure-id mail)))
                :representation :external)))

(define-page mail-archive "courier/^archive/(.+)" (:uri-groups (id))
  (destructuring-bind (subscriber) (decode-id id)
    (let* ((subscriber (ensure-subscriber subscriber))
           (campaign (ensure-campaign (dm:field subscriber "campaign")))
           (page (int* (post/get "page") 0))
           (query (or* (post/get "query"))))
      (render-public-page (dm:field campaign "title")
                          (@template "mail-archive.ctml")
                          :mails (list-mails subscriber :amount 100 :skip (* 100 page) :query query)
                          :campaign campaign
                          :subscriber subscriber))))

(defun archive-url (subscriber)
  (uri-to-url (format NIL "courier/archive/~a" (generate-id subscriber))
              :representation :external))
