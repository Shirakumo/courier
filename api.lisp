#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defun output (object message url-format &rest args)
  (let ((target (url> (apply #'format NIL url-format args)
                      :query `(("message" . ,message)))))
    (if (string= "true" (post/get "browser"))
        (redirect target)
        (api-output object :message message :target target))))

(defun try-compile-content (campaign mail subscriber)
  (handler-case
      (let ((data (compile-mail-content campaign mail subscriber))
            (plump:*tag-dispatchers* plump:*html-tags*))
        (plump:serialize data NIL))
    (markless:parser-warning (e)
      (error 'api-error :message (princ-to-string e)))
    (error (e)
      (error 'api-error :message (princ-to-string e)))))

(define-api courier/host (host) (:access (perm courier user))
  (api-output (check-accessible (ensure-host host))))

(define-api courier/host/list (&optional amount skip) (:access (perm courier user))
  (api-output (list-hosts (auth:current) :amount (int* amount) :skip (int* skip 0))))

(define-api courier/host/new (title address hostname &optional display-name port username password encryption batch-size batch-cooldown) (:access (perm courier host new))
  (check-title title)
  (ratify:with-parsed-forms ((:email address)
                             (:host hostname)
                             (:port port)
                             (:integer encryption batch-size batch-cooldown))
    (db:with-transaction ()
      (let ((host (make-host :author (user:id (auth:current))
                             :title title
                             :display-name display-name
                             :address address
                             :hostname hostname
                             :port port
                             :username username
                             :password (or* password)
                             :encryption encryption
                             :batch-size batch-size
                             :batch-cooldown batch-cooldown)))
        (send-system-mail (@template "email/confirm-host.mess")
                          address host NIL
                          :subject "Confirm your Courier host"
                          :recipient (dm:field host "display-name")
                          :link (url> "courier/api/courier/host/confirm"
                                      :query `(("host" . ,(princ-to-string (dm:id host)))
                                               ("token" . ,(hash (dm:id host)))
                                               ("browser" . "true"))))
        (output host "Host created. Please check your emails to confirm." "courier/host/")))))

(define-api courier/host/edit (host &optional title display-name address hostname port username password encryption batch-size batch-cooldown) :access (perm courier host edit)
  (when title (check-title title))
  (let ((host (check-accessible (ensure-host host))))
    (ratify:with-parsed-forms ((:email address)
                               (:host hostname)
                               (:port port)
                               (:integer encryption batch-size batch-cooldown))
      (db:with-transaction ()
        ;; Only unconfirm the host if relevant settings about it changed.
        (let ((same (macrolet ((same (field) `(string= ,field (dm:field host ,(string field)))))
                      (and (same address) (same hostname) (same port)
                           (same username) (same password) (same encryption)))))
          (edit-host host :title title :display-name display-name :address address
                          :hostname hostname :port port :username username :password password
                          :encryption encryption :batch-size batch-size :batch-cooldown batch-cooldown
                          :confirmed (when (dm:field host "confirmed") same))
          (unless same
            (send-system-mail (@template "email/confirm-host.mess")
                              (dm:field host "address") host NIL
                              :subject "Confirm your Courier host"
                              :recipient (dm:field host "display-name")
                              :link (url> "courier/api/courier/host/confirm"
                                          :query `(("host" . ,(princ-to-string (dm:id host)))
                                                   ("token" . ,(hash (dm:id host)))
                                                   ("browser" . "true")))))))
      (output host "Host edited." "courier/host"))))

(define-api courier/host/delete (host) (:access (perm courier host delete))
  (let ((host (check-accessible (ensure-host (db:ensure-id host)))))
    (delete-host host)
    (output NIL "Host deleted." "courier/host")))

(define-api courier/host/confirm (host token) (:access (perm courier host confirm))
  (db:with-transaction ()
    (let ((host (check-accessible (ensure-host (db:ensure-id host)))))
      (check-hash token (dm:id host))
      (setf (dm:field host "confirmed") T)
      (dm:save host)
      (output NIL "Host confirmed." "courier/host"))))

(define-api courier/host/test (host) (:access (perm courier host test))
  (let ((host (check-accessible (ensure-host (db:ensure-id host)))))
    (send-system-mail (@template "email/test-host.mess")
                      (dm:field host "address") host NIL
                      :subject "Courier host test"
                      :recipient (dm:field host "display-name"))
    (output NIL "Test email sent." "courier/host")))

(define-api courier/campaign (campaign) (:access (perm courier user))
  (api-output (check-accessible (ensure-campaign campaign))))

(define-api courier/campaign/list (&optional amount skip) (:access (perm courier user))
  (api-output (list-campaigns (auth:current) :amount (int* amount) :skip (int* skip 0))))

(define-api courier/campaign/new (host title &optional address description reply-to template report-interval attribute[] attribute-type[] attribute-qualifier[]) (:access (perm courier campaign new))
  (check-title title)
  (ratify:with-parsed-forms ((:email reply-to))
    (db:with-transaction ()
      (let* ((host (check-accessible (ensure-host host) :target 'campaign))
             (campaign (make-campaign (user:id (auth:current)) host title
                                      :description description :address address :reply-to reply-to :template template
                                      :report-interval (int* report-interval 0)
                                      :attributes (loop for title in attribute[]
                                                        for type in attribute-type[]
                                                        for qualifier in attribute-qualifier[]
                                                        collect (list title type (int* qualifier 2))))))
        ;; Compile template once to check for validity
        (compile-mail-content campaign (test-mail campaign) (campaign-author campaign))
        (output campaign "Campaign created." "courier/campaign/")))))

(define-api courier/campaign/edit (campaign &optional title host address description reply-to template report-interval attribute[] attribute-type[] attribute-qualifier[]) :access (perm courier user)
  (when title (check-title title))
  (ratify:with-parsed-forms ((:email reply-to))
    (db:with-transaction ()
      (let ((campaign (check-accessible (ensure-campaign (db:ensure-id campaign)))))
        (edit-campaign campaign :host host :title title :address address :description description :reply-to reply-to :template template
                                :report-interval (int* report-interval)
                                :attributes (loop for title in attribute[]
                                                  for type in attribute-type[]
                                                  for qualifier in attribute-qualifier[]
                                                  collect (list title type (int* qualifier 2))))
        ;; Compile template once to check for validity
        (try-compile-content campaign (test-mail campaign) (campaign-author campaign))
        (output campaign "Campaign edited." "courier/campaign/")))))

(define-api courier/campaign/set-access (campaign &optional user[] access-level[]) (:access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign))))
    (db:with-transaction ()
      (loop for username in user[]
            for access-level in access-level[]
            for user = (or (user:get username :if-does-not-exist NIL)
                           (error 'api-argument-invalid :argument 'user[] :message (format NIL "No such user ~a" username)))
            do (set-access user campaign :access-level (parse-integer access-level))))
    (output NIL "Access updated." "courier/campaign/~a/access" (dm:field campaign "title"))))

(define-api courier/campaign/delete (campaign) (:access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign))))
    (delete-campaign campaign)
    (output campaign "Campaign deleted." "courier/campaign/")))

(define-api courier/campaign/preview (campaign &optional template title description reply-to) (:access (perm courier user))
  (let* ((campaign (check-accessible (ensure-campaign campaign))))
    (setf (content-type *response*) "text/html; encoding=utf-8")
    (setf-dm-fields campaign template title description reply-to)
    (try-compile-content campaign (test-mail campaign) (campaign-author campaign))))

(define-api courier/campaign/import (campaign &optional content file (dataset "subscribers") (if-exists "abort") tag[]) (:access (perm courier user))
  (let* ((campaign (check-accessible (ensure-campaign campaign) :target 'subscriber))
         (subs (import-csv campaign (if file (first file) content)
                           :tags (mapcar #'ensure-tag tag[])
                           :dataset (cond ((string= dataset "subscribers") :subscribers)
                                          ((string= dataset "mails") :mails)
                                          (T (error 'api-argument-invalid :argument 'dataset)))
                           :if-exists (cond ((string= if-exists "abort") :abort)
                                            ((string= if-exists "ignore") :ignore)
                                            ((string= if-exists "overwrite") :overwrite)
                                            (T (error 'api-argument-invalid :argument 'if-exists))))))
    (output subs (format NIL "~d subscriber~:p imported." (length subs)) "courier/campaign/~a/subscriber" (dm:id campaign))))

(define-api courier/campaign/export (campaign &optional (dataset "subscribers") (header "include") (col-separator "comma") (row-separator "crlf") (quotation-use "as-needed") (quotation-mark "double-quote") (quotation-escape "quote") (time-format "iso-8601")) ()
  (flet ((timestamp-formatter (format)
           (lambda (x)
             (local-time:format-timestring NIL (local-time:universal-to-timestamp x) :format format))))
    (let* ((campaign (check-accessible (ensure-campaign campaign) :target 'subscriber))
           (header (cond ((string= header "include") T)
                         ((string= header "exclude") NIL)
                         (T (error 'api-argument-invalid :argument 'header))))
           (col-separator (cond ((string= col-separator "comma") #\,)
                                ((string= col-separator "tab") #\Tab)
                                (T (error 'api-argument-invalid :argument 'col-separator))))
           (row-separator (cond ((string= row-separator "crlf") (coerce '(#\Return #\Linefeed) 'string))
                                ((string= row-separator "lf") (coerce '(#\Linefeed) 'string))
                                (T (error 'api-argument-invalid :argument 'row-separator))))
           (quotation-use (cond ((string= quotation-use "as-needed") NIL)
                                ((string= quotation-use "always") T)
                                (T (error 'api-argument-invalid :argument 'quotation-use))))
           (quotation-mark (cond ((string= quotation-mark "double-quote") #\")
                                 ((string= quotation-mark "single-quote") #\')
                                 (T (error 'api-argument-invalid :argument 'quotation-mark))))
           (quotation-escape (cond ((string= quotation-escape "quote") (list quotation-mark quotation-mark))
                                   ((string= quotation-escape "backslash") (list #\\ quotation-mark))
                                   (T (error 'api-argument-invalid :argument 'qoutation-escape))))
           (time-format (cond ((string= time-format "iso-8601") (timestamp-formatter local-time:+iso-8601-format+))
                              ((string= time-format "rfc-1123") (timestamp-formatter local-time:+rfc-1123-format+))
                              ((string= time-format "timestamp") #'identity)
                              (T (error 'api-argument-invalid :argument 'time-format))))
           (rows (cond ((string= dataset "subscribers")
                        (let ((attributes (list-attributes campaign)))
                          (when header
                            (setf header (list* "Name" "Email Address" "Signup Time" "Status" "Tags"
                                                (loop for attribute in attributes collect (dm:field attribute "title")))))
                          (loop for subscriber in (list-subscribers campaign)
                                for tags = (list-tags subscriber)
                                collect (list* (dm:field subscriber "name")
                                               (dm:field subscriber "address")
                                               (funcall time-format (dm:field subscriber "signup-time"))
                                               (string (id-user-status (dm:field subscriber "status")))
                                               (with-output-to-string (out)
                                                 (loop for tag in tags do
                                                       (format out "~a," (dm:field tag "title"))))
                                               (loop for attribute in attributes
                                                     for value = (db:select 'attribute-value (db:query (:and (:= 'attribute (dm:id attribute))
                                                                                                             (:= 'subscriber (dm:id subscriber)))))
                                                     collect (if value (gethash "value" (first value)) ""))))))
                       ((string= dataset "mails")
                        (when header
                          (setf header (list "Title" "Subject" "Body" "Markup Type" "Time")))
                        (loop for mail in (list-mails campaign)
                              collect (list (dm:field mail "title")
                                            (dm:field mail "subject")
                                            (dm:field mail "body")
                                            (string (id-mail-type (dm:field mail "type")))
                                            (funcall time-format (dm:field mail "time")))))
                       (T (error 'api-argument-invalid :argument 'dataset)))))
      (setf (header "Content-Disposition") (format NIL "inline; filename=~s" (format NIL "~a ~a.csv" (dm:field campaign "title") dataset)))
      (setf (header "Content-Type") "text/csv;charset=utf-8")
      (with-output-to-string (out)
        (cl-csv:write-csv
         (if header
             (list* header rows)
             rows)
         :stream out
         :separator col-separator
         :quote quotation-mark
         :escape quotation-escape
         :newline row-separator
         :always-quote quotation-use)))))

(define-api courier/mail (mail) (:access (perm courier user))
  (api-output (check-accessible (ensure-mail mail))))

(define-api courier/mail/list (campaign &optional amount skip query) (:access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 'mail)))
    (api-output (list-mails campaign :amount (int* amount) :skip (int* skip 0) :query (or* query)))))

(define-api courier/mail/new (campaign title subject body &optional type send) (:access (perm courier user))
  (check-title title)
  (db:with-transaction ()
    (let* ((campaign (check-accessible (ensure-campaign (db:ensure-id campaign)) :target 'mail))
           (type (cond ((or (null type) (string= type "markless")) :markless)
                       ((string= type "text") :text)
                       ((string= type "html") :html)))
           (mail (make-mail campaign :title title :subject subject :body body :type type)))
      ;; Compile template once to check for validity
      (compile-mail-content campaign mail (campaign-author campaign))
      ;; Make sure we wake up the task in a bit from now to account for
      ;; the transaction completing.
      (when send (enqueue-mail mail :time (+ (get-universal-time) 10)))
      (output mail "Mail created." "courier/campaign/~a/mail/" (dm:field mail "campaign")))))

(define-api courier/mail/edit (mail &optional title subject body type) (:access (perm courier user))
  (check-title title)
  (db:with-transaction ()
    (let* ((mail (check-accessible (ensure-mail mail)))
           (type (cond ((string= type "markless") :markless)
                       ((string= type "text") :text)
                       ((string= type "html") :html)))
           (campaign (ensure-campaign (dm:field mail "campaign"))))
      (edit-mail mail :title title :subject subject :body body :type type)
      ;; Compile template once to check for validity
      (try-compile-content campaign mail (campaign-author campaign))
      (output mail "Mail edited." "courier/campaign/~a/mail/" (dm:field mail "campaign")))))

(define-api courier/mail/delete (mail) (:access (perm courier user))
  (let* ((mail (check-accessible (ensure-mail mail))))
    (delete-mail mail)
    (output mail "Mail deleted." "courier/campaign/~a/mail/" (dm:field mail "campaign"))))

(define-api courier/mail/test (mail) (:access (perm courier user))
  (let ((mail (check-accessible (ensure-mail mail))))
    (enqueue-mail mail :target (dm:field (ensure-campaign (dm:field mail "campaign")) "reply-to"))
    (output mail "Mail sent." "courier/campaign/~a/mail/~a/" (dm:field mail "campaign") (dm:id mail))))

(define-api courier/mail/send (mail &optional target-type target-id time) (:access (perm courier user))
  (let ((mail (check-accessible (ensure-mail mail))))
    (when time
      (setf time (local-time:timestamp-to-universal (local-time:parse-timestring time))))
    (if (or* target-type)
        (enqueue-mail mail :target (resolve-typed target-type target-id) :time time)
        (enqueue-mail mail :time time))
    (output mail "Mail sent." "courier/campaign/~a/mail/~a/" (dm:field mail "campaign") (dm:id mail))))

(define-api courier/mail/preview (&optional mail campaign title subject body) (:access (perm courier user))
  (let* ((mail (cond (mail (check-accessible (ensure-mail mail)))
                     (campaign (make-mail (ensure-campaign campaign) :save NIL))
                     (T (error 'api-argument-missing :argument '(mail campaign) :message "Need MAIL or CAMPAIGN."))))
         (campaign (ensure-campaign (dm:field mail "campaign"))))
    (setf (content-type *response*) "text/html; encoding=utf-8")
    (setf-dm-fields mail title subject body)
    (setf (dm:field mail "subject") (or* (dm:field mail "subject") "-"))
    (try-compile-content campaign mail (campaign-author campaign))))

(define-api courier/mail/trend (campaign) (:access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 'mail))
        (labels ())
        (data ()))
    (loop for mail in (dm:get 'mail (db:query (:= 'campaign (dm:id campaign))) :sort '((time :DESC)) :amount 10)
          do (push (dm:field mail "title") labels)
             (push (mail-coverage mail) data))
    (api-output (mktable "labels" labels "points" data))))

(define-api courier/mail/open-rate (mail) (:access (perm courier user))
  (let* ((mail (check-accessible (ensure-mail mail)))
         (opened (db:count 'mail-receipt (db:query (:= 'mail (dm:id mail)))))
         (sent (db:count 'mail-log (db:query (:= 'mail (dm:id mail))))))
    (api-output (mktable "labels" '("Opened" "Unopened")
                         "points" (list opened (- sent opened))))))

(define-api courier/tag (tag) (:access (perm courier user))
  (api-output (check-accessible (ensure-tag tag))))

(define-api courier/tag/list (campaign &optional amount skip) (:access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 'tag)))
    (api-output (list-tags campaign :amount (int* amount) :skip (int* skip 0)))))

(define-api courier/tag/subscribers (tag &optional amount skip) (:access (perm courier user))
  (let ((tag (check-accessible (ensure-tag tag))))
    (api-output (list-subscribers tag :amount (int* amount) :skip (int* skip 0)))))

(define-api courier/tag/tagged-rate (tag) (:access (perm courier user))
  (let* ((tag (check-accessible (ensure-tag tag)))
         (tagged (db:count 'tag-table (db:query (:= 'tag (dm:id tag)))))
         (total (db:count 'subscriber (db:query (:= 'campaign (dm:field tag "campaign"))))))
    (api-output (mktable "labels" '("Tagged" "Untagged")
                         "points" (list tagged (- total tagged))))))

(define-api courier/tag/distribution (campaign) (:access (perm courier user))
  (let* ((campaign (check-accessible (ensure-campaign campaign) :target 'tag))
         (tags (list-tags campaign)))
    (api-output (mktable "labels" (list*
                                   "untagged"
                                   (loop for tag in tags
                                         collect (dm:field tag "title")))
                         "points" (list*
                                   (db:count (rdb:join (subscriber _id) (tag-table subscriber) :left)
                                             (db:query (:and (:null 'tag) (:= 'campaign (dm:id campaign)))))
                                   (loop for tag in tags
                                         collect (db:count 'tag-table (db:query (:= 'tag (dm:id tag))))))))))

(define-api courier/tag/new (campaign title &optional description) (:access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign (db:ensure-id campaign)) :target 'tag)))
    (let ((tag (make-tag campaign :title title :description description)))
      (output tag "Tag created." "courier/campaign/~a/tag" (dm:field tag "campaign")))))

(define-api courier/tag/edit (tag &optional title description) (:access (perm courier user))
  (let ((tag (check-accessible (ensure-tag tag))))
    (edit-tag tag :title (or* title) :description (or* description))
    (output tag "Tag edited." "courier/campaign/~a/tag" (dm:field tag "campaign"))))

(define-api courier/tag/delete (tag) (:access (perm courier user))
  (let* ((tag (check-accessible (ensure-tag tag))))
    (delete-tag tag)
    (output tag "Tag deleted." "courier/campaign/~a/tag" (dm:field tag "campaign"))))

(define-api courier/trigger (trigger) (:access (perm courier user))
  (api-output (check-accessible (ensure-trigger trigger))))

(define-api courier/trigger/list (campaign &optional amount skip) (:access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 'trigger)))
    (api-output (list-triggers campaign :amount (int* amount) :skip (int* skip 0)))))

(define-api courier/trigger/new (campaign source-type source-id target-type target-id &optional description delay tag-constraint rule) (:access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 'trigger))
        (source (check-accessible (resolve-typed source-type source-id)))
        (target (check-accessible (resolve-typed target-type target-id)))
        (rule (not (or (null rule) (string= "" rule) (string-equal "false" rule)))))
    (let ((trigger (make-trigger campaign source target
                                 :description description :delay (parse-integer delay)
                                 :tag-constraint tag-constraint :rule rule)))
      (output trigger "Trigger created." "courier/campaign/~a/trigger" (dm:field trigger "campaign")))))

(define-api courier/trigger/edit (trigger &optional source-type source-id target-type target-id description delay tag-constraint rule) (:access (perm courier user))
  (let ((trigger (check-accessible (ensure-trigger trigger)))
        (source (check-accessible (resolve-typed source-type source-id)))
        (target (check-accessible (resolve-typed target-type target-id))))
    (if rule
        (edit-trigger trigger :description description :delay (when delay (parse-integer delay)) :tag-constraint tag-constraint
                              :source source :target target :rule (string-equal rule "true"))
        (edit-trigger trigger :description description :delay (when delay (parse-integer delay)) :tag-constraint tag-constraint
                              :source source :target target))
    (output trigger "Trigger edited." "courier/campaign/~a/trigger" (dm:field trigger "campaign"))))

(define-api courier/trigger/delete (trigger) (:access (perm courier user))
  (let* ((trigger (check-accessible (ensure-trigger trigger))))
    (delete-trigger trigger)
    (output trigger "Trigger deleted." "courier/campaign/~a/trigger" (dm:field trigger "campaign"))))

(define-api courier/link (link) (:access (perm courier user))
  (api-output (check-accessible (ensure-link link))))

(define-api courier/link/new (campaign url) (:access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 'link)))
    (api-output (make-link campaign :url url))))

(define-api courier/link/list (campaign &optional amount skip) (:access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign))))
    (api-output (list-links campaign :amount (int* amount) :skip (int* skip 0)))))

(define-api courier/subscriber (subscriber) (:access (perm courier user))
  (api-output (check-accessible (ensure-subscriber subscriber))))

(define-api courier/subscriber/list (campaign &optional amount skip query) (:access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 'subscriber)))
    (api-output (list-subscribers campaign :amount (int* amount) :skip (int* skip 0) :query (or* query)))))

(define-api courier/subscriber/tags (subscriber &optional amount skip) (:access (perm courier user))
  (let ((subscriber (check-accessible (ensure-subscriber subscriber))))
    (api-output (list-tags subscriber :amount (int* amount) :skip (int* skip 0)))))

(defun gather-api-attributes (campaign)
  (loop for attribute in (list-attributes campaign)
        for value = (post/get (dm:field attribute "title"))
        if (or* value)
        collect (cons attribute value)
        else
        do (when (dm:field attribute "required")
             (error 'api-argument-missing :argument (dm:field attribute "title")
                                          :message (format NIL "The attribute ~s is required but missing."
                                                           (dm:field attribute "title"))))))

(define-api courier/subscriber/new (campaign name address &optional tag[]) (:access (perm courier user))
  (let* ((campaign (check-accessible (ensure-campaign campaign) :target 'subscriber))
         (attributes (gather-api-attributes campaign))
         (tags (mapcar #'ensure-tag tag[]))
         (subscriber (make-subscriber campaign name address :attributes attributes :tags tags :status :active)))
    (output subscriber "Subscriber added." "courier/campaign/~a/subscriber" (dm:field subscriber "campaign"))))

(define-api courier/subscriber/edit (subscriber &optional name tag[]) (:access (perm courier user))
  (let* ((subscriber (check-accessible (ensure-subscriber subscriber)))
         (campaign (ensure-campaign (dm:field subscriber "campaign")))
         (attributes (gather-api-attributes campaign))
         (tags (mapcar #'ensure-tag tag[])))
    (edit-subscriber subscriber :name name :tags tags :attributes attributes)
    (output subscriber "Subscriber edited." "courier/campaign/~a/subscriber" (dm:field subscriber "campaign"))))

(define-api courier/subscriber/delete (subscriber) (:access (perm courier user))
  (let ((subscriber (check-accessible (ensure-subscriber subscriber))))
    (when (string= (dm:field subscriber "address")
                   (dm:field (ensure-campaign (dm:field subscriber "campaign")) "reply-to"))
      (error 'api-argument-invalid :argument "subscriber" :message "Cannot unsubscribe yourself."))
    (delete-subscriber subscriber)
    (output subscriber "Subscriber deleted." "courier/campaign/~a/subscriber" (dm:field subscriber "campaign"))))

(define-api courier/subscriber/unsubscribe (subscriber) (:access (perm courier user))
  (let ((subscriber (check-accessible (ensure-subscriber subscriber))))
    (when (string= (dm:field subscriber "address")
                   (dm:field (ensure-campaign (dm:field subscriber "campaign")) "reply-to"))
      (error 'api-argument-invalid :argument "subscriber" :message "Cannot unsubscribe yourself."))
    (edit-subscriber subscriber :status :deactivated)
    (output subscriber "Subscriber deactivated." "courier/campaign/~a/subscriber" (dm:field subscriber "campaign"))))

(define-api courier/subscriber/compose (subscriber subject body) (:access (perm courier user))
  (let* ((subscriber (check-accessible (ensure-subscriber subscriber)))
         (mail (make-mail (ensure-campaign (dm:field subscriber "campaign"))
                          :title subject
                          :subject subject
                          :body body
                          :save NIL)))
    (send-mail mail subscriber)
    (output mail "Mail sent." "courier/campaign/~a/subscriber/~a/" (dm:field subscriber "campaign") (dm:id subscriber))))

(define-api courier/subscriber/trend (campaign &optional (scale "week")) (:access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 'subscriber))
        (labels ())
        (data ()))
    (flet ((compute (start step y-labels)
             (loop for i from 0 below (length y-labels)
                   for d = (mod (- start i) (length y-labels))
                   for tmax = (get-universal-time) then tmin
                   for tmin = (- tmax step)
                   do (push (aref y-labels d) labels)
                      (push (db:count 'subscriber (db:query (:and (:= 'campaign (dm:id campaign))
                                                                  (:= 'status (user-status-id :active))
                                                                  (:<= 'signup-time tmax)
                                                                  (:< tmin 'signup-time))))
                            data))))
      (cond ((equal scale "week")
             (compute (nth-value 6 (decode-universal-time (get-universal-time) 0))
                      (* 60 60 24)
                      #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))
            ((equal scale "month")
             (compute (1- (nth-value 3 (decode-universal-time (get-universal-time) 0)))
                      (* 60 60 24)
                      (coerce (nreverse (loop for i downfrom (nth-value 3 (decode-universal-time (get-universal-time) 0)) to 1
                                              collect i))
                              'vector)))
            ((equal scale "year")
             (compute (1- (nth-value 4 (decode-universal-time (get-universal-time) 0)))
                      (* 60 60 24 30)
                      #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
            (T
             (error 'api-argument-invalid :argument "scale"))))
    (api-output (mktable "labels" labels "points" data))))

(define-api courier/subscriber/open-rate (subscriber) (:access (perm courier user))
  (let* ((subscriber (check-accessible (ensure-subscriber subscriber)))
         (opened (db:count 'mail-receipt (db:query (:= 'subscriber (dm:id subscriber)))))
         (sent (db:count 'mail-log (db:query (:= 'subscriber (dm:id subscriber))))))
    (api-output (mktable "labels" '("Opened" "Unopened")
                         "points" (list opened (- sent opened))))))

(define-api courier/file (file) (:access (perm courier user))
  (let ((file (check-accessible (ensure-file file))))
    (setf (dm:field file "url") (file-url file))
    (api-output file)))

(define-api courier/file/list (campaign &optional amount skip) (:access (perm courier user))
  (api-output (list-files (check-accessible (ensure-campaign campaign) :target 'file)
                          :amount (int* amount) :skip (int* skip 0))))

(define-api courier/file/new (campaign file) (:access (perm courier user))
  (let* ((campaign (check-accessible (ensure-campaign campaign) :target 'file))
         (file (make-file campaign (first file) (third file) :filename (second file))))
    (setf (dm:field file "url") (file-url file))
    (output file "File created." "courier/campaign/~a/file" (dm:id campaign))))

(define-api courier/file/delete (file) (:access (perm courier user))
  (let ((file (check-accessible (ensure-file file))))
    (delete-file file)
    (output NIL "File deleted." "courier/campaign/~a/file" (dm:field file "campaign"))))

(define-api courier/sequence (sequence) (:access (perm courier user))
  (let ((sequence (check-accessible (ensure-sequence sequence))))
    (setf (dm:field sequence "triggers") (list-triggers sequence))
    (api-output sequence)))

(define-api courier/sequence/list (campaign &optional amount skip) (:access (perm courier user))
  (api-output (list-sequences (check-accessible (ensure-campaign campaign) :target 'sequence)
                              :amount (int* amount) :skip (int* skip 0))))

(define-api courier/sequence/new (campaign title &optional delay[] subject[]) (:access (perm courier user))
  (let* ((campaign (check-accessible (ensure-campaign campaign) :target 'sequence))
         (sequence (make-sequence campaign title :triggers (loop for delay in delay[]
                                                                 for subject in subject[]
                                                                 collect (list (parse-integer delay) subject)))))
    (output sequence "Sequence created." "courier/campaign/~a/sequence/~a/edit"
            (dm:id campaign) (dm:id sequence))))

(define-api courier/sequence/edit (sequence &optional title trigger[] delay[] subject[]) (:access (perm courier user))
  (let ((sequence (check-accessible (ensure-sequence sequence))))
    (edit-sequence sequence :title title :triggers (loop for trigger in trigger[]
                                                         for delay in delay[]
                                                         for subject in subject[]
                                                         if (string-equal trigger "new")
                                                         collect (list NIL (parse-integer delay) subject)
                                                         else
                                                         collect (list (db:ensure-id trigger) (parse-integer delay) subject)))
    (output sequence "Sequence edited." "courier/campaign/~a/sequence/~a/edit" (dm:field sequence "campaign") (dm:id sequence))))

(define-api courier/sequence/delete (sequence) (:access (perm courier user))
  (let ((sequence (check-accessible (ensure-sequence sequence))))
    (delete-sequence sequence)
    (output NIL "Sequence deleted." "courier/campaign/~a/sequence" (dm:field sequence "campaign"))))

(define-api courier/feed (feed) (:access (perm courier user))
  (let ((feed (check-accessible (ensure-feed feed))))
    (setf (dm:field feed "entries") (db:select 'feed-entry (db:query (:= 'feed (dm:id feed)))))
    (api-output feed)))

(define-api courier/feed/list (campaign &optional amount skip) (:access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 'feed)))
    (api-output (list-feeds campaign :amount (int* amount) :skip (int* skip 0)))))

(define-api courier/feed/new (campaign url &optional title frequency template send-new backfill) (:access (perm courier user))
  (let ((campaign (check-accessible (ensure-campaign campaign) :target 'feed))
        (feed (make-feed campaign url :title title :frequency (int* frequency 10) :template template
                                      :send-new (string= send-new "true") :backfill (string= backfill "true"))))
    (output feed "Feed created." "courier/campaign/~a/feed" (dm:id campaign))))

(define-api courier/feed/edit (feed &optional url title frequency template send-new) (:access (perm courier user))
  (let ((feed (check-accessible (ensure-feed feed))))
    (if send-new
        (edit-feed feed :url url :title title :frequency (int* frequency) :template template :send-new (string= send-new "true"))
        (edit-feed feed :url url :title title :frequency (int* frequency) :template template))
    (output feed "Feed edited." "courier/campaign/~a/feed" (dm:field feed "campaign"))))

(define-api courier/feed/delete (feed) (:access (perm courier user))
  (let ((feed (check-accessible (ensure-feed feed))))
    (delete-feed feed)
    (output NIL "Feed deleted." "courier/campaign/~a/feed" (dm:field feed "campaign"))))

(define-api courier/feed/preview (&optional feed campaign url template) (:access (perm courier user))
  (let* ((feed (cond (feed (check-accessible (ensure-feed feed)))
                     ((and campaign url) (make-feed (ensure-campaign campaign) url :save NIL))
                     (T (error 'api-argument-missing :argument '(mail url campaign) :message "Need a feed URL."))))
         (campaign (ensure-campaign (dm:field feed "campaign")))
         (data (fetch-feed (or* url (dm:field feed "url"))))
         (entry (first (feeder:content data)))
         (content (compile-mail-body (or* template (dm:field feed "template")) :ctml :html
                                     :vars (feed-variables data entry)))
         (mail (make-mail campaign :title (feeder:title entry)
                                   :subject (feeder:title entry)
                                   :body content
                                   :type :ctml
                                   :save NIL)))
    (setf (content-type *response*) "text/html; encoding=utf-8")
    (try-compile-content campaign mail (campaign-author campaign))))

;; User sections
(defvar *tracker* (alexandria:read-file-into-byte-vector (@static "receipt.gif")))

(define-api courier/subscription/new (campaign address &optional name username email) ()
  ;; Honeypot
  (when (or username
            (string/= email (hash (config :salt))))
    (error 'api-argument-invalid :argument 'username :message "Invalid email address."))
  (check-address-valid address)
  (let* ((campaign (ensure-campaign campaign))
         (attributes (gather-api-attributes campaign))
         (name (or* name)))
    (handler-case ;; Try to add normally
        (let ((subscriber (make-subscriber campaign name address :attributes attributes)))
          (send-system-mail (@template "email/confirm-subscription.mess") address
                            (ensure-host (dm:field campaign "host"))
                            campaign
                            :subject (format NIL "Confirm your subscription for ~a" (dm:field campaign "title"))
                            :campaign (dm:field campaign "title")
                            :recipient (dm:field subscriber "name")
                            :link (confirm-url subscriber))
          (if (string= "true" (post/get "browser"))
              (redirect (url> (format NIL "courier/subscription/~a/subscribed" (dm:id campaign))
                              :query `(("address" . ,address))))
              (api-output subscriber)))
      (api-argument-invalid (e)
        ;; If we're already subscribed, just update the account to be active again and change the fields.
        (cond ((eql (slot-value e 'radiance::argument) 'address)
               (let ((subscriber (dm:get-one 'subscriber (db:query (:and (:= 'campaign (dm:id campaign))
                                                                         (:= 'address address))))))
                 (case (id-user-status (dm:field subscriber "status"))
                   (:unconfirmed
                    (if (string= "true" (post/get "browser"))
                        (redirect (url> (format NIL "courier/subscription/~a/subscribed" (dm:id campaign))
                                        :query `(("address" . ,address))))
                        (api-output subscriber)))
                   (:active
                    (if (string= "true" (post/get "browser"))
                        (redirect (url> (format NIL "courier/subscription/~a/confirmed" (dm:id campaign))))
                        (api-output subscriber)))
                   (:deactivated
                    (edit-subscriber subscriber :name name :attributes attributes :status :active)
                    (if (string= "true" (post/get "browser"))
                        (redirect (url> (format NIL "courier/subscription/~a/confirmed" (dm:id campaign))))
                        (api-output subscriber))))))
              (T (error e)))))))

(define-api courier/subscription/confirm (id) ()
  (let ((subscriber (edit-subscriber (first (decode-id id)) :status :active)))
    (if (string= "true" (post/get "browser"))
        (redirect (url> (format NIL "courier/subscription/~a/confirmed" (dm:field subscriber "campaign"))))
        (api-output NIL))))

(defun confirm-url (subscriber)
  (url> "courier/api/courier/subscription/confirm"
        :query `(("id" . ,(generate-id subscriber))
                 ("browser" . "true"))))

(define-api courier/subscription/delete (id) ()
  (let* ((subscriber (ensure-subscriber (first (decode-id id))))
         (campaign (ensure-campaign (dm:field subscriber "campaign"))))
    (when (string= (dm:field subscriber "address")
                   (dm:field campaign "reply-to"))
      (error 'api-argument-invalid :argument "subscriber" :message "Cannot unsubscribe the campaign's primary user."))
    (edit-subscriber subscriber :status :deactivated)
    (if (string= "true" (post/get "browser"))
        (redirect (url> (format NIL "courier/subscription/~a/unsubscribed" (dm:id campaign))
                        :query `(("id" . ,(princ-to-string (dm:id subscriber))))))
        (api-output NIL))))

(define-api courier/invite/accept (id) ()
  (destructuring-bind (subscriber tag) (decode-id id)
    (let* ((tag (ensure-tag tag))
           (subscriber (ensure-subscriber subscriber))
           (campaign (ensure-campaign (dm:field tag "campaign"))))
      (tag subscriber tag)
      (if (string= "true" (post/get "browser"))
          (redirect (url> (format NIL "courier/invite/~a/joined" (dm:field campaign "title"))
                          :query `(("id" . ,id))))
          (api-output NIL)))))

(defun unsubscribe-url (subscriber)
  (url> "courier/api/courier/subscription/delete" 
        :query `(("id" . ,(generate-id subscriber))
                 ("browser" . "true"))))

(define-api courier/mail/receipt (id) ()
  (destructuring-bind (subscriber &optional mail) (decode-id id)
    (when mail
      (mark-mail-received (ensure-mail mail) (ensure-subscriber subscriber))))
  (setf (content-type *response*) "image/gif")
  (setf (header "Cache-Control") "public, max-age=31536000")
  *tracker*)

(defun mail-receipt-url (subscriber mail)
  (when (or (typep mail 'db:id) (not (null (dm:id mail))))
    (url> "courier/api/courier/mail/receipt"
          :query `(("id" . ,(generate-id subscriber (ensure-id mail)))))))

(define-api courier/link/resolve (id) ()
  (destructuring-bind (subscriber link &optional mail) (decode-id id)
    (mark-link-received (ensure-link link) (ensure-subscriber subscriber))
    (when mail (mark-mail-received (ensure-mail mail) (ensure-subscriber subscriber)))
    (let ((link (first (db:select 'link (db:query (:= '_id link)) :fields '("url")))))
      (unless link
        (error 'api-argument-invalid :argument 'id :message "Invalid link ID."))
      (redirect (gethash "url" link)))))

(defun link-receipt-url (subscriber link mail)
  (url> "courier/api/courier/link/resolve"
        :query `(("id" . ,(generate-id subscriber (ensure-id link) (ensure-id mail))))))

(define-api courier/file/view (id) ()
  (let ((file (ensure-file (first (decode-id id)))))
    (setf (header "Cache-Control") "public, max-age=31536000, immutable")
    (setf (header "Content-Disposition") (format NIL "inline; filename=~s" (dm:field file "filename")))
    (serve-file (file-pathname file) (dm:field file "mime-type"))))

(defun file-url (file)
  (url> "courier/api/courier/file/view"
        :query `(("id" . ,(generate-id file)))))
