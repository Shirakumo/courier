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

(define-api courier/host (host) (:access (perm courier host))
  (api-output (check-accessible (ensure-host host))))

(define-api courier/host/list () (:access (perm courier host list))
  (api-output (list-hosts (auth:current))))

(define-api courier/host/new (title address hostname &optional port username password encryption batch-size batch-cooldown) (:access (perm courier host new))
  (check-title title)
  (ratify:with-parsed-forms ((:email address)
                             (:host hostname)
                             (:port port)
                             (:integer encryption batch-size batch-cooldown))
    (let ((host (make-host :author (user:id (auth:current))
                           :title title
                           :address address
                           :hostname hostname
                           :port port
                           :username username
                           :password (or* password)
                           :encryption encryption
                           :batch-size batch-size
                           :batch-cooldown batch-cooldown)))
      (send-system-email (@template "email/confirm-host.mess")
                         address host NIL
                         :subject "Confirm your Courier host"
                         :link (url> "courier/api/courier/host/confirm"
                                     :query `(("host" . ,(princ-to-string (dm:id host)))
                                              ("token" . ,(hash (dm:id host)))
                                              ("browser" . "true"))))
      (output host "Host created. Please check your emails to confirm." "courier/host/"))))

(define-api courier/host/edit (host &optional title address hostname port username password encryption batch-size batch-cooldown) :access (perm courier host new)
  (when title (check-title title))
  (let ((host (check-accessible (ensure-host host))))
    (ratify:with-parsed-forms ((:email address)
                               (:host hostname)
                               (:port port)
                               (:integer encryption batch-size batch-cooldown))
      (setf-dm-fields host title address hostname port username encryption batch-size batch-cooldown)
      (when (or* password) (setf (dm:field host "password") (encrypt password)))
      (dm:save host)
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
    (send-system-email (@template "email/confirm-host.mess")
                       (dm:field host "address") host NIL
                       :subject "Courier host test"
                       :link (url> "courier/api/courier/host/confirm"
                                   :query `(("host" . ,(princ-to-string (dm:id host)))
                                            ("token" . ,(hash (dm:id host)))
                                            ("browser" . "true"))))
    (output NIL "Test email sent." "courier/host")))

(define-api courier/campaign (campaign) (:access (perm courier campaign))
  (api-output (check-accessible (ensure-campaign campaign))))

(define-api courier/campaign/list () (:access (perm courier campaign))
  (api-output (list-campaigns (auth:current))))

(define-api courier/campaign/new (host title &optional description reply-to template attribute[] attribute-type[] attribute-required[]) (:access (perm courier campaign new))
  (check-title title)
  ;; FIXME: Check template valid
  (let* ((host (check-accessible (ensure-host host)))
         (campaign (make-campaign (user:id (auth:current)) host title
                                  :description description :reply-to reply-to :template template
                                  :attributes (loop for title in attribute[]
                                                    for type in attribute-type[]
                                                    for required in attribute-required[]
                                                    collect (list title type required)))))
    (output campaign "Campaign created." "courier/campaign/")))

(define-api courier/campaign/edit (campaign &optional title host description reply-to template attribute[] attribute-type[] attribute-required[]) :access (perm courier campaign edit)
  (when title (check-title title))
  ;; FIXME: Check template valid
  (let ((campaign (check-accessible (ensure-campaign (db:ensure-id campaign)))))
    (edit-campaign campaign :host host :title title :description description :reply-to reply-to :template template
                            :attributes (loop for title in attribute[]
                                              for type in attribute-type[]
                                              for required in attribute-required[]
                                              collect (list title type required)))
    (output campaign "Campaign edited." "courier/campaign/")))

(define-api courier/campaign/delete (campaign) (:access (perm courier campaign delete))
  (let ((campaign (check-accessible (ensure-campaign campaign))))
    (delete-campaign campaign)
    (output campaign "Campaign deleted." "courier/campaign/")))

(define-api courier/campaign/preview (campaign &optional template title description reply-to) (:access (perm courier mail))
  (let* ((campaign (check-accessible (ensure-campaign campaign)))
         (subscriber (dm:get-one 'subscriber (db:query (:and (:= 'campaign (dm:id campaign))
                                                             (:= 'address (dm:field campaign "reply-to"))))))
         (mail (make-mail campaign :title "Test Email"
                                   :subject "This is a test!"
                                   :body (alexandria:read-file-into-string
                                          (@template "email/sample-body.mess"))
                                   :save NIL)))
    (setf (content-type *response*) "text/html; encoding=utf-8")
    (setf-dm-fields campaign template title description reply-to)
    (handler-case
        (let ((data (compile-email-content campaign mail subscriber))
              (plump:*tag-dispatchers* plump:*html-tags*))
          (plump:serialize data NIL))
      (error (e)
        (api-output NIL :status 500 :message (princ-to-string e))))))

(define-api courier/mail (mail) (:access (perm courier mail))
  (api-output (check-accessible (ensure-mail mail))))

(define-api courier/mail/list (campaign) (:access (perm courier mail list))
  (let ((campaign (check-accessible (ensure-campaign campaign))))
    (api-output (list-mails campaign))))

(define-api courier/mail/new (campaign title subject body &optional send) (:access (perm courier mail new))
  (check-title title)
  ;; FIXME: Check body valid
  (let* ((campaign (check-accessible (ensure-campaign (db:ensure-id campaign))))
         (mail (make-mail campaign
                          :title title
                          :subject subject
                          :body body)))
    (when send (enqueue-email mail))
    (output mail "Mail created." "courier/campaign/~a/mail/" (dm:field mail "campaign"))))

(define-api courier/mail/edit (mail &optional title subject body) (:access (perm courier mail edit))
  (check-title title)
  ;; FIXME: Check body valid
  (let* ((mail (check-accessible (ensure-mail mail))))
    (edit-mail mail :title title :subject subject :body body)
    (output mail "Mail edited." "courier/campaign/~a/mail/" (dm:field mail "campaign"))))

(define-api courier/mail/delete (mail) (:access (perm courier mail delete))
  (let* ((mail (check-accessible (ensure-mail mail))))
    (delete-mail mail)
    (output mail "Mail edited." "courier/campaign/~a/mail/" (dm:field mail "campaign"))))

(define-api courier/mail/test (mail) (:access (perm courier mail))
  (let ((mail (check-accessible (ensure-mail mail))))
    (enqueue-email mail :target (dm:field (ensure-campaign (dm:field mail "campaign")) "reply-to"))
    (output mail "Mail sent." "courier/campaign/~a/mail/~a/" (dm:field mail "campaign") (dm:id mail))))

(define-api courier/mail/send (mail &optional target-type target-id time) (:access (perm courier mail send))
  (let ((mail (check-accessible (ensure-mail mail))))
    (when time
      (setf time (local-time:timestamp-to-universal (local-time:parse-timestring time))))
    (if (or* target-type)
        (enqueue-email mail :target (resolve-typed target-type target-id) :time time)
        (enqueue-email mail :time time))
    (output mail "Mail sent." "courier/campaign/~a/mail/~a/" (dm:field mail "campaign") (dm:id mail))))

(define-api courier/mail/preview (&optional mail campaign title subject body) (:access (perm courier mail))
  (let* ((mail (cond (mail (check-accessible (ensure-mail mail)))
                     (campaign (make-mail (ensure-campaign campaign) :save NIL))
                     (T (error "Need MAIL or CAMPAIGN."))))
         (campaign (ensure-campaign (dm:field mail "campaign")))
         (subscriber (dm:get-one 'subscriber (db:query (:and (:= 'campaign (dm:id campaign))
                                                             (:= 'address (dm:field campaign "reply-to")))))))
    (setf (content-type *response*) "text/html; encoding=utf-8")
    (setf-dm-fields mail title subject body)
    (handler-case
        (let ((data (compile-email-content campaign mail subscriber))
              (plump:*tag-dispatchers* plump:*html-tags*))
          (plump:serialize data NIL))
      (error (e)
        (api-output NIL :status 500 :message (princ-to-string e))))))

(define-api courier/mail/trend (campaign) (:access (perm courier subscriber))
  (let ((campaign (check-accessible (ensure-campaign campaign)))
        (labels ())
        (data ()))
    (loop for mail in (dm:get 'mail (db:query (:= 'campaign (dm:id campaign))) :sort '((time :DESC)) :amount 10)
          do (push (dm:field mail "title") labels)
             (push (mail-coverage mail) data))
    (api-output (mktable "labels" labels "points" data))))

(define-api courier/mail/open-rate (mail) (:access (perm courier subscriber))
  (let* ((mail (check-accessible (ensure-mail mail)))
         (opened (db:count 'mail-receipt (db:query (:= 'mail (dm:id mail)))))
         (sent (db:count 'mail-log (db:query (:= 'mail (dm:id mail))))))
    (api-output (mktable "labels" '("Opened" "Unopened")
                         "points" (list opened (- sent opened))))))

(define-api courier/tag (tag) (:access (perm courier tag))
  (api-output (check-accessible (ensure-tag tag))))

(define-api courier/tag/list (campaign) (:access (perm courier tag))
  (let ((campaign (check-accessible (ensure-campaign campaign))))
    (api-output (list-tags campaign))))

(define-api courier/tag/subscribers (tag) (:access (perm courier tag))
  (let ((tag (check-accessible (ensure-tag tag))))
    (api-output (list-subscribers tag))))

(define-api courier/tag/new (campaign title &optional description) (:access (perm courier))
  (let ((campaign (check-accessible (ensure-campaign (db:ensure-id campaign)))))
    (let ((tag (make-tag campaign :title title :description description)))
      (output tag "Tag created." "courier/campaign/~a/tag" (dm:field tag "campaign")))))

(define-api courier/tag/edit (tag &optional title description) (:access (perm courier tag edit))
  (let ((tag (check-accessible (ensure-tag tag))))
    (edit-tag tag :title (or* title) :description (or* description))
    (output tag "Tag edited." "courier/campaign/~a/tag" (dm:field tag "campaign"))))

(define-api courier/tag/delete (tag) (:access (perm courier tag delete))
  (let* ((tag (check-accessible (ensure-tag tag))))
    (delete-tag tag)
    (output tag "Tag deleted." "courier/campaign/~a/tag" (dm:field tag "campaign"))))

(define-api courier/trigger (trigger) (:access (perm courier trigger))
  (api-output (check-accessible (ensure-trigger trigger))))

(define-api courier/trigger/list (campaign) (:access (perm courier trigger))
  (let ((campaign (check-accessible (ensure-campaign campaign))))
    (api-output (list-triggers campaign))))

(define-api courier/trigger/new (campaign source-type source-id target-type target-id &optional description time-offset tag-constraint) (:access (perm courier trigger new))
  (let ((campaign (check-accessible (ensure-campaign campaign)))
        (source (check-accessible (resolve-typed source-type source-id)))
        (target (check-accessible (resolve-typed target-type target-id))))
    (let ((trigger (make-trigger campaign source target
                                 :description description :time-offset time-offset
                                 :tag-constraint tag-constraint)))
      (output trigger "Trigger created." "courier/campaign/~a/trigger" (dm:field trigger "campaign")))))

(define-api courier/trigger/edit (trigger &optional source-type source-id target-type target-id description time-offset tag-constraint) (:access (perm courier trigger edit))
  (let ((trigger (check-accessible (ensure-trigger trigger)))
        (source (check-accessible (resolve-typed source-type source-id)))
        (target (check-accessible (resolve-typed target-type target-id))))
    (edit-trigger trigger :description description :time-offset time-offset :tag-constraint tag-constraint
                          :source source :target target)
    (output trigger "Trigger edited." "courier/campaign/~a/trigger" (dm:field trigger "campaign"))))

(define-api courier/trigger/delete (trigger) (:access (perm courier trigger delete))
  (let* ((trigger (check-accessible (ensure-trigger trigger))))
    (delete-trigger trigger)
    (output trigger "Trigger deleted." "courier/campaign/~a/trigger" (dm:field trigger "campaign"))))

(define-api courier/link (link) (:access (perm courier link))
  (api-output (check-accessible (ensure-link link))))

(define-api courier/link/new (campaign url) (:access (perm courier link new))
  (let ((campaign (check-accessible (ensure-campaign campaign))))
    (api-output (make-link campaign :url url))))

(define-api courier/link/list (campaign) (:access (perm courier link))
  (let ((campaign (check-accessible (ensure-campaign campaign))))
    (api-output (list-links campaign))))

(define-api courier/subscriber (subscriber) (:access (perm courier subscriber))
  (api-output (check-accessible (ensure-subscriber subscriber))))

(define-api courier/subscriber/list (campaign) (:access (perm courier subscriber))
  (let ((campaign (check-accessible (ensure-campaign campaign))))
    (api-output (list-subscribers campaign))))

(define-api courier/subscriber/tags (subscriber) (:access (perm courier subscriber))
  (let ((subscriber (check-accessible (ensure-subscriber subscriber))))
    (api-output (list-tags subscriber))))

(define-api courier/subscriber/new (campaign name address &optional tag[] fields[] values[]) (:access (perm courier subscriber new))
  (let* ((campaign (check-accessible (ensure-campaign campaign)))
         (attributes (loop for field in fields[]
                           for value in values[]
                           for attribute = (dm:get-one 'attribute (db:query (:= '_id (db:ensure-id field))))
                           do (unless (equal (dm:id campaign) (dm:field attribute "campaign"))
                                (error "Invalid attribute field."))
                           collect (cons attribute value)))
         (tags (mapcar #'ensure-tag tag[]))
         (subscriber (make-subscriber campaign name address :attributes attributes :tags tags :confirmed T)))
    (output subscriber "Subscriber added." "courier/campaign/~a/subscriber" (dm:field subscriber "campaign"))))

(define-api courier/subscriber/delete (subscriber) (:access (perm courier subscriber delete))
  (let ((subscriber (check-accessible (ensure-subscriber subscriber))))
    (delete-subscriber subscriber)
    (output subscriber "Subscriber deleted." "courier/campaign/~a/subscriber" (dm:field subscriber "campaign"))))

(define-api courier/subscriber/trend (campaign &optional (scale "week")) (:access (perm courier subscriber))
  (let ((campaign (check-accessible (ensure-campaign campaign)))
        (labels ())
        (data ()))
    (flet ((compute (start step y-labels)
             (loop for i from 0 below (length y-labels)
                   for d = (mod (- start i) (length y-labels))
                   for tmax = (get-universal-time) then tmin
                   for tmin = (- tmax step)
                   do (push (aref y-labels d) labels)
                      (push (db:count 'subscriber (db:query (:and (:= 'campaign (dm:id campaign))
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

;; User sections
(defvar *tracker* (alexandria:read-file-into-byte-vector (@static "receipt.gif")))

(define-api courier/subscription/new (campaign name address &optional fields[] values[]) ()
  (let* ((campaign (ensure-campaign campaign))
         (attributes (loop for field in fields[]
                           for value in values[]
                           for attribute = (dm:get-one 'attribute (db:query (:= '_id (db:ensure-id field))))
                           do (unless (equal (dm:id campaign) (dm:field attribute "campaign"))
                                (error "Invalid attribute field."))
                           collect (cons attribute value)))
         (subscriber (make-subscriber campaign name address :attributes attributes)))
    (send-system-email (@template "email/confirm-subscription.mess") address
                       (ensure-host (dm:field campaign "host"))
                       campaign
                       :subject (format NIL "Confirm your subscription for ~a" (dm:field campaign "title"))
                       :campaign (dm:field campaign "title")
                       :recipient (dm:field subscriber "name")
                       :link (url> "courier/api/courier/subscription/confirm"
                                   :query `(("id" . ,(generate-id subscriber))
                                            ("browser" . "true"))))
    (send-confirm-subscription campaign subscriber)
    (if (string= "true" (post/get "browser"))
        (redirect (url> (format NIL "courier/subscription/~a" (dm:id campaign))
                        :query '(("action" . "subscribed"))))
        (api-output subscriber))))

(define-api courier/subscription/confirm (id) ()
  (let ((subscriber (dm:get-one 'subscriber (db:query (:= '_id (first (decode-id id)))))))
    (setf (dm:field subscriber "confirmed") T)
    (dm:save subscriber)
    (if (string= "true" (post/get "browser"))
        (redirect (url> (format NIL "courier/subscription/~a" (dm:field subscriber "campaign"))
                        :query '(("action" . "confirmed"))))
        (api-output NIL))))

(define-api courier/subscription/delete (id) ()
  (let* ((subscriber (dm:get-one 'subscriber (db:query (:= '_id (first (decode-id id))))))
         (campaign (dm:get-one 'campaign (db:query (:= '_id (dm:field subscriber "campaign"))))))
    (delete-subscriber subscriber)
    (if (string= "true" (post/get "browser"))
        (redirect (url> (format NIL "courier/subscription/~a" (dm:id campaign))
                        :query '(("action" . "unsubscribed"))))
        (api-output NIL))))

(defun unsubscribe-url (subscriber)
  (url> "courier/api/courier/subscription/delete" 
        :query `(("id" . ,(generate-id subscriber))
                 ("browser" . "true"))))

(define-api courier/mail/receipt (id) ()
  (destructuring-bind (subscriber mail) (decode-id id)
    (mark-mail-received (ensure-mail mail) (ensure-subscriber subscriber)))
  (setf (content-type *response*) "image/gif")
  (setf (header "Cache-Control") "public, max-age=31536000")
  *tracker*)

(defun mail-receipt-url (subscriber mail)
  (url> "courier/api/courier/mail/receipt"
        :query `(("id" . ,(generate-id subscriber (ensure-id mail))))))

(define-api courier/link/resolve (id) ()
  (destructuring-bind (subscriber link &optional mail) (decode-id id)
    (mark-link-received (ensure-link link) (ensure-subscriber subscriber))
    (when mail (mark-mail-received (ensure-mail mail) (ensure-subscriber subscriber)))
    (let ((link (db:select 'link (db:query (:= '_id link)) :fields '("url"))))
      (redirect (gethash "url" link)))))

(defun link-receipt-url (subscriber link mail)
  (url> "courier/api/courier/link/resolve"
        :query `(("id" . ,(generate-id subscriber (ensure-id link) (ensure-id mail))))))
