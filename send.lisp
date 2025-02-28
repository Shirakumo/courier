(in-package #:courier)

(defun compile-mail (template &rest args)
  (apply #'r-clip:process
         (etypecase template
           (string template)
           (pathname (@template (namestring template))))
         :copyright (config :copyright)
         :software-title (config :title)
         :software-version (load-time-value (asdf:component-version (asdf:find-system :courier)))
         args))

(defun extract-plaintext (html)
  ;; KLUDGE: This is real dumb.
  ;; FIXME: links do not get retained
  (with-output-to-string (out)
    (lquery:$ html "body" (text) (each (lambda (text) (write-line text out))))))

(defun extract-subject (html)
  (lquery:$1 html "head title" (text)))

(defun format-mail (text)
  (etypecase text
    (string
     (values text))
    (plump:node
     (values (extract-plaintext text)
             (plump:serialize text NIL)))))

(defun send (host to subject message &key plain reply-to message-id campaign unsubscribe)
  (multiple-value-bind (extracted html) (format-mail message)
    (l:trace :courier.mail "Sending to ~a via ~a/~a:~a"
             to
             (dm:field host "address")
             (dm:field host "hostname")
             (dm:field host "port"))
    (let ((cl-smtp::*x-mailer* #.(format NIL "Courier Mailer ~a" (asdf:component-version (asdf:find-system :courier)))))
      (cl-smtp:send-email
       (copy-seq (dm:field host "hostname"))
       (dm:field host "address")
       to subject (or plain extracted)
       :html-message html
       :extra-headers `(,@(when campaign
                            `(("X-Campaign" ,(princ-to-string campaign))
                              ("X-CampaignID" ,(princ-to-string campaign))
                              ("X-CampaignMessageID" 
                               ,(format NIL "<~a@courier.~a>"
                                        (hash (format NIL "~a-~a" (or message-id (get-universal-time)) campaign))
                                        (dm:field host "hostname")))
                              ("List-ID" ,(princ-to-string campaign))))
                        ,@(when unsubscribe
                            `(("List-Unsubscribe" ,unsubscribe)
                              ("List-Unsubscribe-Post" "List-Unsubscribe=One-Click"))))
       :ssl (ecase (dm:field host "encryption")
              (0 NIL) (1 :starttls) (2 :tls))
       :port (dm:field host "port")
       :reply-to reply-to
       :authentication (when (dm:field host "username")
                         (list :plain
                               (dm:field host "username")
                               (decrypt (dm:field host "password"))))
       :display-name (dm:field host "display-name")))))

(defun mail-subscriber-vars (subscriber)
  (list* :archive-url (archive-url subscriber)
         :unsubscribe-url (unsubscribe-url subscriber)
         :to (dm:field subscriber "address")
         :name (dm:field subscriber "name")
         :tags (list-tags subscriber)
         (subscriber-attributes subscriber)))

(defun mail-campaign-vars (campaign)
  (list :campaign (dm:field campaign "title")
        :description (dm:field campaign "description")
        :reply-to (dm:field campaign "reply-to")
        :address (dm:field campaign "address")
        :time (get-universal-time)))

(defun mail-template-vars (campaign mail subscriber)
  (list* :mail-receipt-image (mail-receipt-url subscriber mail)
         :mail-url (mail-url mail subscriber)
         :id (dm:id mail)
         :title (dm:field mail "title")
         :subject (dm:field mail "subject")
         (append (mail-campaign-vars campaign)
                 (mail-subscriber-vars subscriber))))

(defgeneric compile-mail-body (body source-type target-type &key &allow-other-keys))

(defmethod compile-mail-body (body (source-type (eql :text)) (target-type (eql :text)) &key)
  body)

(defmethod compile-mail-body (body (source-type (eql :text)) (target-type (eql :html)) &key)
  (make-instance 'plump-dom:text-node :parent NIL :text body))

(defmethod compile-mail-body (body (source-type (eql :ctml)) (target-type (eql :text)) &key vars)
  ;; FIXME: This does not personalise links.
  (plump-dom:text (apply #'r-clip:process body vars)))

(defmethod compile-mail-body (body (source-type (eql :ctml)) (target-type (eql :html)) &key vars)
  (apply #'r-clip:process body vars))

(defun compile-mail-content (campaign mail subscriber &key (template (dm:field campaign "template")) (type :html))
  (let ((vars (mail-template-vars campaign mail subscriber)))
    (apply #'compile-mail template
           (list* :body (compile-mail-body (dm:field mail "body")
                                           (id-mail-type (dm:field mail "type"))
                                           type
                                           :vars vars
                                           :campaign campaign
                                           :subscriber subscriber
                                           :mail mail)
                  vars))))

(defun send-mail (mail subscriber)
  (l:debug :courier.mail "Sending ~a to ~a" mail subscriber)
  (let* ((campaign (ensure-campaign (dm:field mail "campaign")))
         (host (ensure-host (dm:field campaign "host")))
         html plain)
    (handler-bind ((error (lambda (e)
                            (declare (ignore e))
                            (mark-mail-sent mail subscriber :compile-failed))))
      (setf html (compile-mail-content campaign mail subscriber))
      (setf plain (compile-mail-content campaign mail subscriber :template #p"email/text-template.ctml" :type :text)))
    (handler-bind ((error (lambda (e)
                            (declare (ignore e))
                            (mark-mail-sent mail subscriber :send-failed))))
      (send host (dm:field subscriber "address")
            (extract-subject html)
            html
            :plain (plump:text plain)
            :reply-to (dm:field campaign "reply-to")
            :message-id (dm:id mail)
            :campaign (dm:field mail "campaign")
            :unsubscribe (unsubscribe-url subscriber)))
    (when (dm:id mail)
      (mark-mail-sent mail subscriber))))

(defun send-system-mail (body address host campaign &rest vars)
  (let ((html (apply #'compile-mail #p"email/system-template.ctml"
                     (list* :body (compile-mail-body body :markless :html :vars vars)
                            (append vars (when campaign (mail-campaign-vars campaign))))))
        (host (ensure-host host)))
    (handler-case
        (send host address
              (extract-subject html)
              html
              :reply-to (if campaign
                            (dm:field campaign "reply-to")
                            (dm:field host "address"))
              :campaign (when campaign
                          (dm:id campaign)))
      (usocket:ns-error ()
        (error 'api-error :message (format NIL "Failed to resolve email hostname ~s. Is your hostname correct?"
                                           (dm:field host "hostname"))))
      (usocket:socket-error ()
        (error 'api-error :message (format NIL "Failed to connect to email host ~s. Is the server up and running?"
                                           (dm:field host "hostname"))))
      (cl-smtp:smtp-error ()
        (error 'api-error :message (format NIL "Failed to send email over host ~s. Are your credentials correct?"
                                           (dm:field host "hostname")))))))
