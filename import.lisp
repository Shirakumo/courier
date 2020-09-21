#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defun normalize-fields (field-names fields)
  (loop for name in field-names
        for value in fields
        when (and name (or* value))
        collect (cons name value)))

(defun maybe-parse-date (date)
  (or (local-time:parse-timestring date :fail-on-error NIL :date-separator #\- :date-time-separator #\T)
      (local-time:parse-timestring date :fail-on-error NIL :date-separator #\_ :date-time-separator #\T)
      (local-time:parse-timestring date :fail-on-error NIL :date-separator #\/ :date-time-separator #\T)
      (local-time:parse-timestring date :fail-on-error NIL :date-separator #\- :date-time-separator #\ )
      (local-time:parse-timestring date :fail-on-error NIL :date-separator #\_ :date-time-separator #\ )
      (local-time:parse-timestring date :fail-on-error NIL :date-separator #\/ :date-time-separator #\ )))

(defun normalize-subscriber-field-names (field)
  (let ((normalized (cl-ppcre:regex-replace-all "[_-]" field " ")))
    (loop for (standard . matches) in
          '((:email "email" "mail" "email address" "mail address")
            (:first-name "first name" "first" "name" "surname" "given name")
            (:last-name "last name" "last" "family name")
            (:address "address" "home address" "physical address" "location")
            (:telephone "phone" "phone number" "telephone" "telephone number" "number")
            (:birthady "birthday" "birth day" "birth")
            (:rating "rating" "member rating")
            (:signup-time "signup time" "signed up" "optin time")
            (:confirm-time "confirm time" "confirmed")
            (:ip "ip" "ip address" "confirm ip")
            (:lat "latitude" "lat")
            (:lng "longitude" "lng")
            (:timezone "timezone" "tmz" "zone")
            (:region "region")
            (:notes "notes" "note" "remarks" "remark" "additional notes" "comment" "comments")
            (:tags "tags"))
          do (when (find normalized matches :test #'string-equal)
               (return standard)))))

(defun import-subscriber (campaign fields &key (if-exists :abort) tags)
  (flet ((value (key &optional default)
           (let ((cell (assoc key fields)))
             (if cell (cdr cell) default))))
    (let* ((address (or (value :email)
                        (error "No email address found.")))
           (name (or (when (value :first-name)
                       (format NIL "~a~@[ ~a~]" (value :first-name) (value :last-name)))
                     (value :last-name)
                     ""))
           (signup-time (local-time:timestamp-to-universal
                         (or (when (value :signup-time)
                               (maybe-parse-date (value :signup-time)))
                             (when (value :confirm-time)
                               (maybe-parse-date (value :confirm-time)))
                             (local-time:now))))
           (existing (dm:get-one 'subscriber (db:query (:and (:= 'campaign (dm:id campaign))
                                                             (:= 'address address))))))
      (if existing
          (ecase if-exists
            (:ignore)
            (:abort
             (error 'api-argument-invalid :argument 'csv :message "Subscriber already exists."))
            (:overwrite
             (edit-subscriber existing :name name :status :active)))
          (make-subscriber campaign name address :signup-time signup-time :status :active :tags tags)))))

(defun import-subscribers (campaign csv &key (if-exists :abort) tags)
  (destructuring-bind (fields . entries) csv
    (let ((field-names (mapcar #'normalize-subscriber-field-names fields)))
      (loop for entry in entries
            for fields = (normalize-fields field-names entry)
            collect (import-subscriber campaign fields :if-exists if-exists :tags tags)))))

(defun normalize-mail-field-names (field)
  (let ((normalized (cl-ppcre:regex-replace-all "[_-]" field " ")))
    (loop for (standard . matches) in
          '((:title "title" "name")
            (:subject "subject" "subject line")
            (:type "type" "content type" "markup")
            (:time "time" "creation time" "created")
            (:body "body" "content" "text" "mail" "email"))
          do (when (find normalized matches :test #'string-equal)
               (return standard)))))

(defun import-mail (campaign fields &key (if-exists :abort))
  (flet ((value (key &optional default)
           (let ((cell (assoc key fields)))
             (if cell (cdr cell) default))))
    (let* ((title (or (value :title)
                      (value :subject)
                      (error "No title found.")))
           (subject (or (value :subject)
                        (value :title)
                        (error "No subject found.")))
           (body (or (value :body)
                     (error "No email body found.")))
           (type (or (value :type) "text"))
           (type (cond ((string= type "markless") :markless)
                       ((string= type "ctml") :ctml)
                       ((string= type "html") :ctml)
                       (T :text)))
           (time (or (when (value :time)
                       (maybe-parse-date (value :time)))
                     (get-universal-time)))
           (existing (dm:get-one 'mail (db:query (:and (:= 'campaign (dm:id campaign))
                                                       (:= 'title title))))))
      (if existing
          (ecase if-exists
            (:ignore)
            (:abort
             (error 'api-argument-invalid :argument 'csv :message "Mail already exists."))
            (:overwrite
             (edit-mail existing :title title :subject subject :body body :type type)))
          (make-mail campaign :title title :subject subject :body body :type type :time time)))))

(defun import-mails (campaign csv &key (if-exists :abort))
  (destructuring-bind (fields . entries) csv
    (let ((field-names (mapcar #'normalize-mail-field-names fields)))
      (loop for entry in entries
            for fields = (normalize-fields field-names entry)
            collect (import-mail campaign fields :if-exists if-exists)))))

(defun import-csv (campaign csv &key (if-exists :abort) tags (dataset :subscribers))
  (db:with-transaction ()
    (flet ((process (csv)
             (ecase dataset
               (:subscribers (import-subscribers campaign csv :if-exists if-exists :tags tags))
               (:mails (import-mails campaign csv :if-exists if-exists)))))
      (etypecase csv
        (pathname
         (with-open-file (s csv)
           (process (cl-csv:read-csv s))))
        (string
         (process (cl-csv:read-csv csv)))))))
