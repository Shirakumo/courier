#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defun normalize-field-names (field)
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
           (signup-time (or (when (value :signup-time)
                              (maybe-parse-date (value :signup-time)))
                            (when (value :confirm-time)
                              (maybe-parse-date (value :confirm-time)))
                            (get-universal-time)))
           (existing (dm:get-one 'subscriber (db:query (:and (:= 'campaign (dm:id campaign))
                                                             (:= 'address address))))))
      (if existing
          (ecase if-exists
            (:ignore)
            (:abort
             (error 'api-argument-invalid :argument 'csv :message "Subscriber"))
            (:overwrite
             (edit-subscriber existing :name name :status :active)))
          (make-subscriber campaign name address :signup-time signup-time :status :active :tags tags)))))

(defun import-subscribers (campaign csv &key (if-exists :abort) tags)
  (db:with-transaction ()
    (flet ((process (csv)
             (destructuring-bind (fields . entries) csv
               (let ((field-names (mapcar #'normalize-field-names fields)))
                 (loop for entry in entries
                       for fields = (normalize-fields field-names entry)
                       collect (import-subscriber campaign fields :if-exists if-exists :tags tags))))))
      (etypecase csv
        (pathname
         (with-open-file (s csv)
           (process (cl-csv:read-csv s))))
        (string
         (process (cl-csv:read-csv csv)))))))
