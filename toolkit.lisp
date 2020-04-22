#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defun encrypt (thing)
  (cryptos:encrypt thing (config :private-key)
                   :normalize-key :hash
                   :mode :cbc
                   :iv (defaulted-config (cryptos:make-salt T) :private-key-iv)))

(defun decrypt (thing)
  (cryptos:decrypt thing (config :private-key)
                   :normalize-key :hash
                   :mode :cbc
                   :iv (defaulted-config (cryptos:make-salt T) :private-key-iv)))

(defun hash (thing)
  (let ((thing (etypecase thing
                 (string thing)
                 (integer (princ-to-string thing)))))
    (cryptos:pbkdf2-hash thing (config :private-key) :digest :sha256)))

(defun check-hash (hash thing &optional (argument 'token))
  (unless (equal hash (hash thing))
    (error 'api-argument-invalid :argument argument)))

(defun url> (uri &key query fragment)
  (uri-to-url uri :representation :external
                  :query query
                  :fragment fragment))

(defmacro setf-dm-fields (model &rest vars)
  (let ((modelg (gensym "MODEL")))
    `(let ((,modelg ,model))
       ,@(loop for var in vars
               collect (destructuring-bind (var &optional (field (string-downcase var))) (radiance::enlist var)
                         `(typecase ,var
                            (null)
                            (dm:data-model
                             (setf (dm:field ,modelg ,field) (dm:id ,var)))
                            (T
                             (setf (dm:field ,modelg ,field) ,var)))))
       ,modelg)))

(defun check-title (title)
  (when (string= title "")
    (error 'api-argument-invalid :argument 'title :message "The title cannot be empty."))
  (when (string-equal title "new")
    (error 'api-argument-invalid :argument 'title :message "The title cannot be \"new\"."))
  (when (find #\/ title)
    (error 'api-argument-invalid :argument 'title :message "The title cannot contain a slash.")))

(defun check-title-exists (collection title query)
  (check-title title)
  (when (and title (< 0 (db:count collection query)))
    (error 'api-argument-invalid
           :argument 'title
           :message (format NIL "A ~(~a~) titled ~s already exists."
                            collection title))))

(defun generate-id (subscriber &rest ids)
  (let* ((id (ensure-id subscriber))
         (hash (subseq (cryptos:sha1 (princ-to-string id)) 0 8)))
    (encrypt (format NIL "~a/~a~{ ~a~}" hash id ids))))

(defun decode-id (thing)
  (let* ((string (decrypt thing))
         (slashpos (position #\/ string))
         (buffer (make-string-output-stream))
         (results ()))
    (unless (and (eql 8 slashpos))
      (error 'api-argument-invalid :argument 'id :message "Malformed ID"))
    (flet ((collect ()
             (push (db:ensure-id (get-output-stream-string buffer)) results)))
      (loop for i from (1+ slashpos) below (length string)
            for char = (aref string i)
            do (if (char= char #\Space)
                   (collect)
                   (write-char char buffer))
            finally (collect))
      (nreverse results))))

(defun mktable (&rest entries)
  (let ((table (make-hash-table)))
    (loop for (k v) on entries by #'cddr
          do (setf (gethash k table) v))
    table))

(defun test-mail (campaign)
  (make-mail campaign :title "Test Email"
                      :subject "This is a test!"
                      :body (alexandria:read-file-into-string
                             (@template "email/sample-body.mess"))
                      :save NIL))

(defun int* (thing &optional default)
  (if (and thing (string/= thing ""))
      (parse-integer thing)
      default))

(defun prepare-query (query)
  (with-output-to-string (out)
    (write-string ".*" out)
    (loop for c across query
          do (when (find c ".[]^*+?(){}\\^$|")
               (write-char #\\ out))
             (write-char (char-downcase c) out))
    (write-string ".*" out)))

(defun enlength (sequence length)
  (if (<= (length sequence) length)
      sequence
      (subseq sequence 0 length)))

(defun gravatar (email &key (size 32) (default :mm))
  (format NIL "https://secure.gravatar.com/avatar/~a?s=~d&d=~a"
          (cryptos:md5 (string-downcase email)) size (string-downcase default)))
