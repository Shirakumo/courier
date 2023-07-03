(in-package #:courier)

(defvar *file-directory*
  (environment-module-pathname #.*package* :data "files/"))

(defun campaign-file-directory (campaign)
  (merge-pathnames
   (make-pathname :directory `(:relative ,(princ-to-string (ensure-id campaign))))
   *file-directory*))

(defun file-pathname (file)
  (make-pathname :name (princ-to-string (dm:id file))
                 :type (trivial-mimes:mime-file-type (dm:field file "mime-type"))
                 :defaults (campaign-file-directory (dm:field file "campaign"))))

(defun make-file (campaign file mime-type &key (filename (file-namestring file)) (author (auth:current)))
  (db:with-transaction ()
    (let ((model (dm:hull 'file)))
      (setf-dm-fields model campaign mime-type filename)
      (setf (dm:field model "author") (user:id author))
      (dm:insert model)
      (ensure-directories-exist (file-pathname model))
      (alexandria:copy-file file (file-pathname model) :if-to-exists :error)
      model)))

(defun ensure-file (file-ish)
  (or
   (etypecase file-ish
     (dm:data-model file-ish)
     (T (dm:get-one 'file (db:query (:= '_id (db:ensure-id file-ish))))))
   (error 'request-not-found :message "No such file.")))

(defun delete-file (file)
  (db:with-transaction ()
    (let* ((file (ensure-file file)))
      (cl:delete-file (file-pathname file))
      (dm:delete file))))

(defun list-files (campaign &key amount (skip 0) query)
  (with-query (query filename)
    (dm:get 'file (query (:= 'campaign (dm:id campaign)))
            :amount amount :skip skip)))
