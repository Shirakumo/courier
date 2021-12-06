#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defclass mail-format (markless:output-format)
  ((vars :initarg :vars :reader vars)
   (campaign :initarg :campaign :reader campaign)
   (mail :initarg :mail :reader mail)
   (subscriber :initarg :subscriber :reader subscriber)))

(defmethod variable-value (var (f mail-format))
  (if (radiance::starts-with "pool-entry " var)
      (let* ((name (subseq var (length "pool-entry ")))
             (entry (or (claim-pool-entry (ensure-pool name (campaign f)) (subscriber f))
                        (error "Pool ~a has no more entries left to claim!" name))))
        (dm:field entry "content"))
      (loop for (key val) on (vars f) by #'cddr
            do (when (string-equal key var)
                 (return (values val T)))
            finally (return (values NIL NIL)))))

(defclass html-format (mail-format org.shirakumo.markless.plump:plump) ())
(defclass plain-format (mail-format org.shirakumo.markless:markless) ())

(defclass parser (markless:parser)
  ()
  (:default-initargs
   :directives (list* 'template-var markless:*default-directives*)
   :embed-types (list* 'button 'youtube markless:*default-embed-types*)
   :compound-options (list* 'mail-option 'tag-option 'button-option markless:*default-compound-options*)))

(defun make-link* (f url)
  ;; Do not encode links that are already pointing to Courier or are mailto links.
  (if (or (search (url> "courier/") url)
          (eql 0 (search "mailto:" url))
          (null (campaign f))
          (null (subscriber f)))
      url
      (let ((link (make-link (campaign f) :url url)))
        (link-receipt-url (subscriber f) link (mail f)))))

(defun transform-link (element f)
  (setf (plump-dom:attribute element "target") "_blank")
  (setf (plump-dom:attribute element "href") (make-link* f (plump-dom:attribute element "href"))))

(defmethod markless:output-component ((c components:url) (target plump-dom:nesting-node) (f html-format))
  (let ((element (call-next-method)))
    (when (string= "a" (plump-dom:tag-name element))
      (transform-link element f))))

(defmethod markless:output-component :after ((c components:link-option) (target plump-dom:nesting-node) (f html-format))
  (transform-link target f))

(defmethod markless:output-component ((c components:url) (target stream) (f plain-format))
  (markless:output-component (make-link* f (components:target c)) target f))

(defmethod markless:output-component ((c components:link-option) (target stream) (f plain-format))
  (markless:output-component (format NIL "link ~a" (make-link* f (components:target c))) target f))

(defclass var (components:inline-component)
  ((name :initarg :name :initform (error "NAME required") :accessor name)))

(defmethod markless:output-component ((var var) (target plump-dom:nesting-node) (f html-format))
  (multiple-value-bind (value found) (variable-value (name var) f)
    (cond ((not found)
           (plump-dom:make-text-node target (format NIL "{~a}" (name var))))
          (value
           (plump-dom:make-text-node target (princ-to-string value))))))

(defmethod markless:output-component ((var var) (target stream) (f plain-format))
  (multiple-value-bind (value found) (variable-value (name var) f)
    (cond ((not found)
           (format target "{~a}" (name var)))
          (value
           (princ value target)))))

(defclass button (components:embed)
  ())

(defmethod markless:output-component ((button button) (target plump-dom:nesting-node) (f html-format))
  (let* ((element (plump-dom:make-element target "a"))
         (target (components:target button)))
    (when (and (< 2 (length target))
               (char= #\{ (char target 0))
               (char= #\} (char target (1- (length target)))))
      (let ((value (variable-value (subseq target 1 (1- (length target))) f)))
        (if value
            (setf target (princ-to-string value))
            (error "No such variable ~a for button target." target))))
    (setf (plump-dom:attribute element "class") "button")
    (setf (plump-dom:attribute element "href") target)
    (transform-link element f)
    (loop for option in (components:options button)
          do (when (typep option 'components:caption-option)
               (return (markless:output-component option element f)))
          finally (plump-dom:make-text-node element target))))

(defmethod markless:output-component ((c button) (target stream) (f plain-format))
  (markless:output-component (format NIL "~%[  ~a  ]~%" (make-link* f (components:target c))) target f))

(defclass youtube (components:embed)
  ())

(defun extract-youtube-id (link)
  (or (cl-ppcre:register-groups-bind (id) (".*(?:youtu\\.be/|v/|u/\\w/|embed/|watch\\?v=|\\&v=)([^#&?]*).*" link)
        id)
      (error "This is not a valid YouTube URL: ~a" link)))

(defmethod markless:output-component ((youtube youtube) (target plump-dom:nesting-node) (f html-format))
  (let ((id (extract-youtube-id (components:target youtube)))
        (element (plump-dom:make-element target "a")))
    (setf (plump-dom:attribute element "class") "youtube")
    (setf (plump-dom:attribute element "href") (format NIL "https://www.youtube.com/watch?v=~a" id))
    (transform-link element f)
    (let ((image (plump-dom:make-element element "img")))
      (setf (plump-dom:attribute image "src") (format NIL "https://i.ytimg.com/vi/~a/mqdefault.jpg" id))
      (setf (plump-dom:attribute image "alt") "YouTube")
      (cl-markless-plump::set-plump-embed-options image (components:options youtube) f))
    (let ((image (plump-dom:make-element element "img")))
      (setf (plump-dom:attribute image "class") "overlay")
      (setf (plump-dom:attribute image "src") (uri-to-url "courier/static/courier/youtube-overlay.png" :representation :external))
      (setf (plump-dom:attribute image "alt") "Play overlay"))))

(defmethod markless:output-component ((youtube youtube) (target stream) (f plain-format))
  (markless:output-component (format NIL "~%  ~a~%" (make-link* f (components:target youtube))) target f))

(defclass mail-option (components:compound-option)
  ((mail :initarg :mail :reader mail)))

(defmethod markless:parse-compound-option-type ((proto mail-option) option)
  (make-instance (class-of proto) :mail (subseq option (length "mail "))))

(defmethod markless:output-component ((option mail-option) (target plump:nesting-node) (f html-format))
  (let ((mail (or (dm:get-one 'mail (db:query (:and (:= 'campaign (dm:id (campaign f)))
                                                    (:= 'title (mail option)))))
                  (ensure-mail (mail option)))))
    (setf (plump-dom:tag-name target) "a")
    (setf (plump-dom:attribute target "class") "external-link mail-link")
    (setf (plump-dom:attribute target "href") (mail-url mail (subscriber f)))))

(defmethod markless:output-component ((option mail-option) (target stream) (f plain-format))
  (let ((mail (or (dm:get-one 'mail (db:query (:and (:= 'campaign (dm:id (campaign f)))
                                                    (:= 'title (mail option)))))
                  (ensure-mail (mail option)))))
    (markless:output-component (format NIL "link ~a" (mail-url mail (subscriber f))) target f)))

(defclass tag-option (components:compound-option)
  ((tag-name :initarg :tag-name :reader tag-name)))

(defmethod markless:parse-compound-option-type ((proto tag-option) option)
  (make-instance (class-of proto) :tag-name (subseq option (length "tag "))))

(defmethod markless:output-component ((option tag-option) (target plump:nesting-node) (f html-format))
  (let ((tag (or (dm:get-one 'tag (db:query (:and (:= 'campaign (dm:id (campaign f)))
                                                  (:= 'title (tag-name option)))))
                 (ensure-tag (tag-name option)))))
    (setf (plump-dom:tag-name target) "a")
    (setf (plump-dom:attribute target "class") "external-link tag-link")
    (setf (plump-dom:attribute target "href") (tag-invite-url tag (subscriber f)))))

(defmethod markless:output-component ((option tag-option) (target stream) (f plain-format))
  (let ((tag (or (dm:get-one 'tag (db:query (:and (:= 'campaign (dm:id (campaign f)))
                                                  (:= 'title (tag-name option)))))
                 (ensure-tag (tag-name option)))))
    (markless:output-component (format NIL "link ~a" (tag-invite-url tag (subscriber f))) target f)))

(defclass button-option (components:compound-option)
  ())

(defmethod markless:parse-compound-option-type ((proto button-option) option)
  (make-instance (class-of proto)))

(defmethod markless:output-component ((option button-option) (target plump:nesting-node) (f html-format))
  (setf (plump-dom:attribute target "class") (format NIL "button~@[ ~a~]" (plump-dom:attribute target "class"))))

(defmethod markless:output-component ((option button-option) (target stream) (f plain-format)))

(defclass template-var (markless:inline-directive)
  ())

(defmethod markless:prefix ((_ template-var))
  #("{"))

(defmethod markless:begin ((_ template-var) parser line cursor)
  (let* ((entry (markless:stack-top (markless:stack parser)))
         (children (components::children (markless:stack-entry-component entry))))
    (incf cursor)
    (loop for i from cursor below (length line)
          for char = (aref line i)
          until (char= char #\})
          finally (progn
                    (vector-push-extend (make-instance 'var :name (subseq line cursor i)) children)
                    (return (1+ i))))))

(defmethod compile-mail-body (body (source-type (eql :markless)) (target-type (eql :html)) &key vars campaign subscriber mail)
  (when (stringp body)
    (setf body (cl-ppcre:regex-replace-all "\\r\\n" body (string #\Linefeed))))
  (markless:output (markless:parse body (make-instance 'parser))
                   :target (plump-dom:make-root)
                   :format (make-instance 'html-format
                                          :vars vars
                                          :campaign campaign
                                          :subscriber subscriber
                                          :mail mail)))

(defmethod compile-mail-body (body (source-type (eql :markless)) (target-type (eql :text)) &key vars campaign subscriber mail)
  (when (stringp body)
    (setf body (cl-ppcre:regex-replace-all "\\r\\n" body (string #\Linefeed))))
  (markless:output (markless:parse body (make-instance 'parser))
                   :target NIL
                   :format (make-instance 'plain-format
                                          :vars vars
                                          :campaign campaign
                                          :subscriber subscriber
                                          :mail mail)))
