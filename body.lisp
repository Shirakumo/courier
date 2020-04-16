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
  (loop for (key val) on (vars f) by #'cddr
        do (when (string-equal key var)
             (return val))))

(defclass html-format (mail-format org.shirakumo.markless.plump:plump) ())
(defclass plain-format (mail-format org.shirakumo.markless:markless) ())

(defclass parser (markless:parser)
  ()
  (:default-initargs :directives (list* 'template-var markless:*default-directives*)))

(defun make-link* (f url)
  ;; Do not encode links that are already pointing to Courier.
  (if (search #.(url> "courier/") url)
      url
      (let ((link (make-link (campaign f) :url url)))
        (link-receipt-url (subscriber f) link (mail f)))))

(defun transform-link (element f)
  (when (campaign f)
    (setf (plump-dom:attribute element "target") "_blank")
    (setf (plump-dom:attribute element "href") (make-link* f (plump-dom:attribute element "href")))))

(defmethod markless:output-component ((c components:url) (target plump-dom:nesting-node) (f html-format))
  (let ((element (call-next-method)))
    (when (string= "a" (plump-dom:tag-name element))
      (transform-link element f))))

(defmethod markless:output-component ((c components:compound) (target plump-dom:nesting-node) (f html-format))
  (let ((element (call-next-method)))
    (when (string= "a" (plump-dom:tag-name element))
      (transform-link element f))))

(defmethod markless:output-component ((c components:url) (target stream) (f plain-format))
  (markless:output-component (make-link* f (components:target c)) target f))

(defmethod markless:output-component ((c components:link-option) (target stream) (f plain-format))
  (markless:output-component (format NIL "link ~a" (make-link* f (components:target c))) target f))

(defclass var (components:inline-component)
  ((name :initarg :name :initform (error "NAME required") :accessor name)))

(defmethod markless:output-component ((var var) (target plump-dom:nesting-node) (f html-format))
  (let ((value (variable-value (name var) f)))
    (when value
      (plump-dom:make-text-node target (princ-to-string value)))))

(defmethod markless:output-component ((var var) (target stream) (f plain-format))
  (let ((value (variable-value (name var) f)))
    (when value
      (princ value target))))

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

(defun compile-mail-body (content vars &key campaign subscriber mail (format 'html-format))
  (when (typep content 'string)
    (setf content (cl-ppcre:regex-replace-all "\\r\\n" content (string #\Linefeed))))
  (let ((target (ecase format
                  (html-format (plump-dom:make-root))
                  (plain-format NIL))))
    (markless:output (markless:parse content (make-instance 'parser))
                     :target target
                     :format (make-instance format
                                            :vars vars
                                            :campaign campaign
                                            :subscriber subscriber
                                            :mail mail))))
