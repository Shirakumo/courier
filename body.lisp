#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defclass mail-format (org.shirakumo.markless.plump:plump)
  ((vars :initarg :vars :reader vars)))

(defclass parser (markless:parser)
  ()
  (:default-initargs :directives (list* 'template-var markless:*default-directives*)))

(defclass var (components:inline-component)
  ((name :initarg :name :initform (error "NAME required") :accessor name)))

(defmethod markless:output-component ((var var) (target plump-dom:nesting-node) (f mail-format))
  (let ((value (loop for (key val) on (vars f) by #'cddr
                     do (when (string-equal key (name var))
                          (return val)))))
    (plump-dom:make-text-node target (princ-to-string value))))

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

(defun compile-email-body (content vars)
  (let ((dom (plump-dom:make-root)))
    (markless:output (markless:parse content (make-instance 'parser))
                     :target dom
                     :format (make-instance 'mail-format :vars vars))))
