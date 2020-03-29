#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defun compile-email-body (content vars)
  (cl-markless:output (cl-markless:parse content T)
                      :target NIL :format 'cl-markless-plump:plump))
