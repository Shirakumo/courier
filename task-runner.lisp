#|
 This file is a part of Courier
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:courier)

(defvar *task-runner* NIL)
(defvar *task-condition* (bt:make-condition-variable :name "task runner condition"))
(defvar *task-lock* (bt:make-lock "task runner lock"))
(defvar *tasks* NIL)

(defclass task ()
  ((due-time :initarg :due-time :initform 0 :accessor due-time)))

(defmethod print-object ((task task) stream)
  (print-unreadable-object (task stream :type T)
    (format-human-date (due-time task) stream)))

(defgeneric run-task (task))

(defmethod task-ready-p ((task task))
  (<= (due-time task) (get-universal-time)))

(defmethod reschedule-in (time (task task))
  (setf (due-time task) (+ time (get-universal-time))))

(defun nearest-task (tasks)
  (loop for task in tasks
        minimize (due-time task)))

(defun find-task (name &optional (error T))
  (or (find name *tasks* :key #'type-of)
      (when error (error "No task with name ~s." name))))

(defun ensure-task (task-ish)
  (etypecase task-ish
    (task task-ish)
    (symbol (find-task task-ish))))

(defun add-task (task)
  (pushnew task *tasks* :key #'type-of))

(defun remove-task (task)
  (setf *tasks* (remove (ensure-task task) *tasks*)))

(defun notify-task (task &optional (due-time 0))
  (setf (due-time (ensure-task task)) (min due-time (due-time (ensure-task task))))
  (bt:condition-notify *task-condition*))

(defmacro define-task (name () &body body)
  `(progn
     (defclass ,name (task)
       ())

     (defmethod run-task ((task ,name))
       ,@body)

     (add-task (make-instance ',name))))

(defun run-tasks ()
  (loop while (started-p)
        do (loop for task in *tasks*
                 when (task-ready-p task)
                 do (handler-bind ((error (lambda (e)
                                            (maybe-invoke-debugger e 'ignore-task))))
                      (with-simple-restart (ignore-task "Ignore the task failure.")
                        (run-task task))))
           (bt:with-lock-held (*task-lock*)
             (bt:condition-wait *task-condition* *task-lock*
                                :timeout (max 1 (- (nearest-task *tasks*) (get-universal-time)))))))

(defun start-task-runner ()
  (when (and *task-runner*
             (bt:thread-alive-p *task-runner*))
    (error "The task runner is already running.")) 
  (flet ((task-runner-thunk ()
           (l:info :courier.task-runner "Starting task runner.")
           (unwind-protect (run-tasks)
             (l:info :courier.task-runner "Stopping task runner.")
             (setf *task-runner* NIL))))
    (setf *task-runner* (bt:make-thread #'task-runner-thunk :name "courier task runner"))))

(define-trigger (startup-done start-task-runner) ()
  (unless (and *task-runner*
               (bt:thread-alive-p *task-runner*))
    (start-task-runner)))

(define-trigger (shutdown-done stop-task-runner) ()
  (loop while (and *task-runner*
                   (bt:thread-alive-p *task-runner*))
        do (bt:condition-notify *task-condition*)))
