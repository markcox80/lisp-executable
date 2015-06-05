(in-package "LISP-EXECUTABLE.CREATION")

(defclass program-options ()
  ((executable-options
    :initarg :executable-options)
   (before-executable-creation-function
    :initarg :before-executable-creation-function)))

(defun program-options (name)
  (get name 'program-options))

(defun (setf program-options) (value name)
  (cond
    ((null value)
     (remprop name 'program-options))
    (t
     (setf (get name 'program-options) value))))

(defun executable-options (program-options)
  (etypecase program-options
    (null
     nil)
    (symbol
     (executable-options (program-options program-options)))
    (program-options
     (slot-value program-options 'executable-options))))

(defun before-executable-creation-function (program-options)
  (etypecase program-options
    (null
     nil)
    (symbol
     (before-executable-creation-function (program-options program-options)))
    (program-options
     (slot-value program-options 'before-executable-creation-function))))

(defun before-executable-creation (program-options)
  (let ((fn (before-executable-creation-function program-options)))
    (when fn
      (funcall fn))))

(defun ensure-program-options (name &key executable-options before-executable-creation-function)
  (setf (program-options name) (make-instance 'program-options
                                              :executable-options executable-options
                                              :before-executable-creation-function before-executable-creation-function)))

(defmacro define-program-options (name &body body)
  (flet ((lookup (key)
           (let ((v (find key body :key #'first)))
             (if v
                 (values (rest v) t)
                 (values nil nil)))))
    `(ensure-program-options ',name
                             :executable-options (list ,@(reduce #'append (lookup :executable-options)))
                             :before-executable-creation-function ,(first (lookup :before-executable-creation)))))
