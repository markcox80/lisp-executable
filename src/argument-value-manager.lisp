;; Copyright (c) 2011, Mark Cox
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;; - Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.

;; - Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in the
;;   documentation and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package "LISP-EXECUTABLE.ARGUMENT-VALUE-MANAGER")
;;; Contains the argument value manager.
;;;
;;; Controls the updating process of arguments.
;;; - Performs the conversion function
;;; - Performs the reducing function

(defclass manager ()
  ((arguments
    :initarg :arguments
    :reader arguments)
   (argument-reducing-function-table
    :initform (make-hash-table)
    :reader argument-reducing-function-table)
   (table
    :initform (make-hash-table)
    :reader argument-value-table)))

(defun make-manager (arguments)
  (make-instance 'manager :arguments arguments))

(defun argument-value (manager argument-name &optional default)
  (gethash argument-name (argument-value-table manager) default))

(defun force-argument-value (new-value manager argument-name)  
  (setf (gethash argument-name (argument-value-table manager)) new-value))

(defun perform-type-conversion (argument argument-value)
  (assert (or (optional-parameter-option-argument-p argument)
	      (required-parameter-option-argument-p argument)
	      (non-option-argument-p argument)
	      (others-argument-p argument)))
  (funcall (conversion-function argument) argument-value))

(defun update-argument/no-parameter-option (manager argument)
  (assert (no-parameter-option-argument-p argument))
  (let* ((has-no-value '#:has-no-value)
	 (current-value (argument-value manager (argument-name argument) has-no-value)))
    (force-argument-value (if (eql current-value has-no-value)
			      (funcall (reducing-function manager argument) t)
			      (funcall (reducing-function manager argument) current-value t))
			  manager 
			  (argument-name argument))))

(defun update-argument/optional-parameter-option (manager argument &optional (parameter-value nil parameter-value-p))
  (let* ((has-no-value '#:has-no-value)
	 (current-name-value      (argument-value manager (argument-name argument) has-no-value))
	 (current-parameter-value (argument-value manager (argument-parameter-name argument) has-no-value))
	 (parameter-value         (if parameter-value-p
				      (perform-type-conversion argument parameter-value)
				      (parameter-default-value argument))))
    (force-argument-value t manager (argument-name argument))
    (force-argument-value (if (eql has-no-value current-name-value)
			      (funcall (reducing-function manager argument) parameter-value)
			      (funcall (reducing-function manager argument) current-parameter-value parameter-value))
			  manager
			  (argument-parameter-name argument))))

(defun update-argument/required-parameter-option (manager argument parameter-value)
  (let* ((has-no-value '#:has-no-value)
	 (current-name-value      (argument-value manager (argument-name argument) has-no-value))
	 (current-parameter-value (argument-value manager (argument-parameter-name argument) has-no-value))
	 (parameter-value         (perform-type-conversion argument parameter-value)))
    (force-argument-value t manager (argument-name argument))
    (force-argument-value (if (eql has-no-value current-name-value)
			      (funcall (reducing-function manager argument) parameter-value)
			      (funcall (reducing-function manager argument) current-parameter-value parameter-value))
			  manager
			  (argument-parameter-name argument))))

(defun update-argument/argument (manager argument argument-value)
  (let* ((has-no-value '#:has-no-value)
	 (current-value (argument-value manager (argument-name argument) has-no-value)))
    (assert (eql current-value has-no-value))
    (force-argument-value (perform-type-conversion argument argument-value)
			  manager 
			  (argument-name argument))))

(defun update-argument/others (manager argument argument-value)
  (let ((current-value (argument-value manager (argument-name argument))))
    (force-argument-value (append current-value (list (perform-type-conversion argument argument-value)))
			  manager
			  (argument-name argument))))

(defun update-argument (manager argument &optional (parameter-value nil parameter-value-p))
  (declare (type (or null string) parameter-value))
  (cond
    ((no-parameter-option-argument-p argument)
     (update-argument/no-parameter-option manager argument))
    ((optional-parameter-option-argument-p argument)
     (if parameter-value-p
	 (update-argument/optional-parameter-option manager argument parameter-value)
	 (update-argument/optional-parameter-option manager argument)))
    ((required-parameter-option-argument-p argument)
     (assert parameter-value-p)
     (update-argument/required-parameter-option manager argument parameter-value))
    ((non-option-argument-p argument)
     (assert parameter-value-p)
     (update-argument/argument manager argument parameter-value))
    ((others-argument-p argument)
     (assert parameter-value-p)
     (update-argument/others manager argument parameter-value))
    (t
     (error "Should not get here."))))

(defun reducing-function (manager argument)
  (gethash argument (argument-reducing-function-table manager)))

(defun (setf reducing-function) (new-value manager argument)
  (setf (gethash argument (argument-reducing-function-table manager)) new-value))

(defmethod initialize-instance :after ((self manager) &key)
  (loop
     :for argument :in (arguments self)
     :do     
     (setf (reducing-function self argument) (create-reducing-function argument))
     (when (no-parameter-option-argument-p argument)	
       (force-argument-value (funcall (reducing-function self argument)) self (argument-name argument)))))

;; Querying
(defun find-argument (item manager &key (key #'identity) (test #'eql))
  (find item (arguments manager) :key key :test test))

(defun find-argument-if (predicate manager &key (key #'identity))
  (find-if predicate (arguments manager) :key key))

(defun find-argument-by-name (name manager)
  (find-argument name manager :key #'argument-name))

(defun identifier= (a b)
  (cond
    ((and (stringp a)
	  (stringp b))
     (string= a b))
    ((and (characterp a)
	  (characterp b))
     (char= a b))
    (t
     nil)))

(defun find-argument-by-identifier (identifier manager)
  (find-argument-if #'(lambda (identifiers)
			(member identifier identifiers :test #'identifier=))
		    manager
		    :key #'identifiers))

(defun fetch-argument-by-identifier (identifier manager)
  (let ((argument (find-argument-by-identifier identifier manager)))
    (unless argument
      (error "Unable to find an object for an argument with identifier ~A" identifier))
    argument))

;; Converting
(defgeneric convert-argument-values-to-alist (object))

(defmethod convert-argument-values-to-alist ((object manager))
  (let ((rv nil))
    (labels ((process-table (name value)
	       (push (cons name value) rv)))
      (maphash #'process-table (argument-value-table object)))
    (nreverse rv)))