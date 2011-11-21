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

(in-package "LISP-EXECUTABLE.DEFINITION")

(defun string-to-type-using-read (string type)
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (let ((v (coerce (read-from-string string) type)))
	(unless (typep v type)
	  (error "Unable to convert string ~A to type ~A" string type))
	v))))

(defvar *built-in-conversion-functions* nil)

(defclass built-in-conversion-function ()
  ((name
    :initarg :name
    :reader name)
   (matcher
    :initarg :matcher
    :accessor matcher)
   (converter
    :initarg :converter
    :accessor converter)))

(defun find-built-in-conversion-function/by-name (name)
  (find name *built-in-conversion-functions* :key #'name))

(defun find-built-in-conversion-function/by-type (type)
  (find-if #'(lambda (x)
	       (funcall (matcher x) type))
	   *built-in-conversion-functions*))

(defun find-built-in-conversion-function (&key name type)
  (cond
    (name
     (find-built-in-conversion-function/by-name name))
    (type
     (find-built-in-conversion-function/by-type type))
    (t
     (error "Neither NAME nor TYPE were specified."))))

(defun register-built-in-conversion-function (name matcher converter)
  (let ((v (find-built-in-conversion-function :name name)))
    (if v
	(setf (matcher v)   matcher
	      (converter v) converter)
	(push (make-instance 'built-in-conversion-function 
			     :name name
			     :matcher matcher
			     :converter converter)
	      *built-in-conversion-functions*))))

(defmethod conversion-function :before ((object argument))
  (unless (and (slot-boundp object 'conversion-function)
	       (slot-value object 'conversion-function))
    (setf (slot-value object 'conversion-function) (or (let ((v (find-built-in-conversion-function :type (conversion-function-form object))))
							 (when v
							   (let ((form (conversion-function-form object)))
							     (lambda (string)
							       (funcall (converter v) string form)))))
						       (symbol-function (conversion-function-form object))))))

(register-built-in-conversion-function 'built-ins
				       (let ((types '(number real float single-float double-float
						                  rational integer fixnum
						                           ratio)))
					 #'(lambda (type)
					     (or (find type types)
						 (and (listp type)
						      (find (first type) types)))))
				       #'string-to-type-using-read)

(register-built-in-conversion-function 'keyword
				       #'(lambda (type)
					   (eql type 'keyword))
				       #'(lambda (string type)
					   (declare (type string string))
					   (assert (eql type 'keyword))
					   (multiple-value-bind (rv dont-care) (intern (string-upcase string) (find-package "KEYWORD"))
					     (declare (ignore dont-care))
					     rv)))