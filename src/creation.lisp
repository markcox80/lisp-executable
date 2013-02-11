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

(in-package "LISP-EXECUTABLE.CREATION")

(defvar *lisp-machine-output-stream* *standard-output*)

(defun remove-from-plist-unless-keys-are (plist valid-keys &key (test #'eql))
  "Utility function that helps with implementing
START-NEW-LISP-MACHINE and SAVE-EXECUTABLE-USING-FUNCTION-AND-DIE."
  (loop
     :for (key value) :on plist :by #'cddr
     :append
       (if (member key valid-keys :test test)
	   (list key value)
	   nil)))

(defgeneric start-new-lisp-machine (&rest args &key &allow-other-keys))
(defgeneric lisp-machine-input (lisp-machine))
(defgeneric wait-for-lisp-machine (lisp-machine))
(defgeneric kill-lisp-machine (lisp-machine))
(defgeneric lisp-machine-exit (exit-status))
(defgeneric save-executable-using-code-and-die (code output-file &rest args &key &allow-other-keys))
(defgeneric command-line-arguments ())
(defgeneric executable-files (output-file))
(defgeneric do-with-control-c-handled (function))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-control-c-handled (&body body)
    `(do-with-control-c-handled #'(lambda ()
				    ,@body))))

(defvar *all-systems* nil)
(defclass sticky-beak-op (asdf:operation)
  ())

(defmethod asdf:component-depends-on ((op sticky-beak-op) component)
  (append (list (cons 'sticky-beak-op (asdf::component-load-dependencies component)))
	  (call-next-method)))

(defmethod asdf:perform ((op sticky-beak-op) (component t))
  (declare (ignore op component)))

(defmethod asdf:perform ((op sticky-beak-op) (component asdf:system))
  (declare (ignore op))
  (pushnew component *all-systems*))

(defun determine-complete-set-of-asdf-systems (systems)
  (let ((*all-systems* nil))
    (map nil #'(lambda (system)
		 (asdf:oos 'sticky-beak-op system :force t))
	 systems)
    *all-systems*))

(defun create-executable (program-name output-file &rest args &key asdf-system (if-exists :error) &allow-other-keys)
  (when (probe-file output-file)
    (ecase if-exists
      (:error
       (error "Unable to create executable ~A as the file exists." output-file))
      (:supersede
       (delete-file output-file))))

  (let* ((complete-asdf-systems (determine-complete-set-of-asdf-systems (cons "lisp-executable" (list asdf-system))))
	 (code-to-execute `(;; set up ASDF registry
			    ,@(mapcar #'(lambda (system)
					  `(pushnew ,(asdf:component-pathname system) asdf:*central-registry*))
				      complete-asdf-systems)
			      
			      ;; load the needed systems
			      ,@(mapcar #'(lambda (system)
					    `(asdf:load-system ,(asdf:component-name system)))
					complete-asdf-systems)
			      
			      ;; assign the command line arguments reader
			      (setf lisp-executable:*command-line-arguments-reader* ',lisp-executable:*command-line-arguments-reader*)
			      
			      ;; create the executable
			      (lisp-executable.creation:save-executable-and-die ',program-name ,output-file ,@args))))
    (let ((external-lisp-machine (apply #'start-new-lisp-machine args)))
      (handler-case (progn
		      (with-open-stream (machine-input (lisp-machine-input external-lisp-machine))
			(let ((*package* (find-package "CL-USER")))
			  (map nil #'(lambda (sexp)
				       (write sexp :stream machine-input)
				       (terpri machine-input)
				       (force-output machine-input))
			       code-to-execute)))
		      (wait-for-lisp-machine external-lisp-machine))
	(error (c)
	  (format *error-output* "Error creating executable: ~A" c)
	  (kill-lisp-machine external-lisp-machine)))
      output-file)))

(defun save-executable-and-die (program-name output-file &rest args &key (if-exists :error) &allow-other-keys)
  (when (probe-file output-file)
    (ecase if-exists
      (:error
       (error "Unable to create executable as the file ~A already exists." output-file))
      (:supersede)))

  (apply #'save-executable-using-code-and-die `(progn
						 (handler-case (with-control-c-handled
								 (let ((rv (program-apply ',program-name (command-line-arguments))))
								   (if (integerp rv)
								       (lisp-machine-exit rv)
								       (lisp-machine-exit 0))))
						  (error (c)
						    (format *error-output* "~&Unhandled error: ~A~%" c)
						    (lisp-machine-exit 1)))
						 (error "LISP-EXECUTABLE.CREATION::LISP-MACHINE-EXIT NOT IMPLEMENTED PROPERLY."))
	 output-file
	 args))
