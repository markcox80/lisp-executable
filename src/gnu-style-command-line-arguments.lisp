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

(in-package "LISP-EXECUTABLE.GNU-STYLE")

(defun short-option-p (string)
  (declare (type string))
  (and (= (length string) 2)
       (char= #\- (elt string 0))
       (not (char= #\- (elt string 1)))))

(defun long-option-p (string)
  (declare (type string))
  (and (>= (length string) 3)
       (char= #\- (elt string 0))
       (char= #\- (elt string 1))))

(defun long-option-including-parameter-p (string)
  (declare (type string))
  (and (>= (length string) 4) 
       (long-option-p string)
       (position #\= string :start 3)))

(defun option-p (string)
  (or (short-option-p string)
      (long-option-p string)))

(defun option-identifier-from-string (string)
  (cond
    ((short-option-p string)
     (elt string 1))
    ((long-option-including-parameter-p string)
     (subseq string 2 (position #\= string)))
    ((long-option-p string)
     (subseq string 2))
    (t
     (error "Invalid option string ~A" string))))

(defun options-processing-terminator-p (string)
  (declare (type string))
  (and (= (length string) 2)
       (char= #\- (elt string 0))
       (char= #\- (elt string 1))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-destructuring-long-option-with-parameter (function string)
    (assert (long-option-including-parameter-p string))
    (let ((position-of-= (position #\= string)))
      (funcall function 
	       (subseq string 2 position-of-=)
	       (subseq string (1+ position-of-=)))))

  (defmacro destructuring-long-option-with-parameter ((identifier parameter) string &body body)
    `(do-destructuring-long-option-with-parameter #'(lambda (,identifier ,parameter)
						      ,@body)
       ,string)))

(defun program-command-line-arguments-processor (arguments strings)  
  (let ((manager (make-manager arguments)))
    (loop
       ;; find the non option arguments
       :with non-option-arguments = (remove-if-not #'non-option-argument-p arguments)
       :with others-argument = (find-if #'others-argument-p arguments)
       :with processing-options = t
       
       :with strings = strings
       :while strings
       :do
       (let ((string (pop strings)))
         (cond
	   ((and processing-options (long-option-including-parameter-p string))
	    (destructuring-long-option-with-parameter (identifier parameter) string
	      (let ((argument (fetch-argument-by-identifier identifier manager)))
		(update-argument manager argument parameter))))
	   ((and processing-options (or (short-option-p string)
					(long-option-p string)))
	    (let ((argument (fetch-argument-by-identifier (option-identifier-from-string string) manager)))
	      (cond
		((not (required-parameter-option-argument-p argument))
		 (update-argument manager argument))
		((and strings (not (option-p (first strings))))
		 (update-argument manager argument (pop strings)))
		(t
		 (error "A parameter is required for argument ~A" (argument-name argument))))))
	   ((options-processing-terminator-p string)
	    (setf processing-options nil))
	   (non-option-arguments
	    (update-argument manager (pop non-option-arguments) string))
	   (others-argument
	    (update-argument manager others-argument string))
	   (t
	    (error "Excess arguments ~A found on the command line." (cons string strings))))))
    (convert-argument-values-to-alist manager)))

(defun dispatcher-command-line-arguments-processor (arguments strings)
  (let ((dispatcher-arguments nil))
    (loop       
       :with non-option-arguments = (remove-if-not #'non-option-argument-p arguments)
       :while (and strings non-option-arguments)
       :do
       (let ((string (pop strings)))
	 (when (not (option-p string))
	   (pop non-option-arguments))
	 (push string dispatcher-arguments)))
    (program-command-line-arguments-processor arguments (append (nreverse dispatcher-arguments)
								(list "--")
								strings))))

(defmethod lisp-executable.invocation:process-command-line-arguments ((command-line-arguments-reader (eql 'lisp-executable:gnu-style)) (information program-information) strings)
  (program-command-line-arguments-processor (arguments information) strings))

(defmethod lisp-executable.invocation:process-command-line-arguments ((command-line-arguments-reader (eql 'lisp-executable:gnu-style)) (information dispatcher-information) strings)
  (dispatcher-command-line-arguments-processor (arguments information) strings))