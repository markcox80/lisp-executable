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

(in-package "LISP-EXECUTABLE.INVOCATION")

(defvar *command-line-arguments-reader* 'gnu-style)

(defgeneric process-command-line-arguments (command-line-arguments-reader program-information strings))

(defun program-funcall-with-alist (program-name alist)
  (funcall (symbol-function program-name) alist))

(defun program-funcall-with-plist (program-name plist)
  (program-funcall-with-alist program-name (alexandria:plist-alist plist)))

(defun program-funcall (program-name &rest strings)
  (declare (type symbol program-name))
  (assert (every #'stringp strings))
  (let ((program-information (lisp-executable.definition:command-line-program-information program-name)))
    (unless program-information
      (error "No program information available for ~A" program-name))
    
    (let ((alist (process-command-line-arguments *command-line-arguments-reader* program-information strings)))
      (program-funcall-with-alist program-name alist))))

(defun program-apply (program-name &rest arguments)
  (apply #'(lambda (&rest args)
	     (apply #'program-funcall program-name args))
	 (append (butlast arguments)
		 (car (last arguments)))))