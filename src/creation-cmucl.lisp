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

#-:cmucl (error "Should not be read. CMUCL only.")

(in-package "LISP-EXECUTABLE.CREATION")

(defmethod start-new-lisp-machine (&rest args &key &allow-other-keys)
  (setf args (remove-from-plist-unless-keys-are args '(:env :status-hook)))

  (let ((process (apply #'extensions:run-program (first extensions:*command-line-strings*) nil :wait nil :input :stream :output *lisp-machine-output-stream* :error *lisp-machine-output-stream* args)))
    (unless (eql (extensions:process-status process) :running)
      (error "Unable to start external CMUCL process."))   
    (force-output *lisp-machine-output-stream*)
    process))

(defmethod lisp-machine-input (lisp-machine)
  (unless (eql (extensions:process-status lisp-machine) :running)
    (error "Cannot obtain stream to lisp machine as it is no longer running."))
  (extensions:process-input lisp-machine))

(defmethod wait-for-lisp-machine (lisp-machine)
  (force-output *lisp-machine-output-stream*)
  (extensions:process-wait lisp-machine))

(defmethod kill-lisp-machine (lisp-machine)
  (force-output *lisp-machine-output-stream*)
  (extensions:process-kill lisp-machine 15))

(defmethod save-executable-using-code-and-die (code output-file &rest args &key &allow-other-keys)
  (setf args (remove-from-plist-unless-keys-are args '(:purify :root-structures :load-init-file :site-init)))
  (let ((function (eval `(lambda ()
			   (setf *load-verbose* ,*load-verbose*
				 *compile-verbose* ,*compile-verbose*)
			   ,code))))
    (setf *load-verbose* nil
	  *compile-verbose* nil
	  extensions:*command-switch-demons* nil)
    (apply #'extensions:save-lisp output-file 
	   :init-function function
	   :process-command-line t
	   :print-herald nil
	   :executable t
	   args)))

(defmethod command-line-arguments ()
  (rest extensions:*command-line-strings*))

(defmethod lisp-machine-exit (exit-status)
  (throw 'lisp::%end-of-the-world exit-status))

(defmethod executable-files (output-file)
  (list output-file))