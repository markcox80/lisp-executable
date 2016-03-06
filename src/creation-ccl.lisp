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

#-:ccl (error "Should not be read. Clozure CL only.")

(in-package "LISP-EXECUTABLE.CREATION")

(defmethod start-new-lisp-machine (&rest args &key &allow-other-keys)
  (setf args (remove-from-plist-unless-keys-are args '(:external-format)))

  (let ((process (apply #'ccl:run-program (first (ccl::command-line-arguments)) nil :wait nil :input :stream :output *lisp-machine-output-stream* :error *lisp-machine-output-stream* args)))
    (unless (eql (ccl::external-process-status process) :running)
      (error "Unable to start external CCL process."))   
    (force-output *lisp-machine-output-stream*)
    process))

(defmethod lisp-machine-input (lisp-machine)
  (unless (eql (ccl::external-process-status lisp-machine) :running)
    (error "Cannot obtain stream to lisp machine as it is no longer running."))
  (ccl::external-process-input lisp-machine))

(defmethod wait-for-lisp-machine (lisp-machine)
  (force-output *lisp-machine-output-stream*)
  (ccl::external-process-wait lisp-machine))

(defmethod kill-lisp-machine (lisp-machine)
  (force-output *lisp-machine-output-stream*)
  (ccl::signal-external-process lisp-machine 15))

(defmethod save-executable-using-code-and-die (code output-file &rest args &key &allow-other-keys)
  (setf args (remove-from-plist-unless-keys-are args '(:purify :impurify :init-file)))
  (let* ((bin-output-file (make-pathname :type "bin" :defaults output-file))
         (function (eval `(lambda ()
                            ,code))))
    (with-open-file (out output-file :direction :output :if-exists :supersede)
      (format out "#!/bin/sh
set -e
exec \"`dirname \"$0\"`/~A\" -- \"$@\"
"
              (make-pathname :defaults bin-output-file :directory nil)))
    (let ((external-process (ccl:run-program "/bin/chmod" (list "+x" (namestring output-file))
                                             :wait t)))
      (multiple-value-bind (status exit-code) (ccl:external-process-status external-process)
        (unless (and (eql status :exited)
                     (zerop exit-code))
          (error "Unable to execute chmod on shell script."))))
    (apply #'ccl:save-application
	   bin-output-file
	   :toplevel-function function
	   :prepend-kernel t	   
	   :error-handler :quit
	   args)))

(defmethod command-line-arguments ()
  ;; Position:
  ;;   0   The executable name.  
  ;;   1   The command line argument '--' added by the wrapper script.  
  (cddr ccl:*command-line-argument-list*))

(defmethod lisp-machine-exit (exit-status)
  (ccl:quit exit-status))

(defmethod executable-files (output-file)
  (list output-file
        (make-pathname :type "bin" :defaults output-file)))

(defmethod do-with-control-c-handled (function)
  (let ((ccl:*break-hook* #'(lambda (condition hook)
			      (cond
				((typep condition 'ccl:interrupt-signal-condition)
				 (lisp-machine-exit 1))
				(t
				 ;; I can not (quickly) find what I am supposed to do here.
				 (funcall ccl:*break-hook* condition hook))))))
    (funcall function)))
