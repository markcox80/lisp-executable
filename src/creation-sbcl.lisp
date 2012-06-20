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

#-:sbcl (error "Should not be read. SBCL only.")

(in-package "LISP-EXECUTABLE.CREATION")

(defmethod start-new-lisp-machine (&rest args &key &allow-other-keys)
  (setf args (remove-from-plist-unless-keys-are args '(:environment :env)))

  (let ((process (apply #'sb-ext:run-program (first sb-ext:*posix-argv*) nil :wait nil :input :stream :output *lisp-machine-output-stream* :error *lisp-machine-output-stream* :search t args)))
    (unless (eql (sb-ext:process-status process) :running)
      (error "Unable to start external SBCL process."))   
    (force-output *lisp-machine-output-stream*)
    process))

(defmethod lisp-machine-input (lisp-machine)
  (unless (eql (sb-ext:process-status lisp-machine) :running)
    (error "Cannot obtain stream to lisp machine as it is no longer running."))
  (sb-ext:process-input lisp-machine))

(defmethod wait-for-lisp-machine (lisp-machine)
  (force-output *lisp-machine-output-stream*)
  (sb-ext:process-wait lisp-machine))

(defmethod kill-lisp-machine (lisp-machine)
  (force-output *lisp-machine-output-stream*)
  (sb-ext:process-kill lisp-machine 15))

(defmethod save-executable-using-code-and-die (code output-file &rest args &key &allow-other-keys)
  (setf args (remove-from-plist-unless-keys-are args '(:purify :root-structures :environment-name :compression)))
  (let ((function (eval `(lambda ()
			   ,code))))
    (apply #'sb-ext:save-lisp-and-die output-file 
	   :toplevel function
	   :executable t
	   :save-runtime-options t
	   args)))

(defmethod command-line-arguments ()
  (rest sb-ext:*posix-argv*))

(defmethod lisp-machine-exit (exit-status)
  (sb-ext:exit :code exit-status))

(defmethod executable-files (output-file)
  (list output-file))
