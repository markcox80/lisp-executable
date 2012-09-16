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

#-:ecl (error "Should not be read. ECL only.")

(in-package "LISP-EXECUTABLE.CREATION")

(defmethod start-new-lisp-machine (&rest args &key &allow-other-keys)
  (declare (ignore args))
  (multiple-value-bind (stream something-i-dont-know process) (ext:run-program (ext:argv 0) nil :input :stream :output nil :error nil :wait nil)
    (declare (ignore stream something-i-dont-know))
    (unless (eql (ext:external-process-status process) :running)
      (error "Unable to start external ECL process.")) 
    process))

(defmethod lisp-machine-input (lisp-machine)
  (ext:external-process-input lisp-machine))

(defmethod wait-for-lisp-machine (lisp-machine)
  (ext:external-process-wait lisp-machine t))

(uffi:def-function ("kill" %posix-kill)
    ((pid-t :int)
     (signal :int))
  :returning :int)

(defmethod kill-lisp-machine (lisp-machine)
  (force-output *lisp-machine-output-stream*)
  (%posix-kill (ext:external-process-pid lisp-machine) 15))

(defmethod save-executable-using-code-and-die (code output-file &rest args &key asdf-system (if-exists :error) &allow-other-keys)
  (declare (ignore args))
  (labels ((worker ()
	     (let ((fn (first (asdf:make-build asdf-system
					       :type :program 
					       :monolithic t
					       :epilogue-code code))))
	       (alexandria:copy-file fn output-file :if-to-exists if-exists)
	       (multiple-value-bind (stream exit-code process) (ext:run-program "/bin/chmod" (list "+x" (namestring output-file))
										:input nil
										:output t
										:error :output
										:wait t)
		 (declare (ignore stream))
		 (assert (zerop exit-code))
		 (assert (eql :exited (ext:external-process-status process)))))))
    (handler-case (progn
		    (worker)
		    (ext:quit 0))
      (error (c)
	(format *error-output* "~&Error occurred during SAVE-EXECUTABLE-USING-CODE-AND-DIE.~%~A~%" c)
	(ext:quit 1)))))

(defmethod command-line-arguments ()
  (loop
     :for index :from 1 :below (ext:argc)
     :collect
     (ext:argv index)))

(defmethod lisp-machine-exit (exit-status)
  (ext:quit exit-status))

(defmethod executable-files (output-file)
  (list output-file))
