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

#-:clisp (error "Should not be read. CLISP only.")

(in-package "LISP-EXECUTABLE.CREATION")

(defclass external-process ()
  ((input-stream
    :initarg :input-stream
    :reader external-process-input)
   (output-stream
    :initarg :output-stream
    :reader external-process-output)
   (bidirectional-stream
    :initarg :bidirectional-stream
    :reader external-process-bidirectional-stream)))

(defmethod start-new-lisp-machine (&rest args &key &allow-other-keys)
  (setf args (remove-from-plist-unless-keys-are args '()))

  (multiple-value-bind (bidirectional program-output program-input) (ext:make-pipe-io-stream "clisp")
    (make-instance 'external-process
		   :input-stream program-input
		   :output-stream program-output
		   :bidirectional-stream bidirectional)))

(defmethod lisp-machine-input (lisp-machine)
  (external-process-input lisp-machine))

(defmethod wait-for-lisp-machine (lisp-machine)
  (unwind-protect
       (with-open-stream (out (external-process-output lisp-machine))
	 (loop
	    :for line = (read-line out)
	    :until (alexandria:starts-with-subseq "Bye." line)))
    (close (external-process-bidirectional-stream lisp-machine))
    (close (external-process-input lisp-machine))
    (close (external-process-output lisp-machine))))

(defmethod kill-lisp-machine (lisp-machine)
  (unwind-protect
       (when (open-stream-p lisp-machine)
	 (write '(ext:quit 0) :stream lisp-machine))
    (close (external-process-bidirectional-stream lisp-machine))
    (close (external-process-input lisp-machine))
    (close (external-process-output lisp-machine))))

(defmethod save-executable-using-code-and-die (code output-file &rest args &key &allow-other-keys)
  (setf args (remove-from-plist-unless-keys-are args '()))
  (let ((bin-output-file (make-pathname :type "bin" :defaults output-file))
	(function (eval `(lambda ()
			   (progn
			     (setf *load-verbose* ,*load-verbose*
				   *compile-verbose* ,*compile-verbose*)
			     ,code)))))
    (with-open-file (out output-file :direction :output)
      (format out "#!/bin/sh
set -e
exec \"`dirname \"$0\"`/~A\" -- \"$@\"
" (make-pathname :defaults bin-output-file :directory nil)))
    (when (ext:run-program "/bin/chmod" :arguments (list "+x" (truename output-file)) :wait t)
      (error "Unable to execute chmod on shell script."))
    (dotimes (i 10)
      (print output-file)
      (terpri))
    (unwind-protect
	 (let ((*load-verbose* nil)
	       (*compile-verbose* nil))
	   (apply #'ext:saveinitmem
		  bin-output-file
		  :quiet t
		  :verbose nil
		  :norc t
		  :script nil
		  :documentation nil
		  :init-function function
		  :executable t
		  args))
      (ext:quit 0))))

(defmethod command-line-arguments ()
  ext:*args*)

(defmethod lisp-machine-exit (exit-status)
  (ext:quit exit-status))

(defmethod executable-files (output-file)
  (list output-file
	(make-pathname :type "bin" :defaults output-file)))

(defmethod do-with-control-c-handled (function)
  (handler-case (funcall function)
    (system::simple-interrupt-condition (c)
      (declare (ignore c))
      (lisp-machine-exit 1))))
