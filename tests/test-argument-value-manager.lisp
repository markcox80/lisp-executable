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

(in-package "LISP-EXECUTABLE.TESTS")

(lisp-executable:define-program test-program (lisp-executable:&options   help verbose (debug debug-level 1) (file file-value)
					      lisp-executable:&arguments input output
					      lisp-executable:&others    others)
  (declare (ignore help verbose debug debug-level file file-value input output others)
	   (lisp-executable:identifiers verbose "verbose" #\v)
	   (lisp-executable:identifiers help "help" #\h)
	   (lisp-executable:conversion-function (integer 0) debug)
	   (lisp-executable:conversion-function keyword file)
	   (lisp-executable:conversion-function parse-namestring input output)
	   (lisp-executable:reducing-function lisp-executable:append-policy file)
	   (lisp-executable:reducing-function lisp-executable:count-policy verbose)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-with-arguments-value-manager (function program-name)
    (let* ((arguments (lisp-executable.definition:arguments (lisp-executable.definition:command-line-program-information program-name)))
	   (manager   (lisp-executable.argument-value-manager:make-manager arguments)))
      (funcall function manager)))

  (defmacro with-arguments-value-manager ((manager) program-name &body body)
    `(do-with-arguments-value-manager #'(lambda (,manager)
					  ,@body)
       ',program-name)))

(define-test argument-value-manager/no-option
  (with-arguments-value-manager (manager) test-program
    (let ((help-argument (find-argument-by-name 'help manager)))
      (assert-equal nil (argument-value manager 'help))
      (update-argument manager help-argument)
      (assert-error 'error (update-argument manager help-argument))
      (assert-equal t (argument-value manager 'help)))

    (let ((verbose-argument (find-argument-by-name  'verbose manager)))
      (assert-equal 0 (argument-value manager 'verbose))
      (update-argument manager verbose-argument)
      (assert-equal 1 (argument-value manager 'verbose))
      (update-argument manager verbose-argument)
      (assert-equal 2 (argument-value manager 'verbose)))))

(define-test argument-value-manager/optional-parameter
  (with-arguments-value-manager (manager) test-program
    (let ((debug-argument (find-argument-by-name 'debug manager)))
      (assert-equal nil (argument-value manager 'debug))
      (assert-equal nil (argument-value manager 'debug-level))
      (update-argument manager debug-argument)
      (assert-equal t (argument-value manager 'debug))
      (assert-equal 1 (argument-value manager 'debug-level))

      (assert-error 'error (update-argument manager debug-argument 2))))

  (with-arguments-value-manager (manager) test-program
    (let ((debug-argument (find-argument-by-name  'debug manager)))
      (assert-equal nil (argument-value manager 'debug))
      (assert-equal nil (argument-value manager 'debug-level))
      (update-argument manager debug-argument "5")
      (assert-equal t (argument-value manager 'debug))
      (assert-equal 5 (argument-value manager 'debug-level))
      (assert-error 'error (update-argument manager debug-argument "-1")))))

(define-test argument-value-manager/required-parameter
  (with-arguments-value-manager (manager) test-program
    (let ((file-argument       (find-argument-by-name 'file manager)))
      (assert-equal nil (argument-value manager 'file))
      (assert-equal nil (argument-value manager 'file-value))
      ;; requires a parameter. Should signal an error if this doesn't occur.
      (assert-error 'error (update-argument manager file-argument))

      (update-argument manager file-argument "first")
      (assert-equal t (argument-value manager 'file))
      (assert-equal (list :first) (argument-value manager 'file-value))

      (update-argument manager file-argument "second")
      (assert-equal (list :first :second) (argument-value manager 'file-value)))))

(define-test argument-value-manager/argument
  (with-arguments-value-manager (manager) test-program
    (let ((input-argument (find-argument-by-name 'input manager))
	  (output-argument (find-argument-by-name 'output manager)))
      (assert-equal nil (argument-value manager 'input))
      (assert-equal nil (argument-value manager 'output))

      (update-argument manager input-argument "here.txt")
      (assert-equal #P"here.txt" (argument-value manager 'input))
      (assert-error 'error (update-argument manager input-argument t))
      
      (update-argument manager output-argument "there.txt")
      (assert-equal #P"there.txt" (argument-value manager 'output)))))

(define-test argument-value-manager/others
  (with-arguments-value-manager (manager) test-program
    (let ((others-argument (find-argument-by-name 'others manager)))
      (assert-equal nil (argument-value manager 'others))
      
      (update-argument manager others-argument "first")
      (assert-equal (list "first") (argument-value manager 'others))

      (update-argument manager others-argument "second")
      (assert-equal (list "first" "second") (argument-value manager 'others)))))

