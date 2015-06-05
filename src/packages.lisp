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

(defpackage "LISP-EXECUTABLE"
  (:use "COMMON-LISP")
  (:export #:&options
	   #:&arguments
	   #:&others

	   #:conversion-function
	   #:identifiers
	   #:reducing-function

	   #:toggle-policy
	   #:count-policy
	   #:use-first-policy
	   #:use-last-policy
	   #:append-policy
	   #:error-policy

	   #:define-program
	   #:define-dispatcher-program

           #:define-program-options
           #:ensure-program-options
           #:program-options
           #:before-executable-creation-function
           #:executable-options

	   #:*command-line-arguments-reader*
	   #:gnu-style

	   #:program-funcall
	   #:program-apply
	   #:program-funcall-with-plist
	   #:program-funcall-with-alist

	   #:create-executable
	   #:*lisp-machine-output-stream*

	   #:executable
	   #:executable-files
	   #:create-executables-op))

(defpackage "LISP-EXECUTABLE.DEFINITION"
  (:use "COMMON-LISP" 
	"LISP-EXECUTABLE")
  (:export #:command-line-program-information

	   #:arguments
	   #:argument-name
	   #:argument-parameter-name
	   #:parameter-default-value
	   #:conversion-function
	   #:conversion-function-form
	   #:identifiers
	   #:reducing-function-form
	   #:create-reducing-function

	   #:argumentp
	   #:no-parameter-option-argument-p
	   #:optional-parameter-option-argument-p
	   #:required-parameter-option-argument-p
	   #:non-option-argument-p
	   #:others-argument-p

	   #:program-information
	   #:dispatcher-information))

(defpackage "LISP-EXECUTABLE.INVOCATION"
  (:use "COMMON-LISP"
	"LISP-EXECUTABLE")
  (:export #:process-command-line-arguments))

(defpackage "LISP-EXECUTABLE.ARGUMENT-VALUE-MANAGER"
  (:use "COMMON-LISP"
	"LISP-EXECUTABLE.DEFINITION")
  (:export #:make-manager
	   #:argument-value
	   #:update-argument
	   #:find-argument
	   #:find-argument-if
	   #:find-argument-by-name
	   #:fetch-argument-by-identifier
	   #:convert-argument-values-to-alist))

(defpackage "LISP-EXECUTABLE.GNU-STYLE"
  (:use "COMMON-LISP"
	"LISP-EXECUTABLE.DEFINITION"
	"LISP-EXECUTABLE.ARGUMENT-VALUE-MANAGER")
  (:export #:short-option-p
	   #:long-option-p
	   #:long-opiont-including-parameter-p
	   #:option-p
	   #:option-identifier-from-string
	   #:options-processing-terminator-p

           #:program-command-line-arguments-processor
	   #:dispatcher-command-line-arguments-processor))

(defpackage "LISP-EXECUTABLE.CREATION"
  (:use "COMMON-LISP"
	"LISP-EXECUTABLE")
  (:export #:save-executable-and-die))

(defpackage "LISP-EXECUTABLE.ASDF-SUPPORT"
  (:use "COMMON-LISP"
	"LISP-EXECUTABLE"))
