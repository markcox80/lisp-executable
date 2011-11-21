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

(define-test extract-from-body/body 
  (labels ((test (form)
	     (lisp-executable.definition::extract-from-body/body form)))
    (assert-equal '("hello-there") 
		  (test '("hello-there")))
    (assert-equal '("hello-there") 
		  (test '("something different"
			  "hello-there")))
    (assert-equal '("hello-there")
		  (test '((declare (type integer x))
			  "hello-there")))
    (assert-equal '((incf x)
		    "hello-there")
		  
		  (test '((declare (type integer x))
			  (incf x)
			  "hello-there")))

    (assert-equal '((incf x)
		    "hello-there")
		  
		  (test '((declare (type integer x))
			  (declare (type float y))
			  (incf x)
			  "hello-there")))

    (assert-equal '((incf x)
		    "hello-there")
		  
		  (test '("Documentation String"
			  (declare (type integer x))
			  (incf x)
			  "hello-there")))

    (assert-equal '((incf x)
		    "hello-there")
		  (test '((declare (type integer x))
			  (declare (optimize (speed 3)))
			  "Documentation String"
			  (incf x)
			  "hello-there")))

    (assert-equal '((incf x)
		    "hello-there")
		  (test '((declare (type integer x))
			  "Documentation String"
			  (declare (optimize (speed 3)))
			  (incf x)
			  "hello-there")))))

(define-test extract-from-body/declarations
  (labels ((test (form)
	     (lisp-executable.definition::extract-from-body/declarations form)))
    (assert-equal '()
		  (test '("hello-there")))
    (assert-equal '()
		  (test '("something different"
			  "hello-there")))
    (assert-equal '((type integer x))
		  (test '((declare (type integer x))
			  "hello-there")))
    (assert-equal '((type integer x))
		  
		  (test '((declare (type integer x))
			  (incf x)
			  "hello-there")))

    (assert-equal '((type integer x)
		    (type float y))
		  
		  (test '((declare (type integer x))
			  (declare (type float y))
			  (incf x)
			  "hello-there")))

    (assert-equal '((type integer x))
		  
		  (test '("Documentation String"
			  (declare (type integer x))
			  (incf x)
			  "hello-there")))

    (assert-equal '((type integer x)
		    (optimize (speed 3)))

		  (test '((declare (type integer x))
			  (declare (optimize (speed 3)))
			  "Documentation String"
			  (incf x)
			  "hello-there")))

    (assert-equal '((type integer x)
		    (optimize (speed 3)))
		  (test '((declare (type integer x))
			  "Documentation String"
			  (declare (optimize (speed 3)))
			  (incf x)
			  "hello-there")))))

(define-test extract-from-body/documentation-string
  (labels ((test (form)
	     (lisp-executable.definition::extract-from-body/documentation-string form)))
    (assert-equal '()
		  (test '("hello-there")))
    (assert-equal "something different"
		  (test '("something different"
			  "hello-there")))
    (assert-equal '()
		  (test '((declare (type integer x))
			  "hello-there")))
    (assert-equal '()
		  
		  (test '((declare (type integer x))
			  (incf x)
			  "hello-there")))

    (assert-equal '()
		  
		  (test '((declare (type integer x))
			  (declare (type float y))
			  (incf x)
			  "hello-there")))

    (assert-equal "Documentation String"
		  
		  (test '("Documentation String"
			  (declare (type integer x))
			  (incf x)
			  "hello-there")))

    (assert-equal "Documentation String"
		  (test '((declare (type integer x))
			  (declare (optimize (speed 3)))
			  "Documentation String"
			  (incf x)
			  "hello-there")))

    (assert-equal "Documentation String"
		  (test '((declare (type integer x))
			  "Documentation String"
			  (declare (optimize (speed 3)))
			  (incf x)
			  "hello-there")))

    (assert-equal '()
		  (test '((declare (type integer x))
			  (declare (optimize (speed 3)))
			  "hello-there")))))

(define-test separate-argument-declarations
  (labels ((check-separation (expected-argument-declarations
			      expected-other-declarations
			      declarations)
	     (multiple-value-bind (actual-argument-declarations actual-other-declarations) (lisp-executable.definition::separate-argument-declarations declarations)
	       (assert-equal expected-argument-declarations actual-argument-declarations)
	       (assert-equal expected-other-declarations actual-other-declarations))))
    (check-separation '((lisp-executable:identifiers help "help" #\h)
			(lisp-executable:conversion-function integer debug)
			(lisp-executable:reducing-function lisp-executable:toggle-policy verbose))
		      '((type integer debug)
			(optimize (speed 3)))

		      '((type integer debug)
			(lisp-executable:identifiers help "help" #\h)
			(optimize (speed 3))
			(lisp-executable:conversion-function integer debug)
			(lisp-executable:reducing-function lisp-executable:toggle-policy verbose)))))