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

(defun alistp (object)
  (and (listp object)
       (or (alexandria:emptyp object)
	   (every #'consp object))))

(defun alist-equal (a b)
  (assert (and (alistp a)
	       (alistp b)))
  (let ((keys-a (mapcar #'car a))
	(keys-b (mapcar #'car b)))
    (and (= (length a) (length b))
	 (alexandria:emptyp (set-difference keys-a keys-b))
	 (every #'(lambda (key)
		    (equal (assoc key a) (assoc key b)))
		keys-a))))

;; gnu style tests
(define-test gnu-style-program-command-line-arguments-processor
  (let ((arguments (arguments (command-line-program-information 'test-program))))
    (labels ((parse (&rest strings)
	       (lisp-executable.gnu-style:program-command-line-arguments-processor arguments strings)))
      (assert-error 'error (parse "-h" "--help"))
      (assert-equal t (cdr (assoc 'help (parse "-h"))))
      (assert-equal 2 (cdr (assoc 'verbose (parse "-v" "--verbose"))))
      (assert-true (alist-equal '((help . nil)
				  (verbose . 0) 
				  (debug . t)
				  (debug-level . 1))
				
				(parse "--debug")))
      (assert-true (alist-equal '((help . nil)
				  (verbose . 0)
				  (debug . t)
				  (debug-level . 2))
				(parse "--debug=2")))
      (assert-true (alist-equal '((help . nil)
				  (verbose . 0)
				  (debug . t)
				  (debug-level . 1)
				  (input . #P "here.txt"))
				(parse "--debug" "here.txt")))
      (assert-true (alist-equal '((help . nil)
				  (verbose . 0)
				  (debug . t)
				  (debug-level . 1)
				  (input . #P "here.txt")
				  (output . #P "there.txt")
				  (others . ("i" "am" "rubbish")))
				(parse "--debug" "here.txt" "there.txt" "i" "am" "rubbish")))

      (assert-true (alist-equal '((help . t)
				  (verbose . 0)
				  (debug . t)
				  (debug-level . 1)
				  (input . #P "here.txt")
				  (output . #P "there.txt")
				  (others . ("i" "am" "rubbish")))
				(parse "--debug" "here.txt" "there.txt" "i" "am" "rubbish" "--help")))

      (assert-true (alist-equal '((help . nil)
				  (verbose . 0)
				  (debug . t)
				  (debug-level . 1)
				  (input . #P "here.txt")
				  (output . #P "there.txt")
				  (others . ("i" "am" "rubbish" "--help")))
				(parse "--debug" "here.txt" "there.txt" "i" "am" "rubbish" "--" "--help"))))))

(lisp-executable:define-program test-program-without-others (lisp-executable:&options help 
							     lisp-executable:&arguments input)
  (declare (ignore help input)))

(define-test gnu-style-program-command-line-arguments-processor/no-others 
  (let ((arguments (arguments (command-line-program-information 'test-program-without-others))))
    (labels ((parse (&rest strings)
	       (lisp-executable.gnu-style:program-command-line-arguments-processor arguments strings)))
      (assert-true (parse "first"))
      (assert-error 'error (parse "first" "trouble")))))

(lisp-executable:define-dispatcher-program test-dispatcher-program (lisp-executable:&options help
								    lisp-executable:&arguments input
								    lisp-executable:&others others)
  (declare (ignore help input others)))

(define-test gnu-style-dispatcher-command-line-arguments-processor  
  (let ((arguments (arguments (command-line-program-information 'test-dispatcher-program))))
    (labels ((parse (&rest strings)
	       (lisp-executable.gnu-style:dispatcher-command-line-arguments-processor arguments strings)))
      (assert-true (alist-equal '((help . nil))
				(parse)))

      (assert-true (alist-equal '((help . nil)
				  (input . "test")
				  (others "--help"))
				(parse "test" "--help")))

      (assert-true (alist-equal '((help . t))
				(parse "--help")))

      (assert-true (alist-equal '((help . t)
				  (input . "test")
				  (others "--help"))
				(parse "--help" "test" "--help"))))))