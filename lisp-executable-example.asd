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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system "lisp-executable"))

(defsystem lisp-executable-example
  :author "Mark Cox"
  :description "Example program containing an executable."
  :license "Simplified BSD License variant"
  :serial t
  :depends-on (lisp-executable)
  :components ((:module "example"
			:serial t
			:components ((:file "main")
				     (lisp-executable:executable "example-program" :program ("LISP-EXECUTABLE.EXAMPLE" "EXAMPLE-PROGRAM"))
				     (lisp-executable:executable "control-c-tester" :program ("LISP-EXECUTABLE.EXAMPLE" "CONTROL-C-TESTER"))))))
