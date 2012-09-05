;; Copyright (c) 2012, Mark Cox
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

#-eql (error "This example only works with EQL.")

;; Execute "eql build.lisp" in order to build the example EQL program.

(let ((eclrc-pathname (merge-pathnames ".eclrc" (user-homedir-pathname))))
  (when (probe-file eclrc-pathname)
    (format t "~&;; Loading ~A~%" eclrc-pathname)
    (load eclrc-pathname)))

(when (probe-file "eql-example")
  (delete-file "eql-example"))

(push "../../" asdf:*central-registry*)
(asdf:load-system "eql-example")
(asdf:oos 'lisp-executable:create-executables-op "eql-example")
(eql:qq)
