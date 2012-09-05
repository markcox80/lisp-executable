(defpackage "LISP-EXECUTABLE.EXAMPLE"
  (:use "COMMON-LISP"
	"LISP-EXECUTABLE"))
(in-package "LISP-EXECUTABLE.EXAMPLE")

(define-program example-program (&options help)
  (cond
    (help
     (format t "Help has arrived."))
    (t
     (format t "You are doomed.")))
  (terpri))