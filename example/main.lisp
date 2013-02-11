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

(define-program control-c-tester (&options help (sleep sleep-time))
  (declare (conversion-function (real 0) sleep)
	   (type (or null (real 0)) sleep-time))

  (unless sleep
    (setf sleep-time 20.0))

  (cond
    (help
     (format t "Usage: [options]

Sleep for an amount of time.

Options:
  --help                      This helpful message.
  --sleep <positive real>     How long to sleep for. Default is 20 seconds.
")
     0)
    (t
     (sleep sleep-time)
     0)))
