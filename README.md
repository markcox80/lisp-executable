Lisp Executable
===============

The LISP-EXECUTABLE system provides a language for defining and
creating programs that can be used from the Unix shell instead of the
Lisp read-eval-print-loop (REPL).

The documentation for this project is contained in lisp-executable.org
or [online](http://markcox80.github.io/lisp-executable).

Quick Introduction
------------------

The files `lisp-executable-example.asd` and `example/main.lisp`
provide a quick illustration of how to use this system.

```common-lisp
(asdf:load-system "lisp-executable-example")
(asdf:oos 'lisp-executable:create-executables-op "lisp-executable-example")
```

The above code will generate two executables,
`example/example-program` and `example/control-c-tester`.


### Example Program

```common-lisp
(define-program example-program (&options help)
  (cond
    (help
     (format t "Help has arrived."))
    (t
     (format t "You are doomed.")))
  (terpri))
```

### Control C Tester
```common-lisp
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
```