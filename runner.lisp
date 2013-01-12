(pushnew "./" asdf:*central-registry*)
(asdf:load-system "lisp-unit")
(setf lisp-unit:*print-errors* t
      lisp-unit:*print-failures* t)
(asdf:test-system "lisp-executable")
(unwind-protect
     (asdf:oos 'lisp-executable:create-executables-op "lisp-executable-example")
  
  #+:ecl (ext:quit 0)
  #+:sbcl (sb-ext:exit :code 0)
  #+:ccl (ccl::quit 0)
  #+:cmucl (extensions:quit)
  #+:clisp (ext:quit))
