(pushnew "./" asdf:*central-registry*)
(asdf:oos 'asdf:test-op "lisp-executable-tests")
(unwind-protect
  (asdf:oos 'lisp-executable:create-executables-op "lisp-executable-example")
  
  #+:ecl (ext:quit 0)
  #+:sbcl (sb-ext:quit :unix-status 0)
  #+:ccl (ccl::quit 0)
  #+:cmucl (extensions:quit)
  #+:clisp (ext:quit))
