#+:eql (load "~/.eclrc")
(pushnew "./" asdf:*central-registry*)
(asdf:oos 'asdf:test-op "lisp-executable-tests")
(unwind-protect
  (asdf:oos 'lisp-executable:create-executables-op "lisp-executable-example")

  #+:eql (eql:qquit 0)
  #+(and :ecl (not :eql)) (ext:quit 0)
  #+:sbcl (sb-ext:exit :code 0)
  #+:ccl (ccl::quit 0)
  #+:cmucl (extensions:quit)
  #+:clisp (ext:quit))
