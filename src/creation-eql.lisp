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

#-:eql (error "Should not be read. EQL only.")

(in-package "LISP-EXECUTABLE.CREATION")

(defmethod start-new-lisp-machine (&rest args &key &allow-other-keys)
  (declare (ignore args))
  (multiple-value-bind (stream something-i-dont-know process) (ext:run-program (ext:argv 0) nil :input :stream :output t :error :output :wait nil)
    (declare (ignore stream something-i-dont-know))
    (unless (eql (ext:external-process-status process) :running)
      (error "Unable to start external EQL process."))
    (print (let ((fn (merge-pathnames ".eclrc" (user-homedir-pathname))))
	     `(when (probe-file ,fn)
	       (load ,fn)))
	   (ext:external-process-input process))
    process))

(defmethod lisp-machine-input (lisp-machine)
  (ext:external-process-input lisp-machine))

(defmethod wait-for-lisp-machine (lisp-machine)
  (ext:external-process-wait lisp-machine t))

(uffi:def-function ("kill" %posix-kill)
    ((pid-t :int)
     (signal :int))
  :returning :int)

(defmethod kill-lisp-machine (lisp-machine)
  (force-output *lisp-machine-output-stream*)
  (%posix-kill (ext:external-process-pid lisp-machine) 15))

(defun format-code (code)
  (let ((string (with-output-to-string (out)
		  (pprint code out))))
    (with-output-to-string (out)
      (format out "{ ")
      (dotimes (i (length string))
	(write (char-code (elt string i)) :stream out)
	(format out ", " :stream out))
      (format out "0 }"))))

(defun generate-main-cpp (pathname init-name code &key (if-exists :error))
  (let ((init-function (format nil "init_lib_~A" init-name)))
    (with-open-file (out pathname :direction :output :if-exists if-exists)
      (format out "
#include <ecl/ecl.h>
#include <QtGui/QApplication>
#include \"eql.h\"
")
      (format out "extern \"C\" void ~A(cl_object);~%" init-function)
      (format out "

int
catch_all_qexec()
{
    int ret = 0;
    CL_CATCH_ALL_BEGIN(ecl_process_env())
    {
        ret = QApplication::exec();
    } 
    CL_CATCH_ALL_END;
    return ret;
}

int
main(int argc, char **argv)
{
  EQL::ini(argv);

  QApplication app(argc, argv);

  EQL eql;
")
      (format out "  char initialisation_text[] = ~A;~%" (format-code (let ((fn (merge-pathnames ".eclrc" (user-homedir-pathname))))
									`(when (probe-file ,fn)
									   (load ,fn)))))
      (format out "  // printf(\"%s\\n\",initialisation_text);~%")
      (format out "  eql.eval(initialisation_text);~%")
      (format out "  read_VV(OBJNULL, ~A);~%" init-function)
      (format out "  char text[] = ~A;~%" (format-code code))
      (format out "  // printf(\"%s\\n\",text);~%")
      (format out "  eql.eval(text);~%")
      (format out "

  return catch_all_qexec();
}"))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-with-temporary-file (type default-pathname function)
    (labels ((random-pathname ()
	       (make-pathname :name (with-output-to-string (out)
				      (write-string (pathname-name default-pathname) out)
				      (write-char #\- out)
				      (dotimes (i 5)
					(write-char (code-char (+ (char-code #\A)
								  (random (- (char-code #\Z)
									     (char-code #\A)))))
						    out)))
			      :type type
			      :defaults default-pathname)))
      (let ((pathname (loop
			 :with pn := (random-pathname)
			 :until (open pn :direction :probe :if-exists nil :if-does-not-exist :create)
			 :finally (return pn))))
	(unwind-protect
	     (funcall function pathname)
	  (delete-file pathname)))))

  (defmacro with-temporary-file ((var type template) &body body)
    `(do-with-temporary-file ,type ,template #'(lambda (,var)
						 ,@body))))

(defmethod save-executable-using-code-and-die (code output-file &rest args &key asdf-system (if-exists :error) &allow-other-keys)
  (declare (ignore args))
  (labels ((worker ()	     
	     (with-temporary-file (main-object "o" output-file)
	       (with-temporary-file (main-cpp "cpp" output-file)
		 (let ((lib-fn (first (asdf:make-build asdf-system :type :lib :monolithic t))))
		   (generate-main-cpp main-cpp
				      (format nil "~A_MONO" (substitute #\_ #\- (string-upcase asdf-system))) code
				      :if-exists :supersede)
		   (compile-c++-program main-object main-cpp)
		   (link-c++-program output-file (list main-object lib-fn)))))))
    (handler-case (progn
		    (worker)
		    (eql:qquit 0))
      (error (c)
	(format *error-output* "~&Error occurred during SAVE-EXECUTABLE-USING-CODE-AND-DIE.~%~A~%" c)
	(eql:qquit 1)))))

(defmethod command-line-arguments ()
  (rest (eql:qfun "QApplication" "arguments")))

(defmethod lisp-machine-exit (exit-status)
  (eql:qquit exit-status))

(defmethod executable-files (output-file)
  (list output-file))

;; C++, ECL and Qt stuff

(defun which/all (program)
  (multiple-value-bind (stream exit-code process) (ext:run-program "/usr/bin/which" (list program)
								  :output :stream
								  :error :output)
    (assert (zerop exit-code))
    (assert (eql :exited (ext:external-process-status process)))
    (let ((eof-value '#:eof-char))
      (loop
	 :for line := (read-line stream nil eof-value)
	 :until (eql line eof-value)
	 :collect 
	 (parse-namestring line)))))

(defun which (program)
  (let ((args (which/all program)))
    (case (length args)
      (0
       (error "Unable to find program ~A" program))
      (1
       (first args))
      (t
       (warn "More than one ~S program found." program)
       (first args)))))

(defun Qt-prefix ()
  (let ((qmake-path (which "qmake")))
    (merge-pathnames (make-pathname :directory (butlast (pathname-directory qmake-path))))))

(defun Qt-include-pathnames ()
  (labels ((subdir (&rest args)
	     (merge-pathnames (make-pathname :directory `(:relative ,@args)) (Qt-prefix))))    
    (list (subdir "include")
	  (subdir "include" "QtCore")
	  (subdir "include" "QtGui"))))

(defun Qt-library-directory ()
  (merge-pathnames (make-pathname :directory '(:relative "lib"))
		   (qt-prefix)))

(defun deconstruct-ecl-config-output (line)
  (let ((rv nil))
    (labels ((whitespace-character-p (character)
	       (find character '(#\Space) :test #'char=))
	     (add-entry (start end)
	       (unless (= start end)
		 (push (subseq line start end) rv))))
      (loop
	 :with start := 0
	 :for index :from 0 :below (length line)
	 :finally (add-entry start index)
	 :do
	 (when (whitespace-character-p (elt line index))
	   (add-entry start index)
	   (setf start (1+ index))))
      (nreverse rv))))

(defun ecl-c-flags ()
  (multiple-value-bind (stream exit-code process) (ext:run-program "ecl-config" '("--cflags") :output :stream :error :output)
    (assert (zerop exit-code))
    (assert (eql :exited (ext:external-process-status process)))
    (deconstruct-ecl-config-output (read-line stream))))

(defun ecl-ld-flags ()
  (multiple-value-bind (stream exit-code process) (ext:run-program "ecl-config" '("--ldflags") :output :stream :error :output)
    (assert (zerop exit-code))
    (assert (eql :exited (ext:external-process-status process)))
    (deconstruct-ecl-config-output (read-line stream))))

(defun default-c++-compiler ()
  (labels ((try (path)
	     "Try PATH to see if it exists. If so return PATH."
	     ;; Do not return the output of PROBE-FILE as some
	     ;; programs determine their behaviour based on the
	     ;; program name. Funky.
	     (when (probe-file path) 
	       path)))
    (or (try "/usr/bin/c++")
	(try "/usr/bin/g++")
	(error "Unable to find C++ compiler."))))

(defun default-c++-linker ()
  (default-c++-compiler))

(defun default-eql-cxx-flags ()
  (labels ((include-argument (pathname)
	     (format nil "-I~A" (namestring (truename pathname)))))
    (append (mapcar #'include-argument (qt-include-pathnames))
	    (ecl-c-flags))))

(defun default-eql-ld-flags ()
  (labels ((libdir-argument (pathname)
	     (format nil "-L~A" (namestring (truename pathname)))))
    (append (list (libdir-argument (qt-library-directory))
		  "-lQtCore"
		  "-lQtGui"
		  "-leql")
	    (ecl-ld-flags))))

(defun read-entire-stream (stream)
  (let ((eof-char '#:eof-value))
    (with-output-to-string (out)
      (loop
	 :for line := (read-line stream nil eof-char)
	 :until (eql line eof-char)
	 :do
	 (format out "~A~%" line)))))

(defun compile-c++-program (object-file c++-file &key (compiler (default-c++-compiler)) (user-cxx-flags nil))
  (multiple-value-bind (stream exit-code process) (ext:run-program (namestring compiler)
								   (append (list "-c" "-o" (namestring object-file) (namestring c++-file))
									   (default-eql-cxx-flags)
									   user-cxx-flags)
								   :output :stream
								   :error :output)
    (assert (eql :exited (ext:external-process-status process)))
    (unless (zerop exit-code)
      (error "Unable to compile file ~A.~%~A~%" c++-file (read-entire-stream stream)))
    object-file))

(defun link-c++-program (output-file object-files &key (linker (default-c++-linker)) (user-ld-flags nil))
  (multiple-value-bind (stream exit-code process) (ext:run-program (namestring linker)
								   (append (default-eql-ld-flags)
									   user-ld-flags
									   (list "-o" (namestring output-file))
									   (mapcar #'namestring object-files))
								   :output :stream
								   :error :output)
    (assert (eql :exited (ext:external-process-status process)))
    (unless (zerop exit-code)
      (error "Unable to link file ~A.~%~A~%" output-file (read-entire-stream stream)))
    output-file))
