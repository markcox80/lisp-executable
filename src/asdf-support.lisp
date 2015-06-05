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

(in-package "LISP-EXECUTABLE.ASDF-SUPPORT")

(defclass executable (asdf:component)
  ((program
    :initarg :program
    :reader program-path)))

(defmethod asdf:source-file-type ((component executable) system)
  (declare (ignorable component system))
  nil)

(defclass create-executables-op (#-asdf3 asdf:operation
				 #+asdf3 asdf:downward-operation)
  ())

#-asdf3
(defmethod asdf:component-depends-on ((operation create-executables-op) (component executable))
  (append (list (cons 'asdf:load-op (asdf::component-load-dependencies component)))
          (call-next-method)))

#+asdf3
(defmethod asdf:component-depends-on ((operation create-executables-op) (component executable))
  (append (list (list 'asdf:load-op (asdf::component-system component)))
          (call-next-method)))

(defmethod asdf:input-files ((operation create-executables-op) (component executable))
  (declare (ignorable operation component))
  nil)

(defmethod asdf:output-files ((operation create-executables-op) (component executable))
  (values (list (asdf:component-pathname component))
	  t))

(defmethod asdf:perform ((operation create-executables-op) (component executable))
  (destructuring-bind (package-name symbol-name) (program-path component)
    (lisp-executable:create-executable (intern symbol-name (find-package package-name)) 
                                       (asdf:output-file operation component)
                                       :asdf-system (asdf:component-name (asdf:component-system component))
                                       :if-exists :supersede)))

(defmethod asdf:perform ((operation create-executables-op) (component t))
  (values))

(defmethod asdf:input-files ((operation asdf:compile-op) (component executable))
  (declare (ignorable operation component))
  nil)

(defmethod asdf:perform ((operation asdf:compile-op) (component executable))
  (declare (ignorable operation component))
  nil)

(defmethod asdf:input-files ((operation asdf:load-op) (component executable))
  (declare (ignorable operation component))
  nil)

(defmethod asdf:perform ((operation asdf:load-op) (component executable))
  (declare (ignorable operation component))
  nil)
