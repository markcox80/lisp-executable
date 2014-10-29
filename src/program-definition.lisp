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

(in-package "LISP-EXECUTABLE.DEFINITION")

;;; Everything to do with defining a command line program
;;; - Processing program-lambda-args
;;; - Processing program declarations
;;; - Processing DEFINE-PROGRAM
;;; - Processing DEFINE-DISPATCHER-PROGRAM
;;;
;;; - Definition of arguments
;;;   - no parameter option
;;;   - optional parameter option
;;;   - required parameter option
;;;   - argument
;;;   - others
;;;
;;; - Definition of reducing function policies

(defun command-line-program-information (program-name)
  (get program-name 'command-line-program-information))

(defun (setf command-line-program-information) (new-value program-name)
  (setf (get program-name 'command-line-program-information) new-value))

(defgeneric arguments (command-line-program)
  (:documentation "Returns the arguments used by a command line program."))

(defgeneric argument-let-bindings-from-alist (command-line-program alist-var)
  (:documentation "Returns the let bindings needed for the command
  line program. All arguments are sourced from the alist ALIST-VAR."))

(defgeneric argument-form (argument)
  (:documentation "Returns a form that can be used to recreate the argument."))

(defgeneric argument-name (argument)
  (:documentation "Returns the name of the argument defined in the command line program definition."))

(defgeneric conversion-function (argument)
  (:documentation "Returns the conversion function used by the argument."))

(defgeneric identifiers (argument)
  (:documentation "Returns the identifiers used by the argument."))

(defgeneric reducing-function-form (argument)
  (:documentation "Returns the reducing function form specified for
  the argument. Use the function CREATE-REDUCING-FUNCTION to
  instantiate a reducing function."))

;; Reducing function policies

(defun error-policy (argument-name &optional (default-value nil))
  (let ((call-count 0))
    (lambda (&optional (current nil current-p) (next nil next-p))
      (cond
	((null current-p)
	 default-value)
	((zerop call-count)
	 (incf call-count)
	 (cond
	   ((and current-p (not next-p))
	    current)
	   ((and current-p next-p)
	    (assert (equal current default-value))
	    next)
	   (t
	    (error "Should not get here."))))
	(t
	 (error "Argument ~A specified more than once on the command line. Do not know what to do." argument-name))))))

(defun count-policy (argument-name &optional (starting-number 0))
  (declare (ignore argument-name))
  (let ((call-count starting-number))
    (lambda (&optional (current nil current-p) next)
      (declare (ignore current next))
      (cond
	((null current-p)
	 (assert (= call-count starting-number))
	 call-count)
	(t
	 (incf call-count)
	 call-count)))))

(defun toggle-policy (argument-name &optional (default-value nil))
  (declare (ignore argument-name))
  (lambda (&optional (current nil current-p) (next nil next-p))
    (declare (ignore next))
    (cond
      ((null current-p)
       default-value)
      (next-p
       (not current))
      (t
       t))))

(defun use-first-policy (argument-name)
  (declare (ignore argument-name))
  (lambda (current &optional next)
    (declare (ignore next))
    current))

(defun use-last-policy (argument-name)
  (declare (ignore argument-name))
  (lambda (current &optional (next nil next-p))
    (if next-p
	next
	current)))

(defun append-policy (argument-name)
  (declare (ignore argument-name))
  (lambda (current &optional (next nil next-p))
    (if next-p
	(append current (list next))
	(list current))))

(defun create-reducing-function (argument)
  (unless (or (non-option-argument-p argument)
	      (others-argument-p argument))
    (let ((reducing-function-form (reducing-function-form argument))
	  (argument-name          (argument-name argument)))
      (cond
	((symbolp reducing-function-form)
	 (funcall (symbol-function reducing-function-form) argument-name))
	(t
	 (error "Don't know how to create a reducing function from form ~A" reducing-function-form))))))

;; Form processing
;; - Conversion Function Forms
;;   (conversion-function function-name symbol-1 symbol-2 ... symbol-N)
(defun conversion-function-form-p (form)
  (and (listp form)
       (eql 'conversion-function (first form))
       (second form)
       (every #'symbolp (subseq form 2))))

(defun conversion-function-form-argument-names (form)
  (assert (conversion-function-form-p form))
  (subseq form 2))

(defun conversion-function-form-function-name (form)
  (assert (conversion-function-form-p form))
  (second form))

(defun conversion-function-form-for-argument-name-p (form argument-name)
  (when (conversion-function-form-p form)
    (find argument-name (conversion-function-form-argument-names form))))

(defun argument-declarations/find-conversion-function-form (argument-declarations argument-name)
  (let ((form (find-if #'(lambda (form)
			   (conversion-function-form-for-argument-name-p form argument-name))
		       argument-declarations)))
    (if form
	(conversion-function-form-function-name form)
	'identity)))

;; - Identifiers forms
;;   (identifiers argument-name string-or-character-1 string-or-character-2 ... string-or-character-N)
(defun string-or-character-p (object)
  (or (stringp object)
      (characterp object)))

(defun identifiers-form-p (form)
  (and (listp form)
       (eql 'identifiers (first form))
       (symbolp (second form))
       (every #'string-or-character-p (subseq form 2))))

(defun identifiers-form-argument-name (form)
  (assert (identifiers-form-p form))
  (second form))

(defun identifiers-form-identifiers (form)
  (assert (identifiers-form-p form))
  (subseq form 2))

(defun identifiers-form-for-argument-name-p (form argument-name)
  (when (identifiers-form-p form)
    (eql argument-name (identifiers-form-argument-name form))))

(defun argument-declarations/find-identifiers (argument-declarations argument-name)
  (let ((form (find-if #'(lambda (form)
			   (identifiers-form-for-argument-name-p form argument-name))
		       argument-declarations)))
    (if form
	(identifiers-form-identifiers form)
	(let ((name (string-downcase (symbol-name argument-name))))
	  (list (if (= 1 (length name))
		    (elt name 0)
		    name))))))

;; - Reducing Function forms
;;   (reducing-function function-name argument-name-1 argument-name-2 ... argument-name-N)
(defun reducing-function-form-p (form)
  (and (listp form)
       (eql 'reducing-function (first form))
       (symbolp (second form))
       (every #'symbolp (subseq form 2))))

(defun reducing-function-form-function-name (form)
  (assert (reducing-function-form-p form))
  (second form))

(defun reducing-function-form-argument-names (form)
  (assert (reducing-function-form-p form))
  (subseq form 2))

(defun reducing-function-form-for-argument-name-p (form argument-name)
  (when (reducing-function-form-p form)
    (find argument-name (reducing-function-form-argument-names form))))

(defun argument-declarations/find-reducing-function-form (argument-declarations argument-name)
  (let ((form (find-if #'(lambda (form)
			   (reducing-function-form-for-argument-name-p form argument-name))
		       argument-declarations)))
    (if form
	(reducing-function-form-function-name form)
	'error-policy)))

;; Program definition macro helpers
;;
;; Deconstructing all the options to a program
;;
;; - Make sure you read the hyperspec for DECLARE.  One or more
;; declare expressions can occur and the documentation string can
;; occur anywhere before, during and after the many declaration
;; expressions.
(defun separate-body-components (body)
  (declare (type list body))
  (let ((state :pre-body)
	(documentation-string)
	(declarations nil)
	(body-forms nil))
    (labels ((declaration-form-p (form)
	       (and (listp form) (eql (first form) 'declare))))
      (loop
	 :with body = body
	 :while (or body (not (eql state :body-forms)))
	 :do
	 (let ((form (first body)))
	   (ecase state
	     (:pre-body
	      (cond
		((declaration-form-p form)
		 (setf declarations (append declarations (rest form))))
		((stringp form)
		 (if documentation-string
		     (error "Duplicate documentation string ~A" form)
		     (setf documentation-string form
			   state :potential-documentation-string)))
		(t
		 (setf state :body-forms)
		 (push form body-forms))))
	     (:potential-documentation-string
	      (cond
		((declaration-form-p form)
		 (setf declarations (append declarations (rest form)))
		 (setf state :pre-body))
		((null form)
		 (push documentation-string body-forms)
		 (setf documentation-string nil
		       state :body-forms))
		(t
		 (push form body-forms)
		 (setf state :body-forms))))
	     (:body-forms
	      (push form body-forms))))
	 (setf body (rest body))))
    (values documentation-string
	    declarations
	    (nreverse body-forms))))

(defun extract-from-body/body (body)
  "Returns the function body of body"
  (multiple-value-bind (doc decls body) (separate-body-components body)
    (declare (ignore doc decls))
    body))

(defun extract-from-body/declarations (body)
  "Returns the declarations found in body as a list."
  (multiple-value-bind (doc decls body) (separate-body-components body)
    (declare (ignore doc body))
    decls))

(defun extract-from-body/documentation-string (body)
  "Returns the documentation string found in body"
  (multiple-value-bind (doc decls body) (separate-body-components body)
    (declare (ignore decls body))
    doc))

(defun argument-declaration-p (declaration)
  (declare (type list declaration))
  (member (first declaration) '(conversion-function reducing-function identifiers)))

(defun separate-argument-declarations (declarations)
  (declare (type list declarations))
  (let ((argument-declarations nil)
	(other-declarations nil))
    (dolist (declaration declarations)
      (declare (type list declaration))
      (if (argument-declaration-p declaration)
	  (push declaration argument-declarations)
	  (push declaration other-declarations)))
    (values (nreverse argument-declarations) 
	    (nreverse other-declarations))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-deconstruct-program-body (function body)
    (multiple-value-bind (documentation-string declarations function-body) (separate-body-components body)
      (multiple-value-bind (argument-declarations other-declarations) (separate-argument-declarations declarations)
	(funcall function argument-declarations documentation-string other-declarations function-body))))

  (defmacro deconstruct-program-body ((argument-declarations-var
				       documentation-string-var
				       function-declarations-var
				       function-body-var)
				      body-form
				      &body body)
    `(do-deconstruct-program-body #'(lambda (,argument-declarations-var 
					     ,documentation-string-var
					     ,function-declarations-var
					     ,function-body-var)
				      ,@body)
       ,body-form)))

(defun assemble-declarations (documentation-string declarations)
  (if documentation-string
      `(,documentation-string
	(declare ,@declarations))

      `((declare ,@declarations))))

(defclass argument ()
  ((type
    :initarg :type
    :reader argument-type)
   (name
    :initarg :name
    :reader argument-name)
   (parameter-type
    :initarg :parameter-type
    :reader argument-parameter-type)
   (parmeter-name
    :initarg :parameter-name
    :reader argument-parameter-name)
   (parameter-default-value
    :initarg :parameter-default-value
    :reader parameter-default-value)
   (conversion-function
    :initarg :conversion-function
    :reader conversion-function)
   (conversion-function-form
    :initarg :conversion-function-form
    :reader conversion-function-form)
   (identifiers
    :initarg :identifiers
    :reader identifiers)
   (reducing-function-form
    :initarg :reducing-function-form
    :reader reducing-function-form)))

(defun create-arguments-from-definitions/no-parameter-option (form argument-declarations)
  (assert (symbolp form))
  (let ((argument-name form))
    (make-instance 'argument 
		   :reducing-function-form   (argument-declarations/find-reducing-function-form argument-declarations argument-name)
		   :identifiers              (argument-declarations/find-identifiers argument-declarations argument-name)
		   :conversion-function-form (argument-declarations/find-conversion-function-form argument-declarations argument-name)
		   :type :option
		   :name argument-name
		   :parameter-type :none)))

(defun create-arguments-from-definitions/required-parameter-option (form argument-declarations)
  (assert (and (listp form)
	       (= 2 (length form))
	       (every #'symbolp form)))
  (destructuring-bind (argument-name argument-parameter-name) form
    (make-instance 'argument 
		   :reducing-function-form   (argument-declarations/find-reducing-function-form argument-declarations argument-name)
		   :identifiers              (argument-declarations/find-identifiers argument-declarations argument-name)
		   :conversion-function-form (argument-declarations/find-conversion-function-form argument-declarations argument-name)
		   :type :option
		   :name argument-name
		   :parameter-type :required
		   :parameter-name argument-parameter-name)))

(defun create-arguments-from-definitions/optional-parameter-option (form argument-declarations)
  (assert (and (listp form)
	       (= 3 (length form))
	       (symbolp (first form))
	       (symbolp (second form))))
  (destructuring-bind (argument-name argument-parameter-name parameter-default-value) form
    (make-instance 'argument 
		   :reducing-function-form   (argument-declarations/find-reducing-function-form argument-declarations argument-name)
		   :identifiers              (argument-declarations/find-identifiers argument-declarations argument-name)
		   :conversion-function-form (argument-declarations/find-conversion-function-form argument-declarations argument-name)
		   :type :option
		   :name argument-name
		   :parameter-type :optional
		   :parameter-name argument-parameter-name
		   :parameter-default-value parameter-default-value)))

(defun create-arguments-from-definitions/non-option-argument (form argument-declarations)
  (assert (symbolp form))
  (let ((argument-name form))
    (make-instance 'argument
		   :reducing-function-form   nil
		   :identifiers              (argument-declarations/find-identifiers argument-declarations argument-name)
		   :conversion-function-form (argument-declarations/find-conversion-function-form argument-declarations argument-name)
		   :type :argument
		   :name argument-name)))

(defun create-arguments-from-definitions/others-argument (form argument-declarations)
  (assert (symbolp form))
  (let ((others-name form))
    (make-instance 'argument
		   :reducing-function-form   nil
		   :identifiers              (argument-declarations/find-identifiers argument-declarations others-name)
		   :conversion-function-form (argument-declarations/find-conversion-function-form argument-declarations others-name)
		   :type :others
		   :name others-name)))

(defun create-arguments-from-definitions (program-lambda-args argument-declarations)
  (let ((state :start)
	(arguments nil))
    (loop
       :with program-lambda-args = program-lambda-args
       :while program-lambda-args
       :do
       (let ((sexp (first program-lambda-args)))
	 (ecase state
	   (:start
	    (ecase sexp
	      (&options
	       (setf state :options))
	      (&arguments
	       (setf state :arguments))
	      (&others
	       (setf state :others))))
	   (:options
	    (cond
	      ((symbolp sexp)
	       (case sexp
		 (&options
		  (error "Duplicate &options delimiter found in PROGRAM-LAMBDA-ARGS."))
		 (&arguments
		  (setf state :arguments))
		 (&others
		  (setf state :others))
		 (t
		  (push (create-arguments-from-definitions/no-parameter-option sexp argument-declarations) arguments))))
	      ((listp sexp)
	       (ecase (length sexp)
		 (1
		  (error "Invalid option s-expression ~A" sexp))
		 (2
		  (push (create-arguments-from-definitions/required-parameter-option sexp argument-declarations) arguments))
		 (3
		  (push (create-arguments-from-definitions/optional-parameter-option sexp argument-declarations) arguments))))
	      (t
	       (error "Invalid program-lambda-args s-expression ~A" sexp))))
	   (:arguments
	    (cond
	      ((symbolp sexp)
	       (case sexp
		 (&options
		  (error "&options delimiters must occur before &arguments."))
		 (&arguments
		  (error "Duplicate &arguments delimiter found in PROGRAM-LAMBDA-ARGS."))
		 (&others
		  (setf state :others))
		 (t
		  (push (create-arguments-from-definitions/non-option-argument sexp argument-declarations) arguments))))
	      (t
	       (error "Invalid non option argument s-expression ~A" sexp))))
	   (:others
	    (cond
	      ((symbolp sexp)
	       (case sexp
		 (&options
		  (error "&options delimiter must occur before &arguments section."))
		 (&arguments
		  (error "&arguments delimiter must occur before &others delimiter."))
		 (&others
		  (error "Duplicate &others delimiter found."))
		 (t
		  (push (create-arguments-from-definitions/others-argument sexp argument-declarations) arguments)
		  (setf state :end))))
	      (t
	       (error "Invalid others s-expression ~A" sexp))))
	   (:end
	    (error "Unexpected s-expression found ~A" sexp))))
       (setf program-lambda-args (rest program-lambda-args)))
    (nreverse arguments)))

;; argument predicates
(defun argumentp (object)
  (typep object 'argument))

(defun no-parameter-option-argument-p (object)
  (and (argumentp object)
       (eql :option (argument-type object))
       (eql :none   (argument-parameter-type object))))

(defun optional-parameter-option-argument-p (object)
  (and (argumentp object)
       (eql :option   (argument-type object))
       (eql :optional (argument-parameter-type object))))

(defun required-parameter-option-argument-p (object)
  (and (argumentp object)
       (eql :option   (argument-type object))
       (eql :required (argument-parameter-type object))))

(defun non-option-argument-p (object)
  (and (argumentp object)
       (eql :argument (argument-type object))))

(defun others-argument-p (object)
  (and (argumentp object)
       (eql :others (argument-type object))))

(defmethod print-object ((object argument) stream)
  (pprint-logical-block (stream nil)
    (print-unreadable-object (object stream :type t :identity nil)
      (pprint-indent :current 0 stream)
      (cond
	((no-parameter-option-argument-p object)
	 (format stream "(OPTION) ~A" (argument-name object)))
	((optional-parameter-option-argument-p object)
	 (format stream "(OPTION) ~A [~A]" (argument-name object) (argument-parameter-name object)))
	((required-parameter-option-argument-p object)
	 (format stream "(OPTION) ~A <~A>" (argument-name object) (argument-parameter-name object)))
	((non-option-argument-p object)
	 (format stream "~A" (argument-name object)))
	((others-argument-p object)
	 (format stream "(OTHERS) ~A" (argument-name object)))))))

(defmethod argument-form ((argument argument))
  `(make-instance 'argument
		  :conversion-function-form ',(conversion-function-form argument)
		  :identifiers (list ,@(identifiers argument))
		  :reducing-function-form ',(reducing-function-form argument)
		  :name ',(argument-name argument)
		  :type ,(argument-type argument)
		  ,@(cond
		     ((no-parameter-option-argument-p argument)
		      (list :parameter-name nil
			    :parameter-type :none
			    :parameter-default-value nil))
		     ((optional-parameter-option-argument-p argument)
		      (list :parameter-name          `',(argument-parameter-name argument)
			    :parameter-type          (argument-parameter-type argument)
			    :parameter-default-value (parameter-default-value argument)))
		     ((required-parameter-option-argument-p argument)
		      (list :parameter-name `',(argument-parameter-name argument)
			    :parameter-type (argument-parameter-type argument)))
		     (t
		      nil))))

;; argument let bindings from alist
(defun value-from-alist (alist name)
  (let ((v (assoc name alist)))
    (when v
      (cdr v))))

(defmethod argument-let-bindings-from-alist ((argument argument) alist-var)
  (append `((,(argument-name argument) (value-from-alist ,alist-var ',(argument-name argument))))
	  (cond
	    ((or (optional-parameter-option-argument-p argument)
		 (required-parameter-option-argument-p argument))
	     `((,(argument-parameter-name argument) (value-from-alist ,alist-var ',(argument-parameter-name argument))))))))

;; define-program

(defclass program-information ()
  ((name
    :initarg :name
    :reader information-name)
   (arguments
    :initarg :arguments
    :reader arguments)))

(defmethod argument-let-bindings-from-alist ((program-information program-information) alist-var)
  (reduce #'append (arguments program-information) 
	  :key #'(lambda (x)
		   (argument-let-bindings-from-alist x alist-var))
	  :initial-value nil))

(defun create-program-information-from-definitions (name program-lambda-args argument-declarations)
  (let ((arguments (create-arguments-from-definitions program-lambda-args argument-declarations)))
    (make-instance 'program-information :name name :arguments arguments)))

(defun ensure-program (program-name &key (arguments nil))
  (let ((program-information (make-instance 'program-information :name program-name :arguments arguments)))
    (setf (command-line-program-information program-name) program-information)
    program-information))

(defmacro define-program (name program-lambda-args &body body)
  (deconstruct-program-body (argument-declarations documentation-string declarations body) body
    (let ((alist-var           (gensym "PROGRAM-ARGUMENTS-ALIST"))
	  (program-information (create-program-information-from-definitions name program-lambda-args argument-declarations)))
      `(progn 
	 (ensure-program ',name :arguments (list ,@(mapcar #'argument-form (arguments program-information))))
	 (defun ,name (,alist-var)
           (declare (ignorable ,alist-var))
	   (let (,@(argument-let-bindings-from-alist program-information alist-var))
	     ,@(assemble-declarations documentation-string declarations)
	     ,@body))))))

;; define dispatcher
(defclass dispatcher-information ()
  ((name
    :initarg :name
    :reader information-name)
   (arguments
    :initarg :arguments
    :reader arguments)))

(defmethod argument-let-bindings-from-alist ((dispatcher-information dispatcher-information) alist-var)
  (reduce #'append (arguments dispatcher-information) 
	  :key #'(lambda (x)
		   (argument-let-bindings-from-alist x alist-var))))

(defun create-dispatcher-information-from-definitions (name program-lambda-args argument-declarations)
  (let ((arguments (create-arguments-from-definitions program-lambda-args argument-declarations)))
    (make-instance 'dispatcher-information :name name :arguments arguments)))

(defun ensure-dispatcher-program (dispatcher-name &key (arguments nil))
  (let ((dispatcher-information (make-instance 'dispatcher-information :name dispatcher-name :arguments arguments)))
    (setf (command-line-program-information dispatcher-name) dispatcher-information)
    dispatcher-information))

(defmacro define-dispatcher-program (name program-lambda-args &body body)
  (deconstruct-program-body (argument-declarations documentation-string declarations body) body
    (let ((alist-var           (gensym "DISPATCHER-ARGUMENTS-ALIST"))
	  (dispatcher-information (create-dispatcher-information-from-definitions name program-lambda-args argument-declarations)))
      `(progn 
	 (ensure-dispatcher-program ',name :arguments (list ,@(mapcar #'argument-form (arguments dispatcher-information))))
	 (defun ,name (,alist-var)
           (declare (ignorable ,alist-var))
	   (let (,@(argument-let-bindings-from-alist dispatcher-information alist-var))
	     ,@(assemble-declarations documentation-string declarations)
	     ,@body))))))
