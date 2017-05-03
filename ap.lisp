(defpackage :cl-ada-generator
  (:use :cl)
  (:export))

(defpackage :cl-ada-generator-macros
  (:use :cl :cl-ada-generator))
(in-package :cl-ada-generator)

(setf (readtable-case *readtable*) :invert)


(defun print-sufficient-digits-f32 (f)
  "print a single floating point number as a string with a given nr. of
  digits. parse it again and increase nr. of digits until the same bit
  pattern."
  (let* ((ff (coerce f 'single-float))
	 (s (format nil "~E" ff)))
   (assert (= 0s0 (- ff
		     (read-from-string s))))
   (format nil "~af" s)))

#+nil
(print-sufficient-digits-f32 1s0)

(defun print-sufficient-digits-f64 (f)
  "print a double floating point number as a string with a given nr. of
  digits. parse it again and increase nr. of digits until the same bit
  pattern."
  (let* ((ff (coerce f 'double-float))
	 (s (format nil "~E" ff)))
   (assert (= 0d0 (- ff
		     (read-from-string s))))
   (substitute #\e #\d s)))

#+nil
(print-sufficient-digits-f64 1d0)


(defparameter *env-functions* nil)
(defparameter *env-macros* nil)


(defun emit-ada (&key code (str nil) (clear-env nil))
  (when clear-env
    (setf *env-functions* nil
	  *env-macros* nil))
  (if code
      (if (listp code)
       (case (car code)
	 (with (format str "with ~s;" (cadr code)))
	 (use (format str "use ~s;" (cadr code)))
	 (block (with-output-to-string (s)
		  (format s "begin~%")
		  (loop for e in (cdr code) do
		       (format s "  ~a~%"  (emit-ada :code (append '(statement) e))))
		  (format s "end;~%")))
	 (statements (with-output-to-string (s)
		       (loop for e in (cdr code) do
			    (format s "  ~a~%"  (emit-ada :code (append '(statement) e))))))
	 (defmacro (destructuring-bind ((name params) &rest macro-body) (cdr code)
		     (push (list :name name
				 :params params
				 :body macro-body)
			   *env-macros*)))
	 (macroexpand (destructuring-bind (macro &rest rest) (cdr code)
			(format str "~a" (emit-ada :code (macroexpand-1 macro)))))
	 (:params (loop for e in (cadr code) collect
		       (destructuring-bind (name type &optional (dir :i)) e
			 (format str "~a : ~a ~a"
				 type
				 (ecase dir
				   (:i "in")
				   (:io "in out")
				   (:o "out"))
				 name))))
	 (decl (destructuring-bind (bindings) (cdr code)
		 (with-output-to-string (s)
		   (loop for e  in bindings do
			(destructuring-bind (name type &optional init) e
			  (format s "~a : ~a"
				  (emit-ada :code name)
				  type
				  )
			  (when init
			    (format s " := ~a" (emit-ada :code init))
			    )
			  (format s ";~%")
			  )))))
	 (procedure (destructuring-bind ((name params &optional decl) &rest body) (cdr code)
		      #+nil (push (list :name name
					 :params params
					 :body body)
				   *env-functions*)
		      (format str "procedure ~a ~a is~%~a~%~a"
			      name
			      (format nil "(~{~a~^;~})" (emit-ada :code `(:params ,params)))
			      (if (listp (cdr decl))
				  (emit-ada :code
					    `(statements ,@(loop for e in decl collect e)))
				  (emit-ada :code `(statements ,decl)))
			      (emit-ada :code `(block ,@body)))))
	 (with-compilation-unit (format str "~{~a~^~%~}"
				 (loop for e in (cdr code) collect 
				      (emit-ada :code e))))
	 (if (destructuring-bind (condition true-statement &optional false-statement) (cdr code)
	       (with-output-to-string (s)
		 (format s "if ( ~a ) then ~a"
			 (emit-ada :code condition)
			 (emit-ada :code `(statements ,true-statement)))
		 (when false-statement
		  (format s "else ~a" (emit-ada :code `(statements ,false-statement))))
		 (format s "end if;"))))
	 (setf (destructuring-bind (&rest args) (cdr code)
		(with-output-to-string (s)
		  ;; handle multiple assignments
		  (loop for i below (length args) by 2 do
		       (format s "~a"
			       (emit-ada :code `(statement |:=| ,(elt args i) ,(elt args (1+ i))))))
		  (if (< 2 (length args))
		      (format s "~%")))))
	 (raw (destructuring-bind (string) (cdr code)
		(format str "~a" string)))
	 (statement ;; add semicolon
	  (cond ((member (second code) '(|:=| ))
		 ;; add semicolon to expressions
		 (format str "~a;" (emit-ada :code (cdr code))))
		((member (second code) '(if setf decl procedure))
		 ;; if for, .. don't need semicolon
		 (emit-ada :code (cdr code)))
		(t (format nil "not processable statement: ~a" code))))
	 
	 (t (cond ((and (= 2 (length code)) (member (car code)  '(- ~ !)))
		      ;; handle unary operators, i.e. - ~ !, this code
		      ;; needs to be placed before binary - operator!
		  (destructuring-bind (op operand) code
		    (format nil "(~a (~a))"
			    op
			    (emit-ada :code operand))))
		  ((member (car code) '(+ - * / < <=))
		   ;; handle binary operators
		   ;; no semicolon
		   (with-output-to-string (s)
		     (format s "(")
		     (loop for e in (cdr code)
			and i below (1- (length (cdr code))) do
			  (format s "~a ~a " (emit-ada :code e) (car code)))
		     (format s "~a)" (emit-ada :code (car (last (cdr code)))))))
		  
		  ((member (car code) '(|:=|))
		   ;; handle assignment, i.e. :=
		   (destructuring-bind (op lvalue rvalue) code
		    (format str "~a ~a ~a"
			    (emit-ada :code lvalue)
			    op
			    (emit-ada :code rvalue))))
		  ((member (car code)  '(and or xor not))
		   ;; handle logical operators, i.e. and
		   (destructuring-bind (op left right) code
		    (format str "(~a ~a ~a)"
			    (emit-ada :code left)
			    op
			    (emit-ada :code right))))
		  (t (format nil "not processable: ~a" code)))))
       (cond
	 ((or (symbolp code)
	      (stringp code)) ;; print variable
	  (format nil "~a" code))
	 ((numberp code) ;; print constants
	      (cond ((integerp code) (format str "~a" code))
		    ((floatp code)
		     (typecase code
		       (single-float (format str "(~a)" (print-sufficient-digits-f32 code)))
		       (double-float (format str "(~a)" (print-sufficient-digits-f64 code)))))
		    ))))
      ""))


#+nil
(emit-ada :code `(with-compilation-unit
		     (with Ada)))
#+nil
(emit-ada :code `(with-compilation-unit
		     (with Ada.Text_IO) (use Ada.Text_IO)
		     (with Ada.Integer_Text_IO) (use Ada.Integer_Text_IO)
		     (procedure (Average ((Q Integer)
					  (L Alpha :o))
					 )
				(if (< A Q)
				    (setf A (* A B))
				    (setf B (- B A))
				    ))))

#+nil
(emit-ada :code `(with-compilation-unit
		     (with Ada.Text_IO) (use Ada.Text_IO)
		     (with Ada.Integer_Text_IO) (use Ada.Integer_Text_IO)
		     (procedure (Average ((Q Integer)
					  (L Alpha :o))
					 (decl ((A Integer 3)
						(B Integer))))
				(if (< A Q)
				    (setf A (* A B))
				    (setf B (- B A))
				    ))))

(emit-ada :code `(with-compilation-unit ;; procedure in second level
		     (with Ada.Text_IO) (use Ada.Text_IO)
		     (with Ada.Integer_Text_IO) (use Ada.Integer_Text_IO)
		     (procedure (Average ((Q Integer)
					  (L Alpha :o))
					 ((decl ((A Integer 1)
						 (B Integer 2)))
					  (procedure (Second ((at Integer)))
						     (setf at Q))
					  (decl ((D Integer 3)
						 (C Integer)))))
				(if (< A Q)
				    (setf A (* A B))
				    (setf B (- B A))
				    ))))
