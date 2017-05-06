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

;; http://www.electronicdesign.com/industrial/rust-and-spark-software-reliability-everyone


(defun emit-ada (&key code (str nil) (clear-env nil))
  (when clear-env
    (setf *env-functions* nil
	  *env-macros* nil))
  (if code
      (if (listp code)
	  (case (car code)
	    (with (format str "with ~{~s~^,~};" (cdr code)))
	    (use (format str "use ~{~s~^,~};" (cdr code)))
	    (with-use (format str "with ~{~s~^,~}; use ~{~s~^,~};"
			      (cdr code)
			      (cdr code)))
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
				    name
				    (ecase dir
				      (:i "in")
				      (:io "in out")
				      (:o "out"))
				    type))))
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
	    (function (destructuring-bind ((name params ret &optional decl) &rest body) (cdr code)
			 #+nil (push (list :name name
					   :params params
					   :ret ret
					   :body body)
				     *env-functions*)
			 (format str "procedure ~a ~a return ~a is~%~a~%~a"
				 name
				 (format nil "(~{~a~^;~})" (emit-ada :code `(:params ,params)))
				 (emit-ada :code ret)
				 (if (listp (cdr decl))
				     (emit-ada :code
					       `(statements ,@(loop for e in decl collect e)))
				     (emit-ada :code `(statements ,decl)))
				 (emit-ada :code `(block ,@body)))))
	    (array
	     ;; | (array Error_Code "constant String")                                   | array (Error_Code) of constant String                      |     A     |
	     ;; | (array (range 0 1) Real)                                               | array (0 .. 1) of Real                                     |     B     |
	     ;; | (array (range (+ Start 1) (- Max 1)) Real)                             | array (Start+1 .. Max-1 ) of Real                          |     C     |
	     ;; | (array (range) Real Integer)                                           | array (Integer range <>) of Real                           |     D     |
	     ;; | (array ((range 1 80) (range 1 100)) Boolean)                           | array (1 .. 80, 1 .. 100) of Boolean                       |     E     |
	     ;; | (array ((range) (range Red Green)) Real (Integer Color))               | array (Integer range <>, Color range Red .. Green) of Real |     F     |
	     ;; | (array ((range 2 3) (range Red Green)) Real (nil Color))               | array (2 .. 3, Color range Red .. Green) of Real           |     G     |

	     (destructuring-bind (size type &optional index-type) (cdr code)
	       (format str "array ~a of ~a"
		       (cond ((atom size) (emit-ada :code size)) ;; A  ( enum type )
			     ((listp size) ;; B .. G  
			      (cond
				((eq 'range (car size)) ;; B C D   ( 1d array )
				 (if index-type
				     (format nil "( ~a range ~a )" (emit-ada :code index-type) (emit-ada :code size)) ;; D
				     (format nil "( ~a )" (emit-ada :code size)) ;; B C
				     ))
				(t ;; E F G
				 (format nil "( ~{~a~^,~} )"
					 (if index-type
					     (loop for e in size and f in index-type
						   collect
						   (if f
						       (format nil "~a range ~a" (emit-ada :code f) (emit-ada :code e)) ;; F
						       (format nil "~a"  (emit-ada :code e)) ;; first dimension in G
						       ))
					     (loop for e in size collect (emit-ada :code e)) ;; E
					     )))))
			     (t "unhandled condition in array"))
		       (emit-ada :code type)))
	     )
	    (range
	     (if (cdr code)
		 (destructuring-bind (start end) (cdr code)
		   (format str "~a .. ~a" (emit-ada :code start) (emit-ada :code end)))
		 (format str "<>")))
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
		   ((member (second code) '(if setf decl procedure function))
		    ;; procedure .. don't need semicolon
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

(eq :range (second '(Integer :range (range))))

#|
;; orgtbl-mode
| s-expression                                                           | Ada                                                        | priority |
|------------------------------------------------------------------------+------------------------------------------------------------+----------|
| (. a b c)                                                              | a.b.c                                                      |        0 |
| (aref a 4 3)                                                           | a(4,3)                                                     |        0 |
| (.aref a (4 3) (6))                                                    | a(4,3)(6)                                                  |        4 |
| (range 0 3)                                                            | 0 .. 3                                                     |        0 |
| (range 0 (+ A 3))                                                      | 0 .. A+3                                                   |        0 |
| (range)                                                                | <>                                                         |        0 |
| (aref a (range 0 3))                                                   | a(0 .. 3)                                                  |        0 |
| (attrib a Digits)                                                      | a'Digits                                                   |        0 |
| (attrib a (aref Digits 3) Mod)                                         | a'Digits(3)'Mod                                            |        0 |
| (string bla)                                                           | "bla"                                                      |        0 |
| (char c)                                                               | 'c'                                                        |        0 |
| (hex #x12345FFF)                                                       | 16#1234_5FFF#                                              |        1 |
| (bit #b11100000)                                                       | 2#1110_0000#                                               |        1 |
| (with-use Types)                                                       | with Types; use Types;                                     |        0 |
| (with lib1 lib2)                                                       | use lib1, lib2;                                            |        0 |
| (with Common_Units)                                                    | with Common_Units;                                         |        0 |
| (use PkA PkB)                                                          | use PkA, PkB;                                              |        0 |
| (use-all-type TpA Tf)                                                  | use all type TpA, Tf;                                      |        2 |
| (use-type TpA Tf)                                                      | use type TpA, Tf;                                          |        2 |
| (private-with lib1 lib2)                                               | private with lib1, lib2;                                   |        2 |
| (limited-with .. )                                                     |                                                            |        2 |
| (limited-private-with ..)                                              |                                                            |        2 |
| (procedure (My_Proc ((Q Integer)) ((decl ((A Integer)) (procedure ..)) |                                                            |        0 |
| (procedure (<name> [params] [decl:procedure]) <body>)                  |                                                            |        0 |
| (function (<name> [params] <return> [decl:procedure]) <body)           |                                                            |        0 |
| (if <cond> <yes> [<no>])                                               |                                                            |        0 |
| (array (range 0 1) Real)                                               | array (0 .. 1) of Real                                     |          |
| (array ((range 1 80) (range 1 100)) Boolean)                           | array (1 .. 80, 1 .. 100) of Boolean                       |          |
| (array ((+ Start 1) (- Max 1)) Real)                                   | array (Start+1 .. Max-1 ) of Real                          |          |
| (array Error_Code "constant String")                                   | array (Error_Code) of constant String                      |          |
| (array Error_Code constant-String)                                     | array (Error_Code) of constant String                      |          |
| (array (range) Real Integer)                                           | array (Integer range <>) of Real                           |          |
| (array ((range) (range Red Green)) Real (Integer Color))               | array (Integer range <>, Color range Red .. Green) of Real |          |
| (array ((range 2 3) (range Red Green)) Real (nil Color))               | array (2 .. 3, Color range Red .. Green) of Real           |          |
| (=> (range 1 120) (char *))                                            | 1 .. 120 => '*'                                            |          |
|                                                                        |                                                            |          |


|#                             
  
#+nil
(emit-ada :code `(with-compilation-unit
		     (with Ada)))
#+nil
(emit-ada :code `(with-compilation-unit
		     (with-use Ada Beba)))
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

#+nil
(emit-ada :code `(with-compilation-unit
		     (function (Average ((Q Integer)
					 (L Alpha :o))
					Integer
					((decl ((A Integer 3)
						(B Integer)))))
				(if (< A Q)
				    (setf A (* A B))
				    (setf B (- B A))
				    ))))

;;type CRTP_Raw is array (1 .. CRTP_MAX_DATA_SIZE + 1) of T_Uint8





(loop for e in '((array Error_Code "constant String")                       
   (array (range 0 1) Real)                                   
   (array (range (+ Start 1) (- Max 1)) Real)                 
   (array (range) Real Integer)                               
   (array ((range 1 80) (range 1 100)) Boolean)               
   (array ((range) (range Red Green)) Real (Integer Color))   
	    (array ((range 2 3) (range Red Green)) Real (nil Color))) collect

	    (emit-ada :code e))

#+nil
("array Error_Code of constant String"
 "array ( 0 .. 1 ) of Real"
 "array ( (Start + 1) .. (Max - 1) ) of Real"
 "array ( Integer range <> ) of Real"
 "array ( 1 .. 80,1 .. 100 ) of Boolean"
 "array ( Integer range <>,Color range Red .. Green ) of Real"
 "array ( 2 .. 3,Color range Red .. Green ) of Real")



(progn
  (with-open-file (s "o.adb" :direction :output :if-exists :supersede)
   (emit-ada :str s :code `(with-compilation-unit ;; procedure in second level
			       (with Ada.Text_IO) (use Ada.Text_IO)
			       (with Ada.Integer_Text_IO) (use Ada.Integer_Text_IO)
			       (procedure (Average ((Q Integer :io)
						    (L Integer :o))
						   ((decl ((A Integer 1)
							   (B Integer 2)))
						    (procedure (Second ((whou Integer)))
							       (setf Q whou))
						    (decl ((D Integer 3)
							   (C Integer)))))
					  (if (< A Q)
					      (setf A (* A B))
					      (setf B (- B A 2))
					      )))))
  (sb-ext:run-program "/home/martin/big/ada/bin/gnat" (list "pretty" "-rf" "-P/home/martin/stage/cl-ada-generator/default.gpr" "/home/martin/stage/cl-ada-generator/o.adb")))


