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
	    (with (format str "with ~{~a~^, ~};" (mapcar #'(lambda (x) (emit-ada :code x)) (cdr code))))
	    (use (format str "use ~{~a~^, ~};" (cdr code)))
	    (with-use (format str "with ~{~a~^, ~}; use ~{~a~^, ~};"
			      (cdr code)
			      (cdr code)))
	    (block (with-output-to-string (s) ;; FIXME include declarative part
		     (format s "begin~%")
		     (loop for e in (cdr code) do
		       (format s "  ~a~%"  (emit-ada :code `(statement ,@e))))
		     (format s "end;~%")))
	    (statements (with-output-to-string (s)
			  (loop for e in (cdr code) do
			    (format s "  ~a~%"  (emit-ada :code `(statement ,@e))))))
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
			     (format s "~a : ~a" (emit-ada :code name) (emit-ada :code type))
			     (when init
			       (format s " := ~a" (emit-ada :code init)))
			     (format s ";~%"))))))
	    (discriminant (destructuring-bind (name params) (cdr code)
			 (format str "~a(~{~a~^; ~})"
				 name
				 (emit-ada :code `(:params ,params)))))
	    (procedure (destructuring-bind ((name params &optional decl) &rest body) (cdr code)
			 #+nil (push (list :name name
					   :params params
					   :body body)
				     *env-functions*)
			 (format str "procedure ~a ~a~@[~a~];"
				 name
				 (if params
				     (format nil "(~{~a~^; ~})" (emit-ada :code `(:params ,params)))
				     "")
				 (if body
				     (format nil " is~%~a~%~a"
					   (emit-ada :code  `(statements ,@decl))
					   (emit-ada :code `(block ,@body)))
				     (emit-ada :code  `(statements ,@decl))))))
	    (function (destructuring-bind ((name params ret &optional decl) &rest body) (cdr code)
			 #+nil (push (list :name name
					   :params params
					   :ret ret
					   :body body)
				     *env-functions*)
			 (format str "function ~a ~a return ~a~@[~a~];"
				 name
				 (if params
				     (format nil "(~{~a~^; ~})" (emit-ada :code `(:params ,params)))
				     "")
				 (emit-ada :code ret)
				 (if body
				   (format nil " is~%~a~%~a"
					   (emit-ada :code `(statements ,@decl))
					   (emit-ada :code `(block ,@body)))
				   (emit-ada :code `(statements ,@decl))))))
	    (array
	     #|
	     | (array Error_Code "constant String")                   | array (Error_Code) of constant String                      | A |
	     | (array ((dots 0 1)) Real)                              | array (0 .. 1) of Real                                     | B |
	     | (array ((dots (+ Start 1) (- Max 1))) Real)            | array (Start+1 .. Max-1 ) of Real                          | C |
	     | (array ((range :type Integer)) Real)                    | array (Integer range <>) of Real                           | D |
	     | (array ((dots 1 80) (dots 1 100)) Boolean)             | array (1 .. 80, 1 .. 100) of Boolean                       | E |
	     | (array ((range Integer) (range Color Red Green)) Real) | array (Integer range <>, Color range Red .. Green) of Real | F |
	     | (array ((dots 2 3) (range Color Red Green)) Real)      | array (2 .. 3, Color range Red .. Green) of Real           | G |

|#
	     (destructuring-bind (dimensions type) (cdr code)
	       (format str "array ~a of ~a"
		       (if (atom dimensions)
			   (emit-ada :code dimensions) ;; A  ( enum type )
			   (format nil "(~{ ~a~^, ~} )" (loop for e in dimensions collect (emit-ada :code e))))
		       (emit-ada :code type))))
	    (attrib
	     ;; | (attrib a Digits)                                                      | a'Digits                                                   |        0 |
	     ;; | (attrib a (aref Digits 3) Mod)                                         | a'Digits(3)'Mod                                            |        0 |
	     ;; | (attrib a (aref Digits (+ 1 M)) Length)                                | a'Digits(1+M)'Length                                       |        0 |
	     (destructuring-bind (name &rest attribute-names) (cdr code)
	       (format str "~a~{'~a~}" (emit-ada :code name) (loop for e in attribute-names collect (emit-ada :code e)))))
	    (dots
	     #|
	    | (dots 0 3)       | 0 .. 3   | 0 |
	    | (dots 0 (+ A 3)) | 0 .. A+3 | 0 |
	    | (dots)           | <>       | 0 |
	     
	     |#
	     (if (cdr code)
		 (destructuring-bind (start end) (cdr code)
		   (format str "~a .. ~a" (emit-ada :code start) (emit-ada :code end)))
		 (format str "<>")))
	    (range
	     #|
	     | (range 0 3 :type integer)   | Integer range 0 .. 3 | 0 |
	     | (range 0 3)                 | range 0 .. 3         | 0 |
	     | (range nil nil)             | <>                   |   |
	     | (range nil nil :type Color) | Color range <>       |   |

             |#
	     (destructuring-bind (start end &key type) (cdr code)
	       (format str "~arange ~a"
		       (if type (format nil "~a " (emit-ada :code type)) "")
		       (if (and start end)
			   (emit-ada :code `(dots  ,start ,end))
			   (emit-ada :code `(dots))))))
	    (=>
	     #|
               | (=> ((dots 1 120) (char *)))            | 1 .. 120 => '*'                     | 0 |
               | (=> ((dots 1 5) (=> ((dots 1 8) 0.0)))) | 1 .. 5 => (1 .. 8 => 0.0)           | 0 |
               | (=> ((dots Mon Fri) True) (t False))    | Mon .. Fri => True, others => False | 0 |
	     
	     => (choice-1 stmt-1) (choice-2 stmt-2) 
             |#
	     (destructuring-bind (&rest clauses) (cdr code)
	       (format str "(~{~a~^, ~})" (loop for (choice stmt) in clauses
					      collect
					      (format nil "~@[~a ~]=> ~a" (if (eq t choice)
									 "others"
									 (emit-ada :code choice)) (emit-ada :code stmt))))))

	    (package
	     (destructuring-bind (name &rest body) (cdr code)
	       (format str "package ~a is~%~a~&end ~a;" (emit-ada :code name) (emit-ada :code `(statements ,@body)) (emit-ada :code name))))
	    (package-body
	     (destructuring-bind (name &rest body) (cdr code)
	       (format str "package body ~a is~%~a~&end ~a;" (emit-ada :code name) (emit-ada :code `(statements ,@body)) (emit-ada :code name))))
	    (subtype
	     (destructuring-bind (name definition) (cdr code)
	       (format str "subtype ~a is ~a;" (emit-ada :code name) (emit-ada :code definition))))
	    (type
	     #|
              | (type Color (comma-list White Red Yellow))                                      | type Color is (White, Red, Yellow)                               | 0 |
              | (type Column (range 1 72))                                                      | type Column is range 1 .. 72;                                    | 0 |
              | (type Matrix (array ((range 1 4 :type Integer) (range 1 4 :type Integer)) Real) | type Matrix is array(Integer range <>, Integer range <>) of Real | 0 |

	     |#
	     (destructuring-bind (name definition) (cdr code)
	       (format str "type ~a is ~a;" (emit-ada :code name) (emit-ada :code definition))))
	    (comma-list
	     #|
	     | (comma-list White Red Yellow) | (White, Red, Yellow) | 0 |
	     
	     |#
	     (format str "(~{~a~^, ~})" (loop for e in (cdr code) collect (emit-ada :code e))))
	    (let
		#|
		| (let ((Stars :type (aref String (dots 1 120)) :init (=> ((dots 1 .. 120) (char '*')))))) | Stars : String(1 .. 120) := (1 .. 120 => '*')      | 0 |
		| (let ((C :type "constant Matrix" :init (=> ((dots 1 5) (=> ((dots 1 8) 0.0)))))))        | C : constant Matrix := (1 .. 5 => (1 .. 8 => 0.0)) | 0 |
		
		|#
		(destructuring-bind (bindings &rest body) (cdr code)
		  (format str "declare~{~&  ~a;~}~&~a"
			  (loop for e in bindings collect
			       (destructuring-bind (name &key type init) e
				 (format nil "~a : ~a ~@[ := ~a~]" (emit-ada :code name) (emit-ada :code type)
					 (when init (emit-ada :code init)))))
			  (emit-ada :code `(block ,@body)))))
	    (record (destructuring-bind (bindings) (cdr code)
		      (format str "record~{~&  ~a;~}~&end record"
			      (loop for e in bindings collect
			       (destructuring-bind (name type &key init) e
				 (format nil "~a : ~a ~@[ := ~a~]" (emit-ada :code name) (emit-ada :code type)
					 (when init (emit-ada :code init))))))))
	    (aref
	     #|
	      | (aref (aref img 3) (+ 2 M)) | img(3)(2+M) | 0 |
	      | (aref a 4 3)                | a(4,3)      | 0 |
	      | (aref a (dots 0 3))         | a(0 .. 3)   | 0 |

	     |#
	     (destructuring-bind (name &rest indices) (cdr code)
	       (format str "~a(~{~a~^,~})" (emit-ada :code name) (loop for e in indices collect (emit-ada :code e)))))
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
	    (case
		#|
		case <selecting_expression> is 
		  <alternative>
		  {<alternative>}
                end case;

		altenative ::= when <discrete_choice_list> => <sequence_of_statements>

		(case Sensor
		  ((range Tue Thu) (call Generate_Report))
                  (t               null)
		|#
		(destructuring-bind (select &rest alternatives) (cdr code)
		  (format str "case ~a is~%~{~&  ~a~}end case;"
			  (emit-ada :code select)
			  (loop for e in alternatives
				collect
				(destructuring-bind (choice &rest statements) e
				  (format nil "when ~a => ~a" (if (eq t choice)
								  "others"
								  (emit-ada :code choice))
					  (emit-ada :code `(statements ,@statements))))))))

	    	  #|
		  <loop-id> <scheme> loop <seq-of-statements> end loop <loop-id>;
		  <scheme> ::= while <condition> | for <for-spec> | for <iter-spec>
		  <for-spec> ::= <id> in [reverse] <discrete-subtype>

		  * (loop
		    (call Get Current_Character)
		    (exit-when (= Current_Character (char #\*))))

          	    (loop-while (< price threshold)
		     (call Bid price)
		     (incf N 1))

	          (loop-for (j (attrib Buffer Range))
		    (if (/= (aref Buffer j) Space)
		        (call Put (aref Buffer j))))

		  (loop-for (i (dots 0 10))
		    (call Print i))
		  
		  * (loop-for-reverse (i (dots 0 10))
    		    (call Print i))
	       |#
	
	    (for (destructuring-bind ((id range) &rest body) (cdr code)
			(format str "for ~a in ~a loop~%~aend loop;" (emit-ada :code id)
				(emit-ada :code range) (emit-ada :code `(statements ,@body)))))

	    (while (destructuring-bind (condition &rest body) (cdr code)
			(format str "while ~a loop~%~aend loop;" (emit-ada :code condition)
				(emit-ada :code `(statements ,@body)))))
	    (exit-when (destructuring-bind (&optional condition) (cdr code)
			 (format str "exit when~a;"
				 (if condition (format nil " ~a" (emit-ada :code condition)) ""))))
	    (setf (destructuring-bind (&rest args) (cdr code)
		    (with-output-to-string (s)
		      ;; handle multiple assignments
		      (loop for i below (length args) by 2 do
			   (format s "~a"
				   (emit-ada :code `(statement |:=| ,(elt args i) ,(elt args (1+ i))))))
		      (if (< 2 (length args))
			  (format s "~%")))))
	    (return (destructuring-bind (val) (cdr code)
		      (format str "return ~a" (emit-ada :code val))))
	    (incf (destructuring-bind (var &optional (increment 1)) (cdr code)
		    (emit-ada :code `(setf ,var (+ ,var ,increment)))))
	    (hex (destructuring-bind (number) (cdr code)
		   (format str "16#~x#" number)))
	    (char (destructuring-bind (a) (cdr code)
		    (typecase a
		      (standard-char (format str "'~a'" a))
		      (number (format str "'~a'" (code-char a)))
		      (string (format str "'~a'" (elt a 0))))))
	    (string (destructuring-bind (string) (cdr code)
		      ;; FIXME replace " with "" in string
		      (format str "\"~a\"" string)))
	    (call (destructuring-bind (name &rest rest) (cdr code)
		    (with-output-to-string (s)
		      (format s "~a" (emit-ada :code name))
		      (when rest (format s "(~{~a~^, ~})" (loop for i below (length rest) collect
							       (let ((e (elt rest i)))
								 (if (keywordp e)
								     (prog1
								       (emit-ada :code `(=> (,e ,(elt rest (+ i 1)))))
								       (incf i))
								     (emit-ada :code e)))))))))
	    (raw (destructuring-bind (string) (cdr code)
		   (format str "~a" string)))
	    (and-then (destructuring-bind (clause-1 &rest clauses) (cdr code)
		       (format str "~a~{ and then ~a~}" (emit-ada :code clause-1) (mapcar #'(lambda (x) (emit-ada :code x)) clauses))))
	    (statement ;; add semicolon
	     (cond ((member (second code) '(|:=| call return))
		    ;; add semicolon to expressions
		    (format str "~a;" (emit-ada :code (cdr code))))
		   ((member (second code) '(if setf decl with procedure type record for while package package-body subtype function statement statements incf exit-when raw and-then))
		    ;; procedure .. don't need semicolon
		    (emit-ada :code (cdr code)))
		   (t (format nil "not processable statement: ~a, second code = ~a" code (second code)))))
	 
	    (t (cond ((and (= 2 (length code)) (member (car code)  '(- not)))
		      ;; handle unary operators, i.e. - ~ !, this code
		      ;; needs to be placed before binary - operator!
		      (destructuring-bind (op operand) code
			(format nil "(~a (~a))"
				op
				(emit-ada :code operand))))
		     ((member (car code) '(+ - * / < <= and or /= = &))
		      ;; handle binary operators
		      ;; no semicolon
		      (with-output-to-string (s)
			;(format s "(")
			(loop for e in (cdr code)
			   and i below (1- (length (cdr code))) do
			     (format s "~a ~a " (emit-ada :code e) (car code)))
			(format s "~a" (emit-ada :code (car (last (cdr code)))))))
		  
		     ((member (car code) '(|:=|))
		      ;; handle assignment, i.e. :=
		      (destructuring-bind (op lvalue rvalue) code
			(format str "~a ~a ~a"
				(emit-ada :code lvalue)
				op
				(emit-ada :code rvalue))))
		     ((member (car code)  '(xor rem))
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
| s-expression                                                                             | Ada                                                              | priority |
|------------------------------------------------------------------------------------------+------------------------------------------------------------------+----------|
| (. a b c)                                                                                | a.b.c                                                            |        0 |
| (aref (aref img 3) (+ 2 M))                                                              | img(3)(2+M)                                                      |        0 |
| (aref a 4 3)                                                                             | a(4,3)                                                           |        0 |
| (aref a (range 0 3))                                                                     | a(0 .. 3)                                                        |        0 |
| (.aref a (4 3) (6))                                                                      | a(4,3)(6)                                                        |        4 |
| (range Integer 0 3)                                                                      | Integer range 0 .. 3                                             |        0 |
| (attrib a Digits)                                                                        | a'Digits                                                         |        0 |
| (attrib a (aref Digits 3) Mod)                                                           | a'Digits(3)'Mod                                                  |        0 |
| (attrib a (aref Digits (+ 1 M)) Length)                                                  | a'Digits(1+M)'Length                                             |        0 |
| (string bla)                                                                             | "bla"                                                            |        0 |
| (char c)                                                                                 | 'c'                                                              |        0 |
| (hex #x12345FFF)                                                                         | 16#1234_5FFF#                                                    |        1 |
| (bit #b11100000)                                                                         | 2#1110_0000#                                                     |        1 |
| (with-use Types)                                                                         | with Types; use Types;                                           |        0 |
| (with lib1 lib2)                                                                         | use lib1, lib2;                                                  |        0 |
| (with Common_Units)                                                                      | with Common_Units;                                               |        0 |
| (use PkA PkB)                                                                            | use PkA, PkB;                                                    |        0 |
| (use-all-type TpA Tf)                                                                    | use all type TpA, Tf;                                            |        2 |
| (use-type TpA Tf)                                                                        | use type TpA, Tf;                                                |        2 |
| (private-with lib1 lib2)                                                                 | private with lib1, lib2;                                         |        2 |
| (limited-with .. )                                                                       |                                                                  |        2 |
| (limited-private-with ..)                                                                |                                                                  |        2 |
| (procedure (My_Proc ((Q Integer)) ((decl ((A Integer)) (procedure ..))                   |                                                                  |        0 |
| (procedure (<name> [params] [decl:procedure]) <body>)                                    |                                                                  |        0 |
| (function (<name> [params] <return> [decl:procedure]) <body)                             |                                                                  |        0 |
| (if <cond> <yes> [<no>])                                                                 |                                                                  |        0 |
| (array (dots 0 1) Real)                                                                  | array (0 .. 1) of Real                                           |        0 |
| (array ((dots 1 80) (dots 1 100)) Boolean)                                               | array (1 .. 80, 1 .. 100) of Boolean                             |        0 |
| (array (dots (+ Start 1) (- Max 1)) Real)                                                | array (Start+1 .. Max-1 ) of Real                                |        0 |
| (array Error_Code "constant String")                                                     | array (Error_Code) of constant String                            |        0 |
| (array Error_Code constant-String)                                                       | array (Error_Code) of constant String                            |        0 |
| (array (range Integer) Real)                                                             | array (Integer range <>) of Real                                 |        0 |
| (array ((range Integer) (range Color Red Green)) Real)                                   | array (Integer range <>, Color range Red .. Green) of Real       |        0 |
| (array ((dots 2 3) (range Color Red Green)) Real)                                        | array (2 .. 3, Color range Red .. Green) of Real                 |        0 |
| (=> ((dots 1 120) (char *)))                                                             | 1 .. 120 => '*'                                                  |        0 |
| (=> ((dots 1 5) (=> ((dots 1 8) 0.0))))                                                  | 1 .. 5 => (1 .. 8 => 0.0)                                        |        0 |
| (=> ((dots Mon Fri) True) (t False))                                                     | Mon .. Fri => True, others => False                              |        0 |
| (type Color (comma-list White Red Yellow))                                               | type Color is (White, Red, Yellow)                               |        0 |
| (type Column (range 1 72))                                                               | type Column is range 1 .. 72;                                    |        0 |
| (type Matrix (array ((range 1 4 :type Integer) (range 1 4 :type Integer)) Real)          | type Matrix is array(Integer range <>, Integer range <>) of Real |        0 |
| (let ((Stars :type (aref String (dots 1 120)) :init (=> ((dots 1 .. 120) (char '*')))))) | Stars : String(1 .. 120) := (1 .. 120 => '*')                    |        0 |
| (let ((C :type "constant Matrix" :init (=> ((dots 1 5) (=> ((dots 1 8) 0.0)))))))        | C : constant Matrix := (1 .. 5 => (1 .. 8 => 0.0))               |        0 |
| (comma-list White Red Yellow)                                                            | (White, Red, Yellow)                                             |        0 |
| (exit-when (= Color Red))
| (incf N)
| (incf N 3)
| (call Get)
| (call Put 3)
| (string "hello")
| (hex 32)
| (char #\x)
| (char "x")
| (char 12)
| (raw "pragma;")
| (and-then a b c)
| (with (=> (Pre bla) (Post bla2)))

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
		 (array ((dots 0 1)) Real)                                   
		 (array ((dots (+ Start 1) (- Max 1))) Real)                 
		 (array ((range nil nil :type Integer)) Real)                               
		 (array ((dots 1 80) (dots 1 100)) Boolean)               
		 (array ((range nil nil :type Integer) (range Red Green :type Color)) Real)   
		 (array ((dots 2 3) (range Red Green :type Color)) Real)) collect

	    (emit-ada :code e))

#+nil
("array Error_Code of constant String"
 "array ( 0 .. 1 ) of Real"
 "array ( (Start + 1) .. (Max - 1) ) of Real"
 "array ( Integer range <> ) of Real"
 "array ( 1 .. 80,1 .. 100 ) of Boolean"
 "array ( Integer range <>,Color range Red .. Green ) of Real"
 "array ( 2 .. 3,Color range Red .. Green ) of Real")

(loop for e in '( (aref (aref img 3) (+ 2 M))
		 (aref a 4 3)               
		 (aref a (range 0 3))
		 (aref a)) collect
		 (emit-ada :code e))
#+nil
("img(3)((2 + M))"
 "a(4,3)"
 "a(0 .. 3)"
 "a()")

(loop for e in '((range 0 1)
		 (range 3 4 :type Color)) collect
		 (emit-ada :code e))


(loop for e in '( (attrib a Digits)                          
		 (attrib a (aref Digits 3) Mod)             
		 (attrib a (aref Digits (+ 1 M)) Length)) collect
		 (emit-ada :code e))
#+nil ("a'Digits"
       "a'Digits(3)'Mod"
       "a'Digits((1 + M))'Length")
     

(loop for e in '((=> ((dots 1 120) (char #\*)))             
		 (=> ((dots 1 5) (=> ((dots 1 8) 0.0))))  
		 (=> ((dots Mon Fri) True) (t False))) collect
		 (emit-ada :code e))
#+nil
("(1 .. 120 => '*')"
 "(1 .. 5 => (1 .. 8 => (0.0e+0f)))"
 "(Mon .. Fri => True, others => False)")




(loop for e in '( (type Color (comma-list White Red Yellow))                                       
		 (type Column (range 1 72))                                                       
		 (type Matrix (array ((range 1 4 :type Integer) (range 1 4 :type Integer)) Real))) collect
		 (emit-ada :code e))
#+nil
("type Color is (White, Red, Yellow)" "type Column is range 1 .. 72"
 "type Matrix is array ( Integer range 1 .. 4,  Integer range 1 .. 4 ) of Real")

(loop for e in '( (comma-list White Red Yellow)                                       
		 ) collect
		 (emit-ada :code e))
#+nil
("(White, Red, Yellow)")


(loop for e in '((let ((Stars :type (aref String (dots 1 120)) :init (=> ((dots 1 120) (char #\*))))))
		 (let ((C :type "constant Matrix" :init (=> ((dots 1 5) (=> ((dots 1 8) 0.0))))))))
      collect
      (emit-ada :code e))
#+nil
("Stars : String(1 .. 120) := (1 .. 120 => '*');
"
 "C : constant Matrix := (1 .. 5 => (1 .. 8 => (0.0e+0f)));
")

(loop for e in '((call Get A)
		 (call Print A Q B)
		 (call Stop))
      collect
      (emit-ada :code e))

(loop for e in '((case Sensor
		   ((dots Tue Thu) (call Generate_Report) (call Get))
		   (t               (string "null"))))
      collect
      (emit-ada :code e))

#+nil
("case Sensor is
  when Tue .. Thu =>   Generate_Report;
  Get;
  when others =>   not processable statement: (statement string null), second code = string
end case;")



(loop for e in '((loop-for (j (attrib Buffer Range))
		    (if (/= (aref Buffer j) Space)
		        (call Put (aref Buffer j))))

		  (loop-for (i (dots 0 10))
		     (call Print i)
		     (exit-when (and i (hex 8))))
		 (loop-while (< price threshold)
		     (call Bid price)
		    (incf N 1)
		    (exit-when)))
      collect
      (emit-ada :code e))
#+nil
("for j in Buffer'Range loop
  if ( (Buffer(j) /= Space) ) then   Put(Buffer(j));
end if;
end loop;"
 "for i in 0 .. 10 loop
  Print(i);
  exit when (i and 16#8#);
end loop;"
 "while price < threshold loop
  Bid(price);
  N := N + 1;
  exit when;
end loop;")


(loop for e in '((let ((C :type "constant Matrix" :init (=> ((dots 1 5) (=> ((dots 1 8) 0.0))))))
		   (setf (aref C 3) 2)))
      collect
     (emit-ada :code e))
#+nil
("declare
  C : constant Matrix  := (1 .. 5 => (1 .. 8 => (0.0e+0f)));
begin
  C(3) := 2;
end;
")

(emit-ada :code `(procedure (Enqueue ((Queue Queue_Type :io)
				      (Item Element_Type :i))
				     ((with (=> (Pre (not (call Full Queue)))
						(Post  (and-then (not (call Empty Queue))
								 (= (call Size Queue)
								    (call Size (+ (attrib Queue Old) 1)))
								 (= (call Last_Element Queue) Item))q)))
				      (decl ((A Integer)))))
			    (call New_Line)))


(let ((def `(package Bounded_Queue_V1
		      (subtype Element_Type Integer)
		      (type Queue_Array (array ((range nil nil :type Positive)) Element_Type))
		      (type (discriminant Queue_Type ((MaxSize Positive)))
			    (record ((Count Natural)
				     (Front Positive)
				     (Rear Positive)
				     (Items (aref Queue_Array (dots 1 Max_Size))))))
		      (function (Full ((Queue Queue_Type :i)) Boolean))

		      (function (Empty ((Queue Queue_Type :i)) Boolean))
		      (function (Size ((Queue Queue_Type :i)) Natural))
		      (function (First_Element ((Queue Queue_Type :i)) Element_Type ((with (=> (Pre (not (call Empty Queue))))))))
		      (function (Last_Element ((Queue Queue_Type :i)) Element_Type
					      ((with (=> (Pre (not (call Empty Queue))))))))
		      (function (Clear ((Queue Queue_Type :i)) Element_Type
					      ((with (=> (Post (and-then (call Empty Queue) (= (call Size Queue) 0))))))))

		      (procedure (Enqueue ((Queue Queue_Type :io)
					   (Item Element_Type :i))
					  ((with (=> (Pre (not (call Full Queue)))
						     (Post  (and-then (not (call Empty Queue))
								      (= (call Size Queue)
									 (call Size (+ (attrib Queue Old) 1)))
								      (= (call Last_Element Queue) Item))))))))
		      (procedure (Dequeue ((Queue Queue_Type :io)
					   (Item Element_Type :i))
					  ((with (=> (Pre (not (call Empty Queue)))
						     (Post  (and-then (= Item (call First_Element (attrib Queue Old)))
								      (= (call Size Queue)
									 (- (call Size (attrib Queue Old)) 1)))))))))))
      (code `(package-body Bounded_Queue_V1
		      (function (Full ((Queue Queue_Type :i)) Boolean)
				(return (= Queue.Count Queue.Max_Size)))
		      (function (Empty ((Queue Queue_Type :i)) Boolean)
				(return (= Queue.Count 0)))
		      (function (Size ((Queue Queue_Type :i)) Natural)
				(return Queue.Count))
		      (function (First_Element ((Queue Queue_Type :i)) Element_Type)
				(return (aref Queue.Items Queue.Front)))
		      (function (Last_Element ((Queue Queue_Type :i)) Element_Type)
				(return (aref Queue.Items Queue.Rear)))
		      (procedure (Clear ((Queue Queue_Type :io)))
				 (setf Queue.Count 0
				       Queue.Front 1
				       Queue.Rear Queue.Max_Size))
		      (procedure (Enqueue ((Queue Queue_Type :io)
					   (Item Element_Type :i)))
				 (setf Queue.Rear (rem Queue.Rear (+ Queue.Max_Size 1))))
		      ))
      (call `(with-compilation-unit
		 (with-use Bounded_Queue_V1)
	       (with-use Ada.Text_IO)
	       (procedure (Bounded_Queue_Example_V1 nil ((decl ((My_Queue (call Bounded_Queue_V1.Queue_Type :Max_Size 100))
								(Value Integer)))))
			  (call Clear My_Queue)
			  (for (Count (range 17 52 :type Integer))
			       (call Enqueue (=> (Queue MyQueue)) (=> (Item Count))))
			  (for (Count (range 1 5 :type Integer))
			       (call Dequeue :Queue MyQueue :Item Value)
			       (call Put_Line (attrib Integer (call Image Value))))
			  (call Clear My_Queue)
			  (setf Value (call Size My_Queue))
			  (call Put_Line (& (string "Size of cleared queue is ")
					    (attrib Integer (call Image Value))))))))
  (write-source "Bounded_Queue_V1" "ads" def)
  (write-source "Bounded_Queue_V1" "adb" code)
  (write-source "Bounded_Queue_V1_call" "adb" call)
  (emit-ada :code code))

#+nil
"package Bounded_Queue_V1 is
  subtype Element_Type is Integer;
  type Queue_Array is array ( Positive range <> ) of Element_Type;
  type Queue_Type(MaxSize : in Positive) is record
  Count : Natural ;
  Front : Positive ;
  Rear : Positive ;
  Items : Queue_Array(1 .. Max_Size) ;
end record;
  function Full (Queue : in Queue_Type) return Boolean;
  function Empty (Queue : in Queue_Type) return Boolean;
  function Size (Queue : in Queue_Type) return Natural;
  function First_Element (Queue : in Queue_Type) return Element_Type  with (Pre => (not (Empty(Queue))));
;
  function Last_Element (Queue : in Queue_Type) return Element_Type  with (Pre => (not (Empty(Queue))));
;
  function Clear (Queue : in Queue_Type) return Element_Type  with (Post => Empty(Queue) and then Size(Queue) = 0);
;
  procedure Enqueue (Queue : in out Queue_Type; Item : in Element_Type)  with (Pre => (not (Full(Queue))), Post => (not (Empty(Queue))) and then Size(Queue) = Size(Queue'Old + 1) and then Last_Element(Queue) = Item);
;
  procedure Dequeue (Queue : in out Queue_Type; Item : in Element_Type)  with (Pre => (not (Empty(Queue))), Post => Item = First_Element(Queue'Old) and then Size(Queue) = Size(Queue'Old) - 1);
;
end Bounded_Queue_V1;"

#+nil
"procedure Enqueue (Queue : in out Queue_Type; Item : in Element_Type) is
  with (Pre => (not (Full(Queue))), Post => (not (Empty(Queue))) and then Size(Queue) = Size(Queue'Old + 1) and then Last_Element(Queue) = Item);
  A : Integer;


begin
  New_Line;
end;
"
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

(defparameter *file-hashes* (make-hash-table))

(defun write-source (name extension code)
  (let* ((fn (merge-pathnames (format nil "~a.~a" name extension)
                              (user-homedir-pathname)))
         (code-str (emit-ada
                    :clear-env t
                    :code code))
         (fn-hash (sxhash fn))
         (code-hash (sxhash code-str)))
    (multiple-value-bind (old-code-hash exists) (gethash fn-hash *file-hashes*)
      (when (or (not exists) (/= code-hash old-code-hash))
        ;; store the sxhash of the c source in the hash table
        ;; *file-hashes* with the key formed by the sxhash of the full
        ;; pathname
        (setf (gethash fn-hash *file-hashes*) code-hash)
        (with-open-file (s fn
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
          (write-sequence code-str s))
        ))))


#+nil
(let ((code `(with-compilation-unit
	    (raw "pragma SPARK_Mode(Off);")
	  (with-use Ada.Text_IO)
	  (with-use Ada.Integer_Text_IO)
	  (with-use Ada.Float_Text_IO)
	  (procedure (Average () ((decl ((A Integer)
					 (B Integer)
					 (M Float)))))
		     (call Put_Line (string "Enter two integers."))
		     (call Get A)
		     (call Get B)
		     (call New_Line)
		     (setf M (/ (call Float (+ A B))
				2.0))
		     (call Put (string "The Average of your two numbers"))
		     (call Put M 1 2 0)
		     (call New_Line)))))
  (write-source "average" "adb" code))
;; export PATH=~/big/ada/bin/:$PATH

