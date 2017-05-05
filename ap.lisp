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

;; ada reference manual
;; http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-3-2-2.html

;; spark 2014 reference manual
;; http://docs.adacore.com/spark2014-docs/html/lrm/declarations-and-types.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; decimal numbers			      ;;
;; 					      ;;
;; 12 0 1E6 123_456			      ;;
;; 12.0 0.0 0.456 3.14159_26 -- real literals ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; based numbers	  ;;
;; 			  ;;
;; 2#1111_1111#		  ;;
;; 16#E#E1		  ;;
;; 16#F.FF#E+2		  ;;
;; 16#FF#		  ;;
;; 016#0ff#		  ;;
;; 2#1110_0000#		  ;;
;; 2#1.1111_1111_1110#E11 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; character literals ;;
;; 		      ;;
;; 'A' 'L'	      ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string			        ;;
;; 				        ;;
;; "Hello World" "He said: ""Huh Huh""" ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comment		        ;;
;; 			        ;;
;; -- starts, until end of line ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pragma							   ;;
;; 								   ;;
;; pragma List(Off)						   ;;
;; pragma Assert(Exists(File_Name),Message => "Nonexistent file"); ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *ada-keywords*
  '(abort abs abstract accept access aliased all and array at begin
    body case constant declare delay delta digits do else elsif end
    entry exception exit for function generic goto if in interface isqrt
    limited loop mod new not null of or others out overriding package
    pragma private procedure protected raise range record rem renames
    requeue return reverse select separate some subtype synchronized
    tagged task terminate then type until use when while with xor))


;; declaration

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type declaration						  ;;
;; 								  ;;
;; type Color is (White, Red, Yellow, Green, Blue, Brown, Black); ;;
;; type Column is range 1 .. 72;				  ;;
;; type Table is array(1 .. 10) of Integer;			  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; spark forbids 'access'



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subtype							       ;;
;; 								       ;;
;; subtype Rainbow   is Color range Red .. Blue;        --  see 3.2.1  ;;
;; subtype Red_Blue  is Rainbow;				       ;;
;; subtype Int       is Integer;				       ;;
;; subtype Small_Int is Integer range -10 .. 10;		       ;;
;; subtype Up_To_K   is Column range 1 .. K;            --  see 3.2.1  ;;
;; subtype Square    is Matrix(1 .. 10, 1 .. 10);       --  see 3.6    ;;
;; subtype Male      is Person(Sex => M);               --  see 3.10.1 ;;
;; subtype Binop_Ref is not null Binop_Ptr;			       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; predicates						       ;;
;; 								       ;;
;; subtype Basic_Letter is Character -- See A.3.2 for "basic letter".  ;;
;;    with Static_Predicate => Basic_Letter in 'A'..'Z' | 'a'..'z'|;   ;;
;; subtype Even_Integer is Integer				       ;;
;;    with Dynamic_Predicate => Even_Integer mod 2 = 0,		       ;;
;;        Predicate_Failure => "Even_Integer must be a multiple of 2"; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; object								      ;;
;; 									      ;;
;; --  the multiple object declaration 					      ;;
;; 									      ;;
;; John, Paul : not null Person_Name := new Person(Sex => M);  --  see 3.10.1 ;;
;; 									      ;;
;; --  is equivalent to the two single object declarations in the order given ;;
;; 									      ;;
;; John : not null Person_Name := new Person(Sex => M);			      ;;
;; Paul : not null Person_Name := new Person(Sex => M);			      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variable						  ;;
;; 							  ;;
;; Count, Sum  : Integer;				  ;;
;; Size        : Integer range 0 .. 10_000 := 0;	  ;;
;; Sorted      : Boolean := False;			  ;;
;; Color_Table : array(1 .. Max) of Color;		  ;;
;; Option      : Bit_Vector(1 .. 10) := (others => True); ;;
;; Hello       : aliased String := "Hi, world.";	  ;;
;; θ, φ        : Float range -π .. +π;			  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constant					       ;;
;; 						       ;;
;; Limit     : constant Integer := 10_000;	       ;;
;; Low_Limit : constant Integer := Limit/10;	       ;;
;; Tolerance : constant Real := Dispersion(1.15);      ;;
;; Hello_Msg : constant access String := Hello'Access; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; number declaration								     ;;
;; 										     ;;
;; Two_Pi        : constant := 2.0*Ada.Numerics.Pi;   -- a real number (see A.5)     ;;
;; 10/2										     ;;
;; Max           : constant := 500;                   -- an integer number	     ;;
;; Max_Line_Size : constant := Max/6;                 -- the integer 83		     ;;
;; Power_16      : constant := 2**16;                 -- the integer 65_536	     ;;
;; One, Un, Eins : constant := 1;                     -- three different names for 1 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; derived type								  ;;
;; 									  ;;
;; type Local_Coordinate is new Coordinate;   --  two different types	  ;;
;; type Midweek is new Day range Tue .. Thu;  --  see 3.5.1		  ;;
;; type Counter is new Positive;              --  same range as Positive  ;;
;; type Special_Key is new Key_Manager.Key;   --  see 7.3.1		  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *scalar-subtype-attribute*
  '(First Last Range Base Min Max Succ Pred Wide_Wide_Image Wide_Image Image Wide_Wide_Width
    Wide_Width Width Wide_Wide_Value Wide_Value Value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scalar type						     ;;
;; 							     ;;
;; -10 .. 10						     ;;
;; X .. X + 1						     ;;
;; 0.0 .. 2.0*Pi					     ;;
;; Red .. Green     -- see 3.5.1			     ;;
;; 1 .. 0           -- a null range			     ;;
;; Table'Range      -- a range attribute reference (see 3.6) ;;
;; 							     ;;
;; 							     ;;
;; Examples of range constraints: 			     ;;
;; 							     ;;
;; range -999.0 .. +999.0				     ;;
;; range S'First+1 .. S'Last-1				     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enumeration type								 ;;
;; 										 ;;
;; type Day    is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);				 ;;
;; type Suit   is (Clubs, Diamonds, Hearts, Spades);				 ;;
;; type Gender is (M, F);							 ;;
;; type Level  is (Low, Medium, Urgent);					 ;;
;; type Color  is (White, Red, Yellow, Green, Blue, Brown, Black);		 ;;
;; type Light  is (Red, Amber, Green); -- Red and Green are overloaded		 ;;
;; type Hexa   is ('A', 'B', 'C', 'D', 'E', 'F');				 ;;
;; type Mixed  is ('A', 'B', '*', B, None, '?', '%');				 ;;
;; subtype Weekday is Day   range Mon .. Fri;					 ;;
;; subtype Major   is Suit  range Hearts .. Spades;				 ;;
;; subtype Rainbow is Color range Red .. Blue;  --  the Color Red, not the Light ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
