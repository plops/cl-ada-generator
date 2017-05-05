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

#+nil
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; character type					    ;;
;; 							    ;;
;; type Roman_Digit is ('I', 'V', 'X', 'L', 'C', 'D', 'M'); ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(defparameter *modular-subtype-attribute*
  '(Mod Modulus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integer type										      ;;
;; 											      ;;
;; -- Integer has two predefined subtypes, declared in the visible part of package Standard:  ;;
;; 											      ;;
;; subtype Natural  is Integer range 0 .. Integer'Last;					      ;;
;; subtype Positive is Integer range 1 .. Integer'Last;					      ;;
;; 											      ;;
;; type Page_Num  is range 1 .. 2_000;							      ;;
;; type Line_Size is range 1 .. Max_Line_Size;						      ;;
;; 											      ;;
;; subtype Small_Int   is Integer   range -10 .. 10;					      ;;
;; subtype Column_Ptr  is Line_Size range 1 .. 10;					      ;;
;; subtype Buffer_Size is Integer   range 0 .. Max;					      ;;
;; 											      ;;
;; type Byte        is mod 256; -- an unsigned byte					      ;;
;; type Hash_Index  is mod 97;  -- modulus is prime					      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+nil
(defparameter *discrete-subtype-attribute*
  '(Pos Val First_Valid Last_Valid))

#+nil
(defparameter *floating-point-subtype-attribute*
  '(Digits))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; floating point type								       ;;
;; 										       ;;
;; type Coefficient is digits 10 range -1.0 .. 1.0;				       ;;
;; type Real is digits 8;							       ;;
;; type Mass is digits 7 range 0.0 .. 1.0E35;					       ;;
;; subtype Probability is Real range 0.0 .. 1.0;   --   a subtype with a smaller range ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+nil
(defparameter *fixed-point-subtype-attribute*
  '(Small Delta Fore Aft))
#+nil
(defparameter *decimal-fixed-point-subtype-attribute*
  '(Digits Scale Round))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fixed point type						   ;;
;; 								   ;;
;; type Volt is delta 0.125 range 0.0 .. 255.0;			   ;;
;;   -- A pure fraction which requires all the available	   ;;
;;   -- space in a word can be declared as the type Fraction:	   ;;
;; type Fraction is delta System.Fine_Delta range -1.0 .. 1.0;	   ;;
;;   -- Fraction'Last = 1.0 – System.Fine_Delta			   ;;
;; type Money is delta 0.01 digits 15;  -- decimal fixed point	   ;;
;; subtype Salary is Money digits 10;				   ;;
;;   -- Money'Last = 10.0**13 – 0.01, Salary'Last = 10.0**8 – 0.01 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; array type		(3.6)						    ;;
;; 									    ;;
;; Examples of type declarations with unconstrained array definitions: 	    ;;
;; 									    ;;
;; type Vector     is array(Integer  range <>) of Real;			    ;;
;; type Matrix     is array(Integer  range <>, Integer range <>) of Real;   ;;
;; type Bit_Vector is array(Integer  range <>) of Boolean;		    ;;
;; type Roman      is array(Positive range <>) of Roman_Digit; -- see 3.5.2 ;;
;; 									    ;;
;; 									    ;;
;; Examples of type declarations with constrained array definitions: 	    ;;
;; 									    ;;
;; type Table    is array(1 .. 10) of Integer;				    ;;
;; type Schedule is array(Day) of Boolean;				    ;;
;; type Line     is array(1 .. Max_Line_Size) of Character;		    ;;
;; 									    ;;
;; 									    ;;
;; Examples of object declarations with array type definitions: 	    ;;
;; 									    ;;
;; Grid      : array(1 .. 80, 1 .. 100) of Boolean;			    ;;
;; Mix       : array(Color range Red .. Green) of Boolean;		    ;;
;; Msg_Table : constant array(Error_Code) of access constant String :=	    ;;
;;       (Too_Big => new String'("Result too big"), Too_Small => ...);	    ;;
;; Page      : array(Positive range <>) of Line :=  --  an array of arrays  ;;
;;   (1 | 50  => Line'(1 | Line'Last => '+', others => '-'),  -- see 4.3.3  ;;
;;    2 .. 49 => Line'(1 | Line'Last => '|', others => ' '));		    ;;
;;     -- Page is constrained by its initial value to (1..50)		    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; array with index constraints	 (3.6.1)				         ;;
;; 										 ;;
;; Examples of array declarations including an index constraint: 		 ;;
;; 										 ;;
;; Board     : Matrix(1 .. 8,  1 .. 8);  --  see 3.6				 ;;
;; Rectangle : Matrix(1 .. 20, 1 .. 30);					 ;;
;; Inverse   : Matrix(1 .. N,  1 .. N);  --  N need not be static 		 ;;
;; 										 ;;
;; Filter    : Bit_Vector(0 .. 31);						 ;;
;; 										 ;;
;; 										 ;;
;; Example of array declaration with a constrained array subtype: 		 ;;
;; 										 ;;
;; My_Schedule : Schedule;  --  all arrays of type Schedule have the same bounds ;;
;; 										 ;;
;; 										 ;;
;; Example of record type with a component that is an array: 			 ;;
;; 										 ;;
;; type Var_Line(Length : Natural) is						 ;;
;;    record									 ;;
;;       Image : String(1 .. Length);						 ;;
;;    end record;								 ;;
;; 										 ;;
;; Null_Line : Var_Line(0);  --  Null_Line.Image is a null array		 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(defparameter *array-subtype-attribute*
  '(First First-N Last Last-N Range Range-N Length Length-N))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string type (3.6.3)											    ;;
;; 													    ;;
;; Stars      : String(1 .. 120) := (1 .. 120 => '*' );							    ;;
;; Question   : constant String  := "How many characters?";						    ;;
;;                                                       -- Question'First = 1, Question'Last = 20	    ;;
;;                                                       -- Question'Length = 20 (the number of characters) ;;
;; 													    ;;
;; Ask_Twice  : String  := Question & Question;          -- constrained to (1..40)			    ;;
;; Ninety_Six : constant Roman   := "XCVI";              -- see 3.5.2 and 3.6				    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; discriminated type (3.7)					    ;;
;; 								    ;;
;; type Buffer(Size : Buffer_Size := 100)  is        -- see 3.5.4   ;;
;;    record							    ;;
;;       Pos   : Buffer_Size := 0;				    ;;
;;       Value : String(1 .. Size);				    ;;
;;    end record;						    ;;
;; 								    ;;
;; type Matrix_Rec(Rows, Columns : Integer) is			    ;;
;;    record							    ;;
;;       Mat : Matrix(1 .. Rows, 1 .. Columns);       -- see 3.6    ;;
;;    end record;						    ;;
;; 								    ;;
;; type Square(Side : Integer) is new				    ;;
;;    Matrix_Rec(Rows => Side, Columns => Side);		    ;;
;; 								    ;;
;; type Double_Square(Number : Integer) is			    ;;
;;    record							    ;;
;;       Left  : Square(Number);				    ;;
;;       Right : Square(Number);				    ;;
;;    end record;						    ;;
;; 								    ;;
;; task type Worker(Prio : System.Priority; Buf : access Buffer)    ;;
;;    with Priority => Prio is -- see D.1			    ;;
;;    -- discriminants used to parameterize the task type (see 9.1) ;;
;;    entry Fill;						    ;;
;;    entry Drain;						    ;;
;; end Worker;							    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; discriminant constraints					       ;;
;; 								       ;;
;; Large   : Buffer(200);  --  constrained, always 200 characters      ;;
;;                         --   (explicit discriminant value)	       ;;
;; Message : Buffer;       --  unconstrained, initially 100 characters ;;
;;                         --   (default discriminant value)	       ;;
;; Basis   : Square(5);    --  constrained, always 5 by 5	       ;;
;; Illegal : Square;       --  illegal, a Square has to be constrained ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(defparameter *discriminant-subtype-attribute*
  '(Constrained))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; record type				  ;;
;; 					  ;;
;; type Date is				  ;;
;;    record	         		  ;;
;;       Day   : Integer range 1 .. 31;	  ;;
;;       Month : Month_Name;		  ;;
;;       Year  : Integer range 0 .. 4000; ;;
;;    end record;			  ;;
;; type Complex is			  ;;
;;     record		                  ;;
;;       Re : Real := 0.0;		  ;;
;;       Im : Real := 0.0;		  ;;
;;    end record;			  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [[abstract] tagged] [limited] ;;
;; null record			 ;;
;; component_list can be null;	 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; record with variant part				  ;;
;; 							  ;;
;; type Device is (Printer, Disk, Drum);		  ;;
;; type State  is (Open, Closed);			  ;;
;; 							  ;;
;; type Peripheral(Unit : Device := Disk) is		  ;;
;;    record						  ;;
;;       Status : State;				  ;;
;;       case Unit is					  ;;
;;          when Printer =>				  ;;
;;             Line_Count : Integer range 1 .. Page_Size; ;;
;;          when others =>				  ;;
;;             Cylinder   : Cylinder_Index;		  ;;
;;             Track      : Track_Number;		  ;;
;;          end case;					  ;;
;;       end record;					  ;;
;; 							  ;;
;; Examples of record subtypes: 			  ;;
;; 							  ;;
;; subtype Drum_Unit is Peripheral(Drum);		  ;;
;; subtype Disk_Unit is Peripheral(Disk);                 ;; 
;;
;; Writer   : Peripheral(Unit  => Printer); 
;; Archive  : Disk_Unit;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tagged record type			  ;;
;; 					  ;;
;; for OO with runtime dispatch		  ;;
;; 					  ;;
;; type Point is tagged			  ;;
;;   record				  ;;
;;     X, Y : Real := 0.0;		  ;;
;;   end record;			  ;;
;; 					  ;;
;; type Expression is tagged null record; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; record type with extensions						     ;;
;; 									     ;;
;; type Painted_Point is new Point with					     ;;
;;   record								     ;;
;;     Paint : Color := White;						     ;;
;;   end record;							     ;;
;;     -- Components X and Y are inherited				     ;;
;; 									     ;;
;; Origin : constant Painted_Point := (X | Y => 0.0, Paint => Black);	     ;;
;; 									     ;;
;; type Literal is new Expression with					     ;;
;;   record                 -- a leaf in an Expression tree		     ;;
;;     Value : Real;							     ;;
;;   end record;							     ;;
;; 									     ;;
;; type Expr_Ptr is access all Expression'Class;			     ;;
;;                                -- see 3.10				     ;;
;; 									     ;;
;; type Binary_Operation is new Expression with				     ;;
;;   record                 -- an internal node in an Expression tree	     ;;
;;     Left, Right : Expr_Ptr;						     ;;
;;   end record;							     ;;
;; 									     ;;
;; type Addition is new Binary_Operation with null record;		     ;;
;; type Subtraction is new Binary_Operation with null record;		     ;;
;;   -- No additional components needed for these extensions		     ;;
;; 									     ;;
;; Tree : Expr_Ptr :=         -- A tree representation of “5.0 + (13.0–7.0)” ;;
;;    new Addition'(							     ;;
;;       Left  => new Literal'(Value => 5.0),				     ;;
;;       Right => new Subtraction'(					     ;;
;;          Left  => new Literal'(Value => 13.0),			     ;;
;;          Right => new Literal'(Value => 7.0)));			     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; abstract types and subprograms					 ;;
;; 									 ;;
;; package Sets is							 ;;
;;     subtype Element_Type is Natural;					 ;;
;;     type Set is abstract tagged null record;				 ;;
;;     function Empty return Set is abstract;				 ;;
;;     function Union(Left, Right : Set) return Set is abstract;	 ;;
;;     function Intersection(Left, Right : Set) return Set is abstract;	 ;;
;;     function Unit_Set(Element : Element_Type) return Set is abstract; ;;
;;     procedure Take(Element : out Element_Type;			 ;;
;;                    From : in out Set) is abstract;			 ;;
;; end Sets;								 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interface type								        ;;
;; 										        ;;
;; 										        ;;
;; type Queue is limited interface;						        ;;
;; procedure Append(Q : in out Queue; Person : in Person_Name) is abstract;	        ;;
;; procedure Remove_First(Q      : in out Queue;				        ;;
;;                        Person : out Person_Name) is abstract;		        ;;
;; function Cur_Count(Q : in Queue) return Natural is abstract;			        ;;
;; function Max_Count(Q : in Queue) return Natural is abstract;			        ;;
;; -- See 3.10.1 for Person_Name.						        ;;
;; 										        ;;
;; Queue_Error : exception;							        ;;
;; -- Append raises Queue_Error if Cur_Count(Q) = Max_Count(Q)			        ;;
;; -- Remove_First raises Queue_Error if Cur_Count(Q) = 0			        ;;
;; 										        ;;
;; type Synchronized_Queue is synchronized interface and Queue; -- see 9.11	        ;;
;; procedure Append_Wait(Q      : in out Synchronized_Queue;			        ;;
;;                       Person : in Person_Name) is abstract;			        ;;
;; procedure Remove_First_Wait(Q      : in out Synchronized_Queue;		        ;;
;; 				   Person : out Person_Name) is abstract;	        ;;
;; 				   						        ;;
;; ...										        ;;
;; 										        ;;
;; procedure Transfer(From   : in out Queue'Class;				        ;;
;;                    To     : in out Queue'Class;				        ;;
;;                    Number : in     Natural := 1) is				        ;;
;;    Person : Person_Name;							        ;;
;; begin									        ;;
;;    for I in 1..Number loop							        ;;
;;       Remove_First(From, Person);						        ;;
;;       Append(To, Person);							        ;;
;;    end loop;									        ;;
;; end Transfer;								        ;;
;; 										        ;;
;; type Fast_Food_Queue is new Queue with record ...;				        ;;
;; procedure Append(Q : in out Fast_Food_Queue; Person : in Person_Name);	        ;;
;; procedure Remove_First(Q : in out Fast_Food_Queue; Person : out Person_Name);        ;;
;; function Cur_Count(Q : in Fast_Food_Queue) return Natural;			        ;;
;; function Max_Count(Q : in Fast_Food_Queue) return Natural;			        ;;
;; 										        ;;
;; ...										        ;;
;; 										        ;;
;; Cashier, Counter : Fast_Food_Queue;						        ;;
;; 										        ;;
;; ...										        ;;
;; -- Add George (see 3.10.1) to the cashier's queue:				        ;;
;; Append (Cashier, George);							        ;;
;; -- After payment, move George to the sandwich counter queue:			        ;;
;; Transfer (Cashier, Counter);							        ;;
;; ...										        ;;
;; 										        ;;
;; 										        ;;
;; Example of a task interface:							        ;;
;; 										        ;;
;; type Serial_Device is task interface;  -- see 9.1				        ;;
;; procedure Read (Dev : in Serial_Device; C : out Character) is abstract;	        ;;
;; procedure Write(Dev : in Serial_Device; C : in  Character) is abstract;	        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; access type										 ;;
;; 											 ;;
;; 											 ;;
;; Examples of access-to-object types: 							 ;;
;; 											 ;;
;; type Frame is access Matrix;    --  see 3.6						 ;;
;; type Peripheral_Ref is not null access Peripheral;  --  see 3.8.1			 ;;
;; type Binop_Ptr is access all Binary_Operation'Class;					 ;;
;;                                            -- general access-to-class-wide, see 3.9.1 ;;
;; 											 ;;
;; 											 ;;
;; Example of an access subtype: 							 ;;
;; 											 ;;
;; subtype Drum_Ref is Peripheral_Ref(Drum);  --  see 3.8.1				 ;;
;; 											 ;;
;; 											 ;;
;; Example of an access-to-subprogram type: 						 ;;
;; 											 ;;
;; type Message_Procedure is access procedure (M : in String := "Error!");		 ;;
;; procedure Default_Message_Procedure(M : in String);					 ;;
;; Give_Message : Message_Procedure := Default_Message_Procedure'Access;		 ;;
;; ...											 ;;
;; procedure Other_Procedure(M : in String);						 ;;
;; ...											 ;;
;; Give_Message := Other_Procedure'Access;						 ;;
;; ...											 ;;
;; Give_Message("File not found.");  -- call with parameter (.all is optional)		 ;;
;; Give_Message.all;                 -- call with no parameters				 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
