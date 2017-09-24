(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-ada-generator))
(in-package :cl-ada-generator)

;; https://github.com/AdaCore/Compile_And_Prove_Demo/

(let* ((name "Saturate_angle")
       (dir-name (format nil "/dev/shm/~a/" (string-downcase name)))
       (saturate_ads `(with-compilation-unit
			  (raw "generic Low, High : Float;")
			(function ((Saturate ((Val Float)) :ret Float :cond ((post (in (attrib Saturate Result)
										       (dots Low High)))))))))
       (saturate_adb `(with-compilation-unit
			  (function ((Saturate ((Val Float))
					       :ret Float))
			   (cond ((in Val (dots Low High)) (return Val))
				 ((< Val Low)              (return Low))
				 (t                        (return High))))))
       (saturate_angle_ads `(with-compilation-unit
				(statements (with Saturate))
			      (generic-function Saturate_Angle (new (call Saturate (=> (Low 0.0) (High 360.0) ))))))
       (gpr `(project Main
		      (package Compiler
			       (for-use (call Default_Switches (string "Ada")) (comma-list (string "-gnatwa"))))
		      (package Prove
			       (for-use Switches (comma-list (string "--level=2")
							     (string "-j0"))))
		      (package Builder
			       (for-use Global_Configuration_Pragmas (string "main.adc")))))
       (adc `(with-compilation-unit
		 (pragma (call Profile GNAT_Extended_Ravenscar))
	       (pragma (call Partition_Elaboration_Policy Sequential))
	       (pragma (call SPARK_Mode On))
	       (pragma (call Warnings Off (string "no Global contract available")))
	       (pragma (call Warnings Off (string "subprogram * has no effect"))))))
  (ensure-directories-exist dir-name)
  (write-source dir-name "saturate" "ads" saturate_ads)
  (write-source dir-name "saturate_angle" "ads" saturate_angle_ads)
  (write-source dir-name "saturate" "adb" saturate_adb)
  (write-source dir-name "main" "gpr" gpr)
  (write-source dir-name "main" "adc" adc))

