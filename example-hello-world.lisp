(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-ada-generator))
#+nil
(ql:register-local-projects)

(in-package :cl-ada-generator)



(let ((code `(with-compilation-unit
		 (with Ada.Command_Line Ada.TextIO)
	       (procedure (Hello_World nil
				       ((decl ((You "constant String"
						    (if (< 0 Ada.Command_Line.Argument_Count)
							(string "世界")
							(call Ada.Command_Line.Argument 1)))
					       ))))
			  (call Ada.Text_IO.Put (string "Hello, "))
			  (call Ada.Text_IO.Put_Line You)
			  ))))
  (ensure-directories-exist #P"/dev/shm/hello_world/")
  (write-source #P"/dev/shm/hello_world/" "hello_world" "adb" code))
