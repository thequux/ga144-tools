(in-package :ga144-system)
(defpackage :ga144
  (:use :cl :cl-parsec :rutils)
  (:shadow xor)
  (:export f18a-opc-name f18a-opc-value
	   f18a-slot f18a-dest f18a-assemble-1
	   f18a-disassemble-1

					;opcodes
	   
	   ret ex jump call unext next if -if @p @+ @b @ !p !+ !b ! +*
	   2* |2/| ~ + and xor drop dup pop over a nop push b! a!  ))

(defpackage :ga144-ui
  (:use :clim-lisp :clim :ga144)
  (:shadowing-import-from ga144 xor))

(defpackage :ga144-user
  (:use :cl :ga144 :rutils)
  (:shadowing-import-from ga144 xor))

(in-package :ga144-user)

(defun rld-ga144 ()
  (asdf:load-system 'ga144 :verbose t))
