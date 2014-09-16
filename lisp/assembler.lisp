(in-package :ga144)

(defvar *current-program* nil)

(defstruct asmbler-output
  asm-out-prog
  asm-out-addr)

(defun put-insn (opcode)
  (let* ((dest (when (consp opcode)
		 (cadr opcode)))
	 (rawop (if (consp opcode)
		    (car opcode)
		    opcode))
	 (cslot (length (car (asm-out-prog *current-program*))))
					; In an empty program, expands
					; to (length (car nil)), or 0
	 (shortp (zerop (logand (f18a-opc-value rawop) 3))))
    (cond ((null (asm-out-prog *current-program*))
	   (push (list opcode) (asm-out-prog *current-program*)))
	  ((and (not shortp)
		(eq 3 (length (car (asm-out-prog *current-program*)))))
	   (push (list opcode)
		 (asm-out-prog *current-program*)))
	  ((cons? )))))
