;; for convenience, all 18-bit words are actually 20-bit words, with the bottom two bits unused.

(defvar opcode-aliases
  (let ((ht (make-hash-table)))
    (mapc (lambda (kv)
	    (setf (gethash (car kv) ht) (cadr kv)))
	  '((|;| ret)
	    (|.| nop)))
    ht))

(defun f18a-opc-name (val)
  (if (symbolp val)
      val
    (elt '#(ret ex jump call
		unext next if -if
		@p @+ @b @
		!p !+ !b !
		+* 2* 2/ ~
		+ and xor drop
		dup pop over a
		nop push b! a!)
	 val)))

(defvar f18a-opc-values
  (let ((ht (make-hash-table)))
    (dotimes (i 32)
       (setf (gethash (f18a-opc-name i) ht)
	     i))
    ht))

(defun f18a-opc-value (val)
  (if (numberp val)
      val
   (multiple-value-bind (res found)
       (gethash val opcode-aliases)
     (if found
	 (f18a-opc-value res)
       (multiple-value-bind (res found)
	   (gethash val f18a-opc-values)
	 (unless found
	   (error "Undefined opcode ~a" res))
	 res)))))

(defun f18a-slot (insn slot)
  (let ((insn (logxor insn #x15555)))
    (if (eq slot 3)
	(ash (ldb (byte 3 0) insn) 2)
      (ldb (byte 5 (- (* 5 (- 3 slot)) 2))
	   insn))))

(define-setf-expander f18a-slot (insn slot &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion insn env)
    (let ((store (gensym))
	  (slotv (gensym))
	  (masked (gensym))
	  (stemp (car newval)))
      (values (list* slotv dummies)
	      (list* slot vals)
	      (list store)
	      `(let* ((,masked (logxor ,store
				       (ldb (byte 5 (* 5 (- 3 ,slotv)))
					; #x15555 << 2 (bottom bit is x)
					    #x55555)))
		      (,stemp (if (eq ,slotv 3)
				  (dpb (ash ,masked -2)
				       (byte 3 0)
				       ,getter)
				(dpb ,masked
				     (byte 5 (- (* 5 (- 3 ,slotv)) 2))
				     ,getter))))
		 ,setter
		 ,store)
	      `(f18a-slot ,getter ,slotv)))))

(defun f18a-dest-byte (slot)
  (if (eq slot 3)
      (error "Slot 3 has no destination field")
    (byte (min 10 (- (* 5 (- 3 slot)) 2))
	  0)))

(defun f18a-dest (insn slot)
  (ldb (f18a-dest-byte slot) insn))

(define-setf-expander f18a-dest (insn slot &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion insn env)
    (let ((store (gensym))
	  (slotv (gensym))
	  (stemp (car newval)))
      (values (list* slotv dummies)
	      (list* slot vals)
	      (list store)
	      `(if (eq ,slotv 3)
		   (error "Slot 3 has no destination field")
		 (let* ((,stemp (dpb ,store
				     (f18a-dest-byte ,slotv)
				     ,getter)))
		   ,setter
		   ,store))
	      `(f18a-dest ,getter ,slotv)))))

(defun f18a-opc-has-dest (opcode)
  (when (symbolp opcode)
    (setf opcode (f18a-opc-value opcode)))
  (and (member opcode '(2 3 5 6 7)) t))

(defun f18a-assemble-1 (vals &optional symtab)
  ;; TODO: Range-check dest arguments
  (flet ((resolve-ref (sym)
		      (if (and (symbolp sym) symtab)
			  (multiple-value-bind (val found)
			      (gethash sym symtab)
			    (unless found
			      (error "Undefined symbol ~w" sym))
			    val)
			(if (numberp sym) sym 0))))
    (let ((insn #x15555))
      (mapc (lambda (slot val)
	      (when val
		(if (not (consp val))
		    (setf (f18a-slot insn slot)
			  (f18a-opc-value val))
		  (setf (f18a-slot insn slot)
			(f18a-opc-value (car val))
			(f18a-dest insn slot)
			(resolve-ref (cadr val))))))
	    '(0 1 2 3)
	    vals)
      insn)))

(defun dump-ram (memory &optional dumpp)
  (unless dumpp
    (setf dumpp (constantly nil)))
  (let ((skippedp t)
	(result nil))
    (dotimes (addr 256)
      (if (not (funcall dumpp addr))
	  (setf skippedp t)
	(progn
	  (when skippedp
	   (push (list addr) result)
	   (setf skippedp nil))
	  (push (elt memory addr) (car result)))))
    (nreverse (mapcar #'nreverse result))))

(defun f18a-assemble (prgm)
  (let ((memory (make-array 512))
	(touched-mem (make-array 512 :initial-element nil))
	(symtab (make-hash-table))
	(pass 1))
    (labels ((addr+ (addr)
		    (dpb (1+ addr)
			 (byte 7 0)
			 addr))
	     (proc (addr prgm)
		   (cond ((null prgm) t)
			 ((consp (car prgm))
			  (setf (elt memory addr)
				(f18a-assemble-1 (car prgm)
						 (when (> pass 1) symtab))
				(elt touched-mem addr) t)
			  (proc (addr+ addr) (cdr prgm)))
			 ((numberp (car prgm))
			  (setf (elt memory addr) (car prgm)
				(elt touched-mem addr) t)
			  (proc (addr+ addr) (cdr prgm)))
			 ((not (keywordp (car prgm)))
			  (error "Invalid program element ~p" (car prgm)))
			 ;; :at <addr>
			 ((eq :at (car prgm))
			  (proc (cadr prgm)
				(cddr prgm)))
			 ;; :label <sym>
			 ((eq :label (car prgm))
			  (setf (gethash (cadr prgm) symtab)
				addr)
			  (proc addr (cddr prgm))))))
      (proc 0 prgm)
      (incf pass)
      (proc 0 prgm)
      
      (dump-ram memory (lambda (x)
			 (elt touched-mem x))))))

(defun f18a-disassemble (insn &optional (addr 0))
  (labels ((dis (slot)
		(when (< slot 4)
		  (let ((opc (f18a-opc-name (f18a-slot insn slot))))
		    (if (f18a-opc-has-dest opc)
			`((,opc ,(dpb (f18a-dest insn slot)
				      (f18a-dest-byte slot)
				      addr)))
		      (cons opc (dis (1+ slot))))))))
    (dis 0)))

;;;;;; TEST CODE
(defvar __MATH_ROM
  (cdr '(foo :at #xa1
	     :label relay
	     (pop a! @+ nop)
	     (push @+ (next #xA4))
	     (drop (jump done))
	     (pop over push @p)
	     (a (call relay))
	     (!b !b !b nop)
	     (@+ !b unext nop)
	     :label done
	     (a push a! ret)
	     :label warm
	     ((jump #x195))

	     :at #xB0
	     :label *.17
	     (a! @p push dup)
	     #x10
	     (dup xor nop nop)
	     (+* unext ~ +*)
	     (a (-if #xB6))
	     (drop ~ 2* ret)
	     (drop 2* ~ ret)

	     :label *.
	     ((call *.17))
	     (a 2* (-if #xBB))
	     (drop ~ 2* nop)
	     (~ ret)
	     (drop 2* ret))))


