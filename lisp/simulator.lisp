(in-package :ga144)

(defclass f18a ()
  ((mem :initform (make-array 128
			      :element-type 'f18a-word
			      :initial-element 0)
	:type (simple-array f18a-word 8))
   (stack :initform (make-array 8
				:element-type 'f18a-word
				:initial-element 0)
	  :type (simple-array f18a-word 8))
   (rstack :initform (make-array 8
				 :element-type 'f18a-word
				 :initial-element 0)
	   :type (simple-array f18a-word 8))
   (rstackp :initform 0 :type (integer 0 8))
   (stackp :initform 0 :type (integer 0 8))
   (rp :initform 0 :type (unsigned-byte 10))
   (rr :initform 0 :type f18a-word)
   (rs :initform 0 :type f18a-word)
   (rt :initform 0 :type f18a-word)
   (ra :initform 0 :type f18a-word)
   (rb :initform 0 :type f18a-addr)))

(defun f18a-pop (proc)
  (declare (f18a proc))
  (with-slots (rs rt stackp stack) proc
    (prog1 rt
      (psetf rt rs
	     rs (elt stack stackp)
	     stackp (mod (1- stackp) 8)))))

(defun f18a-push (proc val)
  (declare (f18a proc)
	   (f18a-word val))
  (with-slots (rs rt stackp stack) proc
    (setf stackp (mod (1+ stackp) 8)
	  (elt stack (mod (1+ stackp) 8)) rs
	  rs rt
	  rt val)))

(defun f18a-rpop (proc)
  (with-slots (rr rstack rstackp) proc
    (prog1 rr
      (psetf rr (elt rstack rstackp)
	     rstackp (mod (1- rstackp) 8)))))

(defun f18a-rpush (proc addr)
  (with-slots (rr rstack rstackp) proc
    (setf rstackp (mod (1- rstackp) 8)
	  (elt rstack rstackp) rr
	  rr addr)))


(defun f18a-io-read (proc addr)
  (declare (ignore proc addr))
  0)
(defun f18a-io-write (proc addr val)
  (declare (ignore proc addr))
  val)


(defun f18a-mem (proc addr)
  (cond ((<= 0 addr #x7F)
	 (elt (slot-value proc 'mem) (logand addr #x3F)))
	((<= #x80 addr #xFF)
	 (elt (slot-value proc 'mem) (+ #x40 (logand addr #x3F))))
	((<= #x100 addr)
	 (f18a-io-read proc addr))))

(defun f18a-mem-write (proc addr val)
  (cond ((<= 0 addr #x7F)
	 (setf (elt (slot-value proc 'mem)
		    (logand addr #x3F))
	       val))
	((<= #x80 addr #xFF) val)	; ROM: do not write.
	((<= #x100 addr)
	 (f18a-io-write proc addr val))))

(defsetf f18a-mem f18a-mem-write)


(declaim (inline f18a-incr f18a-decr))

(defun f18a-incr (addr)
  "Increment addr according to F18A rules"
  (if (< addr #x100)
      (dpb (1+ addr)
	   (byte 7 0)
	   addr)
      addr))			     ; IO addresses aren't incremented

(defun f18a-decr (addr)
  "Increment addr according to F18A rules"
  (if (< addr #x100)
      (dpb (1- addr)
	   (byte 7 0)
	   addr)
      addr))			     ; IO addresses aren't incremented


(defun perform-insn (proc insn)
  (symbol-macrolet  ((ra (slot-value proc 'ra))
		     (rb (slot-value proc 'rb))
		     (rp (slot-value proc 'rp))
		     (rs (slot-value proc 'rs))
		     (rt (slot-value proc 'rt))
		     (rr (slot-value proc 'rr))
		     (pop (f18a-pop proc))
		     (rpop (f18a-rpop proc)))
    (block nil
      (dolist (opc (f18a-disassemble-1 insn (slot-value proc 'rp)))
	(ecase (if (consp opc) (car opc) opc)
	  (ret (setf rp rpop)
		(return))
	  (ex (psetf rr rp
		      rp rr)
	       (return))
	  (jump (setf rp (cdr opc))
		 (return))
	  (call (f18a-rpush proc rp)
		 (setf rp (cdr opc))
		 (return))
	  (unext (unless (= rr 0)
		    (setf rp (f18a-decr rp) ; BUG: Fails when
					; executing from IO;
					; instruction should not
					; be fetched again.
			  rr (1- rr))
		    (return))
		  rpop)
	  (next (unless (= rr 0)
		   (setf rp (cdr opc)
			 rr (1- rr))
		   (return))
		 rpop)
	  (if (when (= rt 0)
		 (setf rp (cdr opc)))
	       (return))
	  (-if (unless (logbitp 17 rt)
		  (setf rp (cdr opc)))
		(return))
	  (@p (f18a-push proc (f18a-mem proc rp))
	       (setf rp (f18a-incr rp)))
	  (@+ (f18a-push proc (f18a-mem proc ra))
	       (setf ra (f18a-incr ra)))
	  (@b (f18a-push proc (f18a-mem proc rb)))
	  (@  (f18a-push proc (f18a-mem proc ra)))

	  (!p (setf (f18a-mem proc rp) pop
		     rp (f18a-incr rp)))
	  (!+ (setf (f18a-mem proc ra) pop
		     ra (f18a-incr ra)))
	  (!b (setf (f18a-mem proc rb) pop))
	  (!  (setf (f18a-mem proc ra) pop))

	  #+nil('+* TODO)
	  (2* (setf rt (ash 1 rt)))
	  (|2/| (setf rt (ash -1 rt)))
	  (~ (setf rt (logxor #x3FFFF rt)))
	  (+ (f18a-push proc (logand #x3FFFF (+ pop pop))))
	  (and (f18a-push proc (logand pop pop)))
	  (xor (f18a-push proc (logxor pop pop)))
	  (drop pop)
	  (dup (f18a-push proc rt))
	  (pop (f18a-push proc rpop))
	  (over (f18a-push proc rs))
	  (a (f18a-push proc ra))
	  (nop)
	  (push (f18a-rpush proc pop))
	  (b! (setf rb (ldb (byte 9 0) pop)))
	  (a! (setf ra pop))))
	
      )))
