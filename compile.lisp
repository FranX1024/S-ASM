
(defun make-context ()
  (list (cons 'literals nil)
	(cons 'label-counter 0)
	(cons 'literal-counter 0)
	(cons 'names nil)))

(defun gen-label (context info)
  (concatenate
    'string
    (or info "LABEL")
    "_"
    (let ((p (assoc 'label-counter context)))
      (write-to-string (setf (cdr p) (1+ (cdr p)))))))

(defun make-literal (text context)
  (let* ((p (assoc 'literal-counter context))
	 (lab (concatenate
		'string
		"LITERAL_"
		(write-to-string (setf (cdr p) (1+ (cdr p)))))))
    (push (cons lab text) (cdr (assoc 'literals context)))
    lab))

(defun m68-case (sym)
  (intern
    (coerce
      (loop for chr in (coerce (symbol-name sym) 'list)
	  collect (cond ((char= chr #\-) #\_)
			(t chr)))
      'string)))

(defun m68-code (&rest lines)
  (apply #'concatenate
	 (cons 'string
	       (mapcar (lambda (line) (format nil "~A~%" line))
		       lines))))

(defun iterize (code)
  (lambda (args context a7)
    (apply #'concatenate
	   (cons 'string
		 (loop with first-arg = t
		       for arg in args
		       collect (if first-arg
				 (progn
				   (setf first-arg nil)
				   (m68-compile arg context a7))
				 (concatenate
				   'string
				   (m68-compile arg context (1- a7))
				   code)))))))

(defvar *m68-rules*
  (list
    (cons 'logand (iterize
		    (m68-code
		      "	MOVE.L (A7)+, D0"
		      "	AND.L D0, (A7)")))
    (cons 'logior (iterize
		    (m68-code
		      "	MOVE.L (A7)+, D0"
		      "	OR.L D0, (A7)")))
    (cons 'logxor (iterize
		    (m68-code
		      "	MOVE.L (A7)+, D0"
		      "	EOR.L D0, (A7)")))
    (cons '+ (iterize
	       (m68-code
		 "	MOVE.L (A7)+, D0"
		 "	ADD.L D0, (A7)")))

    (cons '- (lambda (args context a7)
	       (cond
		 ((eq (length args) 1)
		  (concatenate
		    'string
		    (m68-compile (car args) context a7)
		    (m68-code
		      "	MOVE.L (A7), D0"
		      "	NEG.L D0"
		      "	MOVE.L D0, (A7)")))
		 (t
		   (funcall
		     (iterize
		       (m68-code
			 "	MOVE.L (A7)+, D0"
			 "	SUB.L D0, (A7)"))
		     args context a7)))))
    (cons '* (iterize
	       ; [D0 D1] [D2 D3]
	       (m68-code
		 "	BSR IZMNOZI")))
    (cons '/ (lambda (args context a7)
	       (concatenate
		 'string
		 (m68-compile (nth 0 args) context a7)
		 (m68-compile (nth 1 args) context (1- a7))
		 (m68-code
		   "	MOVE.L (A7)+, D1"
		   "	MOVE.L (A7), D0"
		   "	DIVS D1, D0"
		   "	AND.L #$FFFF, D0"
		   "	MOVE.L D0, (A7)"))))

    (cons 'mod (lambda (args context a7)
		 (concatenate
		   'string
		   (m68-compile (nth 0 args) context a7)
		   (m68-compile (nth 1 args) context (1- a7))
		   (m68-code
		     "	MOVE.L (A7)+, D1"
		     "	MOVE.L (A7), D0"
		     "	DIVS D1, D0"
		     "	LSR.L #8, D0"
		     "	LSR.L #8, D0"
		     "	MOVE.L D0, (A7)"))))

    (cons 'shl (lambda (args context a7)
		 (concatenate
		   'string
		   (m68-compile (nth 0 args) context a7)
		   (m68-compile (nth 1 args) context (1- a7))
		   (m68-code
		     "	MOVE.L (A7)+, D1"
		     "	MOVE.L (A7), D0"
		     "	LSL.L D1, D0"
		     "	MOVE.L D0, (A7)"))))

    (cons 'shr (lambda (args context a7)
		 (concatenate
		   'string
		   (m68-compile (nth 0 args) context a7)
		   (m68-compile (nth 1 args) context (1- a7))
		   (m68-code
		     "	MOVE.L (A7)+, D1"
		     "	MOVE.L (A7), D0"
		     "	LSR.L D1, D0"
		     "	MOVE.L D0, (A7)"))))

    (cons 'and (lambda (args context a7)
		 (let ((end-label (gen-label context "KRAJ_I")))
		   (apply #'concatenate
			  (concatenate
			    'list
			    '(string)
			    (list
			      (m68-code "	MOVE.L #0, -(A7)"))
			    (loop for arg in args
				  collect
				  (concatenate
				    'string
				    (m68-compile arg context (1- a7))
				    (m68-code
				      "	TST.L (A7)+"
				      (format nil
					      "	BEQ ~A" end-label))))
			    (list
			      (m68-code "	MOVE.L #1, (A7)"
					end-label)))))))

    (cons 'or (lambda (args context a7)
		(let ((end-label (gen-label context "KRAJ_ILI")))
		  (apply #'concatenate
			 (concatenate
			   'list
			   '(string)
			   (list
			     (m68-code "	MOVE.L #1, -(A7)"))
			   (loop for arg in args
				 collect
				 (concatenate
				   'string
				   (m68-compile arg context (1- a7))
				   (m68-code
				     "	TST.L (A7)+"
				     (format nil
					     "	BNE ~A" end-label))))
			   (list
			     (m68-code "	MOVE.L #0, (A7)"
				       end-label)))))))

    (cons 'not (lambda (args context a7)
		 (concatenate
		   'string
		   (m68-compile (car args) context a7)
		   (m68-code
		     "	CLR.L D0"
		     "	TST.L (A7)"
		     "	SEQ D0"
		     "	MOVE.L D0, (A7)"))))

    (cons 'eq (lambda (args context a7)
		(concatenate
		  'string
		  (m68-compile (nth 0 args) context a7)
		  (m68-compile (nth 1 args) context (1- a7))
		  (m68-code
		    "	MOVE.L (A7)+, D0"
		    "	CLR.L D1"
		    "	CMP.L (A7), D0"
		    "	SEQ D1"
		    "	MOVE.L D1, (A7)"))))

    (cons 'let (lambda (args context a7)
		 (let ((total-offset (length (car args))))
		   (if (or (null (cdr args)) (null (car args)))
		     (error "(m68) Empty let expression in code!"))
		   (concatenate
		     'string
		     (apply #'concatenate
			    (cons 'string
				  (loop with offset = 0
					for par in (car args)
					collect (format nil  "	; var ~A~%" (car par))
					collect (m68-compile (nth 1 par) context (- a7 offset))
					do (push (cons (car par)
						       (- a7 (setf offset (1+ offset))))
						 (cdr (assoc 'names context))))))
		     (format nil "	; ------~%")
		     (apply #'concatenate
			    (cons 'string
				  (loop for argc on (cdr args)
					collect (let ((ccode (m68-compile (car argc) context (- a7 total-offset))))
						  (if (null (cdr argc))
						    ccode
						    (concatenate 'string ccode
								 (m68-code "	TST.L (A7)+")))))))
		     (let ((p (assoc 'names context)))
		       (progn
			 (loop for n from 1 to total-offset
			       do (setf (cdr p) (cdr (cdr p))))
			 (m68-code
			   (format nil "	ADD.L #~d, A7" (* 4 total-offset))
			   (format nil "	MOVE.L ~d(A7), (A7)" (* 4 (- total-offset))))))))))

    (cons 'progn (lambda (args context a7)
		   (apply #'concatenate
			  (cons 'string
				(loop for argcons on args
				      collect (if (null (cdr argcons))
						(m68-compile (car argcons) context a7)
						(concatenate
						  'string
						  (m68-compile (car argcons) context a7)
						  (m68-code
						    "	TST.L (A7)+"))))))))

    (cons 'cond (lambda (args context a7)
		  (let ((endlab (gen-label context "ZADNJI")))
		    (concatenate
		      'string
		      (apply #'concatenate
			     (cons 'string
				   (loop for paircons on args
					 collect
					 (let ((nextlab (gen-label context "SLJEDECI"))
					       (pair (car paircons)))
					   (concatenate
					     'string
					     (m68-compile (nth 0 pair) context a7)
					     (m68-code
					       "	TST.L (A7)+"
					       (format nil
						       "	BEQ ~A" nextlab))
					     (m68-compile (nth 1 pair) context a7)
					     (format nil
						     "	BRA ~A~%" endlab)
					     (m68-code nextlab))))))
		      (m68-code
			"	MOVE.L #0, -(A7)"
			endlab)))))

    (cons 'loop (lambda (args context a7)
		   (let ((endlab (gen-label context "LOOP_END"))
			 (condlab (gen-label context "LOOP_COND")))
		     (if (not (eq (car args) 'while))
		       (error "(m68) Only while loops available currently!"))
		     (concatenate
		       'string
		       (m68-code
			 "	MOVE.L #0, -(A7)"
			 condlab)
		       (m68-compile (nth 1 args) context (1- a7))
		       (m68-code
			 "	TST.L (A7)+"
			 (format nil
				 "	BEQ ~A" endlab))
		       (apply #'concatenate
			      (cons 'string
				    (loop for arg in (nthcdr 3 args)
					  collect
					  (concatenate
					    'string
					    (m68-code
					      "	TST.L (A7)+")
					    (m68-compile arg context a7)))))
		       (m68-code
			 (format nil
				 "	BRA ~A" condlab)
			 endlab)))))

    (cons 'setf (lambda (args context a7)
		  (cond
		    ((symbolp (car args))
		     (let ((p (assoc (car args)
				     (cdr (assoc 'names context)))))
		       (if (null p)
			 (error "(m68) setq received unbound symbol!"))

		       (concatenate
			 'string
			 (m68-compile (nth 1 args) context a7)
			 (format nil
				 "	MOVE.L (A7), ~A(A7) ; ~A~%"
				 (let ((ofs (- (cdr p) (1- a7))))
				   (if (eq ofs 0)
				     ""
				     (write-to-string (* 4 ofs))))
				 (car p)))))

		    ((and (consp (car args)) (eq (car (car args)) 'aref))
		     (concatenate
		       'string
		       (m68-compile (nth 1 (car args)) context a7)
		       (m68-compile (nth 2 (car args)) context (1- a7))
		       (m68-compile (nth 1 args) context (- a7 2))
		       (m68-code
			 "	MOVE.L (A7)+, D0"
			 "	MOVE.L (A7)+, D1"
			 "	MOVE.L (A7), A0"
			 "	LSL.L #2, D1"
			 "	ADD.L D1, A0"
			 "	MOVE.L D0, (A7)"
			 "	MOVE.L D0, (A0)")))

		    ((and (consp (car args)) (eq (car (car args)) 'aref-byte))
		     (concatenate
		       'string
		       (m68-compile (nth 1 (car args)) context a7)
		       (m68-compile (nth 2 (car args)) context (1- a7))
		       (m68-compile (nth 1 args) context (- a7 2))
		       (m68-code
			 "	MOVE.L (A7)+, D0"
			 "	MOVE.L (A7)+, A1"
			 "	MOVE.L (A7), A0"
			 "	ADD.L A1, A0"
			 "	AND.L #$FF, D0"
			 "	MOVE.L D0, (A7)"
			 "	MOVE.B D0, (A0)")))

		    (t (error (format nil "(m68) Improper argument for setf ~A" (car args)))))))

    (cons 'aref (lambda (args context a7)
		  (concatenate
		    'string
		    (m68-compile (nth 0 args) context a7)
		    (m68-compile (nth 1 args) context (1- a7))
		    (m68-code
		      "	MOVE.L (A7)+, D1"
		      "	MOVE.L (A7), A0"
		      "	LSL.L #2, D1"
		      "	ADD.L D1, A0"
		      "	MOVE.L (A0), (A7)"))))

    (cons 'aref-byte (lambda (args context a7)
		       (concatenate
			 'string
			 (m68-compile (nth 0 args) context a7)
			 (m68-compile (nth 1 args) context (1- a7))
			 (m68-code
			   "	MOVE.L (A7)+, A1"
			   "	MOVE.L (A7), A0"
			   "	ADD.L A1, A0"
			   "	CLR.L D0"
			   "	MOVE.B (A0), D0"
			   "	MOVE.L D0, (A7)"))))

    (cons 'trap (lambda (args context a7)
		  (concatenate
		    'string
		    (apply #'concatenate
			   (cons 'string
				 (loop with offset = 0
				       for arg in args
				       while (consp arg)
				       collect (m68-compile (nth 1 arg) context (- a7 offset))
				       do (setq offset (1+ offset)))))
		    (apply #'concatenate
			   (cons 'string
				 (loop for arg in (reverse args)
				       when (consp arg)
				       collect (format nil "	MOVE.L (A7)+, ~A~%" (car arg)))))
		    (format nil
			    "	TRAP #~d~%"
			    (loop for arg in args
				  while (not (numberp arg))
				  finally (return arg)))

		    (let ((outreg (loop for arg in args
					while (not (symbolp arg))
					finally (return arg))))
		      (if (not (symbolp outreg))
			(m68-code
			  "	CLR.L -(A7)")
			(format nil
				"	MOVE.L ~A, -(A7)~%" outreg))))))

    (cons 'array (lambda (args context a7)
		   (let ((lab (make-literal (length args) context)))
		     (concatenate
		       'string
		       (apply #'concatenate
			      (cons 'string
				    (loop with offs = 0
					  for arg in args
					  collect (m68-compile arg context a7)
					  collect (format
						    nil "	MOVE.L (A7)+, D0~%	MOVE.L D0, ~A+~d~%" lab (* 4 offs))
					  do (setf offs (1+ offs)))))
		       (format
			 nil "	MOVE.L #~A, -(A7)~%" lab)))))

    (cons 'make-array (lambda (args context a7)
			(let ((lab (make-literal (car args) context)))
			  (format nil
				  "	MOVE.L #~A, -(A7)~%" lab))))

    (cons 'info (lambda (args context a7)
		  (concatenate
		    'string
		    (apply #'concatenate
			   (cons 'string
				 (mapcar (lambda (c) (format nil "	; ~A~%" c))
					 (nreverse (cdr (reverse args))))))
		    (m68-compile (car (reverse args)) context a7))))))

(loop for par1 in
      '((<  . "LT")
	(>  . "GT")
	(<= . "LE")
	(>= . "GE"))
      do (let ((par par1))
	   (push (cons (car par)
		       (lambda (args context a7)
			 (concatenate
			   'string
			   (m68-compile (nth 0 args) context a7)
			   (m68-compile (nth 1 args) context (1- a7))
			   (m68-code
			     "	MOVE.L (A7)+, D0"
			     "	MOVE.L (A7), D1"
			     "	CLR.L D2"
			     "	CMP.L D0, D1"
			     (format nil "	S~A D2" (cdr par))
			     "	MOVE.L D2, (A7)"))))
		 *m68-rules*)))

(defun m68-compile (tree context a7)
  (cond
    ((eq tree t) (m68-code "	MOVE.L #1, -(A7)"))
    ((eq tree nil) (m68-code "	MOVE.L #0, -(A7)"))
    ((consp tree)
     (let ((p (assoc (car tree) *m68-rules*)))
       (if p
	 (funcall (cdr p) (cdr tree) context a7)
	 (concatenate
	   'string
	   (apply #'concatenate
		  (cons 'string
			(loop with offset = 0
			      for arg in (cdr tree)
			      collect (m68-compile arg context (- a7 offset))
			      do (setf offset (1+ offset)))))
	   (format nil
		   "	BSR ~A~%"
		   (m68-case (car tree)))))))
    ((symbolp tree)
     (let ((p (assoc tree (cdr (assoc 'names context)))))
       (if p
	 (m68-code
	   (let ((offs (* 4 (- (cdr p) a7))))
	     (if (eq offs 0)
	       (format nil "	MOVE.L (A7), D0 ; ~A" (car p))
	       (format nil "	MOVE.L ~d(A7), D0 ; ~A" offs (car p))))
	   "	MOVE.L D0, -(A7)")
	 (error (format nil "(m68) No value associated with symbol ~A, context: ~A" tree context)))))
    ((characterp tree)
     (format nil "	MOVE.L #~d, -(A7)~%" (char-code tree)))
    ((integerp tree)
     (format nil "	MOVE.L #~d, -(A7)~%" tree))
    ((stringp tree)
     (format nil "	MOVE.L #~A, -(A7)~%" (make-literal tree context)))
    (t (error (format nil "(m68) Bad type! ~A" tree)))))

(defvar implementacija-izmnozi
  (m68-code
    ; [D0 D1] [D2 D3]
    ""
    "IZMNOZI"
    "	CLR.L D4"
    "	CLR.L D2"
    "	CLR.L D0"
    "	MOVEM.W 4(A7), D0-D3"
    "	MOVE.W D3, D4"
    "	MULU D1, D4"
    "	MULU D1, D2"
    "	MULU D3, D0"
    "	ADD.L D0, D2"
    "	LSL.L #8, D2"
    "	LSL.L #8, D2"
    "	ADD.L D2, D4"
    "	MOVE.L (A7), 4(A7)"
    "	MOVE.L D4, 8(A7)"
    "	ADD.L #4, A7"
    "	RTS"))

(defun m68-compile-funcs (code)
  (let ((context (make-context)))
    (concatenate
      'string
      (m68-code
	"	ORG $3000")
      implementacija-izmnozi
      (apply #'concatenate
	     (cons 'string
		   (loop for func in code
			 collect
			 (if (eq (car func) 'rem)
			   (apply #'concatenate
				  (cons 'string
					(cons
					  (format nil "~%")
					  (loop for row in (cdr func)
						collect (concatenate
							  'string
							  "; " row (format nil "~%"))))))
			   (progn
			     (if (not (eq (car func) 'defun))
			       (error "(m68) global scope must only contain functions and comments!"))
			     (let ((fname (nth 1 func))
				   (fargs (nth 2 func))
				   (fargc (length (nth 2 func))))

			       (loop with offset = 0
				     for arg in (reverse fargs)
				     do (push (cons arg (setf offset (1+ offset)))
					      (cdr (assoc 'names context))))
			       (concatenate
				 'string
				 (m68-code "" (format nil "; ~A ~A" fname (if (null fargs) "()" fargs)) (m68-case fname))
				 (let ((asm68 (m68-compile (cons 'progn (nthcdr 3 func)) context 0))
				       (p (assoc 'names context)))
				   (setf (cdr p) (nthcdr fargc (cdr p)))
				   asm68)
				 (format nil
					 (concatenate
					   'string
					   "	MOVE.L (A7), D0~%"
					   "	MOVE.L 4(A7), D1~%"
					   "	MOVE.L D0, ~d(A7)~%"
					   "~A~A"
					   "	RTS~%")
					 (* 4 (1+ fargc))
					 (if (eq fargc 1) ""
					   (format nil
						   "	MOVE.L D1, ~d(A7)~%"
						   (* 4 fargc)))
					 (if (eq fargc 0) ""
					   (format nil
						   "	ADD.L #~d, A7~%"
						   (* 4 fargc)))))))))))
      (m68-code
	""
	"START"
	"	BSR MAIN"
	"	MOVE #9, D0"
	"	TRAP #15"
	"")
      (apply #'concatenate
	     (cons 'string
		   (loop for kvp in (reverse (cdr (assoc 'literals context)))
			 collect
			 (cond
			   ((stringp (cdr kvp))
			    (format nil "~A	DC.B '~A',0~%" (car kvp) (cdr kvp)))
			   (t (format nil "~A	DS.L ~d~%" (car kvp) (cdr kvp)))))))
      (m68-code
	""
	"	END START"))))

(defun m68-compile-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((forms '()))
      (loop
	(handler-case
	  (let ((form (read stream nil)))
	    (if form
	      (push form forms)
	      (return (m68-compile-funcs (nreverse forms))))))))))

(format t "~A" (m68-compile-file (nth 1 sb-ext:*posix-argv*)))
