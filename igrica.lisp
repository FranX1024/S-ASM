
(rem "Opcenite utils funkcije")

(defun min (a b)
  (cond ((< a b) a)
	(t b)))

(defun max (a b)
  (cond ((< a b) b)
	(t a)))

(defun length (text)
  (let ((n 0))
    (loop while (aref-byte text n) do
	  (setf n (+ n 1)))))

(defun sleep (loops)
  (loop while loops do
	(setf loops (- loops 1))))

(defun random (seed-toggle)
  (let ((seed (make-array 1)))
    (cond (seed-toggle
	    (info "Prvi put kad se zove random"
		  "treba se pozvati s argumentom 1"
		  "tako da seed postavi na trenutno vrijeme"
		  "u stotinkama."
		  (setf (aref seed 0)
			(trap (D0 8)
			      D1 15)))))
    (info "Za generiranje novih random brojeva"
	  "koristi se Xor shift PRNG"
	  (setf (aref seed 0)
		(let ((s (aref seed 0)))
		  (setf s (logxor s (shl s 5)))
		  (setf s (logxor s (shr s 3)))
		  (setf s (logxor s (shl s 7))))))))

(defun set-rect (arr a b c d)
  (setf (aref arr 0) a)
  (setf (aref arr 1) b)
  (setf (aref arr 2) c)
  (setf (aref arr 3) d))

(rem "User input funkcije")

(defun keydown (keycode)
  (trap (D0 19)
	(D1 keycode)
	D1 15))

(rem "Funkcije za grafiku")

(defun double-buffering (toggle)
  (trap (D0 92)
	(D1 (+ 16 toggle))
	15))

(defun repaint-screen ()
  (trap (D0 94)
	15))

(defun draw-text (text row col)
  (trap (D0 11)
	(D1 (logior (shl col 8) row))
	15)
  (trap (D0 14)
	(A1 text)
	15))

(defun draw-number (num row col)
  (trap (D0 11)
	(D1 (logior (shl col 8) row))
	15)
  (trap (D0 3)
	(D1 num)
	15))

(defun set-color (color)
  (trap (D0 80)
	(D1 color)
	15)
  (trap (D0 81)
	(D1 color)
	15))

(defun draw-rect (x1 y1 x2 y2)
  (trap (D0 87)
	(D1 x1) (D2 y1)
	(D3 (- x2 1)) (D4 (- y2 1))
	15))

(defun draw-text-centered (text row)
  (draw-text text row (shr (- 80 (length text)) 1)))

(rem "Mehanika igre"
     "Palice su dimenzija 10x60")

(defun handle-collision (rx1 ry1 rx2 ry2 bx1 by1 bx2 by2 bvx bvy)
  (let ((colx1 (max rx1 (+ bx1 bvx)))
	(coly1 (max ry1 (+ by1 bvy)))
	(colx2 (min rx2 (+ bx2 bvx)))
	(coly2 (min ry2 (+ by2 bvy)))
	(new-vel (info "handle-collision vraca nove vrijednosti za"
		       "komponente vektora kretanja loptice (bvx i bvy),"
		       "u slucaju da se desio collision. To radi na nacin"
		       "da ili vraca adresu niza od dva clana ili vraca 0."
		       (make-array 2))))

    (cond ((and (< colx1 colx2) (< coly1 coly2))
	   (progn
	     (info "Kako se lopta uvijek krece pod kutem"
		   "(2k-1)pi/2, ako je jedna stranica pravokutnika"
		   "presjeka dulja od druge, mozemo zakljuciti da"
		   "je na tu stranu loptica lupila"
		   (cond ((> (- colx2 colx1) (- coly2 coly1))
			  (setf bvy (- bvy)))
			 (t (setf bvx (- bvx)))))
	     (setf (aref new-vel 0) bvx)
	     (setf (aref new-vel 1) bvy)
	     new-vel))
	  (t 0))))

(defun draw-palice (leftp rightp)
  (draw-rect 20 leftp 30 (+ leftp 60))
  (draw-rect 155 rightp 165 (+ rightp 60))
  (draw-rect 610 rightp 620 (+ rightp 60))
  (draw-rect 475 leftp 485 (+ leftp 60)))

(defun declare-winner (name)
  (set-color 0)
  (double-buffering nil)
  (draw-text-centered "@==========================@" 10)
  (draw-text-centered "|   Pobjednik igre je...   |" 11)
  (draw-text-centered "|                          |" 12)
  (draw-text-centered "|                          |" 13)
  (draw-text-centered "|                          |" 14)
  (draw-text-centered "|--------------------------|" 15)
  (draw-text-centered "| Kliknite B za pocetni    |" 16)
  (draw-text-centered "| ekran...                 |" 17)
  (draw-text-centered "@==========================@" 18)
  (draw-text-centered name 13)
  (loop while (not (keydown #\B))))

(defun play-game ()
  (double-buffering t)
  (let ((ball-speed 4)
	(ballvx (* (- (logand (shr (random t) 1) 2) 1) ball-speed))
	(ballvy (* (- (logand (shr (random nil) 1) 2) 1) ball-speed))
	(left-points 0)
	(right-points 0)
	(ballx 310) (bally 230)
	(leftp 210) (rightp 210)

	(walls (info "Prvih 6 elemenata niza su"
		     "zidovi / rubovi prostora igre,"
		     "a preostala 4 su rezevriana za palice,"
		     "da bi kod za collision bio jednostavniji."

		     (array (array 5 5 635 15)
			    (array 5 465 635 475)
			    (array 5 15 15 120)
			    (array 5 360 15 475)
			    (array 625 15 635 120)
			    (array 625 360 635 475)
			    (make-array 4)
			    (make-array 4)
			    (make-array 4)
			    (make-array 4)))))
    (set-color 0)
    (draw-rect 0 0 640 480)
    (set-color #x00ffffff)

    (info "Crtanje zidova"
	  (let ((i 0))
	    (loop while (< i 6) do
		  (let ((wall (aref walls i)))
		    (draw-rect (aref wall 0) (aref wall 1)
			       (aref wall 2) (aref wall 3)))
		  (setf i (+ i 1)))))

    (info "Crta posredini"
	  (draw-rect 315 5 325 475))

    (draw-palice leftp rightp)
    (repaint-screen)

    (loop while (not (or (keydown #\B)
			 (eq (max left-points right-points) 5)))
	  do
	  (set-color 0)
	  (draw-rect ballx bally (+ ballx 20) (+ bally 20))
	  (draw-palice leftp rightp)
	  (cond ((keydown #\Q) (setf leftp (max 15 (- leftp    ball-speed)))))
	  (cond ((keydown #\A) (setf leftp (min 405 (+ leftp   ball-speed)))))
	  (cond ((keydown #\O) (setf rightp (max 15 (- rightp  ball-speed)))))
	  (cond ((keydown #\L) (setf rightp (min 405 (+ rightp ball-speed)))))

	  (draw-number left-points 2 37)
	  (draw-number right-points 2 42)

	  (set-color #x00ffffff)
	  (draw-palice leftp rightp)

	  (info "Crtanje zidova"
		(let ((i 0))
		  (loop while (< i 6) do
			(let ((wall (aref walls i)))
			  (draw-rect (aref wall 0) (aref wall 1)
				     (aref wall 2) (aref wall 3)))
			(setf i (+ i 1)))))

	  (info "Postavi zadnja 4 elementa niza"
		"walls na parametre palica za collision handling"
		(progn
		  (set-rect (aref walls 6) 20 leftp 30 (+ leftp 60))
		  (set-rect (aref walls 7) 155 rightp 165 (+ rightp 60))
		  (set-rect (aref walls 8) 610 rightp 620 (+ rightp 60))
		  (set-rect (aref walls 9) 475 leftp 485 (+ leftp 60))))

	  (let ((i 0)
		(collided nil))

	    (info "Collision handling (velocity update)"
		  (loop while (< i 10) do
			(let ((wall (aref walls i))
			      (new-velocity (handle-collision
					      (aref wall 0) (aref wall 1)
					      (aref wall 2) (aref wall 3)
					      ballx bally (+ ballx 20) (+ bally 20)
					      ballvx ballvy)))
			  (cond
			    (new-velocity (progn (setf collided t)
						 (setf ballvx (aref new-velocity 0))
						 (setf ballvy (aref new-velocity 1))))))
			(setf i (+ i 1))))

	    (info "Update pozicije loptice te"
		  "po potrebi promijeni boju ako"
		  "se desila kolizija"
		  (progn
		    (setf ballx (+ ballx ballvx))
		    (setf bally (+ bally ballvy))
		    (cond (collided (set-color #x00ff0000)))
		    (draw-rect ballx bally (+ ballx 20) (+ bally 20))
		    (cond (collided (set-color #x00ffffff))))))

	  (info "Linija posred"
		(draw-rect 315 5 325 475))

	  (info
	    "U slucaju pogodka, povecaj bodove"
	    "jednog od igraca te po potrebi prikazi"
	    "declare-winner ekran."
	    (cond ((or (<= ballx 0) (>= ballx 620))
		 (progn
		   (cond ((<= ballx 0) (setf right-points (+ right-points 1)))
			 (t            (setf left-points  (+ left-points  1))))

		   (set-color 0)
		   (draw-number left-points 2 37)
		   (draw-number right-points 2 42)
		   (set-color #x00ffffff)

		   (cond ((eq (max left-points right-points) 5)
			  (declare-winner (cond ((eq left-points 5) "LIJEVI IGRAC")
						(t "DESNI IGRAC"))))
			 (t (progn
			      (set-color 0)
			      (draw-rect ballx bally (+ ballx 20) (+ bally 20))
			      (setf ballx 310)
			      (setf bally 230)
			      (setf ball-speed (+ ball-speed 1))
			      (setf ballvx (* (- (logand (shl (random nil) 1) 2) 1) ball-speed))
			      (setf ballvy (* (- (logand (shl (random nil) 1) 2) 1) ball-speed)))))))))

	  (repaint-screen))))

(rem "Main screen funkcija")

(defun main ()
  (loop while t do
	(double-buffering nil)
	(set-color 0)
	(draw-rect 0 0 640 480)

	(draw-text-centered "ddb   db  .d88b.  db   dD d88888b    d88b" 4)
	(draw-text-centered "d88   88 .8P  Y8. 88 ,8P* 88*        *8P*" 5)
	(draw-text-centered "d88ooo88 88    88 88,8P   88ooooo     88 " 6)
	(draw-text-centered "d88~~~88 88    88 88*8b   88~~~~~     88 " 7)
	(draw-text-centered "d88   88 *8b  d8* 88 *88. 88.     db. 88 " 8)
	(draw-text-centered "dYP   YP  *Y88P*  YP   YD Y88888P Y8888P " 9)

	(draw-text-centered "Kontrole lijevog: Q i A  " 11)
	(draw-text-centered "Kontrole desnog:  O i L  " 12)
	(draw-text-centered "Dodji prvi do 5 pogodaka." 13)

	(draw-text-centered "Klikni S za start..." 20)

	(loop while (not (keydown #\S)))
	(play-game)))
