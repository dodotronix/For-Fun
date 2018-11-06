; TETRIS

;size of field [10x14 (char)]
(defun gamefield ()
    (list (list "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-~%")
	  (list "|" " " " " " " " " " " " " " " " " " " " " "|~%")
	  (list "|" " " " " " " " " " " " " " " " " " " " " "|~%")
	  (list "|" " " " " " " " " " " " " " " " " " " " " "|~%")
	  (list "|" " " " " " " " " " " " " " " " " " " " " "|~%")
	  (list "|" " " " " " " " " " " " " " " " " " " " " "|~%")
	  (list "|" " " " " " " " " " " " " " " " " " " " " "|~%")
	  (list "|" " " " " " " " " " " " " " " " " " " " " "|~%")
	  (list "|" " " " " " " " " " " " " " " " " " " " " "|~%")
	  (list "|" " " " " " " " " " " " " " " " " " " " " "|~%")
	  (list "|" " " " " " " " " " " " " " " " " " " " " "|~%")
	  (list "|" " " " " " " " " " " " " " " " " " " " " "|~%")
	  (list "|" " " " " " " " " " " " " " " " " " " " " "|~%")
	  (list "|" " " " " " " " " " " " " " " " " " " " " "|~%") 
	  (list "|" " " " " " " " " " " " " " " " " " " " " "|~%")
	  (list "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-~%")
    )
)

; vypise tolik prazdnych radku, kolik je N
(defun prazdno (N)
  (cond ((= N 0))
    (T(vypis (list "~%")) (prazdno (- N 1)))
  )
)

;vypise obecny seznam
(defun vypis (x)
  (cond ((Null x) )
    ((atom (car x)) (format t (car x)) (vypis (cdr x)))
    (T (vypis (car x)) (vypis (cdr x)))
  )
)

;vyjme dany prvek N
(defun vyjmi (L N)
  (cond ((Null L) NIL)
    ((atom (car L)) (cond ((= N 1) (cdr L))
		      (T (cons (car L) (vyjmi (cdr L) (- N 1))))
		    )
    )
  )
)

; vlozi kosticku(L1) na N misto v poli(L2)
; - podle vzoroveho seznamu
(defun vlozcube (L1 L2 N)
		(cond	((NULL L1) L2)
				((atom (car L1)) (cond ((equal (car L1) "#") (vlozcube (cdr L1) (vloz L2 N) (+ N 1)))
									 (T (vlozcube (cdr L1) L2 (+ N 1)))
							     )
				)
			  (T (vlozcube (car L1) (vlozcube (cdr L1) L2 (+ N (/(pocet (gamefield)) (- (listnumber (gamefield)) 1))) )
														  (+ N (/(pocet (gamefield)) (- (listnumber (gamefield)) 1)))) 
			  )
		)
)


;vlozi do seznamu znak # na misto N
(defun vloz (L N)
	(cond ((Null L) NIL)
		((atom (car L)) (cond ((= N 1) (cons "#" (cdr L)))
								  (T (cons (car L) (vloz (cdr L) (- N 1))))
						)
		)
		(T (cons (vloz (car L) N) (vloz (cdr L) (- N (pocet (car L))))))
	)
)

; pocet prvku obecneho seznamu pocet(seznam)
(defun pocet (L)
  (cond ((NULL L) 0)
	((atom (car L)) (+ 1 (pocet (cdr L)))) ; prvni prvek je atom
	(T (+ (pocet (car L)) (pocet (cdr L)))) ; prvni prvek je seznam
  )
)

;pocet seznamu
(defun listnumber (L)
	(cond ((NULL L)1)
		  ((atom (car L)) (listnumber (cdr L)))
		  (T  (+ (listnumber (car L)) (listnumber (cdr L))))
	)
)

; Vrati prvek N z obecneho seznamu (seznam vrati jako prvek)
(defun jaky0 (L N)
	(cond ((NULL L) NIL)
			((atom (car L)) (cond ((= N 1) (car L))
								  (T (jaky0 (cdr L) (- N 1)))
							)
			)
	        (T 		(cond ((= N 1) (car L))
							(T (jaky0 (cdr L) (- N 1)))
						
					 )
			)
	)
)


; funkce vrati prvek na N miste 
(defun jaky (L N)
	(cond ((NULL L) NIL)
			((atom (car L)) (cond ((= N 1) (car L))
								  (T (jaky (cdr L) (- N 1)))
							)
			)
			(T (cond ((jaky (car L) N) (jaky (car L) N))
					 (T (jaky (cdr L) (- N (pocet (car L)))))))
	)
)

(defun podminka1 (L1 N1 L2 N2)
				(cond ((equal (jaky L2 (+ N2 (* 3 (/(pocet (gamefield)) (- (listnumber (gamefield)) 1))))) "#") Nil)
						  (T (podminka L1 (+ N1 1) L2 (+ N2 1)))
				)
)

(defun podminka0 (L1 N1 L2 N2)
				(cond ((equal (jaky L2 (+ N2 (* 2 (/(pocet (gamefield)) (- (listnumber (gamefield)) 1))))) "#") Nil)
						  (T (podminka L1 (+ N1 1) L2 (+ N2 1)))
				)
)


; Kdyz bude splnena fce podminka, tak se bude moc provest pretransformovani kosticky z maleho seznamu do gamefield.

(defun podminka (L1 N1 L2 N2)
	(cond ((= N1 4) T)
			(T (cond ((equal (jaky L1 (+ N1 4)) "#") (podminka1 L1 N1 L2 (+ N2 1)))
					 
					 (T (podminka0 L1 N1 L2 (+ N2 1)))
			   )  
			)	
	)
)

; vypocet stredu radku v poli
(defun stred ()
	(cond ((= (mod (/(pocet (gamefield)) (- (listnumber (gamefield)) 1)) 2) 0)
														( /(- (/(pocet (gamefield)) (- (listnumber (gamefield)) 1)) 2) 2)
		  )
		  (T (/ (- (- (/(pocet (gamefield)) (- (listnumber (gamefield)) 1)) 2) 1) 2))
	)
)

(defun rand0 (N)
	(cond ((= N 0) (rand0 (random 7)))
			(T (jaky0 (cube) N))
	)
)

;vrati nahodne kosticku
(defun rand ()
	(rand0 (random 7))
)



;objekty

;tetris kosticky


;cube0
(defun cube0 ()
  (list (list "#" "#" "~%")
	(list "#" "#" "~%")
  )
)

;cube1 (a)
(defun cube1a ()
  (list (list "#" "#" "#" "#" "~%"))
)

;cube1 (b)
(defun cube1b ()
  (list (list "#" " " "~%")
	(list "#" " " "~%")
	(list "#" " " "~%")
	(list "#" " " "~%")
  )
)

(defun cube1 ()
  (list (cube1a) (cube1b))
)

;cube2a 
(defun cube2a ()
  (list (list "#" "#" " " "~%")
	(list " " "#" "#" "~%")
  )
)

;cube2b
(defun cube2b ()
  (list (list " " "#" "~%")
	(list "#" "#" "~%")
	(list "#" " " "~%")
  )
)

(defun cube2 ()
  (list (cube2a) (cube2b))
)

;cube3
(defun cube3a ()
  (list (list " " "#" "#" "~%")
	(list "#" "#" " " "~%")
  )
)

(defun cube3b ()
  (list (list "#" " " "~%")
	(list "#" "#" "~%")
	(list " " "#" "~%")
  )
)

(defun cube3 ()
  (list(cube3a) (cube3b))
)

;cube4
(defun cube4a ()
  (list (list " " " " "#" "~%")
	(list "#" "#" "#" "~%")
  )
)

(defun cube4b ()
  (list (list "#" " " "~%")
	(list "#" " " "~%")
	(list "#" "#" "~%")
  )
)

(defun cube4c ()
  (list (list "#" "#" "#" "~%")
	(list "#" " " " " "~%")
  )
)

(defun cube4d ()
  (list (list "#" "#" "~%")
	(list " " "#" "~%")
	(list " " "#" "~%")
  )
)

(defun cube4 ()
  (list (cube4a) (cube4b) (cube4c) (cube4d))
)

;cube5
(defun cube5a ()
  (list (list "#" "#" "#" "~%")
	(list " " " " "#" "~%")
  )
)

(defun cube5b ()
  (list (list " " "#" "~%")
	(list " " "#" "~%")
	(list "#" "#" "~%")
  )
)

(defun cube5c ()
  (list (list "#" " " " " "~%")
	(list "#" "#" "#" "~%")
  )
)

(defun cube5d ()
  (list (list "#" "#" "~%")
	(list "#" " " "~%")
	(list "#" " " "~%")
  )
)

(defun cube5 ()
  (list (cube5a) (cube5b) (cube5c) (cube5d))
)

;cube6
(defun cube6a ()
  (list (list " " "#" " " "~%")
	(list "#" "#" "#" "~%")
  )
)

(defun cube6b ()
  (list (list "#" " " "~%")
	(list "#" "#" "~%")
	(list "#" " " "~%")
  )
)

(defun cube6c ()
  (list (list "#" "#" "#" "~%")
	(list " " "#" " " "~%")
  )
)

(defun cube6d ()
  (list (list " " "#" "~%")
	(list "#" "#" "~%")
	(list " " "#" "~%")
  )
)

(defun cube6 ()
  (list (cube6a) (cube6b) (cube6c) (cube6d))
)


(defun cube ()
  (list (cube0)
	(car (cube1))
	(car (cube2))
	(car (cube3))
	(car (cube4))
	(car (cube5))
	(car (cube6))
  )
)
;_______________________________________________________________


; padajici kosticka
(defun fallcube (L1 L2 N R)
  (cond ((podminka L1 1 L2 (stred))
	(prazdno 80)
	(vypis (vlozcube L1 L2 (+ N 1)))
	(cond ((>= N (- (pocet L2) (* 3 (/(pocet L2) (- (listnumber L2 ) 1))))) (fallcube (rand) (vlozcube L1 L2 (+ N 1)) (stred) R) ) 
	      (T
		(cond ((podminka L1 1 L2 N) (sleep R) (prazdno 40) (fallcube L1 L2 (+ N (/(pocet L2) (- (listnumber L2) 1))) R))
		      (T 
			(fallcube (rand) (vlozcube L1 L2 (+ N 1)) (stred) R))
		)
	      )
	)
	)https://github.com/dodotronix/For-Fun.git
	(T(princ "game over"))
  )
)

(printc "hello world")
; vytvorit podminku pro fallcube, na urcovani konce pole (podminka: je tam "-")
