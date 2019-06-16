#learn how to do multiple args
#(de native-mc (...) (native PATH-MATHC "mat4_perspective" NIL ...))

#load vs new

#Static allocate these upfront and reuse
(setq *mat4-in  (native "@" "malloc" 'N 64))
(setq *mat4-out (native "@" "malloc" 'N 64))
(setq *vec3-out (native "@" "malloc" 'N 12))

(setq *mat4-t   (native "@" "malloc" 'N 64))
(setq *mat4-r-x (native "@" "malloc" 'N 64))
(setq *mat4-r-y (native "@" "malloc" 'N 64))
(setq *mat4-r-z (native "@" "malloc" 'N 64))
(setq *mat4-s   (native "@" "malloc" 'N 64))
(setq *mat4-0   (native "@" "malloc" 'N 64))
(setq *mat4-1   (native "@" "malloc" 'N 64))
(setq *mat4-i   (native "@" "malloc" 'N 64))

#do the below need cons?
(de mat4-perspective (Fov-y Aspect Near Far)
    (native PATH-MATHC "mat4_perspective"
	    'N
	    *mat4-out
	    (cons Fov-y -1.0)
	    (cons Aspect -1.0)
	    (cons Near -1.0)
	    (cons Far -1.0))
    (struct *mat4-out (-1.0 . 16)))


(de mat4-ortho (Left Right Bottom Top Near Far)
    (native PATH-MATHC "mat4_ortho"
	    'N
	    *mat4-out
	    (cons Left -1.0)
	    (cons Right -1.0)
	    (cons Bottom -1.0)
	    (cons Top -1.0)
	    (cons Near -1.0)
	    (cons Far -1.0))
    (struct *mat4-out (-1.0 . 16)))


(de mat4-mul (M0 M1)
    (struct *mat4-0 'N (list -1.0 (car (nth M0 1))  (car (nth M0 2))  (car (nth M0 3))  (car (nth M0 4))
			     (car (nth M0 5))  (car (nth M0 6))  (car (nth M0 7))  (car (nth M0 8))
			     (car (nth M0 9))  (car (nth M0 10)) (car (nth M0 11)) (car (nth M0 12))
			     (car (nth M0 13)) (car (nth M0 14)) (car (nth M0 15)) (car (nth M0 16))))
    (struct *mat4-1 'N (list -1.0 (car (nth M1 1))  (car (nth M1 2))  (car (nth M1 3))  (car (nth M1 4))
			     (car (nth M1 5))  (car (nth M1 6))  (car (nth M1 7))  (car (nth M1 8))
			     (car (nth M1 9))  (car (nth M1 10)) (car (nth M1 11)) (car (nth M1 12))
			     (car (nth M1 13)) (car (nth M1 14)) (car (nth M1 15)) (car (nth M1 16))))
    (native PATH-MATHC "mat4_multiply"
	    'N
	    *mat4-out
	    *mat4-0
	    *mat4-1)
    (struct *mat4-out (-1.0 . 16)))

(de mat4-rotation-x (F)
    (native PATH-MATHC "mat4_identity"
	    'N
	    *mat4-r-x)
    (native PATH-MATHC "mat4_rotation_x"
	    'N
	    *mat4-r-x
	    (cons F -1.0))
    (struct *mat4-r-x (-1.0 . 16)))    

(de mat4-rotation-y (F)
    (native PATH-MATHC "mat4_identity"
	    'N
	    *mat4-r-y)
    (native PATH-MATHC "mat4_rotation_y"
	    'N
	    *mat4-r-y
	    (cons F -1.0))
    (struct *mat4-r-y (-1.0 . 16)))    

(de mat4-rotation-z (F)
    (native PATH-MATHC "mat4_identity"
	    'N
	    *mat4-r-z)
    (native PATH-MATHC "mat4_rotation_z"
	    'N
	    *mat4-r-z
	    (cons F -1.0))
    (struct *mat4-r-z (-1.0 . 16)))    

(de mat4-scaling (V)
    (native PATH-MATHC "mat4_identity"
	    'N
	    *mat4-i)
    (struct *vec3-out 'N (list -1.0
			       (car (nth V 1))
			       (car (nth V 2))
			       (car (nth V 3))))
    (native PATH-MATHC "mat4_scaling"
	    'N
	    *mat4-s
	    *mat4-i
	    *vec3-out)
    (struct *mat4-s (-1.0 . 16)))

(de mat4-translation (V)
    (native PATH-MATHC "mat4_identity"
	    'N
	    *mat4-i)
    (struct *vec3-out 'N (list -1.0
			       (car (nth V 1))
			       (car (nth V 2))
			       (car (nth V 3))))
    (native PATH-MATHC "mat4_translation"
	    'N
	    *mat4-t
	    *mat4-i
	    *vec3-out)
    (struct *mat4-t (-1.0 . 16)))

(de mat4-identity ()
    (native PATH-MATHC "mat4_identity"
	    'N
	    *mat4-out)
    (struct *mat4-out (-1.0 . 16)))

(de mat4-inverse (M0)
    (struct *mat4-0 'N (list -1.0 (car (nth M0 1))  (car (nth M0 2))  (car (nth M0 3))  (car (nth M0 4))
			     (car (nth M0 5))  (car (nth M0 6))  (car (nth M0 7))  (car (nth M0 8))
			     (car (nth M0 9))  (car (nth M0 10)) (car (nth M0 11)) (car (nth M0 12))
			     (car (nth M0 13)) (car (nth M0 14)) (car (nth M0 15)) (car (nth M0 16))))
    (native PATH-MATHC "mat4_inverse"
	    'N
	    *mat4-out
	    *mat4-0)
    (struct *mat4-out (-1.0 . 16)))

(de mat4-transpose (M0)
    (struct *mat4-0 'N (list -1.0 (car (nth M0 1))  (car (nth M0 2))  (car (nth M0 3))  (car (nth M0 4))
			     (car (nth M0 5))  (car (nth M0 6))  (car (nth M0 7))  (car (nth M0 8))
			     (car (nth M0 9))  (car (nth M0 10)) (car (nth M0 11)) (car (nth M0 12))
			     (car (nth M0 13)) (car (nth M0 14)) (car (nth M0 15)) (car (nth M0 16))))
    (native PATH-MATHC "mat4_transpose"
	    'N
	    *mat4-out
	    *mat4-0)
    (struct *mat4-out (-1.0 . 16)))

(de print-mat4 (m)
    (println (cut 4 'm))
    (println (cut 4 'm))
    (println (cut 4 'm))
    (println (cut 4 'm)))

(de print-mat4-2 (m)
    (prinl (format (car (nth m 1)) *Scl) " "
	   (format (car (nth m 2)) *Scl) " "
	   (format (car (nth m 3)) *Scl) " "
	   (format (car (nth m 4)) *Scl) " ")
    (prinl (format (car (nth m 5)) *Scl) " "
	   (format (car (nth m 6)) *Scl) " "
	   (format (car (nth m 7)) *Scl) " "
	   (format (car (nth m 8)) *Scl) " ")
    (prinl (format (car (nth m 9)) *Scl) " "
	   (format (car (nth m 10)) *Scl) " "
	   (format (car (nth m 11)) *Scl) " "
	   (format (car (nth m 12)) *Scl) " ")
    (prinl (format (car (nth m 13)) *Scl) " "
	   (format (car (nth m 14)) *Scl) " "
	   (format (car (nth m 15)) *Scl) " "
	   (format (car (nth m 16)) *Scl) " "))
