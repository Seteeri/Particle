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

(de mat4-perspective (Fov-y Aspect Near Far)
    (native PATH-MATHC "mat4_perspective"
	    'N
	    *mat4-out
	    (cons Fov-y -1.0)
	    (cons Aspect -1.0)
	    (cons Near -1.0)
	    (cons Far -1.0))
    (struct *mat4-out '(-1.0 . 16)))


(de mat4-ortho (Left Right Bottom Top Near Far)
    (println Left Right Bottom Top Near Far)
    (native PATH-MATHC "mat4_ortho"
	    'N
	    *mat4-out
	    (cons Left -1.0)
	    (cons Right -1.0)
	    (cons Bottom -1.0)
	    (cons Top -1.0)
	    (cons Near -1.0)
	    (cons Far -1.0))
    (struct *mat4-out '(-1.0 . 16)))


(de mat4-mul (M0 M1)
    (struct *mat4-0 (-1.0 (nth 1  M0) (nth 2  M0) (nth 3  M0) (nth 4  M0)
			  (nth 5  M0) (nth 6  M0) (nth 7  M0) (nth 8  M0)
			  (nth 9  M0) (nth 10 M0) (nth 11 M0) (nth 12 M0)
			  (nth 13 M0) (nth 14 M0) (nth 15 M0) (nth 16 M0)))
    (struct *mat4-1 (-1.0 (nth 1  M1) (nth 2  M1) (nth 3  M1) (nth 4  M1)
			  (nth 5  M1) (nth 6  M1) (nth 7  M1) (nth 8  M1)
			  (nth 9  M1) (nth 10 M1) (nth 11 M1) (nth 12 M1)
			  (nth 13 M1) (nth 14 M1) (nth 15 M1) (nth 16 M1)))
    (native PATH-MATHC "mat4_multiply"
	    'N
	    *mat4-out
	    *mat4-0
	    *mat4-1)
    (struct *mat4-out '(-1.0 . 16)))

(de mat4-rotation-x (F)
    (native PATH-MATHC "mat4_identity"
	    'N
	    *mat4-r-x)
    (native PATH-MATHC "mat4_rotation_x"
	    'N
	    *mat4-r-x
	    (cons F -1))
    (struct *mat4-r-x '(-1.0 . 16)))    

(de mat4-rotation-y (F)
    (native PATH-MATHC "mat4_identity"
	    'N
	    *mat4-r-y)
    (native PATH-MATHC "mat4_rotation_y"
	    'N
	    *mat4-r-y
	    (cons F -1))
    (struct *mat4-r-y '(-1.0 . 16)))    

(de mat4-rotation-z (F)
    (native PATH-MATHC "mat4_identity"
	    'N
	    *mat4-r-z)
    (native PATH-MATHC "mat4_rotation_z"
	    'N
	    *mat4-r-z
	    (cons F -1))
    (struct *mat4-r-z '(-1.0 . 16)))    

(de mat4-scaling (V)
    (native PATH-MATHC "mat4_identity"
	    'N
	    *mat4-s)   
    (native PATH-MATHC "mat4_scaling"
	    'N
	    *mat4-s
	    (struct *vec3-out (-1.0 (nth 1 V) (nth 2 V) (nth 3 V))))
    (struct *mat4-s '(-1.0 . 16)))

(de mat4-translation (V)
    (native PATH-MATHC "mat4_identity"
	    'N
	    *mat4-t)    
    (native PATH-MATHC "mat4_translation"
	    'N
	    *mat4-t
	    (struct *vec3-out (-1.0 (nth 1 V) (nth 2 V) (nth 3 V))))
    (struct *mat4-t '(-1.0 . 16)))

(de mat4-identity ()
    (native PATH-MATHC "mat4_identity"
	    'N
	    *mat4-out)
    (struct *mat4-out '(-1.0 . 16)))

(de mat4-inverse (M0)
    (struct *mat4-0 (-1.0 (nth 1  M0) (nth 2  M0) (nth 3  M0) (nth 4  M0)
			  (nth 5  M0) (nth 6  M0) (nth 7  M0) (nth 8  M0)
			  (nth 9  M0) (nth 10 M0) (nth 11 M0) (nth 12 M0)
			  (nth 13 M0) (nth 14 M0) (nth 15 M0) (nth 16 M0)))
    (native PATH-MATHC "mat4_inverse"
	    'N
	    *mat4-0
	    *mat4-out)
    (struct *mat4-out '(-1.0 . 16)))

(de print-mat4 (M0)
    (println M0)
    (let (m (struct M0 '(-1.0 . 16)))
      (println (cut 4 'm))
      (println (cut 4 'm))
      (println (cut 4 'm))
      (println (cut 4 'm))))
