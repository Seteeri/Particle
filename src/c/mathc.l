(def 'PATH-MATHC "/home/user/Git/particle/ext/mathc/libmathc.so")

#learn how to do multiple args
#(de native-mc (...) (native PATH-MATHC "mat4_perspective" NIL ...))

#load vs new

#Static allocate these upfront and reuse
(def '*mat4-in  (native "@" "malloc" 'N 64))
(def '*mat4-out (native "@" "malloc" 'N 64))
(def '*vec3-out (native "@" "malloc" 'N 12))

#Poss use single alloc + offsets
(def '*mat4-t   (native "@" "malloc" 'N 64))
(def '*mat4-r-x (native "@" "malloc" 'N 64))
(def '*mat4-r-y (native "@" "malloc" 'N 64))
(def '*mat4-r-z (native "@" "malloc" 'N 64))
(def '*mat4-s   (native "@" "malloc" 'N 64))
(def '*mat4-0   (native "@" "malloc" 'N 64))
(def '*mat4-1   (native "@" "malloc" 'N 64))
(def '*mat4-i   (native "@" "malloc" 'N 64))


(de to-radians (F)
  (native PATH-MATHC "to_radians" -1.0 (cons F -1.0)))


#do the below need cons?
(de mat4-perspective (Fov-Y A N F)
    (native PATH-MATHC "mat4_perspective" 'N
	    *mat4-out
	    (cons Fov-Y -1.0)
	    (cons A     -1.0)
	    (cons N     -1.0)
	    (cons F     -1.0))
    (struct *mat4-out (-1.0 . 16)))


(de mat4-ortho (L R B T N F)
    (native PATH-MATHC "mat4_ortho" 'N
	    *mat4-out
	    (cons L -1.0)
	    (cons R -1.0)
	    (cons B -1.0)
	    (cons T -1.0)
	    (cons N -1.0)
	    (cons F -1.0))
    (struct *mat4-out (-1.0 . 16)))


(de mat4-mul (M0 M1)
    (struct *mat4-0 'N (list -1.0 (get M0 1) (get M0 2) (get M0 3) (get M0 4)
			     (get M0 5) (get M0 6) (get M0 7) (get M0 8)
			     (get M0 9) (get M0 10) (get M0 11) (get M0 12)
			     (get M0 13) (get M0 14) (get M0 15) (get M0 16)))
    (struct *mat4-1 'N (list -1.0 (get M1 1) (get M1 2) (get M1 3) (get M1 4)
			     (get M1 5) (get M1 6) (get M1 7) (get M1 8)
			     (get M1 9) (get M1 10) (get M1 11) (get M1 12)
			     (get M1 13) (get M1 14) (get M1 15) (get M1 16)))
    (native PATH-MATHC "mat4_multiply" 'N
	    *mat4-out
	    *mat4-0
	    *mat4-1)
    (struct *mat4-out (-1.0 . 16)))


(de mat4-rotation-x (F)
    (native PATH-MATHC "mat4_identity" 'N
	    *mat4-r-x)
    (native PATH-MATHC "mat4_rotation_x" 'N
	    *mat4-r-x
	    (cons F -1.0))
    (struct *mat4-r-x (-1.0 . 16)))    


(de mat4-rotation-y (F)
    (native PATH-MATHC "mat4_identity" 'N
	    *mat4-r-y)
    (native PATH-MATHC "mat4_rotation_y" 'N
	    *mat4-r-y
	    (cons F -1.0))
    (struct *mat4-r-y (-1.0 . 16)))    


(de mat4-rotation-z (F)
    (native PATH-MATHC "mat4_identity" 'N
	    *mat4-r-z)
    (native PATH-MATHC "mat4_rotation_z" 'N
	    *mat4-r-z
	    (cons F -1.0))
    (struct *mat4-r-z (-1.0 . 16)))    


(de mat4-scaling (V)
    (native PATH-MATHC "mat4_identity" 'N
	    *mat4-i)
    (struct *vec3-out 'N (list -1.0
			       (get V 1)
			       (get V 2)
			       (get V 3)))
    (native PATH-MATHC "mat4_scaling" 'N
	    *mat4-s
	    *mat4-i
	    *vec3-out)
    (struct *mat4-s (-1.0 . 16)))


(de mat4-translation (V)
    (native PATH-MATHC "mat4_identity" 'N
	    *mat4-i)
    (struct *vec3-out 'N (list -1.0
			       (get V 1)
			       (get V 2)
			       (get V 3)))
    (native PATH-MATHC "mat4_translation" 'N
	    *mat4-t
	    *mat4-i
	    *vec3-out)
    (struct *mat4-t (-1.0 . 16)))


(de mat4-identity ()
    (native PATH-MATHC "mat4_identity" 'N
	    *mat4-out)
    (struct *mat4-out (-1.0 . 16)))


(de mat4-inverse (M0)
    (struct *mat4-0 'N (list -1.0 (get M0 1) (get M0 2) (get M0 3) (get M0 4)
			     (get M0 5) (get M0 6) (get M0 7) (get M0 8)
			     (get M0 9) (get M0 10) (get M0 11) (get M0 12)
			     (get M0 13) (get M0 14) (get M0 15) (get M0 16)))
    (native PATH-MATHC "mat4_inverse" 'N
	    *mat4-out
	    *mat4-0)
    (struct *mat4-out (-1.0 . 16)))


(de mat4-transpose (M0)
    (struct *mat4-0 'N (list -1.0 (get M0 1) (get M0 2) (get M0 3) (get M0 4)
			     (get M0 5) (get M0 6) (get M0 7) (get M0 8)
			     (get M0 9) (get M0 10) (get M0 11) (get M0 12)
			     (get M0 13) (get M0 14) (get M0 15) (get M0 16)))
    (native PATH-MATHC "mat4_transpose" 'N
	    *mat4-out
	    *mat4-0)
    (struct *mat4-out (-1.0 . 16)))


(de print-mat4 (m)
    (println (cut 4 'm))
    (println (cut 4 'm))
    (println (cut 4 'm))
    (println (cut 4 'm)))


#Normal
(de print-mat4-2 (M)
    (prinl (format (get M 1) *Scl) " "
	   (format (get M 2) *Scl) " "
	   (format (get M 3) *Scl) " "
	   (format (get M 4) *Scl) " ")
    (prinl (format (get M 5) *Scl) " "
	   (format (get M 6) *Scl) " "
	   (format (get M 7) *Scl) " "
	   (format (get M 8) *Scl) " ")
    (prinl (format (get M 9) *Scl) " "
	   (format (get M 10) *Scl) " "
	   (format (get M 11) *Scl) " "
	   (format (get M 12) *Scl) " ")
    (prinl (format (get M 13) *Scl) " "
	   (format (get M 14) *Scl) " "
	   (format (get M 15) *Scl) " "
	   (format (get M 16) *Scl) " "))

#Transposed
(de print-mat4-3 (N)
    (let M (mat4-transpose N)
	 (prinl (format (get M 1) *Scl) " "
		(format (get M 2) *Scl) " "
		(format (get M 3) *Scl) " "
		(format (get M 4) *Scl) " ")
	 (prinl (format (get M 5) *Scl) " "
		(format (get M 6) *Scl) " "
		(format (get M 7) *Scl) " "
		(format (get M 8) *Scl) " ")
	 (prinl (format (get M 9) *Scl) " "
		(format (get M 10) *Scl) " "
		(format (get M 11) *Scl) " "
		(format (get M 12) *Scl) " ")
	 (prinl (format (get M 13) *Scl) " "
		(format (get M 14) *Scl) " "
		(format (get M 15) *Scl) " "
		(format (get M 16) *Scl) " ")))
