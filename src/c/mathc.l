(def 'PATH-MATHC "/home/user/Particle/particle/ext/mathc/libmathc.so")

(def '*mat4-a   (native "@" "malloc" 'N 64))
(def '*mat4-b   (native "@" "malloc" 'N 64))
(def '*mat4-z   (native "@" "malloc" 'N 64))
(def '*vec3-z (native "@" "malloc" 'N 12))


(de to-rads (F)
  (native PATH-MATHC "to_radians" -1.0 (cons F -1.0)))


(de mat4-perspective (Fov-Y A N F)
  (struct
    (native PATH-MATHC "mat4_perspective" 'N
      *mat4-z
      (cons Fov-Y -1.0)
      (cons A     -1.0)
      (cons N     -1.0)
      (cons F     -1.0))
    (-1.0 . 16)))


(de mat4-ortho (L R B Tt N F)
  (struct 
    (native PATH-MATHC "mat4_ortho" 'N *mat4-z
      (cons L -1.0)
      (cons R -1.0)
      (cons B -1.0)
      (cons Tt -1.0)
      (cons N -1.0)
      (cons F -1.0))
    (-1.0 . 16)))


(de mat4-mul (M N)
  (struct
    (native PATH-MATHC "mat4_multiply" 'N
      *mat4-z
      (struct *mat4-a 'N (cons -1.0 M))
      (struct *mat4-b 'N (cons -1.0 N)))
    (-1.0 . 16)))


(de mat4-rotation-x (F)
  (struct
    (native PATH-MATHC "mat4_rotation_x" 'N
      (native PATH-MATHC "mat4_identity" 'N *mat4-z)
      (cons F -1.0))
    (-1.0 . 16)))


(de mat4-rotation-y (F)
  (struct
    (native PATH-MATHC "mat4_rotation_y" 'N
      (native PATH-MATHC "mat4_identity" 'N *mat4-z)
      (cons F -1.0))
    (-1.0 . 16)))


(de mat4-rotation-z (F)
  (struct
    (native PATH-MATHC "mat4_rotation_z" 'N
      (native PATH-MATHC "mat4_identity" 'N *mat4-z)
      (cons F -1.0))
    (-1.0 . 16)))


(de mat4-scaling (V)
  (struct
    (native PATH-MATHC "mat4_scaling" 'N
      *mat4-z
      (native PATH-MATHC "mat4_identity" 'N *mat4-a)
      (struct *vec3-z 'N (cons -1.0 V))) (-1.0 . 16)))


(de mat4-translation (V)
  (struct
    (native PATH-MATHC "mat4_translation" 'N
      *mat4-z 
      (native PATH-MATHC "mat4_identity" 'N *mat4-a)
      (struct *vec3-z 'N (cons -1.0 V)))
    (-1.0 . 16)))


(de mat4-identity ()
  (struct
    (native PATH-MATHC "mat4_identity" 'N *mat4-z)
    (-1.0 . 16)))


(de mat4-inverse (M)
  (struct
    (native PATH-MATHC "mat4_inverse" 'N 
      *mat4-z
      (struct *mat4-a 'N (cons -1.0 M)))
    (-1.0 . 16)))


(de mat4-transpose (M)
  (struct
    (native PATH-MATHC "mat4_transpose" 'N
      *mat4-z
      (struct *mat4-a 'N (cons -1.0 M)))
    (-1.0 . 16)))


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
