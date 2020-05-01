(def 'SZ-PV (+ 9 64 24 64 12 12))


(class +Projview)
(dm T (Width
       Height
       Type-Proj
       Mat-Proj
       Fov
       Near
       Far
       Scale-Ortho
       Near-Ortho
       Ortho-Far
       Mat-View
       Pos
       Rot
       Displace)
  (=: width       (when Width @))
  (=: height      (when Height @))
  (=: type-proj   (when Type-Proj @))
  (=: mat-proj    (when Mat-Proj @))

  # Move below into perspective and ortho classes
  
  (=: fov         (if Fov @ (to-rads 45.0)))
  (=: near        (if Near @ 0.1))
  (=: far         (if Far @ 100.0))
        
  (=: scale-ortho (if Scale-Ortho @ 32.0))
  (=: near-ortho  (if Near-Ortho @ 1.0))
  (=: ortho-far   (if Ortho-Far @ 512.0))

  (=: mat-view    (when Mat-View @))

  (=: pos    (if Pos @ (11.0 0.0 10.0)))
  (=: rot    (if Rot @ (0.0 0.0 0.0))))


(dm wr-ptr> (Ptr)
  (struct Ptr 'N 
          (cons (: width) 4)
          (cons (: height) 4)
          (char (name (: type-proj)))
          (cons -1.0 (: mat-proj))
          (list -1.0 (: fov)
                     (: near)
                     (: far)
                     (: scale-ortho)
                     (: near-ortho)
                     (: ortho-far))
          (cons -1.0 (: mat-view))
          (cons -1.0 (: pos))
          (cons -1.0 (: rot)))
  SZ-PV)
          

(dm rd-ptr> (Ptr)
  (let ((Width Height Type-Proj Mat-Proj Fov Near Far Scale-Ortho Near-Ortho
         Ortho-Far Mat-View Pos Rot Displace)
        (struct Ptr
                '((I . 1)
                  (I . 1)
                  (C . 1)
                  (-1.0 . 16)
                  (-1.0 . 1)
                  (-1.0 . 1)
                  (-1.0 . 1)
                  (-1.0 . 1)
                  (-1.0 . 1)
                  (-1.0 . 1)
                  (-1.0 . 16)
                  (-1.0 . 3)
                  (-1.0 . 3))))
                                    
    (=: width (car Width))
    (=: height (car Height))
    (=: type-proj (intern (car Type-Proj)))
    (=: mat-proj Mat-Proj)
    (=: fov (car Fov))
    (=: near (car Near))
    (=: far (car Far))
    (=: scale-ortho (car Scale-Ortho))
    (=: near-ortho (car Near-Ortho))
    (=: ortho-far (car Ortho-Far))
    (=: mat-view Mat-View)
    (=: pos Pos)
    (=: rot Rot)))

    
(dm upd-mat-proj> ()
  (=: mat-proj
      (if (= (: type-proj) 'p)
          (mat4-perspective (: fov)
                            (*/ 1.0 (: width) (: height))
                            (: near)
                            (: far))
          (make-mat-ortho (: width)
                          (: height)
                          (: scale-ortho)
                          (: near-ortho)
                          (: ortho-far)))))


(de make-mat-ortho (W H S N F)
  (let (Ar (*/ 1.0 W H)
        X  (*/ S 0.5 1.0)
        Y  (*/ 1.0 (*/ S 0.5 1.0) Ar))
    (mat4-ortho (- X) X (- Y) Y N F)))


(dm upd-mat-view> ()
    
  #https://www.3dgep.com/understanding-the-view-matrix/
  #V=(T(Rx(RyRz)))^-1

  #https://www.scratchapixel.com/lessons/mathematics-physics-for-computer-graphics/geometry/row-major-vs-column-major-vector
  #COL: P′ = Ry ∗ Rz ∗ T  ∗ P
  #ROW: P′ = P  ∗ T  ∗ Rz ∗ Ry

  #Rot always in the middle
  #Here makes no difference since default rotation is an identity matrix

  #Create macro for below so that the intermediate matrix is reused
  
  #This is performing innermost first so:
  #(INV (((T * Rz) Ry) Rx))
  (=: mat-view 
      (mat4-inverse (mat4-mul 
        (mat4-rotation-x (get (: rot) 1))
          (mat4-mul (mat4-rotation-y (get (: rot) 2))
            (mat4-mul (mat4-rotation-z (get (: rot) 3))
              (mat4-translation (: pos))))))))


(dm wr-mat-proj-ptr> (Ptr)
  (wr-mat-to-ptr (: mat-proj) Ptr))


(dm wr-mat-view-ptr> (Ptr)
  (wr-mat-to-ptr (: mat-view) Ptr))


(de wr-mat-to-ptr (M Ptr)
  (struct Ptr 'N (cons -1.0 M)))
                 

(dm cpy-to-ptr> (Ptr)  
  (wr-ptr> This Ptr)
  SZ-PV)


(dm cpy-to-obj> (Ptr)
  (rd-ptr> This Ptr))


(dm fmt-msg-send> (Off)
  (pack "(mc projview NIL " SZ-PV " " Off ")"))  
  
  
(dm fmt-msg-recv> (Off)
  (pack "(mc NIL projview " SZ-PV " " Off ")"))
