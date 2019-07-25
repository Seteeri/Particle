(def 'SZ-PV (+ 9 64 24 64 12 12))


(class +Projview)
(dm T (Width
       Height
       Type-proj
       Mat-proj
       Fov
       Near
       Far
       Scale-ortho
       Near-ortho
       Ortho-far
       Mat-view
       Pos
       Rot
       Displace)
    (=: width       (when Width Width))
    (=: height      (when Height Height))
    (=: type-proj   (when Type-proj Type-proj))
    (=: mat-proj    (when Mat-proj Mat-proj))

    # Move below into perspective and ortho classes
    
    (=: fov         (if Fov Fov (to-radians 45.0)))
    (=: near        (if Near Near 0.1))
    (=: far         (if Far Far 100.0))
          
    (=: scale-ortho (if Scale-ortho Scale-ortho 48.0))
    (=: near-ortho  (if Near-ortho Near-ortho 1.0))
    (=: ortho-far   (if Ortho-far Ortho-far 512.0))

    (=: mat-view    (when Mat-view Mat-view))

    (=: pos    (if Pos Pos (0.0 0.0 10.0)))
    (=: rot    (if Rot Rot (0.0 0.0 0.0)))

    #scale is (1.0 1.0 1.0)...
    
    # Move this out
    (=: ptr         (native "@" "malloc" 'N 1024)))


(dm serialize-to-ptr> (Ptr)
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
          

(dm deserialize-fm-ptr> (Ptr)
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
    (=: Far (car far))
    (=: scale-ortho (car Scale-Ortho))
    (=: near-ortho (car Near-Ortho))
    (=: ortho-far (car Ortho-Far))
    (=: mat-view Mat-View)
    (=: pos Pos)
    (=: rot Rot)))

    
(dm update-mat-proj> ()
  (=: mat-proj
      (if (= (: type-proj) 'p)
          (mat4-perspective (: fov)
                            (*/ 1.0 (: width) (: height))
                            (: near)
                            (: far))
          (make-matrix-ortho (: width)
                             (: height)
                             (: scale-ortho)
                             (: near-ortho)
                             (: ortho-far)))))


(de make-matrix-ortho (Width
           Height
           Scale
           Near
           Far)
    (let (Aspect-ratio (*/ 1.0 Width Height)
          X            (*/ Scale 0.5 1.0)
          Y            (*/ 1.0 (*/ Scale 0.5 1.0) Aspect-ratio))
      (mat4-ortho (- X) X (- Y) Y Near Far)))


(dm update-mat-view> ()
    
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
    (=: mat-view (mat4-inverse (mat4-mul (mat4-rotation-x (car (nth (: rot) 1)))
           (mat4-mul (mat4-rotation-y (car (nth (: rot) 2)))
               (mat4-mul (mat4-rotation-z (car (nth (: rot) 3)))
                   (mat4-translation (: pos))))))))


(dm serialize-mat-proj-to-ptr> (Ptr)
  (serialize-mat-to-ptr (: mat-proj) Ptr))


(dm serialize-mat-view-to-ptr> (Ptr)
  (serialize-mat-to-ptr (: mat-view) Ptr))


(de serialize-mat-to-ptr (M Ptr)
  (struct Ptr 'N (cons -1.0 M)))
                 

(dm cpy-to-ptr> (Ptr)  
  (serialize-to-ptr> This Ptr)
  SZ-PV)


(dm cpy-to-obj> (Ptr)  
  (deserialize-fm-ptr> This Ptr))


(dm fmt-msg-send> (Off)
  (pack "(mc projview NIL " SZ-PV " " Off ")"))  
  
  
(dm fmt-msg-recv> (Off)
  (pack "(mc NIL projview " SZ-PV " " Off ")"))
  
               