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
    (=: type-proj   (when Type-poj Type-proj))
    (=: mat-proj    (when Mat-proj Mat-proj))

    #Move below into perspective and ortho classes
    
    (=: fov         (if Fov Fov (to-radians 45)))
    (=: near        (if Near Near 0.1))
    (=: far         (if Far Far 100.0))
	        
    (=: scale-ortho (if Scale-ortho Scale-ortho 48.0))
    (=: near-ortho  (if Near-ortho Near-ortho 1.0))
    (=: ortho-far   (if Ortho-far Ortho-far 512.0))

    (=: mat-view    (when Mat-view Mat-view))

    (=: position    (if Pos Pos (11.0 -8.0 10.0)))
    (=: rotation    (if Rot Rot (0.0 0.0 0.0)))
    #scale is (1.0 1.0 1.0)
    (=: displace    (if Displace Displace (8.0 8.0 4.0))))
#call update-mat-proj?

    
(dm update-mat-proj> ()    
    (=: mat-proj
	(if (= type-proj 'perspective)
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
      (mat4-ortho (- X)
		  X
		  (- Y)
		  Y
		  Near
		  Far)))


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
    (=: mat-view (mat4-inverse (mat4-mul (mat4-rotation-x (car (nth (: rotation) 1)))
					 (mat4-mul (mat4-rotation-y (car (nth (: rotation) 2)))
						   (mat4-mul (mat4-rotation-z (car (nth (: rotation) 3)))
							     (mat4-translation (: position))))))))

    #our translation matrix
    #(1000000 0 0 0)
    #(0 1000000 0 0)
    #(0 0 1000000 0)
    #(11000000 -8000000 10000000 1000000)

    #transpose above to get below
    
    #translation matrix col-order
    #{1.0 0.0 0.0 11.0
      0.0 1.0 0.0 -8.0
      0.0 0.0 1.0 10.0
      0.0 0.0 0.0  1.0}#

    #view matrix
    #{1.0 0.0 0.0 -11.0
      0.0 1.0 0.0   8.0
      0.0 0.0 1.0 -10.0
      0.0 0.0 0.0   1.0}#


(de write-mat-proj-to-shm (Mmap Projview)
  (write-matrix-to-ptr (get Mmap 'ptr)
		       (get Projview 'mat-proj)
		       0))


(de write-mat-view-to-shm (Mmap Projview)
  (write-matrix-to-ptr (get Mmap 'ptr)
		       (get Projview 'mat-view)
		       16))

(de write-matrix-to-ptr (Ptr
			 M
			 Offset)
    (struct (+ Ptr Offset) 'N
	    (list -1.0 (car (nth M 1))  (car (nth M 2))  (car (nth M 3))  (car (nth M 4))
                       (car (nth M 5))  (car (nth M 6))  (car (nth M 7))  (car (nth M 8))
	               (car (nth M 9))  (car (nth M 10)) (car (nth M 11)) (car (nth M 12))
		       (car (nth M 13)) (car (nth M 14)) (car (nth M 15)) (car (nth M 16)))))

