(class +Projview)

(dm T (Width
       Height
       Mat-proj
       Type-proj
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
    (=: mat-proj    (when Mat-proj Mat-proj))
    (=: type-proj   (when Type-poj Type-proj))

    #Move below into perspective and ortho classes
    
    (=: fov         (if Fov Fov 0.785398)) # 45-deg
    (=: near        (if Near Near 0.1))
    (=: far         (if Far Far 100.0))
	        
    (=: scale-ortho (if Scale-ortho Scale-ortho 48.0))
    (=: near-ortho  (if Near-ortho Near-ortho 1.0))
    (=: ortho-far   (if Ortho-far Ortho-far 512.0))

    (=: mat-view    (when Mat-view Mat-view))

    (=: position    (if Pos Pos (11.0 -8.0 10.0)))
    (=: rotation    (if Rot Rot (0.0 0.0 0.0)))
    (=: displace    (if Displace Displace (8.0 8.0 4.0)))

    #call update-mat-proj?
    
    T)

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

    (println (format (*/ Scale 0.5 1.0) *Scl))
    
    (let (Aspect-ratio (*/ 1.0 Width Height)
		       X            (*/ Scale 0.5 1.0)
		       Y            (*/ 1.0 (*/ Scale 0.5 1.0) Aspect-ratio))
      (mat4-ortho (- X)
		  X
		  (- Y)
		  Y
		  Near
		  Far)))

(de update-mat-view> ()
    
    #{
    (minv (m* (mat4-translation Pos)
	      (mat4-rotation_z Res (vz3 rot))
	      (mat4-rotation_y Res (vy3 rot))
	      (mat4-rotation_x Res (vx3 rot))
	      (mat4-scaling Scale)))
    }#

    (println "HERE")
    (mat4-scaling    (list 1.0 1.0 1.0))

    (bye)
    
    (=: mat-view (mat4-inverse
		  (mat4-mul (mat4-translation (: pos))
			    (mat4-mul (mat4-rotation-z (nth 3 (: rot)))
				      (mat4-mul (mat4-rotation-y (nth 2 (: rot)))
						(mat4-mul (mat4-rotation-x (nth 1 (: rot)))
							  (mat4-scaling    (: scale)))))))))
