(class +Projview)

(dm T (Width
       Height
       Mat-proj
       Type-proj
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
    (=: scale-ortho (if Scale-ortho Scale-ortho 48))
    (=: near-ortho  (if Near-ortho Near-ortho 1))
    (=: ortho-far   (if Ortho-far Ortho-far 512))
    (=: mat-view    (when Mat-view Mat-view))
    (=: position    (if Pos Pos (11 -8 10)))
    (=: rotation    (if Rot Rot (0 0 0)))
    (=: displace    (if Displace Displace (8 8 4))))

(dm update-mat-proj> ()    
    (setq mat-proj
	  (if (= type-proj 'perspective)
	      (make-matrix-perspective (: width)
				       (: height))
	      (make-matrix-ortho (: width)
				 (: height)
				 (: scale-ortho)
				 (: near-ortho)
				 (: ortho-far)))))

(de make-matrix-perspective (Width
			     Height)
    #Pass adtl arguments    
    (mperspective 0.785398 # 45-deg
		  (/ Width Height)
		  0.1
		  100.0))

(de make-matrix-ortho (Width
		       Height
		       Scale
		       Near
		       Far)
  (let (Aspect-ratio (/ Width Height)
	X            (* Scale 0.5)
	Y            (/ (* Scale 0.5) Aspect-ratio))
    (mortho (- X)
	    X
	    (- Y)
	    Y
	    Near
	    Far)))

(de update-mat-view> ()
    (=: mat-view NIL))

    #{
    (minv (m* (mtranslation pos)
	      (mrotation +vz+ (vz3 rot))
	      (mrotation +vy+ (vy3 rot))
	      (mrotation +vx+ (vx3 rot))
	      (mscaling (vec3 1 1 1))))
    }#
