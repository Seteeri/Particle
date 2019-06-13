(setq PATH-MATHC "/home/user/quicklisp/local-projects/protoform/src/model/libmathc.so")


#learn how to do multiple args
#(de native-mc (...) (native PATH-MATHC "mat4_perspective" NIL ...))


(de mperspective (Fov-y Aspect Near Far)
    
    (let (Perspective (native "@" "malloc" 'N 64))
      
      (native PATH-MATHC "mat4_perspective"
	      'N
	      Perspective
	      (cons Fov-y -1000)
	      (cons Aspect -1000)
	      (cons Near -1000)
	      (cons Far -1000))
      
      Perspective))


(de mortho (Left Right Bottom Top Near Far)
        
    (let (Ortho (native "@" "malloc" 'N 64))
      
      (native PATH-MATHC "mat4_ortho"
      	      'N
	      Ortho
	      (cons Left -1000)
	      (cons Right -1000)
	      (cons Bottom -1000)
	      (cons Top -1000)
	      (cons Near -1000)
	      (cons Far -1000))
      
      Ortho))

