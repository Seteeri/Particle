(de cad-shader (Type
		Program
		Paths)
    
    (let (Shader (compile-shader Type
		   Paths)
          Info-Log (gl~get-shader-info-log-2 Shader))
      
      (if Info-Log
	  Info-Log
	  (prog
	      (gl~attach-shader Program Shader)
	     (gl~delete-shader Shader)))))


(de compile-shader (Type
		    Paths)
    (let (Shader (gl~create-shader Type))

      #Function will manage memory, need only pass file paths
      (gl~shader-source-2 Shader
			  Paths)
      
      (gl~compile-shader Shader)
      
      Shader))


(de init-prog-rast ()
  (let (Program (gl~create-program))
    (let (Log-Vert (cad-shader gl~VERTEX-SHADER
                               Program
		               *paths-rast-vert-shader)
          Log-Frag (cad-shader gl~FRAGMENT-SHADER
	  	               Program
	  		       *paths-rast-frag-shader))
      
      (if Log-Vert
	  (prinl "init-prog-rast " "Vert Shader Log: " Log-vert)
	  (prinl "init-prog-rast " "Compiled and attached vertex shader sucessfully"))
      (if Log-Frag
	  (prinl "init-prog-rast " "Frag Shader Log: " Log-vert)
	  (prinl "init-prog-rast " "Compiled and attached fragment shader sucessfully"))
  
      (gl~link-program Program)
      
      (let (Log-Prog (gl~get-program-info-log-2 Program))
	(if Log-Prog
	    (prinl "init-prog-rast-msdf " "Program Log: " Log-Prog)
	    (prinl "init-prog-rast-msdf " "Compiled program sucessfully"))))

    (bye)
    
    program))
