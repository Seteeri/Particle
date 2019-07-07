(de init-prog-rast (Paths-Vert
		    Paths-Frag)
  (let (Program (create-program))
    (let (Log-Vert (cad-shader VERTEX-SHADER
                               Program
		               Paths-Vert)
          Log-Frag (cad-shader FRAGMENT-SHADER
	  	               Program
	  		       Paths-Frag))
      (if Log-Vert
          (prinl (usec) " | RENDER | init-prog-rast | Vert Shader Log: " Log-Vert)
          (prinl (usec) " | RENDER | init-prog-rast | Compiled and attached vertex shader sucessfully"))
      (if Log-Frag
          (prinl (usec) " | RENDER | init-prog-rast | Frag Shader Log: " Log-Vert)
          (prinl (usec) " | RENDER | init-prog-rast | Compiled and attached fragment shader sucessfully")))
  
    (link-program Program)
    (query-program-info-log Program)
    Program))


(de init-prog-comp (Paths)
  (let (Program (create-program))
    (let (Log-Comp (cad-shader COMPUTE-SHADER
                               Program
		               Paths))
      (if Log-Comp
        (prinl (usec) " | RENDER | init-prog-rast | " "Comp Shader Log: " Log-Comp)
        (prinl (usec) " | RENDER | init-prog-rast | " "Compiled and attached compute shader sucessfully")))
  
    (link-program Program)
    (query-program-info-log Program)
    Program))

  
(de query-program-info-log (Program)
    (let (Log-Prog (get-program-info-log-2 Program))
      (if Log-Prog
          (prinl (usec) " | RENDER | init-prog-rast | " "Program Log: " Log-Prog)
          (prinl (usec) " | RENDER | init-prog-rast | " "Compiled program sucessfully"))))
