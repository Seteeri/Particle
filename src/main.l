#Launch processes

(de main ()

    (when T
      (unless (fork)
	(exec "pil" "model/model.l" "-model~main" "1920" "1080" "1")))
    
    (when T
      (unless (fork)
	(exec "pil" "input/input.l" "-input~main" "1920" "1080")))
    
    (when NIL
      (unless (fork)
	(exec "pil" "render/render.l" "-render~main")))
    
    #(wait 1000000)
    (println "Forked processes"))
