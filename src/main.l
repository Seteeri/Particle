#Launch processes

(de main ()

    (when T
      (unless (fork)
	(exec "pil" "model/model.l" "-model~main" "1" "1" "1")))
    
    (when NIL
      (unless (fork)
	(exec "pil" "input/input.l" "-input~main")))
    
    (when NIL
      (unless (fork)
	(exec "pil" "render/render.l" "-render~main")))
    
    #(wait 1000000)
    (println "Forked processes"))
