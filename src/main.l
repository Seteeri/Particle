#Launch processes

(de main ()

    (when T
      (unless (fork)
	(exec "pil" "model/model.l" "-model~main" "1920" "1080" (format (** 2 19)))))

    (wait 2000)
    
    (when NIL
      (unless (fork)
	(exec "pil" "input/input.l" "-input~main" "1920" "1080")))
    
    (when NIL
      (unless (fork)
	(exec "pil" "render/render.l" "-render~main" "1920" "1080" (format (** 2 19)))))
    
    #(wait 1000000)
    (println "Forked processes"))
