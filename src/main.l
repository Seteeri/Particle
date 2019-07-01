#Launch processes

(de main ()

    #2**19

    (let (Width  (format (/ 1920 2))
	  Height (format (/ 1080 2))
	  Verts  (format (** 2 4)))
    
      (when T
	(unless (fork)
	  (exec "pil" "model/model.l" "-model~main" Width Height Verts)))

      (wait 2000)
      
      (when NIL
	(unless (fork)
	  (exec "pil" "input/input.l" "-input~main" Width Height)))
      
      (when T
	(unless (fork)
	  (exec "pil" "render/render.l" "-render~main" Width Height Verts))))
    
    #(wait 1000000)
    (println "Forked processes"))
