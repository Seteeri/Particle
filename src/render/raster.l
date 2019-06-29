(de init-rast ()

    (init-buffs-rast)
    #(init-tex-glyphs (get (get '*buffer-objects 'texture-glyphs) 'gl~ptr))        

    (setq *prog-rast (gl~init-prog-rast *paths-rast-vert-shader *paths-rast-frag-shader))
    
    (gl~use-program *prog-rast)
    (setq *vao (car (gl~gen-vertex-arrays 1)))
    (gl~bind-vertex-array *vao))


(de init-buffs-rast ()

    #{
    * Some buffers have a different bind layout per shader stage
    * Texture requires setting fmt after and other ops
    * Set initial data for buffers element and draw-indirect
    * glMapNamedBuffer is unavailable so to persistently map the buffer, it needs to be bound...
    * to bind a buffer, requires an appropriate program
    * What is the behavior when binding a buffer with no program bound?
    * Doesn't matter here...
    }#
    
    (for Params *params-buffer
	 
	 (with Params
	       
	       (let (Buffer (new '(gl~+Buffer-Object)
				   (: size-b)
				   (: target)
				   (if (> (: index-vs) -1)
				       (: index-vs)
				       (: index-cs))
				   T))
		 
		 #Use symbol properties
		 (put '*buffer-objects (: name) Buffer)
		 
		 #Poss more idiomatic way to do this?
		 (when (= (: target) gl~TEXTURE-BUFFER)
		   
		   #texturei max - GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
		   #already active?
		   (gl~active-texture gl~TEXTURE0)
		   
		   (gl~bind-b> Buffer)
		   
		   (gl~tex-buffer gl~TEXTURE-BUFFER
				  gl~RGBA8
				  (: buffer)))))))


(de init-tex-glyphs (Ptr)
    #{
    * To load faster, do bulk memcpy or convert to bytes
    * Possible Configurations:
      * all ppm, all metrics
      * per ppm+metrics
      * per ppm, per metrics (current)
      * Need separate ppm since data is copied to shm?
      * Or list of lists with each list starting with the name
   }#

   (let Offset 0
	(for Code 255
	     (let (Path-glyph (pack PATH-GLYPHS "/tex/" Code "-data.l"))
	       #(println "Reading " Path-glyph " ...")
	       (any (in Path-glyph (till NIL T)))
	       (struct (+ Ptr Offset) 'N
		       (0 . 4)
		       (2 . 4)
		       (1 . 4)
		       (0 . 4)
		       (3 . 4)
		       (2 . 4))
	       (inc 'Offset)))))

