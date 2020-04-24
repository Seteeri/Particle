(de init-rast ()

  (setq *prog-rast (if (info *path-prog-rast)
                       (gl~up-prg *paths-rast-vert-shader
                                  *paths-rast-frag-shader
                                  *path-prog-rast)
                       (gl~dl-prg *paths-rast-vert-shader
                                  *paths-rast-frag-shader
                                  *path-prog-rast)))
                                  
  (println 'init-rast *prog-rast)
    
  (gl~use-program *prog-rast)

  (setq *vao (car (gl~gen-vertex-arrays 1)))
  (gl~bind-vertex-array *vao)

  (init-buffs-rast)

  #Load default buffer data
  (when TEST-GLYPH (init-tex-glyphs))
  (init-di)
  (init-verts)
  (init-el)
  
  (println 'init-rast "DONE"))


(de init-buffs-rast ()
  #{
    Some buffers have a different bind layout per shader stage
    Texture requires setting fmt after and other ops
    Set initial data for buffers element and draw-indirect
    glMapNamedBuffer is unavailable so to persist map the buffer, must be bound
    ...to bind a buffer, requires an appropriate program
    What is the behavior when binding a buffer with no program bound?
    Doesn't matter here...
  }#
  
  (for Params *params-buffer
    
    # -> Use method
    (with Params
         
      (let Buffer (new '(gl~+Buffer)
                       (: size-b)
                       (: target)
                       (if (> (: index-vs) -1)
                           (: index-vs)
                           (: index-cs))
                       T)

        # Use symbol properties
        (put '*buffer-objects (: name) Buffer)

        # Poss more idiomatic way to do this?
        # -> Use method
        (when (= (: target) gl~TEXTURE-BUFFER)

          # Already active by default?
          (gl~active-texture gl~TEXTURE0)

          (gl~bind-b> Buffer)

          (gl~tex-buffer gl~TEXTURE-BUFFER
                         gl~RGBA8
                         (get Buffer 'gl~buffer)))))))


(de init-tex-glyphs ()
  #{
    To load faster, convert files to bin and do memcpy
  }#
  (let (Ptr (get '*buffer-objects 'texture-glyphs 'gl~ptr)
        Off 0)
      (for Code 255
        (let Path-Glyph (pack *path-gly "tex/" Code "-data.l")
            #(prinl (usec) " | RENDER | init-tex-glyphs | " Code "-data.l...")
            #More idiomatic way to do this...
            (let Data (any (in Path-Glyph (till NIL T)))
              (for B Data
                (struct (+ Ptr Off) 'N (cons B 1))
                (inc 'Off)))))))


(de init-di ()
  (struct (get '*buffer-objects 'draw-indirect 'gl~ptr) 'N
          (6 . 4)
          (cons *verts-max 4)
          (0 . 4)
          (0 . 4)
          (0 . 4)))


(de init-verts ()
  #{
  ;;top right, bottom right, bottom left, top left
  ;;
  ;;3---0
  ;;| / |
  ;;2---1
  ;;
  ;;ccw: 0 2 1 0 3 2
  }#
  (struct (get '*buffer-objects 'vertices 'gl~ptr) 'N
    (list -1.0  1.0  1.0  0.0  1.0
           1.0  0.0  0.0  1.0
           0.0  0.0  0.0  1.0
           0.0  1.0  0.0  1.0)))


(de init-el ()
  (struct (get '*buffer-objects 'element 'gl~ptr) 'N
    (0 . 4)
    (2 . 4)
    (1 . 4)
    (0 . 4)
    (3 . 4)
    (2 . 4)))
