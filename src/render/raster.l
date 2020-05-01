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

  (ld-glys-bin (pack *path-gly "DejaVuSansMono.rgba8.bin"))
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


(de ld-glys ()
  (let (Ptr (get '*buffer-objects 'texture-glyphs 'gl~ptr)
        Path-Gly-Def (pack *path-gly "l/32.ppm.l"))
    (for Code 255
      (let Path-Glyph (pack *path-gly "l/" Code ".ppm.l")
        (inc 'Ptr (ld-gly (if (info Path-Glyph)
                                Path-Glyph
                                Path-Gly-Def)))))))


(de ld-gly (Path)
  (let Data (car (str (in Path
                        (till NIL T))))
    (eval (append '(struct Ptr N) Data))
    (length Data)))
    

(de ld-glys-bin (Path)
  (let Dst (get '*buffer-objects 'texture-glyphs 'gl~ptr)
    (let? Sz (car (info Path))
      (let (Fd (open Path T)
            Src (native "@" "mmap" 'N 0 Sz (| PROT-READ PROT-WRITE) (| MAP-PRIVATE) Fd 0))
        #(println Dst Src Path Sz Fd)
        (native "@" "memcpy" NIL Dst Src Sz)
        (native "@" "munmap" Src Sz)
        (close Fd)
        Sz))))


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
