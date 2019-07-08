#Node tex dims are all 96x96
(def '*dims-tex-node (list 96 96))

(def '*color-vert-default (list 0.5137255 0.5803922 0.60784316 1.0 
                                0.5137255 0.5803922 0.60784316 1.0
				                        0.5137255 0.5803922 0.60784316 1.0 
                                0.5137255 0.5803922 0.60784316 1.0))


(class +Vertex)
(dm T (Char       #Use to get offset, if char then data is char also
       RGBA
       Pos
       Rota
       Sca)
       
  (=: char NIL)
  (=: off-texel NIL)
  (=: rgba RGBA)
  
  # Either keep these here or move to model matrix...
  (=: pos Pos)
  (=: rota Rota)
  (=: sca Sca)
  (=: mm (mat4-identity))
      
  (=: uv NIL)
  
  #Reuse this, also use as cache for optimization
  #Or maintain lispy list of bytes?
  (=: ptr (native "@" "malloc" 'N 208))
      
  (update-glyph> This Char)
  (update-model-matrix> This))


(dm update-glyph> (Char)
  #{
  translation = baseline (get-origin-from-node-pos *node-ptr-main*)
  Create diagrams for metrics calculations so easier to understand/visualize
  }#
  
  (=: char Char)
  (=: off-texel (* (- (: char) 1) (get *dims-tex-node 1) (get *dims-tex-node 2)))

  (let Metrics (get *metrics (: char))   
   
    (=: uv (get Metrics 'uv))
    (=: pos (calc-pos-metrics Metrics (: pos)))
    (=: sca (calc-sca-metrics Metrics))))


# Move to Metrics?
(de calc-pos-metrics (Metrics Pos)

  (let Bounds (get Metrics 'bounds)

    (list (+ (get Pos 1) (*/ (get Bounds 1) *sca-vert 1.0))
          (+ (get Pos 2) (*/ (get Bounds 2) *sca-vert 1.0))
          1.0)))

                  
# Move to Metrics?
(de calc-sca-metrics (Metrics Sca)

  (let Dims (get Metrics 'dims)

    (list (*/ (get Dims 1) *sca-vert 1.0)
          (*/ (get Dims 2) *sca-vert 1.0)
          1.0)))


(dm update-model-matrix> ()

  (=: mm (mat4-mul (mat4-scaling (: sca))
                   (mat4-mul (mat4-rotation-x (get (: rota) 1))
                             (mat4-mul (mat4-rotation-y (get (: rota) 2))
                                       (mat4-mul (mat4-rotation-z (get (: rota) 3))
                                                 (mat4-translation (: pos))))))))
         

(dm serialize-to-ptr> ()

  (let Off (: ptr)     

    (write-matrix-to-ptr       Off     (: mm)   0)
    (write-matrix-to-ptr (inc 'Off 64) (: rgba) 0)
    (write-matrix-to-ptr (inc 'Off 64) (: uv)   0)
    (struct (inc 'Off 64) 'N (cons (: off-texel) 4)
                             (cons 96 4) #x-sz
                             (cons 96 4) #y-sz
                             (cons 0 4) #flag
                             ))
  (: ptr))


(dm serialize-to-lst> ()

  (serialize-to-ptr> This)
  (struct (: ptr) '(B . 208)))


(dm serialize-to-str> ()

  (pack "("
        (glue " " (let L NIL
           (for N (serialize-to-lst> This)
                (push 'L (format N)))
           (flip 'L)
           L))
        ")"))