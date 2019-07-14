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
  
  #Reuse across all vertices for txfer
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
         

(de serialize-mat-to-ptr (M Ptr)
    (struct Ptr 'N
	    (list -1.0 (get M 1)  (get M 2)  (get M 3)  (get M 4)
                       (get M 5)  (get M 6)  (get M 7)  (get M 8)
	               (get M 9)  (get M 10) (get M 11) (get M 12)
		       (get M 13) (get M 14) (get M 15) (get M 16)))
    Ptr)


(dm serialize-to-ptr> (Ptr)

  (let Off Ptr

    (serialize-mat-to-ptr (: mm)   Off)
    
    (serialize-mat-to-ptr (: rgba) (inc 'Off 64))
    
    (serialize-mat-to-ptr (: uv)   (inc 'Off 64))
    
    (struct (inc 'Off 64) 'N (cons (: off-texel) 4)
                             (cons 96 4) #x-sz
                             (cons 96 4) #y-sz
                             (cons 0 4) #flag
                             )))


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
        
        
################################################################################

(de get-vert-off (Cnt)
  (* (- Cnt 1) 208))


(dm cpy-to-ptr> (Ptr)
  (serialize-to-ptr> This Ptr)
  208)


(dm cpy-to-obj> (Ptr)
  # Refactor to method
  (let ((Mm Rgba Uv (Off-Texel Sz-X Sz-Y Flag))
        (struct Ptr '((-1.0 . 16) (-1.0 . 16) (-1.0 . 16) (I . 4))))

    (prinl "IMPLEMENT CPY TO OBJ FOR VERTEX!")

    (println Mm Rgba Uv Off-Texel Sz-X Sz-Y Flag)
    
    (=: char 42)
    (=: pos (list 0.0 0.0 0.0))
    (=: rota (list 0.0 0.0 0.0))
    (=: sca (list 1.0 1.0 1.0))
    
    (=: mm Mm)
    (=: rgba Rgba)
    (=: uv Uv)
    (=: off-texel Off-Texel)))


(dm fmt-msg-send> (Off)
  (pack "(mc nodes NIL 208 " Off ")"))
  
  
(dm fmt-msg-recv> (Off)
  (pack "(mc NIL nodes 208 " Off ")"))
  
