#Node tex dims are all 96x96
(def '*dims-tex-node (list 96 96))

(def '*color-vert-default (list 0.5137255 0.5803922 0.60784316 1.0 
                                0.5137255 0.5803922 0.60784316 1.0
				                        0.5137255 0.5803922 0.60784316 1.0 
                                0.5137255 0.5803922 0.60784316 1.0))


(def 'SZ-VERT (+ 1 12 12 12 64 64 64 4))


# Reuse across all vertices for txfer
(def '*ptr-vert (native "@" "malloc" 'N SZ-VERT))


(class +Vertex)
(dm T (Chr       #Use to get offset, if char then data is char also
       RGBA
       Pos
       Rota
       Sca)
       
  (=: chr NIL)
  
  # Either keep these here or move to model matrix...
  (=: pos Pos)
  (=: rota Rota)
  (=: sca Sca)
  (=: mm (mat4-identity))
  
  (=: rgba RGBA)      
  (=: uv NIL)
  (=: off-texel NIL)
          
  (update-glyph> This Chr)
  (update-model-matrix> This))


(dm update-glyph> (Chr)
  #{
  translation = baseline (get-origin-from-node-pos *node-ptr-main*)
  Create diagrams for metrics calculations so easier to understand/visualize
  }#
  
  (=: chr (char Chr))
  (=: off-texel (* (- (: chr) 1) (get *dims-tex-node 1) (get *dims-tex-node 2)))

  (let Metrics (get *metrics (: chr))
   
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
  (struct Ptr 'N (cons -1.0 M)))


(dm serialize-to-ptr> (Ptr)
  (struct Ptr 'N 
          (: chr)
          (cons -1.0 (: pos))
          (cons -1.0 (: rota))
          (cons -1.0 (: sca))
          (cons -1.0 (: mm))
          (cons -1.0 (: rgba))          
          (cons -1.0 (: uv))
          (cons (: off-texel) 4)))


(dm deserialize-fm-ptr> (Ptr)

  (let ((Chr Pos Rota Sca Mm Rgba Uv Off-Texel)
        (struct Ptr 
                '((B . 1)
                  (-1.0 . 3)
                  (-1.0 . 3)
                  (-1.0 . 3)
                  (-1.0 . 16)
                  (-1.0 . 16)
                  (-1.0 . 16)
                  (I . 1))))
    
    (=: chr (car Chr))
        
    (=: pos  Pos)
    (=: rota Rota)
    (=: sca  Sca)
    (=: mm Mm)
    
    (=: rgba Rgba)
    
    (=: uv Uv)
    (=: off-texel (car Off-Texel))))
        
        
################################################################################


(de get-vert-off (Cnt)
  (* (- Cnt 1) 208)) # use gl size


(dm cpy-to-ptr> (Ptr)
  (serialize-to-ptr> This Ptr)
  SZ-VERT)


(dm cpy-to-obj> (Ptr)
  (deserialize-fm-ptr> This Ptr))


(dm fmt-msg-send> (Off)
  (pack "(mc nodes NIL " SZ-VERT " " Off ")"))
  
  
(dm fmt-msg-recv> (Off)
  (pack "(mc NIL nodes " SZ-VERT " " Off ")"))
  
