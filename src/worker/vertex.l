#Node tex dims are all 96x96
(def '*dims-tex-node (list 96 96))

(def '*color-vert-default (list 0.5137255 0.5803922 0.60784316 1.0 
                                0.5137255 0.5803922 0.60784316 1.0
				                        0.5137255 0.5803922 0.60784316 1.0 
                                0.5137255 0.5803922 0.60784316 1.0))


(def 'SZ-VERT (+ 8 1 12 12 12 64 64 64 4))
(def 'SZ-GL-VERT 208)

# Reuse across all vertices for txfer
(def '*ptr-vert (native "@" "malloc" 'N SZ-VERT))


(class +Vertex)
(dm T (Off-Gl
       Ch       #Use to get offset, if char then data is char also
       RGBA
       Pos
       Rota
       Sca)

  # Offset into render gl buffer
  (=: off-gl Off-Gl)

  (=: ch Ch)
  
  # POSS:
  # Either keep these here or move to model matrix...
  # Instead of storing pos/rot/sca, directly modify mm
  # - Downside is mm must be calculated repeatedly
  (=: pos Pos)
  (=: rota Rota)
  (=: sca Sca)
  (=: mm (mat4-identity))
  
  (=: rgba RGBA)      
  (=: uv)
  (=: off-texel)
  
  (when Ch
    (upd-ch> This Ch)
    (upd-mod-mat> This)))


(dm upd-ch> (Ch)
  
  (when Ch
    (=: ch (char Ch)))

  (=: off-texel (* (- (: ch) 1)
                   (get *dims-tex-node 1)
                   (get *dims-tex-node 2)))

  (unless (: met)
    (=: met (get *metrics (: ch)))
    (unless (: met)
      (prinl-info "upd-ch>" (pack "Unable to find metrics: " (: ch)))))
                   
  (let? Met (: met)
        
    (=: uv (get Met 'uv))
    (=: pos (pos> Met (: pos)))
    (=: sca (sca> Met))))
        
         
(dm upd-mod-mat> ()
  (=: mm (mat4-mul (mat4-translation (: pos))
                   (mat4-mul (mat4-rotation-z (get (: rota) 3))
                             (mat4-mul (mat4-rotation-y (get (: rota) 2))
                                       (mat4-mul (mat4-rotation-x (get (: rota) 1))
                                                 (mat4-scaling (: sca))))))))
         

(de wr-mat-to-ptr (M Ptr)
  (struct Ptr 'N (cons -1.0 M)))


(dm wr-ptr> (Ptr)
  (struct Ptr 'N
          (cons (: off-gl) 8)
          (: ch)
          (cons -1.0 (: pos))
          (cons -1.0 (: rota))
          (cons -1.0 (: sca))
          (cons -1.0 (: mm))
          (cons -1.0 (: rgba))          
          (cons -1.0 (: uv))
          (cons (: off-texel) 4))
  SZ-VERT)


(dm rd-ptr> (Ptr)

  (let ((Off-Gl Ch Pos Rota Sca Mm Rgba Uv Off-Texel)
        (struct Ptr 
                '((N . 1)
                  (B . 1)
                  (-1.0 . 3)
                  (-1.0 . 3)
                  (-1.0 . 3)
                  (-1.0 . 16)
                  (-1.0 . 16)
                  (-1.0 . 16)
                  (I . 1))))

    (=: off-gl Off-Gl)
    
    (=: ch (car Ch))
        
    (=: pos  Pos)
    (=: rota Rota)
    (=: sca  Sca)
    (=: mm Mm)
    
    (=: rgba Rgba)
    
    (=: uv Uv)
    (=: off-texel (car Off-Texel))))
        
        
################################################################################

(de get-vert-ix (Off)
  (+ (/ Off SZ-GL-VERT) 1))

(de get-vert-off (Cnt)
  (* (- Cnt 1) SZ-GL-VERT)) # use gl size


(dm cpy-to-ptr> (Ptr)
  (wr-ptr> This Ptr)
  SZ-VERT)


(dm cpy-to-obj> (Ptr)
  (rd-ptr> This Ptr))


(dm fmt-msg-send> (Off)
  (pack "(mc nodes NIL " SZ-VERT " " Off ")"))
  
  
(dm fmt-msg-recv> (Off)
  (pack "(mc NIL nodes " SZ-VERT " " Off ")"))
  
