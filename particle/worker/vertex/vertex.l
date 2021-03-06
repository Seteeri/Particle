# X / Y / NIL
# Save fg for comments
# https://unicode-table.com/en/sets/arrow-symbols/

(def '*ch-dot (char (hex "00B7")))

(class +Vertex)
(with *Class
  (=: sz-gl 208)
  (=: sz-pl (+ 8 1 12 12 12 64 64 64 4 4))
  (=: ptr   (native "@" "malloc" 'N (: sz-pl)))
  
  # init should set this from ortho-scale
  (=: sca-vert    (*/ 1.0 1.0 32.0))
  (=: col-def     *selb-fg-0)

  (=: cols-pair
      (list *selb-bg-1
            *selb-bg-1
            *selb-bg-2))

  (=: chs-pair
      (list (char (hex "21B4"))
            (char (hex "21B3"))
            "NIL"))                 
                      
  # +Font should set this
  # Or create class and store as prop
  #
  # Adv is dependent upon char for proportional fonts
  (=: adv-glyph   19.265625)
  
  (=: adv-vert    (*/ (: adv-glyph)
                      (: sca-vert)
                      1.0))
  (=: adv-vert-2  (+ (: adv-vert)
                     (: adv-vert))))


(dm T (Off-Gl
       Ch       #Use to get offset, if char then data is char also
       Rgba
       Pos
       Rota
       Sca
       Val)

  # Ptr offset in render gl buffer
  (=: off-gl Off-Gl)
  (=: ch (char Ch))
  (=: val Val)  
  (=: rgba Rgba)      
  
  # Either keep these here or move to model matrix...
  (=: pos Pos)
  (=: rota Rota)
  (=: sca Sca)
  
  (=: mm (mat4-id))
  
  (=: cen)
  (=: mat4-cen)
  
  # Reg to Ch
  (=: uv)
  (=: off-texel)
  
  (=: dirt T)
      
  (when Ch
    (upd-met> This)
    (upd-mod> This)
    (upd-mod-mat> This)))


(dm del> ()
  (send-msg-rend (pack "(zv " (: off-gl) '")"))
  #(println "+Vert.del>" This)
  (push '*vert-offs (: off-gl))
  (=: off-gl NIL)
  (dec '*cnt-v))
    
    
(dm upd-met> (Ch)

  (when Ch
    (=: ch (char Ch)))

  (=: off-texel (lup-tex-ixs-cdr> *font (: ch)))
  
  (unless (: off-texel)
    (prinl-info "upd-met>" (pack "Unable to find off-texel: " (: ch))))
  
  (=: met (lup-mets-cdr> *font (: ch)))
  
  (unless (: met)
    (prinl-info "upd-met>" (pack "Unable to find metrics: " (: ch))))
  
  (unless (get (: met) 'cen)
    (calc-cen> (: met)))
  (=: mat4-cen (get (: met) 'mat4-cen))
  (=: mat4-cen-i (get (: met) 'mat4-cen-i))
                 
  (=: uv (get (: met) 'uv))
  
  # Convert rx shape -> px
  (=: rx (*/ (get (: met) 'range)
             (get (: met) 'scale)
             1.0))  
  (=: uv (place 3
                (: uv)
                (: rx)))
  
  This)

    
(dm upd-mod> ()
  (let? Met (: met)
    (=: pos (pos> Met (: pos)))
    (=: sca (sca> Met))
    (upd-mod-mat> This)))
        
         
(dm upd-mod-mat> ()
  # T * R * S

  (=: mm
      (mat4-mul@
        (mat4-tra (: pos))
        (mat4-rz (caddr (: rota)))
        (mat4-ry (cadr  (: rota)))
        (mat4-rx (car   (: rota)))
        (mat4-sca (: sca)))))


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
          (cons (: off-texel) 4)
          (cons -1.0 (list (: rx)))
          )
  (meta '(+Vertex) 'sz-pl))


(dm rd-ptr> (Ptr)

  (let ((Off-Gl Ch Pos Rota Sca Mm Rgba Uv Off-Texel Rx)
        (struct Ptr 
                '((N . 1)
                  (B . 1)
                  (-1.0 . 3)
                  (-1.0 . 3)
                  (-1.0 . 3)
                  (-1.0 . 16)
                  (-1.0 . 16)
                  (-1.0 . 16)
                  (I . 1)
                  (-1.0 . 1)
                  )))

    (=: off-gl Off-Gl)
    (=: ch (car Ch))
    (=: pos  Pos)
    (=: rota Rota)
    (=: sca  Sca)
    
    (=: mm Mm)
    (=: rgba Rgba)
    (=: uv Uv)
    (=: off-texel (car Off-Texel))
    
    (=: rx Rx)
    ))
        
        
(de get-vert-ix (Off)
  (+ (/ Off (meta '(+Vertex) 'sz-gl)) 1))

  
(de vert-off-gl (Cnt)
  (* (- Cnt 1)
     (meta '(+Vertex) 'sz-gl))) # use gl size


(dm fmt-msg-send> (Off)
  (pack "(mc nodes NIL " (meta '(+Vertex) 'sz-pl) " " Off ")"))
  
  
(dm fmt-msg-recv> (Off)
  (pack "(mc NIL nodes " (meta '(+Vertex) 'sz-pl) " " Off ")"))
