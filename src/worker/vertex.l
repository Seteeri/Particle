(class +Vertex)
(with *Class
  (=: sz-gl 208)
  (=: sz-pl (+ 8 1 12 12 12 64 64 64 4 4))
  (=: ptr   (native "@" "malloc" 'N (: sz-pl)))
  
  (=: sca-vert    (*/ 1.0 1.0 32.0)) # ortho-scale
  (=: col-def (list 0.5137255 0.5803922 0.60784316 1.0 
                      0.5137255 0.5803922 0.60784316 1.0
                      0.5137255 0.5803922 0.60784316 1.0 
                      0.5137255 0.5803922 0.60784316 1.0))
  
  # These are font dependent,
  # so when multiple fonts loaded,
  # move these to +Metrics  
  
  # Adv is dependent upon char for proportional fonts
  (=: adv-glyph   19.265625)
  (=: adv-vert    (*/ (: adv-glyph)
                      (: sca-vert)
                      1.0))
  (=: adv-vert-2  (+ (: adv-vert)
                     (: adv-vert))))


(dm T (Off-Gl
       Ch       #Use to get offset, if char then data is char also
       RGBA
       Pos
       Rota
       Sca
       Val)

  # Ptr offset in render gl buffer
  (=: off-gl Off-Gl)
  (=: ch (char Ch))
  (=: val Val)  
  (=: rgba RGBA)      
  
  # Either keep these here or move to model matrix...
  (=: pos Pos)
  (=: rota Rota)
  (=: sca Sca)
  
  (=: mm (mat4-id))
  
  (=: cen)
  (=: mat4-cen)
  
  # Rel to Ch
  (=: uv)
  (=: off-texel)
  
  (=: dirt T)
      
  (when Ch
    (upd-met> This)
    (upd-mod> This)
    (upd-mod-mat> This)))


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


(dm cpy-to-ptr> (Ptr)
  (wr-ptr> This Ptr))


(dm cpy-to-obj> (Ptr)
  (rd-ptr> This Ptr))


(dm fmt-msg-send> (Off)
  (pack "(mc nodes NIL " (meta '(+Vertex) 'sz-pl) " " Off ")"))
  
  
(dm fmt-msg-recv> (Off)
  (pack "(mc NIL nodes " (meta '(+Vertex) 'sz-pl) " " Off ")"))
  
########################

# X / Y / NIL
(def '*pair-col-def 
     (list *selb-fg-0
           *selb-fg-1
           *selb-fg-0))

(de get-color-type (Any)
  #{
                        Cell
                          |
              +-----------+-----------+
              |           |           |
           Number       Symbol       Pair
                          |
                          |
     +--------+-----------+-----------+
     |        |           |           |
    NIL   Internal    Transient    External  
    
    Notes
    - Mark namespace symbol (or binary tree?)
    - Consolidate symbols?    
    - R G B = UNUSED NUM SYMS
    
    Base-01 = X Pair (darkest - most common)
    Base-1  = Y Pair (lightest - least common)
    Base-0  = NIL (med - 2nd most common)
        
    Yellow = Ptr
    Orange = ?
    Red = ?
    Magenta = ?
    
    Violet = Int/Sym
    Blue = Str,Box/Sym/Trans
    Cyan = Ext/Sym
    Green = Num
  }#

  # Adjust brightness for time...or alpha?

  (cond ((not Any)  (caddr *pair-col-def))
        ((lst? Any) (car *pair-col-def))
        ((num? Any) *selb-green)
        ((str? Any) *selb-blue)
        ((ext? Any) *selb-cyan)
        ((sym? Any) *selb-violet)))

# Small down tri
#(def '*char-dot (char (hex "02D1")))

# Small dash
#(def '*char-dot (char (hex "02D7")))

# Two vert  tris
#(def '*char-dot (char (hex "02D0")))
        

(def '*char-dot (char (hex "00B7")))
#(def '*char-dot (char (hex "2022")))
#(def '*char-dot (char (hex "2219")))
#(def '*char-dot (char (hex "25CF")))

(de make-vert-glyph (Ch Col Val)
  (inc '*cnt-v)
  (let ((R G B) Col
        Sca (meta '(+Vertex) 'sca-vert))
    (new '(+Vertex)
         (pop '*vert-offs)
         Ch
         (list R G B 1.0
               R G B 1.0
               R G B 1.0
               R G B 1.0)
         (need 3 0.0)
         (need 3 0.0)
         (need 3 Sca)
         Val)))


(de make-vert-repr (Repr Col)
  (make
    (for Ch Repr
      (case Ch
        ("^J" (link (make-vert-glyph "\^" Col)
                    (make-vert-glyph "J"  Col Ch)))
        ("^M" (link (make-vert-glyph "\^" Col)
                    (make-vert-glyph "M"  Col Ch)))
        ("^I" (link (make-vert-glyph "\^" Col)
                    (make-vert-glyph "I"  Col Ch)))
        (T    (link (make-vert-glyph Ch Col Ch)))))))

  
(de make-vert-str (Str Col)
  (let Verts (make-vert-repr (chop Str) Col)
    (list Verts
          (*/ (meta '(+Vertex) 'adv-vert) (num-to-fl (- (length Verts) 1)) 1.0))))

  
(de make-vert-num (Num Col)
  (let Verts (make-vert-repr (chop (format Num)) Col)
    (list Verts
          (*/ (meta '(+Vertex) 'adv-vert) (num-to-fl (- (length Verts) 1)) 1.0))))

          
(de make-vert-nil (Sym Col)
  (let Verts (make-vert-repr (chop "NIL") Col) # (list *char-dot)
    (list Verts
          (*/ (meta '(+Vertex) 'adv-vert) (num-to-fl (- (length Verts) 1)) 1.0))))          
          

(de make-vert-sym (Sym Col)
  (let Verts (make-vert-repr (chop (sym Sym)) Col)
    (list Verts
          (*/ (meta '(+Vertex) 'adv-vert) (num-to-fl (- (length Verts) 1)) 1.0))))
          

(de make-vert-atom (Atom Col)

  (unless Col (setq Col (get-color-type Atom)))

  (cond ((num? Atom)             (make-vert-num Atom Col))
        ((and (str? Atom) 
              (not (box? Atom))) (make-vert-str Atom Col))
        ((not Atom)              (make-vert-nil Atom Col))
        (T           (make-vert-sym Atom Col))))


(de make-vert-pair (Char Col Self)

  # (char 187) (char 166)
  
  (unless Char (setq Char *char-dot))
  (unless Col (setq Col (car *pair-col-def)))
  
  (if Self
      (list (list (make-vert-glyph Char Col) (make-vert-glyph Char Col))
            (meta '(+Vertex) 'adv-vert-2))
      (list (list (make-vert-glyph Char Col))
            (meta '(+Vertex) 'adv-vert))))                  
