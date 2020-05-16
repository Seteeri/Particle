(class +Vertex)
(with *Class
  (=: sz-gl 208)
  (=: sz-pl (+ 8 1 12 12 12 64 64 64 4))
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
          (cons (: off-texel) 4))
  (meta '(+Vertex) 'sz-pl))


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

#{
  https://ethanschoonover.com/solarized/

  Thus in the case of a dark background colorscheme, the normal relationship for background and body text is base03:base0 (please note that body text is not base00). Note also that in cases where the background and foreground can be specified as a pair value, text can be highlighted using a combination of base02:base1. The L*a*b lightness difference between base03:base0 and base02:base1 is identical by design, resulting in identical readability against both normal and highlighted backgrounds. An example use case is folded text in Vim which uses base02 for the background and base1 for the foreground.

  The values in this example are simply inverted in the case of a light background.

          bg:fg
          
  dark  | base03:base0
  light | base02:base1
  high  | base02:base1
}#

(def '*sol-base-03 (list (*/ 1.0 0.0   255.0) (*/ 1.0 43.0  255.0) (*/ 1.0 54.0  255.0)))
(def '*sol-base-02 (list (*/ 1.0 0.0   255.0) (*/ 1.0 54.0  255.0) (*/ 1.0 66.0  255.0)))
(def '*sol-base-01 (list (*/ 1.0 88.0  255.0) (*/ 1.0 110.0 255.0) (*/ 1.0 117.0 255.0)))
(def '*sol-base-00 (list (*/ 1.0 101.0 255.0) (*/ 1.0 123.0 255.0) (*/ 1.0 131.0 255.0)))

(def '*sol-base-0  (list (*/ 1.0 131.0 255.0) (*/ 1.0 148.0 255.0) (*/ 1.0 150.0 255.0)))
(def '*sol-base-1  (list (*/ 1.0 147.0 255.0) (*/ 1.0 161.0 255.0) (*/ 1.0 161.0 255.0)))
(def '*sol-base-2  (list (*/ 1.0 238.0 255.0) (*/ 1.0 232.0 255.0) (*/ 1.0 213.0 255.0)))
(def '*sol-base-3  (list (*/ 1.0 253.0 255.0) (*/ 1.0 246.0 255.0) (*/ 1.0 227.0 255.0)))

(def '*sol-violet  (list (*/ 1.0 108.0 255.0) (*/ 1.0 113.0  255.0) (*/ 1.0 196.0 255.0)))
(def '*sol-blue    (list (*/ 1.0 38.0  255.0) (*/ 1.0 139.0  255.0) (*/ 1.0 210.0 255.0)))
(def '*sol-cyan    (list (*/ 1.0 42.0  255.0) (*/ 1.0 161.0  255.0) (*/ 1.0 152.0 255.0)))
(def '*sol-green   (list (*/ 1.0 133.0 255.0) (*/ 1.0 153.0  255.0) (*/ 1.0 0.0   255.0)))
(def '*sol-yellow  (list (*/ 1.0 181.0 255.0) (*/ 1.0 137.0  255.0) (*/ 1.0 0.0   255.0)))

(def '*sol-orange  (list (*/ 1.0 203.0 255.0) (*/ 1.0 75.0 255.0) (*/ 1.0 22.0  255.0)))
(def '*sol-red     (list (*/ 1.0 220.0 255.0) (*/ 1.0 50.0 255.0) (*/ 1.0 47.0  255.0)))
(def '*sol-magenta (list (*/ 1.0 211.0 255.0) (*/ 1.0 54.0 255.0) (*/ 1.0 130.0 255.0)))

# X / Y / NIL
(def '*pair-col-def (list *sol-base-01 *sol-base-1 *sol-base-0))

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
    
    Base-01 = X Pair (darkest - most common)
    Base-1  = Y Pair (lightest - least common)
    Base-0  = NIL (med - 2nd most common)
    
    Mark?
    - Namespace symbol (or binary tree?)
    
    Yellow = Ptr
    Orange = Num
    Red = ?
    Magenta = ?
    
    Violet = Int/Sym
    Blue = Str/Sym/Trans
    Cyan = Ext/Sym
    Green = Box/Sym/Trans
  }#

  # Adjust brightness for time...or alpha?

  (cond ((not Any)  (caddr *pair-col-def))
        ((lst? Any) (car *pair-col-def))
        ((num? Any) *sol-orange)
        ((box? Any) *sol-green)
        ((str? Any) *sol-blue)
        ((ext? Any) *sol-cyan)
        ((sym? Any) *sol-violet)))

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
  (let Verts (make-vert-repr (list *char-dot) Col)
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
