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

  (cond ((== Any NIL)     (caddr *pair-col-def))
        ((lst? Any)       (car *pair-col-def))
        ((num? Any)       *sol-orange)
        ((box? Any)       *sol-green)
        ((str? Any)       *sol-blue)
        ((ext? Any)       *sol-cyan)
        ((sym? Any)       *sol-violet)))

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


(de make-vert-sym (Sym Col)
  (let Verts (make-vert-repr (chop (sym Sym)) Col)
    (list Verts
          (*/ (meta '(+Vertex) 'adv-vert) (num-to-fl (- (length Verts) 1)) 1.0))))
          

(de make-vert-atom (Atom Col)

  (unless Col (setq Col (get-color-type Atom)))

  (cond ((num? Atom)             (make-vert-num Atom Col))
        ((and (str? Atom) 
              (not (box? Atom))) (make-vert-str Atom Col))
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
