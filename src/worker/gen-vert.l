# bg:fg/text = base03:base0

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
    
    Base = X Pair
    Base-01 = Y Pair
    
    Yellow = Ptr
    Orange = NIL/T
    Red = Namespace symbol (or binary tree?)
    Magenta = Num
    
    Violet = Int/Sym
    Blue = Str/Sym/Trans
    Cyan = Ext/Sym
    Green = Box/Sym/Trans
  }#

  # Adjust brightness for time...or alpha?

  (cond ((or (== Any NIL)
             (== Any T))  *sol-orange)
        ((lst? Any)       *sol-base-0)
        ((num? Any)       *sol-magenta)
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

(de gen-vert-dot (Char Col)
  (gen-vert-glyph Char
                  (if Col @ *sol-base-0)))


(de gen-vert-glyph (Ch Col Val)
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


(de gen-vert-repr (Repr Col)
  (let Verts ()
    (for Ch Repr
      (case Ch
        ("^J" (push 'Verts (gen-vert-glyph "\^" Col)
                           (gen-vert-glyph "J"  Col Ch)))
        ("^M" (push 'Verts (gen-vert-glyph "\^" Col)
                           (gen-vert-glyph "M"  Col Ch)))
        ("^I" (push 'Verts (gen-vert-glyph "\^" Col)
                           (gen-vert-glyph "I"  Col Ch)))
        (T    (push 'Verts (gen-vert-glyph Ch Col Ch)))))
        
    (flip Verts)))

  
(de gen-vert-str (Str Col)
  (let Verts (gen-vert-repr (chop Str) Col)
    (list Verts
          (*/ (meta '(+Vertex) 'adv-vert) (num-to-fl (- (length Verts) 1)) 1.0))))

  
(de gen-vert-num (Num Col)
  (let Verts (gen-vert-repr (chop (format Num)) Col)
    (list Verts
          (*/ (meta '(+Vertex) 'adv-vert) (num-to-fl (- (length Verts) 1)) 1.0))))


(de gen-vert-sym (Sym Col)
  (let Verts (gen-vert-repr (chop (sym Sym)) Col)
    (list Verts
          (*/ (meta '(+Vertex) 'adv-vert) (num-to-fl (- (length Verts) 1)) 1.0))))
          

(de gen-vert-atom (Atom Col)

  (unless Col (setq Col (get-color-type Atom)))

  (cond ((num? Atom)             (gen-vert-num Atom Col))
        ((and (str? Atom) 
              (not (box? Atom))) (gen-vert-str Atom Col))
        (T           (gen-vert-sym Atom Col))))


(de gen-vert-pair (Col Self Char)

  # (char 187) (char 166)
  # (gen-vert-chars "[" *sol-base-0 T)
  
  (unless Char (setq Char *char-dot))
  
  (if Self
      (list (list (gen-vert-dot Char Col) (gen-vert-dot Char Col))
            (meta '(+Vertex) 'adv-vert-2))
      (list (list (gen-vert-dot Char Col))
            (meta '(+Vertex) 'adv-vert))))
