# bg:fg/text = base03:base0

(def '*col-base-03 (list (*/ 1.0 0.0   255.0) (*/ 1.0 43.0  255.0) (*/ 1.0 54.0  255.0)))
(def '*col-base-02 (list (*/ 1.0 0.0   255.0) (*/ 1.0 54.0  255.0) (*/ 1.0 66.0  255.0)))
(def '*col-base-01 (list (*/ 1.0 88.0  255.0) (*/ 1.0 110.0 255.0) (*/ 1.0 117.0 255.0)))
(def '*col-base-00 (list (*/ 1.0 101.0 255.0) (*/ 1.0 123.0 255.0) (*/ 1.0 131.0 255.0)))

(def '*col-base-0  (list (*/ 1.0 131.0 255.0) (*/ 1.0 148.0 255.0) (*/ 1.0 150.0 255.0)))
(def '*col-base-1  (list (*/ 1.0 147.0 255.0) (*/ 1.0 161.0 255.0) (*/ 1.0 161.0 255.0)))
(def '*col-base-2  (list (*/ 1.0 238.0 255.0) (*/ 1.0 232.0 255.0) (*/ 1.0 213.0 255.0)))
(def '*col-base-3  (list (*/ 1.0 253.0 255.0) (*/ 1.0 246.0 255.0) (*/ 1.0 227.0 255.0)))

(def '*col-violet  (list (*/ 1.0 108.0 255.0) (*/ 1.0 113.0  255.0) (*/ 1.0 196.0 255.0)))
(def '*col-blue    (list (*/ 1.0 38.0  255.0) (*/ 1.0 139.0  255.0) (*/ 1.0 210.0 255.0)))
(def '*col-cyan    (list (*/ 1.0 42.0  255.0) (*/ 1.0 161.0  255.0) (*/ 1.0 152.0 255.0)))
(def '*col-green   (list (*/ 1.0 133.0 255.0) (*/ 1.0 153.0  255.0) (*/ 1.0 0.0   255.0)))
(def '*col-yellow  (list (*/ 1.0 181.0 255.0) (*/ 1.0 137.0  255.0) (*/ 1.0 0.0   255.0)))

(def '*col-orange  (list (*/ 1.0 203.0 255.0) (*/ 1.0 75.0 255.0) (*/ 1.0 22.0  255.0)))
(def '*col-red     (list (*/ 1.0 220.0 255.0) (*/ 1.0 50.0 255.0) (*/ 1.0 47.0  255.0)))
(def '*col-magenta (list (*/ 1.0 211.0 255.0) (*/ 1.0 54.0 255.0) (*/ 1.0 130.0 255.0)))


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

    Yellow = NIL (T?)
    Orange = Builtins
    Red = Pair
    Magenta = Num
    
    Violet = Int/Sym
    Blue = Str/Sym/Trans
    Cyan = Ext/Sym
    Green = Box/Sym/Trans

    Base = List
    
    White/Black = Ptrs
  }#

  # Adjust brightness for time...or alpha?

  (cond ((or (== Any NIL)
             (== Any T))  *col-yellow)
        ((lst? Any)       *col-base-0)
        ((num? Any)       *col-magenta)
        ((box? Any)       *col-green)
        ((str? Any)       *col-blue)
        ((ext? Any)       *col-cyan)
        ((sym? Any)       *col-violet)))


(def '*char-dot (char 183))

(de gen-vert-dot (Col)
  (gen-vert-glyph *char-dot 
              (if Col @ *col-base-0)))


(de gen-vert-glyph (Char Col)

  (let (Vert (pop '*verts)
        (R G B) Col)
    
    (inc '*cnt-v)
    
    (with Vert
    
      (=: pos *cur)
    
      (=: rgba (list R G B 1.0
                     R G B 1.0
                     R G B 1.0
                     R G B 1.0)))

    (upd-gly> Vert Char)
    
    (upd-mod-mat> Vert)

    Vert))


(de gen-vert-repr (Repr Col)
  # To handle newline, move cursor when char encountered
  
  (let Verts ()
    (for C Repr
      (push 'Verts (gen-vert-glyph C Col))
      (adv-cur))
    (flip Verts)))

  
(de gen-vert-str (Str Col)
  (let Verts (gen-vert-repr (chop Str) Col)
    (list Verts
          (*/ *adv-vert (num-to-fl (- (length Verts) 1)) 1.0))))

  
(de gen-vert-num (Num Col)
  (let Verts (gen-vert-repr (chop (format Num)) Col)
    (list Verts
          (*/ *adv-vert (num-to-fl (- (length Verts) 1)) 1.0))))


(de gen-vert-sym (Sym Col)
  (let Verts (gen-vert-repr (chop (sym Sym)) Col)
    (list Verts
          (*/ *adv-vert (num-to-fl (- (length Verts) 1)) 1.0))))
          

(de gen-vert-atom (Atom Col)

  (unless Col (setq Col (get-color-type Atom)))

  (cond ((num? Atom)             (gen-vert-num Atom Col))
        ((and (str? Atom) 
              (not (box? Atom))) (gen-vert-str Atom Col))
        (T           (gen-vert-sym Atom Col))))


(de gen-vert-pair (Col Self)

  # (char 187) (char 166)
  # (gen-vert-chars "[" *col-base-0 T)
  
  (if Self
      (list (list (gen-vert-dot Col) (gen-vert-dot Col))
            *adv-vert-2)
      (list (list (gen-vert-dot Col))
            *adv-vert)))


#{
(de gen-vert-chars (Chars Col)
  # Mostly used to draw non-code and accents etc.

  (let Verts ()

    (for C (chop Chars)
    
      (let Vert (gen-vert-glyph C Col)
        
        (push 'Verts Vert)
        (cmd-upd-vert *ipc Vert))
    
      (adv-cur))
      
    (adv-cur -1.0)

    (flip Verts)))
}#
