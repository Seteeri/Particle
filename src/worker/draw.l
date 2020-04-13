#{
  TODO: 
  * Batch updates
}#


(def '*space-line (+ *adv-vert *adv-vert))

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
    Violet = SYM
    Blue = NUM/STR
    Cyan = Box
    Green = Ext
    Yellow = NIL/T
    Orange = 
    Red = 
    Magenta = 

    * What to use base colors for?
    * Comments are considered strings
      * When save/load, use a form: (com blah)
    * Other possibilities: built-in keywords
    * Make STR base-01/base-0
      * Double Quotes and Delimeters base-0 (less noticeable)
    * Comments are really just strings...
      * On load: convert to STR
      * On save: convert to comment
  }#

  # Adjust brightness for time...or alpha?

  (cond ((== Any NIL) *col-yellow)
        ((lst? Any) *col-base-0)
        (T
         (cond ((== Any T)  *col-yellow)
               ((num? Any)  *col-blue)
               ((box? Any)  *col-orange)
               ((str? Any)  *col-base-01)
               ((ext? Any)  *col-magenta)
               ((sym? Any)  *col-violet)))))


(def '*char-dot (char 183))

(de draw-dot (Col)
  (draw-glyph *char-dot 
              (if Col Col *col-base-0)))


(de draw-glyph (Char Col)

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


(de draw-repr (Repr Col)
  # To handle newline, move cursor when char encountered
  
  (let Verts ()
    (for C Repr
      (push 'Verts (draw-glyph C Col))
      (adv-cur))
    (flip Verts)))

  
(de draw-str (Str Col)
  (let Verts (draw-repr (chop Str) Col)
    (list Verts
          (*/ *adv-vert (num-to-fl (- (length Verts) 1)) 1.0))))

  
(de draw-num (Num Col)
  (let Verts (draw-repr (chop (format Num)) Col)
    (list Verts
          (*/ *adv-vert (num-to-fl (- (length Verts) 1)) 1.0))))


(de draw-sym (Sym Col)
  (let Verts (draw-repr (chop (sym Sym)) Col)
    (list Verts
          (*/ *adv-vert (num-to-fl (- (length Verts) 1)) 1.0))))
          

(de draw-atom (Atom Col)

  (unless Col (setq Col (get-color-type Atom)))

  (cond ((num? Atom) (draw-num Atom Col))
        ((str? Atom) (draw-str Atom Col))
        (T           (draw-sym Atom Col))))


(de draw-cell (Col Self)

  # (char 187) (char 166)
  # (draw-chars "[" *col-base-0 T)
  
  (if Self
      (list (list (draw-dot Col) (draw-dot Col))
            *adv-vert-2)
      (list (list (draw-dot Col))
            *adv-vert)))


#{
(de draw-chars (Chars Col)
  # Mostly used to draw non-code and accents etc.

  (let Verts ()

    (for C (chop Chars)
    
      (let Vert (draw-glyph C Col)
        
        (push 'Verts Vert)
        (cmd-upd-vert *ipc Vert))
    
      (adv-cur))
      
    (adv-cur -1.0)

    (flip Verts)))
}#
