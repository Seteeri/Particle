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
    Blue-Green = Data
    Green-Red = Sub-Data
    Red-Blue = 
  
    Violet = STR
    Blue =   SYM
      * Extern/Box have physical delimiters, {} and $...
      so can also fall under symbols
    Cyan =   NUM
    Green =  CONS
    Yellow = selected
    Orange = T/NIL and builtin symbols?
      * Builtin > FUN
    Red = FUN
    Magenta = Comments?

    * What to use base colors for?
    * Comments are considered strings
      * When save/load, use a form: (com blah)
    * Other possibilities: built-in keywords
    * Make STR base-01/base-0
      * Double Quotes and Delimeters base-0 (less noticeable)
    * Comments are really just strings...
      * On load: convert to STR
      * On save: convert to comment
      
  TODO: Use assoc list instead
  }#

  # Adjust brightness for time

  (cond ((== Any NIL) *col-yellow)
        ((lst? Any)
         (if (lst? (cdr Any))
             *col-base-0
             *col-green)) # pair
        (T
         (cond ((== Any T)  *col-yellow)
               ((num? Any)  *col-blue)
               ((box? Any)  *col-orange)
               ((ext? Any)  *col-magenta)
               ((str? Any)  *col-base-01)
               ((sym? Any)  *col-violet)))))


(de draw-dot (Adv)
  (draw-glyph (char 183) *col-base-0)
  (when Adv (adv-ptr)))


(de draw-chars (Chars Col Adv)
  # Mostly used to draw non-code and accents etc.

  (let Verts ()

    (for C (chop Chars)
      (push 'Verts (draw-glyph C Col))
      (when Adv (adv-ptr)))

    (flip Verts)))


(de draw-glyph (Char Col)

  (let Vert (pop '*verts)

    # Reset vertex
    # Pos = baseline = X,0,0
    (with Vert
      (=: pos (get *vert-ptr 'pos))
      (=: rgba (list (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0)))

    (update-glyph> Vert Char)
    (update-model-matrix> Vert)
    (cmd-update-vert *ipc Vert)

    (inc '*cnt-v)

    Vert))


(de draw-repr (Repr Col Adv)

  (let Verts ()

    (for C Repr
      (push 'Verts (draw-glyph C Col))
      (when Adv (adv-ptr)))

    (flip Verts)))

  
(de draw-str (Str Col Adv) (draw-repr (chop (pack "\"" Str "\"")) Col Adv))
(de draw-num (Num Col Adv) (draw-repr (chop (format Num)) Col Adv))
(de draw-sym (Sym Col Adv) (draw-repr (chop (sym Sym)) Col Adv))


(de draw-atom (Atom Col Adv) 
  (let Col-2 (if Col Col (get-color-type Atom))
    (cond ((num? Atom) (draw-num Atom Col-2 Adv))
          ((str? Atom) (draw-str Atom Col-2 Adv))
          (T           (draw-sym Atom Col-2 Adv)))))

