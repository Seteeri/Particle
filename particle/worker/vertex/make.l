# X / Y / NIL
# Save fg for comments

(def '*ch-dot (char (hex "00B7")))

# Mov to +Vertex
(def '*cols-pair-def
     (list *selb-bg-1
           *selb-bg-1
           *selb-bg-2))

           
# https://unicode-table.com/en/sets/arrow-symbols/
(def '*ch-pair-def
     (list (char (hex "21B4"))
           (char (hex "21B3"))
           "NIL"))

           
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

  (cond ((not Any)  (caddr *cols-pair-def))
        ((lst? Any) (car *cols-pair-def))
        ((num? Any) *selb-green)
        ((str? Any) *selb-blue)
        ((ext? Any) *selb-cyan)
        ((sym? Any) *selb-violet)))


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

  
(de make-vert-str (Str Col Ch)
  (let Verts (make-vert-repr (chop Str) Col)
    (list Verts
          (*/ (meta '(+Vertex) 'adv-vert) (num-to-fl (- (length Verts) 1)) 1.0))))

  
(de make-vert-num (Num Col Ch)
  (let Verts (make-vert-repr (chop (format Num)) Col)
    (list Verts
          (*/ (meta '(+Vertex) 'adv-vert) (num-to-fl (- (length Verts) 1)) 1.0))))

          
(de make-vert-nil (Sym Col Ch)
  (let Verts (make-vert-repr (chop (if Ch @ "NIL"))
                             Col)
    (list Verts
          (*/ (meta '(+Vertex) 'adv-vert) (num-to-fl (- (length Verts) 1)) 1.0))))          
          

(de make-vert-sym (Sym Col Ch)
  (let Verts (make-vert-repr (chop (sym Sym)) Col)
    (list Verts
          (*/ (meta '(+Vertex) 'adv-vert) (num-to-fl (- (length Verts) 1)) 1.0))))
          

(de make-vert-atm (Atm Col Ch)

  # ch depends on atom
  (unless Col (setq Col (get-color-type Atm)))

  (cond ((num? Atm)             (make-vert-num Atm Col Ch))
        ((and (str? Atm) 
              (not (box? Atm))) (make-vert-str Atm Col Ch))
        ((not Atm)              (make-vert-nil Atm Col Ch))
        (T           (make-vert-sym Atm Col Ch))))


(de make-vert-pair (Ch Col Self)
  
  (unless Ch (setq Ch (car *ch-pair-def)))
  (unless Col (setq Col (car *cols-pair-def)))
  
  (if Self
      (list (list (make-vert-glyph Ch Col) (make-vert-glyph Ch Col))
            (meta '(+Vertex) 'adv-vert-2))
      (list (list (make-vert-glyph Ch Col))
            (meta '(+Vertex) 'adv-vert))))                  
