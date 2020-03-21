(de base-cur-x (Ref)
  # Move cur to x base of Ref and y origin of Ref
  (let Bounds (if (= Ref *part-main)
                  (calc-bounds> Ref T T)
                  (calc-bounds> Ref NIL T))
    (set-cur-x (get Bounds 1))
    (set-cur-y (get (get-origin> Ref) 2))))

        
(de base-cur-y (Ref)
  # Move cur to x origin of Ref and y base of Ref
  (let Bounds (if (= Ref *part-main)
                  (calc-bounds> Ref T T)
                  (calc-bounds> Ref NIL T))
    (set-cur-x (get (get-origin> Ref) 1))
    (set-cur-y (get Bounds 2))))


(de base-cur (Ref)
  # x: CDR is to the right; use X bounds
  # y: CDR is below; use Y bounds
  (if (= (get Ref 'lay) 'x)
    (prog (base-cur-x Ref) (adv-cur 2.0))
    (prog (base-cur-y Ref) (nl-cur 2.0))))

######################

(de repl-cdr (Any Lay)
  # Replace CDR

  (let (Ref (val> p0)
        Part (con-list Any Lay))
    (base-cur Ref)
    (lay-part Part *cur)
    Part))


(de con-list (Any Lay)
  (let (Ref (val> p0)
        Cdr (get Ref 'b)
        Lst-Any (list Any)
        Part (gen-part Lst-Any Lay NIL T))
    
    # Ref -> Cdr

    # Ref -> Part
    (put Ref 'b Part)
    
    # Ref -> Part -> Cdr
    (put Part 'b Cdr)
    
    # Connect new part data to list part data
    # Need separate function if not at end
    #   (conc (: data) Lst-Any (get (get Ref 'b) 'data))
    (unless (= Ref *part-main)
      (with Ref
        (con (car (:: data)) Lst-Any)))
    
    # Remember, *part-main's value is always the particle itself
    # The value of the symbol *part-main is actually 'b
    #(println (get (get *part-main 'b) 'data))
        
    Part))

######################

(de repl-ref (Any Lay)
  # Replace entire Ref (CAR)

  # Handle case when there is something after ref

  (let (Ref (val> p0)
        Pre (let Pre (car *part-lsts)
              (loop
                (NIL (get (get (get Pre 'b) 'b) 'b))
                (setq Pre (get Pre 'b)))
              Pre)
        Lst-Any (list (list Any))
        Part (gen-part Lst-Any Lay))
        
    (repl-list Part Pre)

    # Don't use base-cur since cur at start

    # Either use Ref or Last

    # HANDLE Y LAYOUT!
    
    # Move new part at extents of Pre
    (base-cur Pre)
    (lay-part Part *cur)

    # Move car one space further down
    (mov-cur-part Part 'start)
    (nl-cur 3.0)
    (with Part
      (put (: a) 'lay Lay)
      (lay-part (: a) *cur))

    Part))


(de repl-list (Part Pre)
      
  # Delete ref
  (let Ref (val> p0)
    (del> Ref T T))
  
  # Connect cdr of ref to particle b
  # WATCH FOR NIL

  # Unlink ref
  (put Pre 'b) # data still there
  (put p0 'b)

  (with Pre
    (=: b Part)
    (con (car (:: data)) (get Part 'data))))
