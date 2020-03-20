(de base-cur-x (Ref)
  # Move cur to x base of Ref and y origin of Ref
  (let Bounds (if (= Ref *part-main)
                  (calc-bounds> Ref T T)
                  (calc-bounds> Ref NIL T))
    (set-cur-x (get Bounds 1))
    (set-cur-y (get (get-origin> Ref) 2)))
  (adv-cur 2.0))

        
(de base-cur-y (Ref)
  # Move cur to x origin of Ref and y base of Ref
  (let Bounds (if (= Ref *part-main)
                  (calc-bounds> Ref T T)
                  (calc-bounds> Ref NIL T))
    (set-cur-x (get (get-origin> Ref) 1))
    (set-cur-y (get Bounds 2)))
  (nl-cur 2.0))


(de base-cur (Ref)
  # x: CDR is to the right; use X bounds
  # y: CDR is below; use Y bounds
  (if (= (get Ref 'lay) 'x)
    (base-cur-x Ref) 
    (base-cur-y Ref)))


(de con-any (Any Lay)
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

    # Set cdr of cur-part to new-part
    # [c] -> [n]
    (put Ref 'b Part)
    
    # Set cdr of new-part to cdr-part
    # [n] -> [b] -> ...
    (put Part 'b Cdr)
    
    # Connect new part data to list part data
    # Need separate function if not at end
    (with *part-main
      (=: data (conc (: data) Lst-Any)))
      
    Part))


(de repl-any (Any Lay)
  # Handle case when there is something after ref

  (let (Ref (val> p0)
        Last (let Last (car *part-lsts)
                (loop
                  (NIL (get (get (get Last 'b) 'b) 'b))
                  (setq Last (get Last 'b)))
                Last)
        Lst-Any (list (list Any))
        Part (gen-part Lst-Any Lay))
        
    (repl-list Part Last)

    # We don't use base-cur since we move to beginning

    # Mov cursor below
    (mov-cur-part Last 'end)    
    (adv-cur 2.0)
    
    (lay-part Part *cur)

    # Move inner list one space further down
    (mov-cur-part Last 'end)
    (adv-cur 2.0) (nl-cur 3.0)
    
    (with Part
      (put (: a) 'lay Lay)
      (lay-part (: a) *cur))

    Part))


(de repl-list (Part Pre)
    
  # Delete particle first
  (del> (val> p0) T T)
  (put Pre 'b) # data still there
  (put p0 'b)

  (with Pre
    (=: b Part)
    (con (car (:: data)) (get Part 'data))))
