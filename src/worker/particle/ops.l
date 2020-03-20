(de base-cur-x (Ref)
  # Move cur to x base of Ref and y origin of Ref
  (let Bounds NIL
    (if (= Ref *part-main)
        (setq Bounds (calc-bounds> Ref T T))
        (setq Bounds (calc-bounds> Ref NIL T)))
    (set-cur-x (get Bounds 1))
    (set-cur-y (get (get-origin> Ref) 2)))
  (adv-cur 2.0))

        
(de base-cur-y (Ref)
  # Move cur to x origin of Ref and y base of Ref
  (let Bounds NIL
    (if (= Ref *part-main)
        (setq Bounds (calc-bounds> Ref T T))
        (setq Bounds (calc-bounds> Ref NIL T)))
    (println 'Bounds Bounds)
    (set-cur-x (get (get-origin> Ref) 1))
    (set-cur-y (get Bounds 2)))
  (nl-cur 2.0))


(de con-any (Any Lay)

  (let (Ref (val> p0)
        Dat-Ref (car (get Ref 'data))
        Lay-Ref (get Ref 'lay)        
        Part (con-list Any Lay))
        
    # x: CDR is to the right; use X bounds
    # y: CDR is below; use Y bounds
    (if (= Lay-Ref 'x)
      (base-cur-x Ref) 
      (base-cur-y Ref))
        
    (lay-part Part *cur)
    
    Part))


(de con-list (Any Lay)
  (let (Lst-Any (list Any)
        Val (val> p0)
        Cdr (get Val 'b)
        Part (gen-part Lst-Any NIL T))

    # MOVE TO GEN-PART
    (put Part 'lay Lay)
    (put (get Part 'a) 'lay Lay)
    (put (get Part 'b) 'lay Lay)

    # Set cdr of cur-part to new-part
    # [c] -> [n]
    (put Val 'b Part)
    
    # Set cdr of new-part to cdr-part
    # [n] -> [b] -> ...
    (put Part 'b Cdr)
    
    # Connect new part data to list part data
    # Need separate function if not at end
    (with *part-main
      (=: data (conc (: data) Lst-Any)))
      
    Part))


(de repl-any (Any Lay)
  # Use Lay

  (let (Ref (val> p0)
        Dat-Ref (car (get Ref 'data))
        Last (let Last (car *part-lsts)
                (loop
                  (NIL (get (get (get Last 'b) 'b) 'b))
                  (setq Last (get Last 'b)))
                Last)
        Lst-Any (list (list Any))
        Part (gen-part Lst-Any))
        
    (repl-list Part Last)

    # Mov cursor below
    (mov-cur-part Last 'end) (adv-cur 2.0)
    
    (put Part 'lay 'x)
    (lay-part Part *cur)

    # Move inner list one space further down
    (mov-cur-part Last 'end) (adv-cur 2.0) (nl-cur 3.0)
    
    (with Part
      (put (: a) 'lay 'x)
      (lay-part (: a) *cur))

    Part))


(de repl-list (Part Pre)
  # Pass Lay
  #
  # Replace NIL with List
  #
  # Handle case when there is something after ref
    
  # Delete particle first
  (del> (val> p0) T T)
  (put Pre 'b) # data still there
  (put p0 'b)

  # Set cursor 2 spaces above ptr
  (nl-cur -2.0)

  (with Pre
    (=: b Part)
    (con (car (:: data)) (get Part 'data))))
