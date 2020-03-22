(de base-cur (Ref)  
  # x: CDR is to the right; use X bounds
  # y: CDR is below; use Y bounds
  (let Bounds (if (= (get Ref 'data) '*part-main)
                  (calc-bounds> Ref T T)
                  (calc-bounds> Ref NIL T))
    (if (= (get Ref 'lay) 'x)
        (prog
          # Move cur to x base of Ref and y origin of Ref
          (set-cur-x (get Bounds 1))
          (set-cur-y (get (get-origin> Ref) 2))
          (adv-cur 2.0))
        (prog
          # Move cur to x origin of Ref and y base of Ref
          (set-cur-x (get (get-origin> Ref) 1))
          (set-cur-y (get Bounds 2))
          (nl-cur 2.0)))))


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
    
    # Back ref
    (put Part 'c Ref)
    
    # Connect new part data to list part data
    # Need separate function if not at end
    #   (conc (: data) Lst-Any (get (get Ref 'b) 'data))
    
    # Remember, *part-main's value is always the particle itself
    # The value of the symbol *part-main is actually 'b    
    
    #(println 'con-list Ref (type Ref) (get Ref 'data))
    
    (unless (isa '+Sym Ref)
      (with Ref
        (con (car (:: data)) Lst-Any)))
        
    Part))

######################

(de repl-ref (Any Lay)
  # Replace entire Ref (CAR)

  # Handle case when there is something after ref

  (let (Ref (val> p0)
        Prv (get Ref 'c)
        Lst-Any (list (list Any))
        Part (gen-part Lst-Any Lay))
        
    (repl-list Part Prv)

    # Don't use base-cur since cur at start

    # Either use Ref or Last

    # HANDLE Y LAYOUT!
    
    # Move new part at extents of Prv
    (base-cur Prv)
    (lay-part Part *cur)

    # Move car one space further down
    (mov-cur-part Part 'start)
    (nl-cur 3.0)
    (with Part
      (put (: a) 'lay Lay)
      (lay-part (: a) *cur))

    Part))


(de repl-list (Part Prv)
      
  # Delete ref
  (let Ref (val> p0)
    (del> Ref T T))
  
  # Connect cdr of ref to particle b
  # WATCH FOR NIL

  # Unlink ref
  (put Prv 'b) # data still there
  (put p0 'b)

  (with Prv
    (=: b Part)
    (con (car (:: data)) (get Part 'data)))

  #(println 'repl-list Prv (type Prv) (get Prv 'data))
    
  (put Part 'c Prv))


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
    
    # Back ref
    (put Part 'c Ref)
    
    # Connect new part data to list part data
    # Need separate function if not at end
    #   (conc (: data) Lst-Any (get (get Ref 'b) 'data))
    
    # Remember, *part-main's value is always the particle itself
    # The value of the symbol *part-main is actually 'b    
    
    #(println 'con-list Ref (type Ref) (get Ref 'data))
    
    (unless (isa '+Sym Ref)
      (with Ref
        (con (car (:: data)) Lst-Any)))
        
    Part))

######################

(de repl-ref (Any Lay)
  # Replace entire Ref (CAR)

  # Handle case when there is something after ref

  (let (Ref (val> p0)
        Prv (get Ref 'c)
        Lst-Any (list (list Any))
        Part (gen-part Lst-Any Lay))
        
    (repl-list Part Prv)

    # Don't use base-cur since cur at start

    # Either use Ref or Last

    # HANDLE Y LAYOUT!
    
    # Move new part at extents of Prv
    (base-cur Prv)
    (lay-part Part *cur)

    # Move car one space further down
    (mov-cur-part Part 'start)
    (nl-cur 3.0)
    (with Part
      (put (: a) 'lay Lay)
      (lay-part (: a) *cur))

    Part))


(de repl-list (Part Prv)
      
  # Delete ref
  (let Ref (val> p0)
    (del> Ref T T))
  
  # Connect cdr of ref to particle b
  # WATCH FOR NIL

  # Unlink ref
  (put Prv 'b) # data still there
  (put p0 'b)

  (with Prv
    (=: b Part)
    (con (car (:: data)) (get Part 'data)))

  #(println 'repl-list Prv (type Prv) (get Prv 'data))
    
  (put Part 'c Prv))
(de base-cur (Ref)  
  # x: CDR is to the right; use X bounds
  # y: CDR is below; use Y bounds
  (let Bounds (if (= (get Ref 'data) '*part-main)
                  (calc-bounds> Ref T T)
                  (calc-bounds> Ref NIL T))
    (if (= (get Ref 'lay) 'x)
        (prog
          # Move cur to x base of Ref and y origin of Ref
          (set-cur-x (get Bounds 1))
          (set-cur-y (get (get-origin> Ref) 2))
          (adv-cur 2.0))
        (prog
          # Move cur to x origin of Ref and y base of Ref
          (set-cur-x (get (get-origin> Ref) 1))
          (set-cur-y (get Bounds 2))
          (nl-cur 2.0)))))


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
    
    # Back ref
    (put Part 'c Ref)
    
    # Connect new part data to list part data
    # Need separate function if not at end
    #   (conc (: data) Lst-Any (get (get Ref 'b) 'data))
    
    # Remember, *part-main's value is always the particle itself
    # The value of the symbol *part-main is actually 'b    
    
    #(println 'con-list Ref (type Ref) (get Ref 'data))
    
    (unless (isa '+Sym Ref)
      (with Ref
        (con (car (:: data)) Lst-Any)))
        
    Part))

######################

(de repl-ref (Any Lay)
  # Replace entire Ref (CAR)

  # Handle case when there is something after ref

  (let (Ref (val> p0)
        Prv (get Ref 'c)
        Lst-Any (list (list Any))
        Part (gen-part Lst-Any Lay))
        
    (repl-list Part Prv)

    # Don't use base-cur since cur at start

    # Either use Ref or Last

    # HANDLE Y LAYOUT!
    
    # Move new part at extents of Prv
    (base-cur Prv)
    (lay-part Part *cur)

    # Move car one space further down
    (mov-cur-part Part 'start)
    (nl-cur 3.0)
    (with Part
      (put (: a) 'lay Lay)
      (lay-part (: a) *cur))

    Part))


(de repl-list (Part Prv)
      
  # Delete ref
  (let Ref (val> p0)
    (del> Ref T T))
  
  # Connect cdr of ref to particle b
  # WATCH FOR NIL

  # Unlink ref
  (put Prv 'b) # data still there
  (put p0 'b)

  (with Prv
    (=: b Part)
    (con (car (:: data)) (get Part 'data)))

  #(println 'repl-list Prv (type Prv) (get Prv 'data))
    
  (put Part 'c Prv))
(de base-cur (Ref)  
  # x: CDR is to the right; use X bounds
  # y: CDR is below; use Y bounds
  (let Bounds (if (= (get Ref 'data) '*part-main)
                  (calc-bounds> Ref T T)
                  (calc-bounds> Ref NIL T))
    (if (= (get Ref 'lay) 'x)
        (prog
          # Move cur to x base of Ref and y origin of Ref
          (set-cur-x (get Bounds 1))
          (set-cur-y (get (get-origin> Ref) 2))
          (adv-cur 2.0))
        (prog
          # Move cur to x origin of Ref and y base of Ref
          (set-cur-x (get (get-origin> Ref) 1))
          (set-cur-y (get Bounds 2))
          (nl-cur 2.0)))))


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
    
    # Back ref
    (put Part 'c Ref)
    
    # Connect new part data to list part data
    # Need separate function if not at end
    #   (conc (: data) Lst-Any (get (get Ref 'b) 'data))
    
    # Remember, *part-main's value is always the particle itself
    # The value of the symbol *part-main is actually 'b    
    
    #(println 'con-list Ref (type Ref) (get Ref 'data))
    
    (unless (isa '+Sym Ref)
      (with Ref
        (con (car (:: data)) Lst-Any)))
        
    Part))

######################

(de repl-ref (Any Lay)
  # Replace entire Ref (CAR)

  # Handle case when there is something after ref

  (let (Ref (val> p0)
        Prv (get Ref 'c)
        Lst-Any (list (list Any))
        Part (gen-part Lst-Any Lay))
        
    (repl-list Part Prv)

    # Don't use base-cur since cur at start

    # Either use Ref or Last

    # HANDLE Y LAYOUT!
    
    # Move new part at extents of Prv
    (base-cur Prv)
    (lay-part Part *cur)

    # Move car one space further down
    (mov-cur-part Part 'start)
    (nl-cur 3.0)
    (with Part
      (put (: a) 'lay Lay)
      (lay-part (: a) *cur))

    Part))


(de repl-list (Part Prv)
      
  # Delete ref
  (let Ref (val> p0)
    (del> Ref T T))
  
  # Connect cdr of ref to particle b
  # WATCH FOR NIL

  # Unlink ref
  (put Prv 'b) # data still there
  (put p0 'b)

  (with Prv
    (=: b Part)
    (con (car (:: data)) (get Part 'data)))

  #(println 'repl-list Prv (type Prv) (get Prv 'data))
    
  (put Part 'c Prv))
(de base-cur (Ref)  
  # x: CDR is to the right; use X bounds
  # y: CDR is below; use Y bounds
  (let Bounds (if (= (get Ref 'data) '*part-main)
                  (calc-bounds> Ref T T)
                  (calc-bounds> Ref NIL T))
    (if (= (get Ref 'lay) 'x)
        (prog
          # Move cur to x base of Ref and y origin of Ref
          (set-cur-x (get Bounds 1))
          (set-cur-y (get (get-origin> Ref) 2))
          (adv-cur 2.0))
        (prog
          # Move cur to x origin of Ref and y base of Ref
          (set-cur-x (get (get-origin> Ref) 1))
          (set-cur-y (get Bounds 2))
          (nl-cur 2.0)))))


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
    
    # Back ref
    (put Part 'c Ref)
    
    # Connect new part data to list part data
    # Need separate function if not at end
    #   (conc (: data) Lst-Any (get (get Ref 'b) 'data))
    
    # Remember, *part-main's value is always the particle itself
    # The value of the symbol *part-main is actually 'b    
    
    #(println 'con-list Ref (type Ref) (get Ref 'data))
    
    (unless (isa '+Sym Ref)
      (with Ref
        (con (car (:: data)) Lst-Any)))
        
    Part))

######################

(de repl-ref (Any Lay)
  # Replace entire Ref (CAR)

  # Handle case when there is something after ref

  (let (Ref (val> p0)
        Prv (get Ref 'c)
        Lst-Any (list (list Any))
        Part (gen-part Lst-Any Lay))
        
    (repl-list Part Prv)

    # Don't use base-cur since cur at start

    # Either use Ref or Last

    # HANDLE Y LAYOUT!
    
    # Move new part at extents of Prv
    (base-cur Prv)
    (lay-part Part *cur)

    # Move car one space further down
    (mov-cur-part Part 'start)
    (nl-cur 3.0)
    (with Part
      (put (: a) 'lay Lay)
      (lay-part (: a) *cur))

    Part))


(de repl-list (Part Prv)
      
  # Delete ref
  (let Ref (val> p0)
    (del> Ref T T))
  
  # Connect cdr of ref to particle b
  # WATCH FOR NIL

  # Unlink ref
  (put Prv 'b) # data still there
  (put p0 'b)

  (with Prv
    (=: b Part)
    (con (car (:: data)) (get Part 'data)))

  #(println 'repl-list Prv (type Prv) (get Prv 'data))
    
  (put Part 'c Prv))
(de base-cur (Ref)  
  # x: CDR is to the right; use X bounds
  # y: CDR is below; use Y bounds
  (let Bounds (if (= (get Ref 'data) '*part-main)
                  (calc-bounds> Ref T T)
                  (calc-bounds> Ref NIL T))
    (if (= (get Ref 'lay) 'x)
        (prog
          # Move cur to x base of Ref and y origin of Ref
          (set-cur-x (get Bounds 1))
          (set-cur-y (get (get-origin> Ref) 2))
          (adv-cur 2.0))
        (prog
          # Move cur to x origin of Ref and y base of Ref
          (set-cur-x (get (get-origin> Ref) 1))
          (set-cur-y (get Bounds 2))
          (nl-cur 2.0)))))


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
    
    # Back ref
    (put Part 'c Ref)
    
    # Connect new part data to list part data
    # Need separate function if not at end
    #   (conc (: data) Lst-Any (get (get Ref 'b) 'data))
    
    # Remember, *part-main's value is always the particle itself
    # The value of the symbol *part-main is actually 'b    
    
    #(println 'con-list Ref (type Ref) (get Ref 'data))
    
    (unless (isa '+Sym Ref)
      (with Ref
        (con (car (:: data)) Lst-Any)))
        
    Part))

######################

(de repl-ref (Any Lay)
  # Replace entire Ref (CAR)

  # Handle case when there is something after ref

  (let (Ref (val> p0)
        Prv (get Ref 'c)
        Lst-Any (list (list Any))
        Part (gen-part Lst-Any Lay))
        
    (repl-list Part Prv)

    # Don't use base-cur since cur at start

    # Either use Ref or Last

    # HANDLE Y LAYOUT!
    
    # Move new part at extents of Prv
    (base-cur Prv)
    (lay-part Part *cur)

    # Move car one space further down
    (mov-cur-part Part 'start)
    (nl-cur 3.0)
    (with Part
      (put (: a) 'lay Lay)
      (lay-part (: a) *cur))

    Part))


(de repl-list (Part Prv)
      
  # Delete ref
  (let Ref (val> p0)
    (del> Ref T T))
  
  # Connect cdr of ref to particle b
  # WATCH FOR NIL

  # Unlink ref
  (put Prv 'b) # data still there
  (put p0 'b)

  (with Prv
    (=: b Part)
    (con (car (:: data)) (get Part 'data)))

  #(println 'repl-list Prv (type Prv) (get Prv 'data))
    
  (put Part 'c Prv))
(de base-cur (Ref)  
  # x: CDR is to the right; use X bounds
  # y: CDR is below; use Y bounds
  (let Bounds (if (= (get Ref 'data) '*part-main)
                  (calc-bounds> Ref T T)
                  (calc-bounds> Ref NIL T))
    (if (= (get Ref 'lay) 'x)
        (prog
          # Move cur to x base of Ref and y origin of Ref
          (set-cur-x (get Bounds 1))
          (set-cur-y (get (get-origin> Ref) 2))
          (adv-cur 2.0))
        (prog
          # Move cur to x origin of Ref and y base of Ref
          (set-cur-x (get (get-origin> Ref) 1))
          (set-cur-y (get Bounds 2))
          (nl-cur 2.0)))))


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
    
    # Back ref
    (put Part 'c Ref)
    
    # Connect new part data to list part data
    # Need separate function if not at end
    #   (conc (: data) Lst-Any (get (get Ref 'b) 'data))
    
    # Remember, *part-main's value is always the particle itself
    # The value of the symbol *part-main is actually 'b    
    
    #(println 'con-list Ref (type Ref) (get Ref 'data))
    
    (unless (isa '+Sym Ref)
      (with Ref
        (con (car (:: data)) Lst-Any)))
        
    Part))

######################

(de repl-ref (Any Lay)
  # Replace entire Ref (CAR)

  # Handle case when there is something after ref

  (let (Ref (val> p0)
        Prv (get Ref 'c)
        Lst-Any (list (list Any))
        Part (gen-part Lst-Any Lay))
        
    (repl-list Part Prv)

    # Don't use base-cur since cur at start

    # Either use Ref or Last

    # HANDLE Y LAYOUT!
    
    # Move new part at extents of Prv
    (base-cur Prv)
    (lay-part Part *cur)

    # Move car one space further down
    (mov-cur-part Part 'start)
    (nl-cur 3.0)
    (with Part
      (put (: a) 'lay Lay)
      (lay-part (: a) *cur))

    Part))


(de repl-list (Part Prv)
      
  # Delete ref
  (let Ref (val> p0)
    (del> Ref T T))
  
  # Connect cdr of ref to particle b
  # WATCH FOR NIL

  # Unlink ref
  (put Prv 'b) # data still there
  (put p0 'b)

  (with Prv
    (=: b Part)
    (con (car (:: data)) (get Part 'data)))

  #(println 'repl-list Prv (type Prv) (get Prv 'data))
    
  (put Part 'c Prv))
(de base-cur (Ref)  
  # x: CDR is to the right; use X bounds
  # y: CDR is below; use Y bounds
  (let Bounds (if (= (get Ref 'data) '*part-main)
                  (calc-bounds> Ref T T)
                  (calc-bounds> Ref NIL T))
    (if (= (get Ref 'lay) 'x)
        (prog
          # Move cur to x base of Ref and y origin of Ref
          (set-cur-x (get Bounds 1))
          (set-cur-y (get (get-origin> Ref) 2))
          (adv-cur 2.0))
        (prog
          # Move cur to x origin of Ref and y base of Ref
          (set-cur-x (get (get-origin> Ref) 1))
          (set-cur-y (get Bounds 2))
          (nl-cur 2.0)))))


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
    
    # Back ref
    (put Part 'c Ref)
    
    # Connect new part data to list part data
    # Need separate function if not at end
    #   (conc (: data) Lst-Any (get (get Ref 'b) 'data))
    
    # Remember, *part-main's value is always the particle itself
    # The value of the symbol *part-main is actually 'b    
    
    #(println 'con-list Ref (type Ref) (get Ref 'data))
    
    (unless (isa '+Sym Ref)
      (with Ref
        (con (car (:: data)) Lst-Any)))
        
    Part))

######################

(de repl-ref (Any Lay)
  # Replace entire Ref (CAR)

  # Handle case when there is something after ref

  (let (Ref (val> p0)
        Prv (get Ref 'c)
        Lst-Any (list (list Any))
        Part (gen-part Lst-Any Lay))
        
    (repl-list Part Prv)

    # Don't use base-cur since cur at start

    # Either use Ref or Last

    # HANDLE Y LAYOUT!
    
    # Move new part at extents of Prv
    (base-cur Prv)
    (lay-part Part *cur)

    # Move car one space further down
    (mov-cur-part Part 'start)
    (nl-cur 3.0)
    (with Part
      (put (: a) 'lay Lay)
      (lay-part (: a) *cur))

    Part))


(de repl-list (Part Prv)
      
  # Delete ref
  (let Ref (val> p0)
    (del> Ref T T))
  
  # Connect cdr of ref to particle b
  # WATCH FOR NIL

  # Unlink ref
  (put Prv 'b) # data still there
  (put p0 'b)

  (with Prv
    (=: b Part)
    (con (car (:: data)) (get Part 'data)))

  #(println 'repl-list Prv (type Prv) (get Prv 'data))
    
  (put Part 'c Prv))
