# Move more stuff to methods
# Standardize checks
# Standardize prim ops

#####################
#####################
# SYM

(de make-str (Data Lay)

  (let Ref (get *0 'b)

    (if *on-car

        # If start of list, must replace entire thing
        (if (and (= (get Ref 'c 'c 'a) (get Ref 'c))
                 (= (get Ref 'c 'a) Ref))
              
            (prog
              (repl-list-car Data Lay))
              
            (prog
              # If end-of-list NIL
              (if (and (isa '(+Nil) Ref)
                       (= (get Ref 'c 'b) Ref))
                  (prog
                    (ins-cdr Data Lay)
                    (point-sub Ref))
                  (prog
                    (repl-car Data Lay)))))
        
        (if (= (get Ref 'c 'a) Ref) # If direct sublist

            (prog
              (ins-list-car Data Lay))

            (prog
              (ins-cdr Data Lay))))))


(de ins-cdr (Data Lay)
  #{
  
    Insert item before pair ptr
    
    
    [Y]  [X]  NIL
         a
    *
    NIL
    
    - Prv needed to make a line for NIL...
    - NIL is X layout now
          
    
    Special case:
    
    [X]  [X]
    a    b
    
    *
    [Y]  [X]  NIL
         c
         
    NIL
    
    - Normally Part would be right of B which is awkward,
    so ins Part as newline before Y
    
  }#

  (let (Ref (get *0 'b)
        Prv (get Ref 'c)
        Part (gen-part (list Data) Lay NIL T))

    (println 'ins-cdr "con-aft: " Prv (get Prv 'data))

    (con-aft Prv Part)
  
    # Make line as necessary
    (if (and (= (get Ref 'lay) 'y)
                (isa '(+Pair) Ref))
        (make-line Part)
        (when (get Ref 'line)
          (xchg-nl Cdr Part)))
    
    (upd-tree)
    (mov-*1 *line)
    
    # Same ref, relayout
    (mov-cur-abv Ref)
    (lay-part-start *0 *line 'skip-cdr)
    
    Part))



(de repl-list-car (Data Lay)
  #{
    Replace beginning of list when ptr on car
    
    [ ]  
    a    
    
    [ ]  [ ]  [ ]
         b    c
         *

         
    [ ]
    a    
    
    [ ]  [ ]  [ ]
         x    c
         *
         
    Effectively, replace entire list
    
    Refactor:
    * Use original Pair
  }#

  (let (Ref (get *0 'b)
        Cc (get Ref 'c 'c)
        Cdr (get Ref 'c 'b)
        Part (gen-part (list (list Data)) Lay NIL T)
        Car (get Part 'a))
        
    # Txfer line before Ref gets deleted
    (when (get Ref 'c)
      (xchg-nl (get Ref 'c) Car))

    (with Part
      (=: lay (get Cc 'lay))
      (put (: b) 'lay (get Cc 'b 'lay)))

    (repl-pair Cc Part)

    # Relink Car to Cdr
    (con> Car Cdr)
    (put Cdr 'c Car)

    (upd-tree)
    (mov-*1 *line)    

    (put *0 'b Car)
    (point-nxt)

    (setq *line Car)
    
    Part))


(de repl-car (Data Lay)

  #{
    Replace car with same type

    [ ]  [ ]  [ ]  NIL
    a    b    c
         *0

    Ref = b/Car
    Prv = b/Pair
    Part = x

    [ ]  [ ]  [ ]  NIL
    a    x    c
              *0
              
    Refactor
    * Set data for Cc
    * Gen part for sym/str
    * Combine with above fn
      * Connect Cdr if exists
  }#

  (let (Ref (get *0 'b)
        Prv (get Ref 'c)
        Part (gen-part (list Data)
                       Lay
                       NIL
                       T))

    (repl-pair Prv Part)

    (upd-tree)
    (mov-*1 *line)    
    
    (if (= Lay 'x)

      (prog

        (put *0 'b Part)
        (point-nxt))

      (prog
        (println 'repl-car "IMPLEMENT Y LAYOUT")))

  Part))


(de ins-list-car (Data Lay)

  #{
  
    Replace beginning of list when ptr over
    
         *
    [ ]  [ ] NIL
         X
    NIL
    
              *
    [ ]  [ ]  [ ] NIL
         Z    X
    NIL    
    
    Effectively, replace entire list
    
    Refactor:
    * Reuse Cc
  }#

  (let (Ref (get *0 'b)
        Prv (get Ref 'c)
        Cdr (get Ref 'b)
        Part (gen-part (list (list Data)) Lay NIL T)
        Car (get Part 'a))

    # Txfer line before Ref gets deleted
    (when (get Ref 'line)
      (xchg-nl Ref Car))
    
    (put Part 'lay (get Prv 'lay))

    (repl-pair Prv Part 'skip-del)

    # Relink to Ref
    (con> Car Ref)
    (put Ref 'c Car)

    (upd-tree)
    (mov-*1 *line)

    # Same ref, relayout
    (mov-cur-abv Ref)
    (lay-part-start *0 *line 'skip-cdr)

    Part))
