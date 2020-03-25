(de cmd-swap-layout (Keysym)
  (println "Swap layout")
  
  (let Prv (get (val> p0) 'c)
      
    (with Prv
      (=: lay (if (= (: lay) 'x) 'y 'x)))
  
    (mov-cur-part Prv 'start)
    
    (setq *nl-pair NIL)
    (lay-part Prv *cur))
    (setq *nl-pair T))


(de cmd-mov-nl (Keysym)
  (println "Move newline")
  
  # Is this appropriate for Y layout? ...since it already moves to newline
  
  # Move pointer and move NIL (CDR)
  # Character after this will replace NIL
    
  (let (Ref (val> p0)
        Ori (get-origin> (get *main 'b))
        Bnds (calc-bounds> (get *main 'b)))
      
    (setq *cur (list (get Ori 1)
                     (get Bnds 2)
                     (get *cur 3)))
    (nl-cur 2.0)                               
    
    (lay-part Ref *cur)
    
    (put p0 'b Ref)
    (mov-part-abv> p0 Ref (cons T))))
  
  
(de cmd-mov-bl (Keysym)

  # This moves in front of prev

  (let Ref (val> p0)
  
    (base-cur (get Ref 'c))
    (lay-part Ref *cur)
    
    (put p0 'b Car Ref)
    (mov-part-abv> p0 Ref (cons T))))
      

######################
# Semantic (Selection)

(de cmd-point-ptr-pre (Keysym)
  # Move to prev item in the list
  (println "Move to prev item")
  # Get previous item from pointer
  
  # Either use doubly-linked list or
  # spatial index

  (let? Prv (get (val> p0) 'c)
    (put p0 'b Prv)
    (mov-part-abv> p0 Prv (cons T))))
  

(de cmd-point-ptr-nxt (Keysym)
  # Move to next item in the list
  (println "Move to next item")
  
  (let? Cdr (get (val> p0) 'b)
    (put p0 'b Cdr)
    (mov-part-abv> p0 Cdr (cons T))))


(de cmd-point-ptr-out (Keysym)
  # Move to super list
  
  #{
    (a (b c) e)
  
    [ ] - [*] - [ ] - NIL
     a     |     e
           |
          [ ] - [ ] - NIL
           b     c
           
    Stop when CAR is a Pair instead of an atom
    
    *a = e
    *b = (b c)
  }#
  
  (println "Move to outer list")
  
  #(println 'cmd-point-ptr-out Part (type Part) (get Part 'data) (get Part 'c))
  
  (let Part (val> p0)
    (loop
      (T (or (isa '+Pair (get Part 'a))
             (not Part)))
      (setq Part (get Part 'c)))
      
    (put p0 'b Part)
    (mov-part-abv> p0 Part (cons T))))


(de cmd-point-ptr-in (Keysym)
  # Move to next list if ref is a list
  
  (println "Move to inner list")
  
  (let (Ref (val> p0)
        Car (get Ref 'a)
        Data (car (get Ref 'data)))

    # Car either has data or is NIL

    (cond ((= (type (get Car 'a)) '(+Pair))
            # Set pointer and do normal
            # Point to Car
            (put p0 'b (get Ref 'a))            
            (mov-part-abv> p0 (get Ref 'a) (cons T)))
            
          ((isa '+Nil Car)
            # Move Ref/((NIL)) to newline if not already
            # Get base x from Ref
            (let (Ori (get-origin> (get *main 'b))
                  Bnds (calc-bounds> (get *main 'b)))
              
              (setq *cur (list (get Ori 1)
                               (get Bnds 2)
                               (get *cur 3)))
              (nl-cur 2.0)                               
              (put Ref 'lay 'y)
              (lay-part Ref *cur))

            (put p0 'b Car Ref)
            (mov-part-abv> p0 Ref (cons T))

            # If not set, Cdr of NIL will be replaced instead of the CAR/entire list
            (setq *mode-repl T)
            ))))
  

###########
# Euclidean


(de cmd-trans-ptr (Cnt Fn)
  
  (let Ref (val> p0)

    (mov-cur-part p0 'start)
    (eval Fn)

    # Ignore Ref/Cdr
    (put p0 'b)
    (lay-part p0 *cur)
    (put p0 'b Ref)))
    

(de cmd-trans-ptr-px (Keysym)
  (cmd-trans-ptr 1 '(adv-cur 1.0)))
  
(de cmd-trans-ptr-nx (Keysym)
  (cmd-trans-ptr 1 '(adv-cur -1.0)))
  
(de cmd-trans-ptr-py (Keysym)
  (cmd-trans-ptr 2 '(nl-cur -1.0)))
  
(de cmd-trans-ptr-ny (Keysym)
  (cmd-trans-ptr 2 '(nl-cur 1.0)))


###########
# Manhatten
