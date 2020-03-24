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
        Data (car (get Ref 'data)))

    # Check pointer is a list; watch for cons == list
    (when (or (pair Data) 
              (== Data NIL))
      
      # Point to Car
      (put p0 'b (get Ref 'a))
      
      # Car either has data or is NIL
      (if (get (get Ref 'a) 'data)
      
          (prog
            # Set pointer and do normal
            (mov-part-abv> p0 (get Ref 'a) (cons T)))
            
          (prog
            # NIL so must replace it
            # Check layout...
            (mov-part-bel> p0 (get Ref 'a) (cons T))
            (setq *mode-repl T))))))
  

################################

(de cmd-point-ptr-out-2 (Keysym)
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
  
  (let Part (val> p0)
    (until (or (isa '+Pair (get Part 'a))
               (not Part))
      (println Part (get Part 'data))
      (setq Part (get Part 'c))))
  (println Part)
  
  (println "Move to outer list")
  
  # Set main particle to old one
  (setq *part-main (pop '*part-lsts))

  (let? Last-1 (last-1> *part-main)

    # Set ptr to last item (non-NIL)
    (put p0 'b Last-1)
    (mov-part-abv> p0 Last-1)

    # Move cursor
    (mov-cur-part Last-1)))


(de cmd-point-ptr-in-2 (Keysym)
  # Move to next list if ref is a list
  
  (println "Move to inner list")
  
  (let (Ref (val> p0)
        Data (car (get Ref 'data)))

    # Check pointer is a list; watch for cons == list
    (when (or (pair Data)
              (== Data NIL))

      # Push Part to history
      (push '*part-lsts *part-main)

      # DEPENDS ON LAYOUT!
      # Default is to push strings

      # Move cursor/pointer underneath list
      (nl-cur 3.0) (mov> p0 *cur)

      # Do not change pointer data - will push to it
      
      # What if there is data - must move to it

      # Set main particle
      (setq *part-main Ref)
      
      (setq *mode-car T))))


###########
# Euclidean


(de cmd-trans-ptr (Cnt Mov)
  # Move p0
  (let (Ptr p0
        Pos (get-origin> Ptr))
    (mov> Ptr
          (place Cnt
                 Pos
                 (+ (get Pos Cnt) Mov)))))


(de cmd-trans-ptr-px (Keysym)
  (cmd-trans-ptr 1 (*/ *adv-vert 1.0 1.0)))
  
(de cmd-trans-ptr-nx (Keysym)
  (cmd-trans-ptr 1 (*/ *adv-vert -1.0 1.0)))
  
(de cmd-trans-ptr-py (Keysym)
  (cmd-trans-ptr 2 (*/ (+ *adv-vert *adv-vert) 1.0 1.0)))
  
(de cmd-trans-ptr-ny (Keysym)
  (cmd-trans-ptr 2 (*/ (+ *adv-vert *adv-vert) -1.0 1.0)))


###########
# Manhatten
