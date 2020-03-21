######################
# Semantic (Selection)

(de cmd-point-ptr-pre (Keysym)
  # Move to prev item in the list
  (println "Move to prev item")
  # Get previous item from pointer
  
  # Either use doubly-linked list or
  # spatial index

  (let (Ref (val> p0)
        Tgt (get *part-main 'b)
        Prv *part-main)
        
    (loop
      (T (or (nil (get Tgt 'b))
             (= Tgt Ref)))
      (setq Prv Tgt
            Tgt (get Tgt 'b)))

    (put p0 'b Prv)
    (mov-part-abv> p0 Prv (cons T))))
  

(de cmd-point-ptr-nxt (Keysym)
  # Move to next item in the list
  (println "Move to next item")
  
  (let? Cdr (get (val> p0) 'b)
          
    (put p0 'b Cdr)
    (mov-part-abv> p0 Cdr (cons T))))
  

(de cmd-point-ptr-out (Keysym)
  # Move to outer list
  # Works anywhere
  (println "Move to outer list")
  
  # Set main particle to old one
  (setq *part-main (pop '*part-lsts))

  (let? Last-1 (last-1> *part-main)

    # Set ptr to last item (non-NIL)
    (put p0 'b Last-1)
    (mov-part-abv> p0 Last-1)

    # Move cursor
    (mov-cur-part Last-1)))


(de cmd-point-ptr-in (Keysym)
  # Move to inner list
  # Move to next nearest list?
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

      # Set main particle
      (setq *part-main Ref)
      
      (setq *mode-car T))))


###########
# Euclidean


(de cmd-trans-ptr (Cnt Mov)
  # Move p0
  (let (Ptr (car *part-ptrs)
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
