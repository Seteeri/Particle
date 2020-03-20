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
  
  (let (Part-Ptr (val> p0)
        Cdr      (get Part-Ptr 'b))
    
    (when Cdr
      
      (put p0 'b Cdr)
      (mov-part-abv> p0 Cdr (cons T)))))
  

(de cmd-point-ptr-out (Keysym)
  # Move to outer list
  # Works anywhere
  (println "Move to outer list")
  T)


(de cmd-point-ptr-in (Keysym)
  # Move to inner list
  # Move to next nearest list?
  (println "Move to inner list")
  T)


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
