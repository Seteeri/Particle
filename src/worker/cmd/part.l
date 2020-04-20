(de cmd-reset (Keysm)

  (println 'cmd-reset)

  # Clear ptr
  (put *0 'b)

  (println 'cmd-reset *master (get *master 'data))
  (del> *master T T)

  (gc))


(de cmd-swap-layout (Keysym)
  (println "Swap layout")

  # Note this causes bounds change also
  # Maintain x and y bnds?

  # If NIL, get prev, else do ref

  (let (Ref (get *0 'b)
        Prv (get Ref 'c)
        Tgt (if (= (get Ref 'data) NIL) Prv Ref))
    (when Tgt
      (println "Swap!" Tgt (get Tgt 'data) (get Tgt 'lay) '-> (if (= (get Tgt 'lay) 'x) 'y 'x))
      (swap-lay Tgt))))

      
(de cmd-mov-nl (Keysym)
  (println "Move newline")

  (let Ref (get *0 'b)
    (if *on-car
        (make-list Ref)
        (make-line Ref))))


(de cmd-mov-bl (Keysym)
  
  # This moves Ref front of prv
  # Only applies to non-list...
    
  (let (Ref (get *0 'b)
        Prv (get Ref 'c))

    (when (and (get Ref 'line 'p)
               (not (= (get Ref 'data) '*start)))
    
      (unless (or (isa '(+Pair) (get Ref 'a))
                  (isa '(+Nil) (get Ref 'a))
                  (= (get Prv 'a) Ref))
                                    
        (setq *line (get Ref 'line 'p))
        # Remove line
        (del-line Ref)
        (put Ref 'line)

        (upd-tree)
        (mov-*1 *line)      
        
        # Ptr remains on Ref
        # How about for on-car?
        (mov-cur-abv Ref)
        (lay-part-start *0 *line 'skip-cdr)))))


###########################################################

# Advanced

(de cmd-intern (Keysym) (prinl-info "Use (intern <data>)"))
(de cmd-any (Keysym) (prinl-info "Use (any ...); str -> any"))
(de cmd-sym () (prinl-info "Use (sym ...); rev of (any ...)"))
