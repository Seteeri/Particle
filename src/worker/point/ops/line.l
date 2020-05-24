(de make-line (Ref)

  #{

    [X]  [X]  [X]  NIL
    A    B    C  
  
    Mov to nl:
    

    [X]  [X]
    A    B       
    
    [X]  NIL
    C

  }#
  
  (put *line 'line 'n Ref)  
  
  (let Rb (get Ref 'b)  
  
    (put Ref 'line
      (new '(+Line)
           *line
           Rb
           (if (get *line 'line 'l) @ *line)))
          
    (if (get Rb 'line)
        (put @ 'p Ref)
        (put Rb 'line (new '(+Line)
                          Ref
                          NIL
                          (if (get Ref 'line 'l) @ Ref)))))
  
  (upd-tree)
  (mov-⇲ Ref)
  
  (setq *line Ref)
  (mov-cur-abv Ref)
  (lay-ptr *fwd-in)
  
  (println 'make-line "Made newline")
    
  Ref)


(de ins-line-pre (Part)

  (when (get *line 'p)
    (put @ 'line 'n Part))

  (put *line 'line 'p Part))


(de del-line (Part)

  (let? Nl (get Part 'line)

    (when (get Nl 'p)
      (put @ 'line 'n (get Nl 'n)))

    (when (get Nl 'n)
      (put @ 'line 'p (get Nl 'p)))
      
    (put Part 'line)))

      
(de put-line (Src Dst)

  (let (Nl (get Src 'line)
        Nxt (get Nl 'n))

    (when (get Nl 'p)
      (put @ 'line 'n Dst))

    (when Nxt
      (put Nxt 'line 'p Dst))
      
    (when (= Nxt Dst)
      (put Nl 'n))
      
    # Update children's lists when Src is NIL NIL
    (when (and (not (get Nl 'p)) (not (get Nl 'l)))
      (let (Child (get Nl 'n)
            Visited NIL)
        (while (and Child
                    (not (member Child Visited)))
          (println 'put-line "Set list: " Child (get Child 'data))
          (push 'Visited Child)
          (put Child 'line 'l Dst)
          (setq Child (get Child 'line 'n))
          #(when (member Child Visited)
          #  (println 'put-line "WARNING! Cycle detected: " Child (get Child 'data)))
          )))

    (put Dst 'line Nl))

  (put Src 'line)
  
  (println 'put-line "Put line: " Src (get Src 'data) " -> " Dst (get Dst 'data))

  (setq *line Dst))  
  

(de bsp-line (Ref)  
  (let Prv (get Ref 'c)

    (when (and (get Ref 'line 'p)
               (not (= (get Ref 'data) '*start)))
    
      (unless (or (has-pair?> Ref)
                  (has-nil?> Ref)
                  (of-car?> Prv A))
                                    
        (setq *line (get Ref 'line 'p))
        # Remove line
        (del-line Ref)
        (put Ref 'line)

        (upd-tree)
        (mov-⇲ *line)      
        
        # Ptr remains on Ref
        # How about for on-car?
        (mov-cur-abv Ref)
        (lay-ptr *fwd-in)))))  
