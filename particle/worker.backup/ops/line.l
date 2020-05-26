#{
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
  (lay-ptr *ptr)
  
  (println 'make-line "Made newline")
    
  Ref)
}#

(de make-line (Ref)
  (unless (get Ref 'line)
    (put Ref 'line
      (new '(+Line)
           *line
           (get *line 'line 'n)
           (if (get *line 'line 'l) @ *line)))
    (let (P (get *line 'line 'n)
          N (get *line 'line 'p))
      (put P 'line 'n Ref)
      (put N 'line 'p Ref))
    
    (upd-tree)
    (mov-⇲ Ref)
    
    (setq *line Ref)
    (mov-cur-abv Ref)
    (lay-ptr *ptr)))
        

(de ins-line-pre (Po)

  (when (get *line 'p)
    (put @ 'line 'n Po))

  (put *line 'line 'p Po))


(de del-line (Po)

  (let? Nl (get Po 'line)

    (when (get Nl 'p)
      (put @ 'line 'n (get Nl 'n)))

    (when (get Nl 'n)
      (put @ 'line 'p (get Nl 'p)))
      
    (put Po 'line)))

      
(de mov-line (Src Dst)

  (let (Nl (get Src 'line)
        Nxt (get Nl 'n))

    (when (get Nl 'p)
      (put @ 'line 'n Dst))

    (when Nxt
      (put Nxt 'line 'p Dst))
      
    (when (= Nxt Dst)
      (put Nl 'n))
    
    # When changing root list
    # update subsequent lists since
    # they all ref it through 'l
    (when (and (not (get Nl 'p))
               (not (get Nl 'l)))
      (let (Child (get Nl 'n)
            Visited NIL)
        (while (and Child
                    (not (member Child Visited)))
          (println 'mov-line "Set list: " Child (car (get Child 'data)))
          (push 'Visited Child)
          (put Child 'line 'l Dst)
          (setq Child (get Child 'line 'n))
          #(when (member Child Visited)
          #  (println 'mov-line "WARNING! Cycle detected: " Child (get Child 'data)))
          )))

    (put Dst 'line Nl))

  (put Src 'line)
  
  (println 'mov-line "Put line: " Src (get Src 'data) " -> " Dst (get Dst 'data))

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
        (lay-ptr *ptr)))))  
