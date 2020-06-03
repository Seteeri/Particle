(de make-line ()
  
  # Only applies to Cdr X Pair
        
  (let (Po-Ref *ptr
        Ref (get Po-Ref 'data))
                   
    (unless (get Po-Ref 'line)
    
      (put Po-Ref 'line
        (new '(+Line)
              *line
              (get *line 'line 'n)
              (if (get *line 'line 'l) @ *line)))
      
      (let Nxt (get *line 'line 'n)
        # Con cur line
        (put *line 'line 'n Po-Ref)
        # Con new line to prv nxt line
        (when Nxt
          (put Nxt 'line 'p Po-Ref)))
              
      (if (get Po-Ref 'data)
          (ptr-ref '*ptr (lup-pt (get Po-Ref 'data)) 'b)
          (ptr-ref '*ptr (ptr-nil '*ptr) 'b Po-Ref))
      
      (upd-tree)
      (mov-⇲ (car (get *ptr 'ln)))
      
      (top> Po-Ref)
      (ptr-lay '*ptr)
      
      (setq *line Po-Ref))))
        

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
  

(de bsp-line (Po-Ref)  

  (let Ln-P (get Po-Ref 'line 'p)
                                      
    # Remove line
    (del-line Po-Ref)
    
    (setq *line Ln-P)

    (upd-tree)
    (mov-⇲ (car (get *ptr 'ln)))      
    
    # Ptr remains on Ref
    # Handle NIL?
    (ptr-ref '*ptr (get Po-Ref 'data) 'b)
    
    (top> Po-Ref)
    (ptr-lay '*ptr)))
