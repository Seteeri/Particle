(de make-line ()
  
  # Only applies to X Pair Cdr
  # X Pair Car can have a line but only applies to lists
  # If arbitrarily done, would break consistency...
  
  (let (Pt-Tgt (if (get *ptr 'pair)
                   (get *ptr 'ref)
                   (lup-pt (get *ptr 'ref)))
        Ln (c-lns> *ptr))
                   
    (unless (get Pt-Tgt 'line)
    
      (ins-line-aft Ln Pt-Tgt)
          
      (upd-tree)
      (mov-⇲ Ln)
      
      (top> Pt-Tgt)
      (draw> *ptr)
      
      (set-ln-n> *ptr Pt-Tgt))))

        
(de ins-line-aft (Ln Po)
  
  (let Nxt (get Ln 'line 'n)  
  
    (put Po 'line
      (new '(+Line)
            Ln
            (unless (= Nxt Po) Nxt)))
            
    (if Ln

        (prog
          (put Ln 'line 'n Po)
          (when Nxt
            (unless (= Nxt Po)
              (put Nxt 'line 'p Po))))
        
        # Start of line
        (let? Nxt (get Ln 'line 'n)
          (put Ln 'line 'n Po)))))    
        

(de del-line (Po)

  (let? Nl (get Po 'line)

    (when (get Nl 'p)
      (put @ 'line 'n (get Nl 'n)))

    (when (get Nl 'n)
      (put @ 'line 'p (get Nl 'p)))
      
    (put Po 'line)))

    
(de pop-line (Po)
  # Similar to del, except don't remove +Line
  
  (let? Nl (get Po 'line)

    (when (get Nl 'p)
      (put @ 'line 'n (get Nl 'n)))

    (when (get Nl 'n)
      (put @ 'line 'p (get Nl 'p)))))
    
      
(de mov-line (Src Dst)

  (let (Nl (get Src 'line)
        Nxt (get Nl 'n))

    (when (get Nl 'p)
      (put @ 'line 'n Dst))

    (when Nxt
      (put Nxt 'line 'p Dst))
      
    (when (= Nxt Dst)
      (put Nl 'n))
    
    (put Dst 'line Nl))

  (put Src 'line)
  
  #(println 'mov-line "Put line: " Src (get Src 'any) " -> " Dst (get Dst 'any))
  )
  

(de del-line-cur ()

  #{
  
    [X]
     .

    [X]
     .

    [X]
     .
     
  }#
                                      
  (del-line (c-lns> *ptr))
    
  (lns> *ptr)

  (upd-tree)
  
  (mov-⇲ (c-lns> *ptr))
        
  (with *ptr
    (top> (if (: ref) @ (lup-pt (: pair)))))
  (draw> *ptr))
