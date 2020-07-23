(class +Pointer)

(dm pt-pair> (Reg Drw)
  
  # TODO: Handle y-pair
  
  (let ((Pt-Pair Pt-Reg) (get-ref-pt> *ptr)
        Tgt (if (= Reg 'a)
                (car (get Pt-Reg 'any))
                (cdr (get Pt-Reg 'any)))
        Pt-Tgt (if (atom Tgt)
                   (get Pt-Reg Reg)
                   (lup-pt Tgt)))

    ~(assert Pt-Tgt)            
                      
    (pt> This (get Pt-Reg 'any) Reg T)
    (set-ln-n> This Pt-Tgt)
               
    (when Drw
      (top-ref> This)
      (draw> This))))
  

(dm pt-sym> (Reg Drw)

  #{
  
    Can only traverse car if expanded atom
    
  }#
      
  (let ((Pt-Pair Pt-Reg) (get-ref-pt> *ptr)
        Pt-Tgt (if (= Reg 'a)
                   (get Pt-Reg Reg)
                   (if (val (: any))
                       (lup-pt @)
                       (get Pt-Reg Reg))))

    (when Pt-Tgt
                       
      (pt> This (: any) Reg T)

      (set-ln-n> This Pt-Tgt)
      
      (when Drw
        (top-ref> This)
        (draw> This)))))
    
    
(dm pt-bwd> (Reg Drw)

  (let ((C Pt-C Reg-C
         B Pt-B Reg-B
         A Pt-A) (refs-2> *ptr))  
    
    # make sure A not NIL...
    
    (when T
      (prinl)
      (println 'pt-bwd> C Pt-C Reg-C)
      (println 'pt-bwd> B Pt-B Reg-B)
      (println 'pt-bwd> A Pt-A))
         
    (set-ln-p> This
               Pt-C
               Pt-B)
    
    # TEMP:
    (if (= B '*start)
        
        (prog
          (pt> This B B T)
          (when Drw
            (top> Pt-B)
            (draw> This)))
          
        (prog
          (pt> This A Reg-B T)
          (when Drw
            (top> Pt-B)
            (draw> This))))))
            

(dm pt-fwd> (Reg Drw)

  (cond

    ((nil?> This)
     (when (sym? (: any))
        (pt-sym> This Reg Drw)))
  
    ((pair?> This)
      (pt-pair> This Reg Drw))
    
    ((atom?> This)
      (pt-sym> This Reg Drw))))


(dm pre> (Drw) (pt-bwd> This 'b Drw))
(dm nex> (Drw) (pt-fwd> This 'b Drw))
(dm sup> (Drw) (pt-bwd> This 'a Drw))
(dm sub> (Drw) (pt-fwd> This 'a Drw))
