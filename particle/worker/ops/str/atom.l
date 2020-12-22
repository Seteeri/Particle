(de mak-str-x-a (Str Reg)

  #{
  
    Handles both X and Y pairs
  
    Need to move to next car
    
  }#

  (println 'mak-str-x-a)
  
  (let ((Pt-Pair Pt-Del) (get-ref-pt> *ptr)
        Pair (get Pt-Pair 'any)
        #Pt-Del (if (get *ptr 'pair)
        #           (get Pt-Pair (get *ptr 'reg))
        #           (lup-pt (get *ptr 'atom)))
        Pt-Str (gen-point Str 'x NIL T))

    (when (get Pt-Del 'line)
      (mov-line Pt-Del Pt-Str)
      (lns> *ptr))
    (set-ln-n> *ptr Pt-Str)
    
    (if (= Reg 'a)
        (set> Pt-Pair Str Pt-Str T)
        (con> Pt-Pair Str Pt-Str T))
    
    (pt> *ptr Pair Reg T T)
    
    (lay-*start)
    
    (when (and (pair Pair)
              (pair (cdr Pair))
              (str? (car @)))
      (sup> *ptr NIL)
      (nex> *ptr NIL)
      (sub> *ptr T))))


(de mak-str-x-b (Str)

  #{
  
    Connect new list cdr to cur atom
    Update pointer before updating cur pair
    Then connect new list to cur pair
    Draw main
    Mov cur to ptr
    Draw ptr
    
  }#

  (println 'mak-str-x-b)
  
  (let ((Pt-Pair Pt-Reg) (get-ref-pt> *ptr)
        Pt-Cdr (get Pt-Pair 'b)
        Ls (list Str)
        Pt-Ls (gen-point Ls 'x NIL T))
    
    (when (get Pt-Cdr 'line)
      (mov-line Pt-Cdr Pt-Ls)
      (lns> *ptr))
    (set-ln-n> *ptr Pt-Ls)

    (con> Pt-Ls (get Pt-Cdr 'any) Pt-Cdr T)
    
    # Point to cdr of new list
    # aka cur atom
    (pt> *ptr Ls 'b T)
    
    (con> Pt-Pair Ls)
    
    (lay-*start)
    
    (top> Pt-Cdr)
    (draw> *ptr)))
        
        
(de mak-str-y-a (Str)

  (println 'mak-str-y-a)
  
  (let ((Pt-Pair Pt-Reg) (get-ref-pt> *ptr)
        Pt-Car (get Pt-Pair 'a)
        Ls (list Str)
        Pt-Ls (gen-point Ls 'x NIL T))
    
    ~(assert (get Pt-Car 'line))
    (mov-line Pt-Car Pt-Ls)
    (lns> *ptr)
    (set-ln-n> *ptr Pt-Ls)
    
    (set> Pt-Pair Ls Pt-Ls)
    (con> Pt-Ls (get Pt-Car 'any) Pt-Car T)
    
    (with *ptr
      (=: any Ls)
      (=: reg 'b))
  
    (lay-*start)
    
    (top> Pt-Car)
    (draw> *ptr)))
