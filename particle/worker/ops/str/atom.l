(de mak-str-x-a (Str Reg-C Pt-B Pt-C)

  (println 'mak-str-x-a)
  
  (let (B (get Pt-B 'any)
        Pt-Str (gen-point Str 'x NIL T))
    
    (if (= Reg-C 'a)
        (set> Pt-B Str Pt-Str T)
        (con> Pt-B Str Pt-Str T))
    
    (when (get Pt-C 'line)
      (mov-line Pt-C Pt-Str)
      (lns> *ptr))
    (set-ln-n> *ptr Pt-Str)
    
    # use pt to col ref also
    (pt> *ptr B Reg-C T T)
    
    (lay-*start)
    
    # Move to next car
    (when (and (pair B)
               (pair (cdr B))
               (str? (car @)))
      (sup> *ptr)
      (nex> *ptr)
      (sub> *ptr T))))


(de mak-str-x-b (Str Pt-B Pt-C)
  (println 'mak-str-x-b)
  #(mak-str-x-a Str 'b Pt-B Pt-C)
  (con-b-pair-x Str Pt-B Pt-C))
        
        
(de mak-str-y-a (Str Pt-B Pt-C)
  (println 'mak-str-y-a)
  (con-a-pair-x Str Pt-B Pt-C))

    
(de mak-str-y-b (Str Pt-B Pt-C)
  (println 'mak-str-y-b)
  (con-b-pair-x Str Pt-B Pt-C))
