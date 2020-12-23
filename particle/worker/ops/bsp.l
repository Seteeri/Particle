#{

  x-a
  x-b
  y-a
  y-b
  
  ya-x
  yb-x
  ya-y
  yb-y

  xa-x
  xb-x
  xa-y
  xb-y

  this rem back, del rem front
  
  atom:
    cdr = del par, con cur
    car = del par, con nxt
      alternatives? 
        - del all
        - make NIL
      
  pair:
    del prv, con cur and/or nxt
  
}#


(de bsp-pt ()

  (let ((C Pt-C Reg-C
         B Pt-B Reg-B
         A Pt-A)      (refs-2> *ptr))

    # pass refs-2> list...plist or class?
         
    (cond
      
      ((isa '+Atom Pt-C)
      
        (cond
          
          ((and (lay-x?> Pt-B) (= Reg-C 'a)) (bsp-x-a Pt-A Pt-B Pt-C))
          ((and (lay-x?> Pt-B) (= Reg-C 'b)) (bsp-x-b Pt-A Pt-B Pt-C Reg-B))

          ((and (lay-y?> Pt-B) (= Reg-C 'a)) (bsp-y-a Pt-A Pt-B Pt-C))
          ((and (lay-y?> Pt-B) (= Reg-C 'b)) (bsp-y-b Pt-A Pt-B Pt-C))))

      ((isa '+Pair Pt-C)
      
        (cond
          
          ((and (lay-x?> Pt-B) (= Reg-C 'a) (lay-x?> Pt-C)) (bsp-xa-x Pt-A Pt-B Pt-C))
          ((and (lay-x?> Pt-B) (= Reg-C 'b) (lay-x?> Pt-C)) (bsp-xb-x Pt-A Pt-B Pt-C))
          
          ((and (lay-x?> Pt-B) (= Reg-C 'a) (lay-y?> Pt-C)) (bsp-xa-y Pt-A Pt-B Pt-C))
          ((and (lay-x?> Pt-B) (= Reg-C 'b) (lay-y?> Pt-C)) (bsp-xb-y Pt-A Pt-B Pt-C))
          
          ((and (lay-y?> Pt-B) (= Reg-C 'a) (lay-x?> Pt-C)) (bsp-ya-x Pt-A Pt-B Pt-C))
          ((and (lay-y?> Pt-B) (= Reg-C 'b) (lay-x?> Pt-C)) (bsp-yb-x Pt-A Pt-B Pt-C))
          
          ((and (lay-y?> Pt-B) (= Reg-C 'a) (lay-y?> Pt-C)) (bsp-ya-y Pt-A Pt-B Pt-C))
          ((and (lay-y?> Pt-B) (= Reg-C 'b) (lay-y?> Pt-C)) (bsp-yb-y Pt-A Pt-B Pt-C))))

      (T
        (quit "bsp-pt: unknown cls")))
        
    (lay-*start)
    
    (top-ref> *ptr)
    (draw> *ptr)))


(de bsp-x-b (Pt-A Pt-B Pt-C Reg-B)

  #{
  
    If ln del, auto con line
  
  }#

  # Remove B
  
  # if B has line, pop it
  # auto link A -> C
  (when (get Pt-B 'line)
    (pop-line Pt-B)
    (lns> *ptr))
  
  # set cur ln as needed
  (set-ln-n> *ptr Pt-C)
  
  # del it with car, skip cdr
  (del> Pt-B T NIL)
  
  # Con A-C
  #   Dep if Reg-B is a/b
  
  (if (= Reg-B 'a)
      (bsp-link-a Pt-A Pt-C)
      (bsp-link-b Pt-A Pt-C))
  
  # ignore draw
  # can set any directly
  
  (with *ptr
    (=: any (get Pt-A 'any))
    (=: reg (if (= Reg-B 'a) 'a 'b))))


(de bsp-link-a (Pt-A Pt-C)
      
  # set creates line as needed
  (set> Pt-A (get Pt-C 'any) Pt-C)
  
  (set-ln-n> *ptr Pt-C))
  

(de bsp-link-b (Pt-A Pt-C)

  (con> Pt-A (get Pt-C 'any) Pt-C))
