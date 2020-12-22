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
  
  atom: make pair with cur as car
        car creates y pair (x pair is other cmd)
        cdr creates x pair (use alt-lay to chg to lay y)

        
  list: make pair with cur as car
        car creates y pair (x pair is other cmd)
        cdr creates y pair
        
}#

(de make-pair-y (Str)

  (let ((C Pt-C Reg-C
         B Pt-B Reg-B
         A Pt-A)      (refs-2> *ptr))

    (cond
      
      ((isa '+Atom Pt-C)
      
        (cond
        
          # should be consistent
          
          ((and (lay-x?> Pt-B) (= Reg-C 'a)) (mak-pair-x-a Pt-B Pt-C))
          ((and (lay-x?> Pt-B) (= Reg-C 'b)) (mak-pair-x-b Pt-B Pt-C))

          ((and (lay-y?> Pt-B) (= Reg-C 'a)) (mak-pair-y-a Pt-B Pt-C))
          ((and (lay-y?> Pt-B) (= Reg-C 'b)) (mak-pair-y-b Pt-B Pt-C))))

      ((isa '+Pair Pt-C)
      
        (cond
          
          ((and (lay-x?> Pt-B) (= Reg-C 'a) (lay-x?> Pt-C)) (mak-pair-xa-x Pt-B Pt-C))
          ((and (lay-x?> Pt-B) (= Reg-C 'b) (lay-x?> Pt-C)) (mak-pair-xb-x Pt-B Pt-C))
          
          ((and (lay-x?> Pt-B) (= Reg-C 'a) (lay-y?> Pt-C)) (mak-pair-xa-y Pt-B Pt-C))
          ((and (lay-x?> Pt-B) (= Reg-C 'b) (lay-y?> Pt-C)) (mak-pair-xb-y Pt-B Pt-C))
          
          ((and (lay-y?> Pt-B) (= Reg-C 'a) (lay-x?> Pt-C)) (mak-pair-ya-x Pt-B Pt-C))
          ((and (lay-y?> Pt-B) (= Reg-C 'b) (lay-x?> Pt-C)) (mak-pair-yb-x Pt-B Pt-C))
          
          ((and (lay-y?> Pt-B) (= Reg-C 'a) (lay-y?> Pt-C)) (mak-pair-ya-y Pt-B Pt-C))
          ((and (lay-y?> Pt-B) (= Reg-C 'b) (lay-y?> Pt-C)) (mak-pair-yb-y Pt-B Pt-C))))

      (T
        (quit "make-str: unknown cls")))))
