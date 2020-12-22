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

  Make these methods...?
  - who owns it? ptr or pt?
  - trace is done on pt
  
  atoms have no lay (except for exp sym?)
  
}#


(de make-str (Str)

  (let ((C Pt-C Reg-C
         B Pt-B Reg-B
         A Pt-A)      (refs-2> *ptr))

    (cond
      
      ((isa '+Atom Pt-C)
      
        (cond

          ((and (lay-x?> Pt-B) (= Reg-C 'a)) (mak-str-x-a Str 'a))
          ((and (lay-x?> Pt-B) (= Reg-C 'b)) (mak-str-x-b Str))

          ((and (lay-y?> Pt-B) (= Reg-C 'a)) (mak-str-y-a Str))
          ((and (lay-y?> Pt-B) (= Reg-C 'b)) (mak-str-x-b Str)))) # mak-str-y-b

      ((isa '+Pair Pt-C)
      
        (cond
          
          ((and (lay-x?> Pt-B) (= Reg-C 'a) (lay-x?> Pt-C)) (mak-str-xa-x Str))
          ((and (lay-x?> Pt-B) (= Reg-C 'b) (lay-x?> Pt-C)) (mak-str-xb-x Str))
          
          ((and (lay-x?> Pt-B) (= Reg-C 'a) (lay-y?> Pt-C)) (mak-str-xa-y Str))
          ((and (lay-x?> Pt-B) (= Reg-C 'b) (lay-y?> Pt-C)) (mak-str-xb-y Str))
          
          ((and (lay-y?> Pt-B) (= Reg-C 'a) (lay-x?> Pt-C)) (mak-str-ya-x Str))
          ((and (lay-y?> Pt-B) (= Reg-C 'b) (lay-x?> Pt-C)) (mak-str-yb-x Str))
          
          ((and (lay-y?> Pt-B) (= Reg-C 'a) (lay-y?> Pt-C)) (mak-str-ya-y Str))
          ((and (lay-y?> Pt-B) (= Reg-C 'b) (lay-y?> Pt-C)) (mak-str-yb-y Str))))

      (T
        (quit "make-str: unknown cls")))))
