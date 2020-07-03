(de cmd-make-pair-y (Ks)
  #(println 'cmd-make-pair-y)

  # Make these methods?
  
  (cond
  
    ((a-atom?> *ptr)
      #(println 'make-pair "A ATOM/NIL")
      (if (lay-x?> *ptr)
          (make-pair-a-atom-x)
          (make-pair-a-atom-y)))
      
    ((b-atom?> *ptr)
      #(println 'make-pair "B ATOM/NIL")
      (make-pair-b-atom))
    
    ((a-pair?> *ptr)
      #(println 'make-pair "A PAIR")
      (make-pair-a-pair))
      
    ((b-pair?> *ptr)
      #(println 'make-pair "B PAIR")
      (make-pair-b-pair)
      # (if (lay-x?> *ptr) ...)
      )))
            

(de cmd-make-pair-x (Ks))

            
(de cmd-del (Ks)

  #(prinl-info "cmd-del")

  (del-pt))


(de cmd-bsp (Ks)

  #(prinl-info "cmd-bsp")
  
  (bsp-pt))

  
(de cmd-alt-reg (Ks)
  (alt-reg))

  
(de cmd-rot (Ks))
(de cmd-flip (Ks))
(de cmd-rev (Ks))
