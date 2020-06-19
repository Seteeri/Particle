(de cmd-make-pair (Ks)
  #(println 'cmd-make-pair)

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
      (if (lay-x?> *ptr)
          (make-pair-b-pair-x)
          (make-pair-b-pair-y)))))
            

(de cmd-del (Dir)

  (prinl-info "cmd-del")

  (del-pt))


(de cmd-bs (Dir)

  (prinl-info "cmd-bs")
  
  (del-bs))

      
(de cmd-alt-lay (Ks)
  (if (lay-x?> *ptr)
      (alt-lay-y)
      (alt-lay-x)))
  
  
(de cmd-alt-reg (Ks)
  (alt-reg))
