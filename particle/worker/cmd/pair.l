(de cmd-make-pair (Ks)
  (println 'cmd-make-pair)

  # Make these methods?
  
  (cond
  
    ((a-atom?> *ptr)
      (println 'make-pair "A ATOM/NIL")
      (if (lay-x?> *ptr)
          (make-pair-a-atom-x)
          (make-pair-a-atom-y)))
      
    ((b-atom?> *ptr)
      (println 'make-pair "B ATOM/NIL")
      (make-pair-b-atom))
    
    ((a-pair?> *ptr)
      (println 'make-pair "A PAIR")
      (make-pair-a-pair))
      
    ((b-pair?> *ptr)
      (println 'make-pair "B PAIR")
      (if (lay-x?> *ptr)
          (make-pair-b-pair-x)
          (make-pair-b-pair-y)))))
            

(de cmd-del (Dir)

  (prinl-info "cmd-del")

  (let? Ref (get-ptr-ref)
  
    (let Tgt (if *ptr-on-car (get Ref 'c) Ref)
  
      (del-pt Tgt))))


(de cmd-bs (Dir)

  (prinl-info "cmd-bs")

  (let? Ref (get-ptr-ref)
  
    (let (C (get Ref 'c)
          Tgt (if (and *ptr-on-car
                       (not (is-nil? Ref)))
                  (get C 'c)
                  C))
  
      (del-pt Tgt))))

      
(de cmd-alt-lay (Ks)
  (if (lay-x?> *ptr)
      (alt-lay-y)
      (alt-lay-x)))
  
  
(de cmd-alt-reg (Ks)
  (alt-reg))
