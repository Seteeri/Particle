(de cmd-make-pair (Ks)
  (println 'cmd-make-pair)
  
  (make-pt-pair '*ptr))


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

      
(de cmd-xchg-lay (Ks)
  T)
  
  
(de cmd-xchg-reg (Ks)
  T)
