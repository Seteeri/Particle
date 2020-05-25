(de cmd-make-list (Ks)
  (println 'cmd-make-list)

  #{
      [ ]  [ ]  [ ]
      a    b    c

      [ ]  [ ]  [ ]
      d    e    f
          *
          
      [ ]  [ ]  [ ]
      g    h    i
      
      NIL
      
      
      If on car:
      
        (a b c d e f g h i) -> (a b c d (e) f g h i)
      
      If on cdr:
      
        (a b c d e f g h i) -> (a b c d (e f g h i))
  }#  
  
  (let Ref (get *ptr 'b)
    (if *on-car
        (make-list-car Ref)
        (make-list-cdr Ref))))


(de cmd-del (Dir)

  (prinl-info "cmd-del")

  (let? Ref (get *ptr 'b)
  
    (let Tgt (if *on-car (get Ref 'c) Ref)
  
      (del-pt Tgt))))


(de cmd-bs (Dir)

  (prinl-info "cmd-bs")

  (let? Ref (get *ptr 'b)
  
    (let (C (get Ref 'c)
          Tgt (if (and *on-car
                       (not (is-nil? Ref)))
                  (get C 'c)
                  C))
  
      (del-pt Tgt))))
