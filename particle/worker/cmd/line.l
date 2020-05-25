(de cmd-mov-nl (Ks)
  (println 'cmd-mov-nl)
  (let Ref (get *fwd-in 'b)
    (unless *on-car
      (make-line Ref))))


(de cmd-mov-bl (Ks)
  
  # This moves Ref front of prv
  # Only applies to non-list...
    
  (let? Ref (get *fwd-in 'b)
    (bsp-line Ref)))


(de cmd-bs (Dir)

  (prinl-info "cmd-bs")

  (let? Ref (get *fwd-in 'b)
  
    (del-c Ref)))
