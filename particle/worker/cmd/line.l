(de cmd-mov-nl (Ks)
  (println 'cmd-mov-nl)
  (let Ref (get-ptr-ref)
    (unless *on-car
      (make-line Ref))))


(de cmd-mov-bl (Ks)
  
  # This moves Ref front of prv
  # Only applies to non-list...
    
  (let? Ref (get-ptr-ref)
    (bsp-line Ref)))

