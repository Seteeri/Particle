(de cmd-mov-nl (Ks)
  (println 'cmd-mov-nl)
  (let Ref (get *ptr 'b)
    (unless *on-car
      (make-line Ref))))


(de cmd-mov-bl (Ks)
  
  # This moves Ref front of prv
  # Only applies to non-list...
    
  (let? Ref (get *ptr 'b)
    (bsp-line Ref)))

