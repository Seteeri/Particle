(de cmd-mov-nl (Ks)
  (println 'cmd-mov-nl)
  
  (make-line))


(de cmd-mov-bl (Ks)
  (println 'cmd-mov-bl)
  
  (let Po-Ref (lup-pt *ptr)
    (when (get Po-Ref 'line)
      (bsp-line Po-Ref))))

