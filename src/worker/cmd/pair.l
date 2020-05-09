(de cmd-reset (Keysm)

  (println 'cmd-reset)

  # Clear ptr
  (put *0 'b)

  (println 'cmd-reset *master (get *master 'data))
  (del> *master T T)

  (gc))

      
(de cmd-mov-nl (Keysym)
  (println "Move newline")

  (let Ref (get *0 'b)
    (if *on-car
        (make-list Ref)
        (make-line Ref))))


(de cmd-mov-bl (Keysym)
  
  # This moves Ref front of prv
  # Only applies to non-list...
    
  (let? Ref (get *0 'b)
    (backspace-line Ref)))


(de cmd-intern (Keysym)
  # Convert string to symbol
  # Build char until space
  
  (let Ref (get *0 'b)
  
    (when (isa '(+Nil) Ref)
      (setq Ref (get Ref 'c)))
  
    (when (isa '(+Str) (get Ref 'a))  
  
      (let (End Ref
            (Sta Lst) (build-str-sp End)
            Data (intern (pack Lst)))
            
        (println 'cmd-intern Sta (get Sta 'data) "TO" End (get End 'data))

        (repl-list-str Sta End Data)))))
