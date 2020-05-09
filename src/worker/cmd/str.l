(de cmd-make-char (Ks)

  #{
    \r = ^M
    \n = ^J
    \t = ^I
  }#
  
  (let (Data (cond ((= Ks ksd~Return) "^J")
                   ((= Ks ksd~Tab) "^I")
                   (T (char Ks)))
        Lay 'x)

    # Push to cmd list
    # Make class
    # Put properties

    (make-str Data Lay)))


(de cmd-make-nil (Ks)
  (prinl-info "cmd-make-nil")
  (make-str NIL 'x))


(de cmd-make-num (Ks)

  (println "cmd-make-num")

  (make-str (format (char Ks)) 'x))


(de cmd-format (Ks)

  (prinl-info "cmd-format")

  (make-str (format (get *0 'b 'c 'a 'data)) 'x))

#####################

# Reuse strs for these fns

(de cmd-pack-syms (Ks)

  (prinl-info "cmd-pack-syms")

  # Pack from Ptr until invalid
  # Pack line then replace it

  (let Ref (get *0 'b)
  
    (when (isa '(+Nil) Ref)
      (setq Ref (get Ref 'c)))
  
    (when (isa '(+Str) (get Ref 'a))  
  
      (let (End Ref
            (Sta Lst) (build-str End)
            Data (pack Lst))
            
        (println 'cmd-pack Sta (get Sta 'data) "TO" End (get End 'data))

        (repl-list-str Sta End Data)))))

        
(de cmd-chop (Ks)

  (prinl-info "cmd-chop")

  (let Ref (get *0 'b)
  
    (when (isa '(+Nil) Ref)
      (setq Ref (get Ref 'c)))
  
    (when (isa '(+Str) (get Ref 'a))  
    
      (let (Sta (get Ref 'c)
            End (get Ref 'b)
            Prv NIL
            List (chop (get Ref 'a 'data)))
  
        (repl-str-list Ref List)))))
  

#######

    
(de cmd-glue-sp-syms (Ks)

  (prinl-info "cmd-glue-sp-syms")
  
  (let Ref (get *0 'b)
  
    (when (isa '(+Nil) Ref)
      (setq Ref (get Ref 'c)))
  
    (when (isa '(+Str) (get Ref 'a)) 
  
      (let (End Ref
            (Sta Lst) (build-str End)
            Data (glue " " Lst))
            
        (println 'cmd-glue-sp Sta (get Sta 'data) "TO" End (get End 'data))
            
        (repl-list-str Sta End Data)))))
    
    
(de cmd-split-sp (Ks)

  (prinl-info "cmd-split-sp")
  
  # Split current Ref by space
  
  (let Ref (get *0 'b)
  
    (when (isa '(+Nil) Ref)
      (setq Ref (get Ref 'c)))
  
    (when (isa '(+Str) (get Ref 'a))
      
      (let (Sta (get Ref 'c)
            End (get Ref 'b)
            List (mapcar pack (split (chop (get Ref 'a 'data)) " ")))

        (repl-str-list Ref List)))))
        
        
(de cmd-intern (Ks)
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
