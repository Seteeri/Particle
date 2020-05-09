(de cmd-make-char (Keysym)

  #{
    \r = ^M
    \n = ^J
    \t = ^I
  }#
  
  (let (Data (cond ((= Keysym ksd~Return) "^J")
                   ((= Keysym ksd~Tab) "^I")
                   (T (char Keysym)))
        Lay 'x)

    # Push to cmd list
    # Make class
    # Put properties

    (make-str Data Lay)))


(de cmd-make-nil (Keysym)
  (prinl-info "cmd-make-nil")
  (make-str NIL 'x))


(de cmd-make-num (Keysym)

  (println "cmd-make-num")

  (make-str (format (char Keysym)) 'x))


(de cmd-format (Keysym)

  (prinl-info "cmd-format")

  (make-str (format (get *0 'b 'c 'a 'data)) 'x))

#####################

# Reuse strs for these fns

(de cmd-pack-syms (Keysym)

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

        
(de cmd-chop (Keysym)

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

    
(de cmd-glue-sp-syms (Keysym)

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
    
    
(de cmd-split-sp (Keysym)

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
