(de cmd-make-char (Ks)

  #{
    \r = ^M
    \n = ^J
    \t = ^I
  }#
  
  (let (Dat (cond ((= Ks ksd~Return) "^J")
                   ((= Ks ksd~Tab) "^I")
                   (T (char Ks)))
        Lay 'x)

    # Push to cmd list
    # Make class
    # Put properties

    (make-pair-str Dat Lay)))


(de cmd-make-nil (Ks)
  #(prinl-info "cmd-make-nil")
  (make-pair-str NIL 'x))


(de cmd-make-num (Ks)

  (println "cmd-make-num")

  (make-pair-str (format (char Ks)) 'x))


(de cmd-format (Ks)

  (prinl-info "cmd-format")

  (make-pair-str (format (get *ptr 'b 'c 'a 'any)) 'x))

#####################

# Reuse strs for these fns

(de cmd-pack-syms (Ks)

  (prinl-info "cmd-pack-syms")

  # Pack from Ptr until invalid
  # Pack line then replace it

  (let Ref (get-ptr-ref)
  
    (when (is-nil? Ref)
      (setq Ref (get Ref 'c)))
  
    (when (has-str?> Ref)  
  
      (let (End Ref
            (Sta Lst) (build-str End)
            Dat (pack Lst))
            
        (println 'cmd-pack Sta (get Sta 'any) "TO" End (get End 'any))

        (repl-list-str Sta End Dat)
        (lay-ptr *ptr)))))

        
(de cmd-chop (Ks)cmd-alt-lay

  (prinl-info "cmd-chop")

  (let Ref (get-ptr-ref)
  
    (when (is-nil? Ref)
      (setq Ref (get Ref 'c)))
  
    (when (has-str?> Ref)  
    
      (let (Sta (get Ref 'c)
            End (get Ref 'b)
            Prv NIL
            List (chop (get Ref 'a 'any)))
  
        (repl-str-list Ref List)
        (lay-ptr *ptr)))))
  

#######

    
(de cmd-glue-sp-syms (Ks)

  (prinl-info "cmd-glue-sp-syms")
  
  (let Ref (get-ptr-ref)
  
    (when (is-nil? Ref)
      (setq Ref (get Ref 'c)))
  
    (when (has-str?> Ref) 
  
      (let (End Ref
            (Sta Lst) (build-str End)
            Dat (glue " " Lst))
            
        (println 'cmd-glue-sp Sta (get Sta 'any) "TO" End (get End 'any))
            
        (repl-list-str Sta End Dat)
        (lay-ptr *ptr)))))
    
    
(de cmd-split-sp (Ks)

  (prinl-info "cmd-split-sp")
  
  # Split current Ref by space
  
  (let Ref (get-ptr-ref)
  
    (when (is-nil? Ref)
      (setq Ref (get Ref 'c)))
  
    (when (has-str?> Ref)

      #{
        : (split (chop "abc   def   ghi") " ")
        -> (("a" "b" "c") NIL NIL ("d" "e" "f") NIL NIL ("g" "h" "i"))        
        
        : (mapcar pack (split (chop "abc   def   ghi") " "))
        -> ("abc" NIL NIL "def" NIL NIL "ghi")
        
        NIL is the token itself - discard it
        This means not directly reversible using glue " "
        if mutiple tokens
      }#    
    
      (let (Sta (get Ref 'c)
            End (get Ref 'b)
            List (make
                   (let L (split (chop (get Ref 'a 'any)) " ")
                     (for S L
                       (when S
                         (link (pack S)))))))
            
        (repl-str-list Ref List)
        (lay-ptr *ptr)))))
        
        
(de cmd-intern (Ks)
  # Convert string to symbol
  # Build char until space
  
  (let Ref (get-ptr-ref)
  
    (when (is-nil? Ref)
      (setq Ref (get Ref 'c)))
  
    (when (has-str?> Ref)  
  
      (let (End Ref
            (Sta Lst) (build-str-sp End)
            Dat (intern (pack Lst)))
            
        (println 'cmd-intern Sta (get Sta 'any) "TO" End (get End 'any))
                
        (repl-list-str Sta End Dat)
        (lay-ptr *ptr)))))
