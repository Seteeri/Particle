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
    #(setq p1 (repl-cdr p1 (glue " " (list (date) (time) "cmd-make-char" Data)) 'y))

    (make-str Data Lay)))


(de cmd-make-nil (Keysym)
  (prinl-info "cmd-make-nil")
  (make-str NIL 'x))


(de cmd-del (Dir)

  (prinl-info "cmd-del")

  #{
  
    Delete previous

    
              *0
    [ ]  [ ]  NIL
    a    b

    #############
    
        *0
    [ ]  [ ]  NIL
    a    b

    #############
    
    *0
    [ ]  [ ]  NIL
    a    b    

    #############
    
    [ ]  [ ]  NIL
    a    b        

    *0
    [ ]  [ ]  NIL
        c        

    #############
    
    [ ]  [ ]  NIL
    a    b        

    
    [ ]  [ ]  NIL
        NIL        
        
    *0     
    NIL
    
  }#

  # Refactor into a fn in ops?

  # Similar to adding items, must update CDR and list CDR

  (let (Ref (get *0 'b)
        Tgt (if *on-car (get Ref 'c 'c) (get Ref 'c))
        Tgt-Prv (get Tgt 'c)
        Tgt-Cdr (get Tgt 'b)
        New-Ref NIL)

    (unless (= (get Tgt 'data) '*start)
    
      (cond ((or (isa '(+Pair) (get Tgt 'a))
                (isa '(+Nil) (get Tgt 'a)))
            # Remove line
            (del-line Tgt)
            (b> Tgt-Prv Tgt-Cdr)
            (put Tgt-Cdr 'line 'l (get Tgt 'line 'l))
            (println (get Tgt 'line 'l))
            (setq New-Ref Tgt-Cdr))
    
            # If Tgt prv is Car of Pair, set Car...
            ((= (get Ref 'c 'a) Ref)
              (put Tgt-Prv 'a Tgt-Cdr)
              # To connect, pop, then push
              (with Tgt-Prv
                (pop (:: data))
                (push (:: data) (get Tgt-Cdr 'data)))
              (put Tgt-Cdr 'c Tgt-Prv)
              (put-line Tgt Tgt-Cdr)
              
              # Tgt-Prv = Y pair
              # It's list has to be modified also
              # so get item before and connect new data
              (with (get Tgt-Prv 'c)
                (con (car (:: data)) (get Tgt-Prv 'data)))
                
              (setq New-Ref Ref))
                
            (T
            (b> Tgt-Prv Tgt-Cdr)
            (put Tgt-Cdr 'c Tgt-Prv)
            (when (get Tgt 'line)
              (put-line Tgt Tgt-Cdr))
            (setq New-Ref Ref)))
      
      # Unlink b before del
      (put Tgt 'b)
      (del> Tgt T T)
      
      (upd-tree)
      (mov-*1 *line)      
      
      # Ptr remains on Ref
      # How about for on-car?
      (put *0 'b New-Ref)
      (mov-cur-abv New-Ref)
      (lay-part-start *0 *line 'skip-cdr)
          
      T)))


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


(de cmd-eval ()
  (prinl-info "cmd-eval")
  
  # Eval Ref data directly
  
  (let (Ref (get *0 'b)
        In (get Ref 'a 'data)
        Out (eval In))

    (point-nxt)        
        
    (make-str Out 'x)))
  

(de cmd-str ()
  (prinl-info "cmd-str")
  
  # Parse str/code to lisp data
  
  (let Ref (get *0 'b)
  
    (when (isa '(+Nil) Ref)
      (setq Ref (get Ref 'c)))
  
    (when (isa '(+Str) (get Ref 'a))
      
      (let (In (get Ref 'a 'data)
            Out (str In))
                        
        (make-str (car Out) 'x)))))
        
        
(de cmd-eval-str ()
  (prinl-info "cmd-eval-str")
  
  # Eval Ref data directly
  
  (let Ref (get *0 'b)
  
    (when (isa '(+Nil) Ref)
      (setq Ref (get Ref 'c)))
  
    (when (isa '(+Str) (get Ref 'a))
  
      (let (In (get Ref 'a 'data)
            Out (eval (car (str In))))

        # Move to next line
        (make-line (make-str Out 'x))))))
