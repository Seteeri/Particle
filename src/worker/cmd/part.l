(de cmd-reset (Keysm)

  (println 'cmd-reset)

  # Clear ptr
  (put *0 'b)

  (println 'cmd-reset *master (get *master 'data))
  (del> *master T T)

  (gc))


(de cmd-make-char (Keysym)

  #{
    ksd~Return
    ksd~Tab
  }#

  (let (Data (when Keysym
               (cond ((= Keysym ksd~space) " ")
                     ((= Keysym ksd~Return) " ")
                     ((= Keysym ksd~Tab) " ")
                     (T (char Keysym))))
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
      
        (repl-line-str Sta End Data)))))

        
(de cmd-chop (Keysym)

  (prinl-info "cmd-chop")

  (let Ref (get *0 'b)
  
    (when (isa '(+Nil) Ref)
      (setq Ref (get Ref 'c)))
  
    (when (isa '(+Str) (get Ref 'a))  
  
      (let (Sta (get Ref 'c)
            End (get Ref 'b)
            List (chop (get Ref 'a 'data)))
      
        # Add list, item by item
        (let Prv Sta
        
          (for Data List
          
            (let Part (gen-part (list Data) 'x NIL T)
              
              # T must be NIL to skip
              (con-sym Prv Part T)
              
              (setq Prv Part)))

          (b> Prv End))
        
        (println Ref (get Ref 'data) (get Ref 'line))
        (put-line Ref (get Sta 'b))
        
        # Del Ref except Cdr
        (del-car> Ref)    
        
        (upd-tree)
        (mov-*1 *line)        
                
        # Put Ptr above new part
        (put *0 'b End)
        (mov-cur-abv End)
        (lay-part-start *0 *line 'skip-cdr)))))
        

#######

    
(de cmd-glue-sp-syms (Keysym)

  (prinl-info "cmd-glue-sp-syms")
  
  (let Ref (get *0 'b)
  
    (when (isa '(+Nil) Ref)
      (setq Ref (get Ref 'c)))
  
    (when (isa '(+Str) (get Ref 'a)) 
  
      (let (End Ref
            (Sta Lst) (build-str End)
            Data (glue "_" Lst))
            
        (println 'cmd-glue-sp Sta (get Sta 'data) "TO" End (get End 'data))
            
        (repl-line-str Sta End Data)))))
    
    
(de cmd-split-sp (Keysym)

  (prinl-info "cmd-split-sp")
  
  # Split current Ref by space
  
  (let Ref (get *0 'b)
  
    (when (isa '(+Nil) Ref)
      (setq Ref (get Ref 'c)))
  
    (when (isa '(+Str) (get Ref 'a))
      
      (let (Sta (get Ref 'c)
            End (get Ref 'b)
            List (mapcar pack (split (chop (get Ref 'a 'data)) "_")))
        
        # Add list, item by item
        (let Prv Sta
        
          (for Data List
          
            (let Part (gen-part (list Data) 'x NIL T)
              
              # T must be NIL to skip
              (con-sym Prv Part T)
              
              (setq Prv Part)))

          (b> Prv End))
        
        (println Ref (get Ref 'data) (get Ref 'line))
        (put-line Ref (get Sta 'b))
        
        # Del Ref except Cdr
        (del-car> Ref)    
        
        (upd-tree)
        (mov-*1 *line)        
                
        # Put Ptr above new part
        (put *0 'b End)
        (mov-cur-abv End)
        (lay-part-start *0 *line 'skip-cdr)))))


(de cmd-eval ()
  (prinl-info "cmd-eval")
  
  # Eval Ref data directly
  
  (let (Ref (get *0 'b)
        Sta (get Ref 'c)
        End (get Ref 'b)
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
      
      (let (Sta (get Ref 'c)
            End (get Ref 'b)
            In (get Ref 'a 'data)
            Out (str In))
            
        (point-nxt)
            
        (make-str (car Out) 'x)))))
  

############################


(de cmd-swap-layout (Keysym)
  (println "Swap layout")

  # Note this causes bounds change also
  # Maintain x and y bnds?

  # If NIL, get prev, else do ref

  (let (Ref (get *0 'b)
        Prv (get Ref 'c)
        Tgt (if (= (get Ref 'data) NIL) Prv Ref))
    (when Tgt
      (println "Swap!" Tgt (get Tgt 'data) (get Tgt 'lay) '-> (if (= (get Tgt 'lay) 'x) 'y 'x))
      (swap-lay Tgt))))

      
(de cmd-mov-nl (Keysym)
  (println "Move newline")

  (let Ref (get *0 'b)
    (if *on-car
        (make-list Ref)
        (make-line Ref))))


(de cmd-mov-bl (Keysym)
  
  # This moves Ref front of prv
  # Only applies to non-list...
    
  (let (Ref (get *0 'b)
        Prv (get Ref 'c))

    (when (and (get Ref 'line 'p)
               (not (= (get Ref 'data) '*start)))
    
      (unless (or (isa '(+Pair) (get Ref 'a))
                  (isa '(+Nil) (get Ref 'a))
                  (= (get Prv 'a) Ref))
                                    
        (setq *line (get Ref 'line 'p))
        # Remove line
        (del-line Ref)
        (put Ref 'line)

        (upd-tree)
        (mov-*1 *line)      
        
        # Ptr remains on Ref
        # How about for on-car?
        (mov-cur-abv Ref)
        (lay-part-start *0 *line 'skip-cdr)))))


###########################################################

# Advanced

(de cmd-intern (Keysym) (prinl-info "Use (intern <data>)"))
(de cmd-any (Keysym) (prinl-info "Use (any ...); str -> any"))
(de cmd-sym () (prinl-info "Use (sym ...); rev of (any ...)"))
