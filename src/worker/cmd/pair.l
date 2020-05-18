(de cmd-make-list (Ks)
  (println 'cmd-make-list)

  #{
      [ ]  [ ]  [ ]
      a    b    c

      [ ]  [ ]  [ ]
      d    e    f
          *
          
      [ ]  [ ]  [ ]
      g    h    i
      
      NIL
      
      
      If on car:
      
        (a b c d e f g h i) -> (a b c d (e) f g h i)
      
      If on cdr:
      
        (a b c d e f g h i) -> (a b c d (e f g h i))
  }#  
  
  (let Ref (get *fwd-in 'b)
    (if *on-car
        (make-list-car Ref)
        (make-list-cdr Ref))))


(de cmd-mov-nl (Ks)
  (println 'cmd-mov-nl)
  (let Ref (get *fwd-in 'b)
    (unless *on-car
      (make-line Ref))))


(de cmd-mov-bl (Ks)
  
  # This moves Ref front of prv
  # Only applies to non-list...
    
  (let? Ref (get *fwd-in 'b)
    (backspace-line Ref)))


(de cmd-bs (Dir)

  (prinl-info "cmd-bs")

  (let? Ref (get *fwd-in 'b)
  
    (del-c Ref)))


(de cmd-del (Dir)

  (prinl-info "cmd-del")

  (let? Ref (get *fwd-in 'b)
  
    (del-tgt Ref)))    
    

# Evals        
        
        
(de cmd-eval-str ()
  (prinl-info "cmd-eval-str")
  
  # Eval Ref data directly
  
  (let Ref (get *fwd-in 'b)
  
    (when (isa '(+Nil) Ref)
      (setq Ref (get Ref 'c)))
  
    (when (isa '(+Str) (get Ref 'a))
  
      (let (In (get Ref 'a 'data)
            Out (eval (car (str In))))

        # Move to next line
        (make-line (make-str Out 'x))))))


(de cmd-eval ()
  (prinl-info "cmd-eval")
  
  # Eval Ref data directly
  
  # Assumes output
  
  # If user wants no output,
  # like ptr cmd, then another cmd
  
  (let (Ref (get *fwd-in 'b)
        In (get Ref 'a 'data)
        Out (eval In))

    (point-nxt)        
        
    (make-str Out 'x)))
  

(de cmd-str ()
  (prinl-info "cmd-str")
  
  # Parse str/code to lisp data
  
  (let Ref (get *fwd-in 'b)
  
    (when (isa '(+Nil) Ref)
      (setq Ref (get Ref 'c)))
  
    (when (isa '(+Str) (get Ref 'a))
      
      (let (In (get Ref 'a 'data)
            Out (str In))
                        
        (make-str (car Out) 'x)))))        
        
        
(de cmd-pes ()
  # Pack and eval-Str
  
  (let Ref (get *fwd-in 'b)
  
    (when (isa '(+Nil) Ref)
      (setq Ref (get Ref 'c)))
  
    (when (isa '(+Str) (get Ref 'a))  
  
      (let (End Ref
            (Sta Lst) (build-str End)
            Str (pack Lst)
            Out (eval (car (str Str))))
        
        (repl-list-str Sta End Str T)
        (make-line (make-str Out 'x))))))
        

# Fix

(de cmd-reset (Ks)

  (println 'cmd-reset)

  # Clear ptr
  (set-ptr)

  (println 'cmd-reset *master (get *master 'data))
  (del> *master T T)

  (gc))
