(de cmd-mov-nl (Ks)
  (println 'cmd-mov-nl)

  (let Ref (get *fwd-in 'b)
    (if *on-car
        (make-list-car Ref)
        (make-line Ref))))


(de cmd-mov-bl (Ks)
  
  # This moves Ref front of prv
  # Only applies to non-list...
    
  (let? Ref (get *fwd-in 'b)
    (bsp-line Ref)))


(de cmd-del (Dir)

  (prinl-info "cmd-del")

  (let? Ref (get *fwd-in 'b)
  
    (del-pair Ref)))


# Evals        
        
        
(de cmd-eval-str ()
  (prinl-info "cmd-eval-str")
  
  # Eval Ref data directly
  
  (let Ref (get *fwd-in 'b)
  
    (when (is-nil? Ref)
      (setq Ref (get Ref 'c)))
  
    (when (has-str?> Ref)
  
      (let (In (get Ref 'a 'data)
            Out (eval (car (str In))))

        # Move to next line
        (make-line (make-pair-str Out 'x))))))


(de cmd-eval ()
  (prinl-info "cmd-eval")
  
  # Eval Ref data directly
  
  # Assumes output
  
  # If user wants no output,
  # like ptr cmd, then another cmd

  (println (get *fwd-in 'b 'a 'data))
  
  (let (Ref (get *fwd-in 'b)
        In (get Ref 'a 'data)
        Out (eval In))
        
    (point-sup)
    (point-nxt)        
        
    (make-pair-str Out 'x)))
  

(de cmd-str ()
  (prinl-info "cmd-str")
  
  # Parse str/code to lisp data
  
  (let Ref (get *fwd-in 'b)
  
    (when (is-nil? Ref)
      (setq Ref (get Ref 'c)))
  
    (when (has-str?> Ref)
      
      (let (In (get Ref 'a 'data)
            Out (str In))
                        
        (make-pair-str (car Out) 'x)))))        
        
        
(de cmd-pes ()
  # Pack and eval-Str
  
  (let Ref (get *fwd-in 'b)
  
    (when (is-nil? Ref)
      (setq Ref (get Ref 'c)))
  
    (when (has-str?> Ref)  
  
      (let (End Ref
            (Sta Lst) (build-str End)
            Str (pack Lst)
            Out (eval (car (str Str))))
        
        (repl-list-str Sta End Str T)
        (make-line (make-pair-str Out 'x))))))
        

# Fix

(de cmd-reset (Ks)

  (println 'cmd-reset)

  # Clear ptr
  (set-ptr)

  (println 'cmd-reset *master (get *master 'data))
  (del> *master T T)

  (gc))
