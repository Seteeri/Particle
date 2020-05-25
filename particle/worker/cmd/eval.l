(de cmd-eval-str ()
  (prinl-info "cmd-eval-str")
  
  # Eval Ref data directly
  
  (let Ref (get *ptr 'b)
  
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
  
  (let (Ref (get *ptr 'b)
        In (get Ref 'data)
        Out (eval In))

    # If list, mov 'c 'b
    # else, append and make nl
    (pt-sup)
    (pt-nxt)        
        
    (make-pair-str Out 'x)))
  

(de cmd-str ()
  (prinl-info "cmd-str")
  
  # Parse str/code to lisp data
  
  (let Ref (get *ptr 'b)
  
    (when (is-nil? Ref)
      (setq Ref (get Ref 'c)))
  
    (when (has-str?> Ref)
      
      (let (In (get Ref 'a 'data)
            Out (str In))
                        
        (make-pair-str (car Out) 'x)))))        
        
        
(de cmd-pes ()
  # Pack and eval-Str
  
  (let Ref (get *ptr 'b)
  
    (when (is-nil? Ref)
      (setq Ref (get Ref 'c)))
  
    (when (has-str?> Ref)  
  
      (let (End Ref
            (Sta Lst) (build-str End)
            Str (pack Lst)
            Out (eval (car (str Str))))
        
        (repl-list-str Sta End Str T)        
        (make-line (make-pair-str Out 'x))))))
