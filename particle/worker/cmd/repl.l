######
# REPL  

(de cmd-rd ())

(de cmd-ev ())

(de cmd-pp ())

(de cmd-rep ())

###################

(de cmd-eval-str ()
  (prinl-info "cmd-eval-str")
  
  # Eval Ref data directly
  
  (let Ref (get-ptr-ref)
  
    (when (is-nil? Ref)
      (setq Ref (get Ref 'c)))
  
    (when (has-str?> Ref)
  
      (let (In (get Ref 'a 'any)
            Out (eval (car (str In))))

        # Move to next line
        (make-line (make-str Out 'x))))))


(de cmd-eval ()
  (prinl-info "cmd-eval")
  
  # Eval Ref data directly
  
  # Assumes output
  
  # If user wants no output,
  # like ptr cmd, then another cmd
  
  (let (Ref (get-ptr-ref)
        In (get Ref 'any)
        Out (eval In))

    # If list, mov 'c 'b
    # else, append and make nl
    (pt-sup)
    (pt-nxt)        
        
    (make-str Out 'x)))
  

(de cmd-str ()
  (prinl-info "cmd-str")
  
  # Parse str/code to lisp data
  
  (let Ref (get-ptr-ref)
  
    (when (is-nil? Ref)
      (setq Ref (get Ref 'c)))
  
    (when (has-str?> Ref)
      
      (let (In (get Ref 'a 'any)
            Out (str In))
                        
        (make-str (car Out) 'x)))))        
        
        
(de cmd-pes ()
  # Pack and eval-Str
  
  (let Ref (get-ptr-ref)
  
    (when (is-nil? Ref)
      (setq Ref (get Ref 'c)))
  
    (when (has-str?> Ref)  
  
      (let (End Ref
            (Sta Ls) (build-str End)
            Str (pack Ls)
            Out (eval (car (str Str))))
        
        (repl-list-str Sta End Str T)        
        (make-line (make-str Out 'x))))))
