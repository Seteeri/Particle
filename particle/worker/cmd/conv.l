###################
# ATOM/STR <-> LIST

(de cmd-chop/split ()

  #{
  
    If atom/str, chop
    If list, split with reg
      Ex: make sp, push to reg, do cmd
      Mov start or end?
  
  }#
  
  (cond
  
    ((a-atom?> *ptr)
      #(println 'make-pair "A ATOM/NIL")
      (if (lay-x?> *ptr)
          (make-pair-a-atom-x)
          (make-pair-a-atom-y)))
      
    ((b-atom?> *ptr)
      #(println 'make-pair "B ATOM/NIL")
      (make-pair-b-atom))
    
    ((a-pair?> *ptr)
      #(println 'make-pair "A PAIR")
      (make-pair-a-pair))
      
    ((b-pair?> *ptr)
      #(println 'make-pair "B PAIR")
      (make-pair-b-pair)
      # (if (lay-x?> *ptr) ...)
      )))  

  
(de cmd-glue/pack ()

  #{
  
    If atom/str, pack
    If list, glue with reg
      Ex: make sp, push to reg, do cmd
      Mov start or end?
  
  }#
  
  (cond
  
    ((a-atom?> *ptr)
      #(println 'make-pair "A ATOM/NIL")
      (if (lay-x?> *ptr)
          (make-pair-a-atom-x)
          (make-pair-a-atom-y)))
      
    ((b-atom?> *ptr)
      #(println 'make-pair "B ATOM/NIL")
      (make-pair-b-atom))
    
    ((a-pair?> *ptr)
      #(println 'make-pair "A PAIR")
      (make-pair-a-pair))
      
    ((b-pair?> *ptr)
      #(println 'make-pair "B PAIR")
      (make-pair-b-pair)
      # (if (lay-x?> *ptr) ...)
      )))


###############
# ATOM <-> ATOM

(de cmd-conv ()
  
  #{
  
    t-sym -> i-sym = intern
    i-sym -> t-sym = sym
    
    num <-> t-sym = format
    
    Note, can only intern str:
    num -> t-str -> i-sym
    
    How to do floating-point?
    - Handle as str + read
    
    'any/'str = str -> data
  
  }#

  T)
  

#####################  
#####################  
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
        

####
        
        
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
