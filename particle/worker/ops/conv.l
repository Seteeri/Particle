############
# CHOP/SPLIT

(de chop-atom-a ())

(de chop-atom-b ())
  
(de split-pair-a ())

(de split-pair-b ())
  

######
# GLUE

(de glue-atom-a ()
  (sup> *ptr)
  
  (glue-pair))

  
(de glue-atom-b ()
  # Pt to start of list  
      
  (let (Pt-Pair (let (P (prop *ptr 'lns)
                      L NIL)
                  (loop (setq L (pop P))
                    (NIL (get L 'line 'p)))
                  (push P L) # Return it
                  L)
        Pair (get Pt-Pair 'any)
        Back (find-back-any> *ptr Pair T)
        Pt-Back (lup-pt Back)
        Reg (if (sym? Back)
                'b
                (if (= (car Back) Pair) 'a 'b)))
    
    (pt> *ptr Pair Reg)
    
    (glue-pair)))


(de glue-pair ()

  # Delete entire list
  # Con new list

  (let (Pair (get *ptr 'ref)
        Pt-Pair (lup-pt Pair)
        Back (find-back-any> *ptr Pair T)
        Pt-Back (lup-pt Back)
        Reg-Pair (if (sym? Back)
                     'b
                     (if (= (car Back) Pair) 'a 'b))
        Lst (list (glue NIL Pair))
        Pt-Lst (gen-point Lst 'x NIL T))
    
    (when (get Pt-Pair 'line)
      (if (= Reg-Pair 'b)
          (mov-line Pt-Pair Pt-Lst)
          (del-line Pt-Pair))
      (lns> *ptr))
    (set-ln-n> *ptr Pt-Lst)
    
    (del> Pt-Pair T T)
    (put *ptr 'pair NIL)
    (put *ptr 'ref NIL)    
    
    (if (= Reg-Pair 'a)
        
        (prog
          (set> Pt-Back Lst Pt-Lst)
          (if (get Pt-Lst 'line)
              (put @ 'p NIL)
              (put Pt-Lst 'line
                (new '(+Line))))
          (pt> *ptr Lst 'a))
      
        (prog
          (con> Pt-Back Lst Pt-Lst)
          (println Lst)
          (pt> *ptr Lst 'b)))

    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    (top> Pt-Lst)
    (draw> *ptr)))
    

###################
###################


  

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
