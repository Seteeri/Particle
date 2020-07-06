#{

  For args: get from registers
  Ex: For space + split (plain), 
      mov any to reg, do cmd
  So split-sp will mov sp to reg, do cm,d rest reg
        
  Workflows
  * Pack Chars into Back Str:
    * Make chars, pack char to back str
  * Edit Words:
    * Mov ptr, chop till sp/reg/ptr
      * Replace with list
      * Reg could be similar to jump nearest...
      * Need not split exactly, just near
      * After split, mov to end of list
    * Pack list(s) recursively until ptr
  * Notes
    * Default is "destructive"
    * Technically, undo will save old str
    
  cmd-glue-sp (list of any -> str)
    (glue NIL (...)) = (pack ... ...)

  cmd-pack (any ... -> str)        

  cmd-chop (str -> list of str)
    (chop any) = ()
    
  cmd-split-sp (list of any -> list)    
  cmd-reverse/flip (list -> list)
  cmd-rot (list -> list)    
  
}#

############
# CHOP/SPLIT

(de chop-atom (Reg)

  # Delete entire list
  # Con new list
  
  # Refactor gen-point
  # * Create lines every X atoms
  # * Reuse particles
  
  (let ((C Pt-C Reg-C
         B Pt-B Reg-B
         A Pt-A) (refs-2> *ptr)
        Lst (chop (Reg B))
        Pt-Lst (gen-point Lst 'x NIL T))
  
    # Move to refs-*> ?
    (when (sym? A)
      (setq Reg-B 'b))
  
    (when (get Pt-B 'line)
    
      (if (= Reg-B 'b)
          (mov-line Pt-B Pt-Lst)
          (del-line Pt-B))
          
      (lns> *ptr))
      
    (set-ln-n> *ptr Pt-Lst)
    
    (del> Pt-B T T)
    
    (clr> *ptr)
    
    (if (= Reg-B 'a)
        
        (prog
          (set> Pt-A Lst Pt-Lst)
          (if (get Pt-Lst 'line)
              (put @ 'p NIL)
              (put Pt-Lst 'line
                (new '(+Line))))
          (pt> *ptr Lst 'a))
      
        (prog
          (con> Pt-A Lst Pt-Lst)
          (println Lst)
          (pt> *ptr Lst 'b)))

    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    (top> Pt-Lst)
    (draw> *ptr)))
    
    
# Refactor gen first

(de split-pair-a ()
  (println 'split-pair-a))

(de split-pair-b ()
  (println 'split-pair-b))
  

######
# GLUE

(de glue-atom-a ()
  (sup> *ptr)
  
  (glue-pair))

  
(de glue-atom-b ()

  # Opts:
  # * Glue entire list -> user points to pair
  # * Glue entire line -> user points to start of line
  # * Glue cur pair to prv -> user points to pair
  
  (ptr-ls-first T)
  (glue-pair))


(de glue-pair ()

  # Delete entire list
  # Con new list

  (let ((C Pt-C Reg-C
         B Pt-B) (refs-1> *ptr)
        Lst (list (glue NIL C))
        Pt-Lst (gen-point Lst 'x NIL T))
  
    # Move to refs-*> ?
    (when (sym? B)
      (setq Reg-C 'b))  
  
    (when (get Pt-C 'line)
      (if (= Reg-C 'b)
          (mov-line Pt-C Pt-Lst)
          (del-line Pt-C))
      (lns> *ptr))
    (set-ln-n> *ptr Pt-Lst)
    
    (del> Pt-C T T)
    
    (clr> *ptr)
    
    (if (= Reg-C 'a)
        
        (prog
          (set> Pt-B Lst Pt-Lst)
          (if (get Pt-Lst 'line)
              (put @ 'p NIL)
              (put Pt-Lst 'line
                (new '(+Line))))
          (pt> *ptr Lst 'a))
      
        (prog
          (con> Pt-B Lst Pt-Lst)
          (pt> *ptr Lst 'b)))

    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    (top> Pt-Lst)
    (draw> *ptr)))
    


#####################  
#####################  
#####################
    
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

