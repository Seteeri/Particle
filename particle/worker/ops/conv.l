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
        Ls (chop (Reg B))
        Pt-Ls (gen-point Ls 'x NIL T))
  
    # Move to refs-*> ?
    (when (sym? A)
      (setq Reg-B 'b))
  
    (when (get Pt-B 'line)
    
      (if (= Reg-B 'b)
          (mov-line Pt-B Pt-Ls)
          (del-line Pt-B))
          
      (lns> *ptr))
      
    (set-ln-n> *ptr Pt-Ls)
    
    (del> Pt-B T T)
    
    (clr> *ptr)
    
    (if (= Reg-B 'a)
        
        (prog
          (set> Pt-A Ls Pt-Ls)
          (if (get Pt-Ls 'line)
              (put @ 'p NIL)
              (put Pt-Ls 'line
                (new '(+Line))))
          (pt> *ptr Ls 'a))
      
        (prog
          (con> Pt-A Ls Pt-Ls)
          (println Ls)
          (pt> *ptr Ls 'b)))

    (lay-*start)
    (top> Pt-Ls)
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
        Ls (list (glue NIL C))
        Pt-Ls (gen-point Ls 'x NIL T))
  
    # Move to refs-*> ?
    (when (sym? B)
      (setq Reg-C 'b))  
  
    (when (get Pt-C 'line)
      (if (= Reg-C 'b)
          (mov-line Pt-C Pt-Ls)
          (del-line Pt-C))
      (lns> *ptr))
    (set-ln-n> *ptr Pt-Ls)
    
    (del> Pt-C T T)
    
    (clr> *ptr)
    
    (if (= Reg-C 'a)
        
        (prog
          (set> Pt-B Ls Pt-Ls)
          (if (get Pt-Ls 'line)
              (put @ 'p NIL)
              (put Pt-Ls 'line
                (new '(+Line))))
          (pt> *ptr Ls 'a))
      
        (prog
          (con> Pt-B Ls Pt-Ls)
          (pt> *ptr Ls 'b)))

    (lay-*start)
    (top> Pt-Ls)
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
            Ls (make
                   (let L (split (chop (get Ref 'a 'any)) " ")
                     (for S L
                       (when S
                         (link (pack S)))))))
            
        (repl-str-list Ref Ls)
        (lay-ptr *ptr)))))

