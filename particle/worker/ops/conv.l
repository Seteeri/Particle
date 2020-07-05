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

  (let (Pair (if (get *ptr 'pair) @ (get *ptr 'ref))
        Pt-Pair (lup-pt Pair)
        Back (find-back-any> *ptr Pair T)
        Pt-Back (lup-pt Back)
        Reg-Pair (if (sym? Back)
                     'b
                     (if (== (car Back) Pair) 'a 'b))
        Lst (chop (Reg Pair))
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
    

(de split-pair-a ())

(de split-pair-b ())
  

######
# GLUE

(de glue-atom-a ()
  (sup> *ptr)
  
  (glue-pair))

  
(de glue-atom-b ()
  # Pt to start of list, then glue
  
  (ptr-ls-first T)
  (glue-pair))


(de glue-pair ()

  # Delete entire list
  # Con new list

  #(let (Pair (if (get *ptr 'pair) @ (get *ptr 'ref))
  #      Pt-Pair (lup-pt Pair)))
  
  (let (Pair (get *ptr 'ref)
        Pt-Pair (lup-pt Pair)
        Back (find-back-any> *ptr Pair T)
        Pt-Back (lup-pt Back)
        Reg-Pair (if (sym? Back)
                     'b
                     (if (== (car Back) Pair) 'a 'b))
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

