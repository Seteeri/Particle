(de list-binds-ops @
  (let L NIL
    (while (next)
      (for Bind (arg)
        (let ((Keys Rep Cmd) Bind
              Keys (mapcar '((K) (if (sym? K) (val K) @))
                           Keys)
              Rep (if (sym? Rep) (val Rep) @)
              Cmd (if (sym? Cmd) (val Cmd) @))
          (push 'L (list Keys Rep Cmd)))))
    L))  


#{
  Conv Atm (Alt-L+space ctx aware)
  - str     -> sym
  - str/num -> num
  - num     -> str
  
  Pack to prv
  
  Split fm spc (chr, NIL=spc)
  - use a sublist for args?
  - compl to pack
  - use to break words at the end
  - poss mov ptr to start, better user use cmd to move start
  - either replace or make sublist
    - repl makes most sense
    - explicit cmd for sublist or use args
    
  Glue w spc (chr, NIL=spc)
  
  Chop not as useful...
  
  New List
  - Mov car, alt enter
  
  Eval
  - ctrl enter?
  
  Command mode:
  - Does not replace universal cmds - allows adv fn/cmd composition
  
}#
  

(def '*binds-ops

  '(
    ((ksd~Return) cmd-make-char)
    ((ksd~Tab)    cmd-make-char)
    
    ((ksd~BackSpace) cmd-bs)
    ((ksd~Delete   ) cmd-del)  
    
    # Make cmds only mod+ASCII? to maintain homerow
    ((ksd~Alt-R   ksd~Return)    cmd-mov-nl)
    ((ksd~Alt-R   ksd~BackSpace) cmd-mov-bl)
    
    # List
    ((ksd~Control-L ksd~r) cmd-make-nil)
    ((ksd~Control-L ksd~t) cmd-make-pair)
    
    ((ksd~Control-L ksd~q) cmd-ptr-ln-first)
    ((ksd~Control-L ksd~e) cmd-ptr-ln-last)
        
    # REPL - use Tab or Space for universal cmds and mode change
    ((ksd~Alt-L   ksd~space) cmd-intern)
    
    ((ksd~Control-L ksd~Tab)           cmd-eval-str)
    ((ksd~Control-L ksd~Alt-L ksd~Tab) cmd-eval)
    ((ksd~Control-L ksd~g)             cmd-pes)    # Pack + Eval-Str
    #((ksd~Alt-L     ksd~Tab)          cmd-str)    
    
    # other cmds...
    
    ##########
    # CUA Ops
    
    #((ksd~Control-L ksd~x) ((Ks) (println "Ctrl-L x: CUT")))
    #((ksd~Control-L ksd~c) ((Ks) (println "Ctrl-L c: COPY")))
    #((ksd~Control-L ksd~v) ((Ks) (println "Ctrl-L v: PASTE")))
    #((ksd~Control-L ksd~z) ((Ks) (println "Ctrl-L z: UNDO")))
    #((ksd~Alt-L     ksd~z) ((Ks) (println "Alt-L z: REDO")))

    #########
    # Str Ops
    
    # split-space (word) : glue " ", 
    # chop (letter) : glue "", pack      
    # reverse, flip
    #
    # cmd-pack-list - ref must be a list or start of list
    # cmd-glue-sp-list - ref must be a list or start of list
    
    ((ksd~Control-L ksd~h) cmd-pack-syms)
    ((ksd~Control-L ksd~j) cmd-chop)
    
    ((ksd~Control-L ksd~y) cmd-glue-sp-syms)
    ((ksd~Control-L ksd~u) cmd-split-sp)
    
    # cmd-reverse
    # cmd-flip
    # cmd-rot
    
    # DEBUG
    ((ksd~Control-R ksd~Return) cmd-print-ptr)


    # Pointerranslation
    ((ksd~Control-L ksd~w) cmd-ptr-up)
    ((ksd~Control-L ksd~a) cmd-ptr-left)      
    ((ksd~Control-L ksd~s) cmd-ptr-down)
    ((ksd~Control-L ksd~d) cmd-ptr-right)

    ((ksd~Right) cmd-trans-ptr-px)
    ((ksd~Left)  cmd-trans-ptr-nx)
    ((ksd~Up)    cmd-trans-ptr-py)
    ((ksd~Down)  cmd-trans-ptr-ny)

    # Cameraranslation
    # Prior/Next
    ((ksd~Alt-R ksd~Right) cmd-move-cam-px)
    ((ksd~Alt-R ksd~Left)  cmd-move-cam-nx)
    ((ksd~Alt-R ksd~Up)    cmd-move-cam-py)
    ((ksd~Alt-R ksd~Down)  cmd-move-cam-ny)
    ((ksd~Prior)            cmd-zoom-cam-in)
    ((ksd~Next)             cmd-zoom-cam-out)
  
  ))    
