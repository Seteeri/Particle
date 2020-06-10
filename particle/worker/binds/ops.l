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
  
  Tab to switch mode:
  * Easiest on the fingers
}#
  

(def '*binds-ops

  '(
    ((ksd~Return) cmd-make-char)
    ((ksd~Tab)    cmd-make-char)
    
    ((ksd~BackSpace) cmd-bs)
    ((ksd~Delete   ) cmd-del)  
    
    # Modifies lines so relate to control chars
    # Poss tab indent list?
    ((ksd~Shift-R   ksd~Return)    cmd-mov-nl)
    ((ksd~Shift-R   ksd~BackSpace) cmd-mov-bl)
    
    ##########
    # List Ops
    # NIL/Pair/List
    ((ksd~Control-L ksd~r) cmd-make-nil)
    ((ksd~Control-L ksd~t) cmd-make-pair)
    ((ksd~Control-L ksd~y) cmd-make-pair)
    ((ksd~Control-L ksd~f) cmd-xchg-lay)
    ((ksd~Control-L ksd~g) cmd-xchg-reg)
    # rot-fwd
    # rot-back
    # flatten
    # split
    # join
    # flip/reverse

    ((ksd~Control-R ksd~r) cmd-make-nil)
    ((ksd~Control-R ksd~t) cmd-make-pair)
    ((ksd~Control-R ksd~y) cmd-make-pair)
    ((ksd~Control-R ksd~f) cmd-xchg-lay)
    ((ksd~Control-R ksd~g) cmd-xchg-reg)    
    
    ##############
    # REPL - Right
    #{
      [] \ = 3 = eval-atom, eval-list
      ;'  = 2
      <>? = 3
    }#
    # intern -> convert function
    # - map to double shift
    ((ksd~Alt-L   ksd~space)           cmd-intern)
    ((ksd~Control-L ksd~Tab)           cmd-eval-str)
    ((ksd~Control-L ksd~Alt-L ksd~Tab) cmd-eval)
    #((ksd~Control-L ...)              cmd-pes)    # Pack + Eval-Str
    #((ksd~Alt-L     ksd~Tab)          cmd-str)    
    
    #################
    # Str Ops - Right
    #{
      Basics:
      * split
      * concat/join
      * reverse/flip
    
      Y U = split-space - glue
      H J = chop - glue
      N M = reverse - flip
    }#
    # split-space : glue " "
    # chop : pack (or glue "")
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
    
    ##########
    # CUA Ops
    
    # Most Microsoft Windows applications: 
    #   Undo: Ctrl+Z or Alt+Backspace
    #   Redo: Ctrl+Y or Ctrl+Shift+Z
    # Most Apple Macintosh applications:
    #   Undo: Command-Z
    #   Redo: Command-Shift-Z or Command-Y
    
    # ESC = exit
    # N = new
    # O = open
    # F = find
    # H = replace
    # G = goto
    
    #((ksd~Control-L ksd~x) ((Ks) (println "Ctrl-L x: CUT")))
    #((ksd~Control-L ksd~c) ((Ks) (println "Ctrl-L c: COPY")))
    #((ksd~Control-L ksd~v) ((Ks) (println "Ctrl-L v: PASTE")))
    #((ksd~Control-L ksd~z) ((Ks) (println "Ctrl-L z: UNDO")))
    #((ksd~Control-L ksd~y) ((Ks) (println "Ctrl-L y: REDO")))
    #((ksd~Control-L ksd~Shift-L ksd~z) ((Ks) (println "Ctrl-L Shift-L z: REDO")))
    
    # DEBUG
    ((ksd~Control-R ksd~Return) cmd-print-ptr)

    ###################
    # Pointer Semantics    
    
    ((ksd~Control-L ksd~w) cmd-ptr-up)
    ((ksd~Control-L ksd~a) cmd-ptr-left)      
    ((ksd~Control-L ksd~s) cmd-ptr-down)
    ((ksd~Control-L ksd~d) cmd-ptr-right)

    ((ksd~Control-R ksd~w) cmd-ptr-up)
    ((ksd~Control-R ksd~a) cmd-ptr-left)      
    ((ksd~Control-R ksd~s) cmd-ptr-down)
    ((ksd~Control-R ksd~d) cmd-ptr-right)
    
    # Ctrl+Q/E = list
    # Ctrl+Shift+Q/E = line
    ((ksd~Control-L ksd~q) cmd-ptr-ln-first)
    ((ksd~Control-L ksd~e) cmd-ptr-ln-last)    

    #############################
    # Pointer Translation - Right
    
    ((ksd~Right) cmd-trans-ptr-px)
    ((ksd~Left)  cmd-trans-ptr-nx)
    ((ksd~Up)    cmd-trans-ptr-py)
    ((ksd~Down)  cmd-trans-ptr-ny)

    ############################
    # Camera Translation - Right

    ((ksd~Alt-R ksd~Right) cmd-move-cam-px)
    ((ksd~Alt-R ksd~Left)  cmd-move-cam-nx)
    ((ksd~Alt-R ksd~Up)    cmd-move-cam-py)
    ((ksd~Alt-R ksd~Down)  cmd-move-cam-ny)
    
    ((ksd~Prior)            cmd-zoom-cam-in)
    ((ksd~Next)             cmd-zoom-cam-out)
  
  ))    
