#{
  Str:
  
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
  
  CUA:
  
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
  
  List:
  
    * rot-fwd
    * rot-back
    * flatten
    * split
    * join
    * flip/reverse  
  
  REPL:  
  
    Conv Atm (Alt-L+space ctx aware)
    - str     -> sym
    - str/num -> num
    - num     -> str    
  
    #{
      [] \ = 3 = eval-atom, eval-list
      ;'  = 2
      <>? = 3
    }#
    # intern -> convert function
    # - map to double shift  
  
  Modes:
    
    * Text
      * Purpose: simulate typing text
      * ASCII = pair/sym
      * Enter = make newline and +Line
    * Cmd
      * Diff sectors on kb have diff functionality
    
    * Changing mode will change bindings
    * Least frequently used commands have
    most difficult finger gymnastics
    
    * Candidates
      * Tab
      * Grave/Tilde
      * F<N>
      * Double shift?
            
      * Use Ctrl/Alt to insert the character
      
  
}#
  

(def '*binds-cmd

  '(
    ########
    # Static
  
    ((ksd~Tab) cmd-chg-mode)
  
    ((ksd~Alt-L ksd~Tab)    cmd-make-char)
    ((ksd~Alt-R ksd~Tab)    cmd-make-char)
    
    ((ksd~BackSpace) cmd-bs)
    ((ksd~Delete   ) cmd-del)  
    
    ((ksd~Shift-R   ksd~Return)    cmd-make-line)
    ((ksd~Shift-R   ksd~BackSpace) cmd-del-line)
    
    # DEBUG
    ((ksd~Control-R ksd~Return) cmd-print-ptr)    
    
    ##########
    # List Ops

    ((ksd~r) cmd-make-nil)
    ((ksd~t) cmd-make-pair)
    ((ksd~y) cmd-make-pair)
    ((ksd~f) cmd-alt-lay)
    ((ksd~g) cmd-alt-reg)
    
    ##############
    # REPL - Right

    # cmd-intern
    # cmd-eval-str
    # cmd-eval
    # cmd-pes
    # cmd-str
    
    #################
    # Str Ops - Right
    
    # cmd-pack-syms
    # cmd-chop
    # cmd-glue-sp-syms
    # cmd-split-sp
    #
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
    
    #((ksd~x) ((Ks) (println "Ctrl-L x: CUT")))
    #((ksd~c) ((Ks) (println "Ctrl-L c: COPY")))
    #((ksd~v) ((Ks) (println "Ctrl-L v: PASTE")))
    #((ksd~z) ((Ks) (println "Ctrl-L z: UNDO")))
    #((ksd~y) ((Ks) (println "Ctrl-L y: REDO")))
    
    ###################
    # Pointer Semantics    
    
    ((ksd~w) cmd-ptr-up)
    ((ksd~a) cmd-ptr-left)      
    ((ksd~s) cmd-ptr-down)
    ((ksd~d) cmd-ptr-right)
    
    ((ksd~q) cmd-ptr-ln-first)
    ((ksd~e) cmd-ptr-ln-last)

    ############################
    # Camera Translation - Right

    ((ksd~Right) cmd-move-cam-px)
    ((ksd~Left)  cmd-move-cam-nx)
    ((ksd~Up)    cmd-move-cam-py)
    ((ksd~Down)  cmd-move-cam-ny)
    
    ((ksd~Prior) cmd-zoom-cam-in)
    ((ksd~Next)  cmd-zoom-cam-out)

    #############################
    # Pointer Translation - Right
    
    # ((ksd~Right) cmd-trans-ptr-px)
    # ((ksd~Left)  cmd-trans-ptr-nx)
    # ((ksd~Up)    cmd-trans-ptr-py)
    # ((ksd~Down)  cmd-trans-ptr-ny)    
    
  ))

  
(de list-binds-cmd @
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
