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
  
    # DEBUG
    ((ksd~Control-R ksd~Return)    cmd-print-ptr)    
    ((ksd~Control-R ksd~BackSpace) cmd-reset-start)  
  
    ((ksd~Alt-L ksd~Tab) cmd-make-char)
    ((ksd~Alt-R ksd~Tab) cmd-make-char)
    ((ksd~BackSpace)     cmd-bsp)
    ((ksd~Delete)        cmd-del)  
            
    ####################
    # Pair Ops = Top Right
    # R T | Y U I O P [ ] \
    
    # cmd-make-pair-y-ln : cur ln or num of following lns
    # or enter can do this
    
    ((ksd~bracketleft)  cmd-make-pair-x)
    ((ksd~bracketright) cmd-make-pair-y)
    ((ksd~backslash)        cmd-alt-reg)
    
    # Shifts
    #((ksd~r) cmd-rot-r)    # Pt to list
    #((ksd~t) cmd-rot-l)    # Pt to list
    #((ksd~y) cmd-flip/rev) # Pt to list
    
    # Links
    #((ksd~u) cmd-flatten) # Pt to list    
    #((ksd~i) cmd-join)    # Pt to list of lists
    #((ksd~o) cmd-split)   # Pt to any in list    
    
    ((ksd~p) cmd-make-nil)
    
    ((ksd~q) cmd-ptr-ls-first)
    ((ksd~e) cmd-ptr-ls-last)
    
    #######################
    # Atomic Ops = Right Mid
    # F G | H J K L ; '
    
    # Atom <-> List
    # * CHOP/SPLIT GLUE/PACK
    # Atom <-> Atom
    # * CONV (format, intern)
    
    #((ksd~h) ?)
    ((ksd~j) cmd-conv)
    ((ksd~k) cmd-chop/split)
    ((ksd~l) cmd-glue)    
              
    ######################
    # REPL = Right Bottom:
    # Str/Code <-> Any
    # Any <-> Any

    # cmd-read = str -> any
    # cmd-eval = any -> any
    # cmd-pp = any -> str

    # cmd-rep = str -> any -> str    
    # cmd-re = str -> any

    ((ksd~n)      cmd-read)
    ((ksd~m)      cmd-eval)
    ((ksd~comma)  cmd-pp)
    ((ksd~period) cmd-rep)
    ((ksd~slash)  cmd-re)
    
    #####################
    # Layout = Ctrl Chars
    # B | N M , . /
    
    # Make B redo?

    ((ksd~n) cmd-ptr-ln-first)
    ((ksd~m) cmd-ptr-ln-last)
    
    ((ksd~Shift-R ksd~BackSpace)  cmd-del-line)
    ((ksd~Shift-R ksd~Return)     cmd-make-line)
    ((ksd~Shift-L ksd~Tab)        cmd-alt-lay)     
        
    #######################
    # CUA Ops = Left Bottom
    
    # Most Microsoft Windows applications: 
    #   Undo: Ctrl+Z or Alt+Backspace
    #   Redo: Ctrl+Y or Ctrl+Shift+Z
    # Most Apple Macintosh applications:
    #   Undo: Command-Z
    #   Redo: Command-Shift-Z or Command-Y
    
    # Cut* : push object once
    # Copy* : push same object repeatedly
    
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
    
    ###############################
    # Pointer Semantics = Left Top/Mid Side  
    
    ((ksd~s) cmd-ptr-up)
    ((ksd~a) cmd-ptr-left)      
    ((ksd~r) cmd-ptr-down)
    ((ksd~t) cmd-ptr-right)
    
    ##################################
    # Camera Translation - Right Side?
    # These should be used sparingly
    # similar to nav keys in Vim

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
