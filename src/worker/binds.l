(def '*binds-ascii

  ## Number
  '(~(let List NIL
      (for (K ksd~d0 (<= K ksd~d9) (inc K))
        (push 'List (list (list ksd~Control-L K) T cmd-make-num))
        (push 'List (list (list ksd~Control-R K) T cmd-make-num)))
      List)
  ## Char Strings
  ~(let List NIL
    (for (K 33 (<= K 255) (inc K))
      (push 'List (list (list K)             T cmd-make-char))
      (push 'List (list (list ksd~Shift-L K) T cmd-make-char))
      (push 'List (list (list ksd~Shift-R K) T cmd-make-char)))
    List)))


(def '*binds-ops

  ### Special cases: Space (32), Return, Tab
  ((`ksd~space)  T `cmd-make-char)
  ((`ksd~Return) T `cmd-make-char)
  ((`ksd~Tab)    T `cmd-make-char)

  ((`ksd~BackSpace) T `cmd-del)
  ((`ksd~Delete   ) T `cmd-del)  
  
  # Make cmds only mod+ASCII? to maintain homerow
  ((`ksd~Alt-R   `ksd~Return)    T `cmd-mov-nl)
  ((`ksd~Alt-R   `ksd~BackSpace) T `cmd-mov-bl)
  
  ((`ksd~Control-L `ksd~r) T `cmd-make-nil)
  ((`ksd~Control-L `ksd~t) T `cmd-mov-nl)  
  
  ((`ksd~Control-L `ksd~q) T `cmd-ptr-ln-first)
  ((`ksd~Control-L `ksd~e) T `cmd-ptr-ln-last)
  # ctrl + alt + q = move to start of list  
  # ctrl + alt + e = move to end of list
    
  # Tab for eval
  # eval-str to pass to str
  # also need str cmd to create data
  ((`ksd~Control-L `ksd~Tab)            T `cmd-eval-str)
  ((`ksd~Alt-L     `ksd~Tab)            T `cmd-str)
  ((`ksd~Control-L `ksd~Alt-L `ksd~Tab) T `cmd-eval)
  
  # other cmds...
  
  ##########
  # CUA Ops
  
  ((`ksd~Control-L `ksd~x) T ((Keysym) (println "Ctrl-L x: CUT")))
  ((`ksd~Control-L `ksd~c) T ((Keysym) (println "Ctrl-L c: COPY")))
  ((`ksd~Control-L `ksd~v) T ((Keysym) (println "Ctrl-L v: PASTE")))
  ((`ksd~Control-L `ksd~z) T ((Keysym) (println "Ctrl-L z: UNDO")))
  ((`ksd~Alt-L     `ksd~z) T ((Keysym) (println "Alt-L z: REDO")))

  #########
  # Str Ops
  
  # split-space (word) : glue " ", 
  # chop (letter) : glue "", pack      
  # reverse, flip
  #
  # cmd-pack-list - ref must be a list or start of list
  # cmd-glue-sp-list - ref must be a list or start of list
  
  ((`ksd~Control-L `ksd~h) T `cmd-pack-syms)
  ((`ksd~Control-L `ksd~j) T `cmd-chop)
  
  ((`ksd~Control-L `ksd~y) T `cmd-glue-sp-syms)
  ((`ksd~Control-L `ksd~u) T `cmd-split-sp)
  
  # cmd-reverse
  # cmd-flip
  
  # DEBUG
  
  #(view (getl (get *0 'b)))
  # If we lay relative to origin should stay in same place
  ((`ksd~Control-R `ksd~Return) T 
  ((Keysym)

    # Print ptr
    (let Ref (get *0 'b)
      (println Ref (get Ref 'data)))

    (println "Master Line: " *master (get *master 'b 'data))

    # Print lines out
    (let Ref (get *0 'b)
    
      # Print current line
      (println "Current Ref: " Ref (get Ref 'data))
      (println "*on-car: " *on-car)
      (println "Current Line:" *line (get *line 'data))
      #(view (getl (get *line 'line)))
      (println 'p (get *line 'line 'p) (get *line 'line 'p 'data))
      (println 'n (get *line 'line 'n) (get *line 'line 'n 'data))
      (println 'l (get *line 'line 'l) (get *line 'line 'l 'data))

      (upd-tree)
      (mov-*1 *line)      
      
      (mov-*1 Line))))

  # Pointer Translation
  ((`ksd~Control-L `ksd~w) T `cmd-ptr-up)
  ((`ksd~Control-L `ksd~a) T `cmd-ptr-left)      
  ((`ksd~Control-L `ksd~s) T `cmd-ptr-down)
  ((`ksd~Control-L `ksd~d) T `cmd-ptr-right)

  ((`ksd~Right) T `cmd-trans-ptr-px)
  ((`ksd~Left)  T `cmd-trans-ptr-nx)
  ((`ksd~Up)    T `cmd-trans-ptr-py)
  ((`ksd~Down)  T `cmd-trans-ptr-ny)

  # Camera Translation
  # Prior/Next
  ((`ksd~Alt-R `ksd~Right) T `cmd-move-cam-px)
  ((`ksd~Alt-R `ksd~Left)  T `cmd-move-cam-nx)
  ((`ksd~Alt-R `ksd~Up)    T `cmd-move-cam-py)
  ((`ksd~Alt-R `ksd~Down)  T `cmd-move-cam-ny)
  ((`ksd~Prior)            T `cmd-zoom-cam-in)
  ((`ksd~Next)             T `cmd-zoom-cam-out))
