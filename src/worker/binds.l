(def '*bindings-key

  ## Number
  '(~(let List NIL
      (for (K ksd~d0 (<= K ksd~d9) (inc K))
        (push 'List (list (list ksd~Control-L K) T cmd-make-num))
        (push 'List (list (list ksd~Control-R K) T cmd-make-num)))
      List)
  ## Char Strings
  ~(let List NIL
    (for (K 33 (<= K 255) (inc K))
      (push 'List (list (list K)                     T cmd-make-char))
      (push 'List (list (list ksd~Shift-L K)         T cmd-make-char))
      (push 'List (list (list ksd~Shift-R K)         T cmd-make-char)))
    List)

  ### Special cases: Space (32), Return, Tab
  ((`ksd~space)  T `cmd-make-char)
  ((`ksd~Return) T `cmd-make-char)
  ((`ksd~Tab)    T `cmd-make-char)

  ((`ksd~BackSpace) T `cmd-del)
  ((`ksd~Delete   ) T `cmd-del)  
  
  # Make cmds only mod+ASCII? to maintain homerow
  ((`ksd~Alt-R   `ksd~Return)    T `cmd-mov-nl)
  ((`ksd~Alt-R   `ksd~BackSpace) T `cmd-mov-bl)
  # mod+Tab = ?
  
  ((`ksd~Control-L `ksd~q) T ((Keysym) (println "Ctrl-L q: MOV-PTR-FIRST")))
  ((`ksd~Control-L `ksd~e) T ((Keysym) (println "Ctrl-L e: MOV-PTR-LAST")))
  ((`ksd~Control-L `ksd~r) T `cmd-make-nil)
  
  # cmd-swap-layout
  # cmd-reset
  # cmd-any
  # cmd-sym
  # cmd-intern 
  
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
  
  ((`ksd~Control-L `ksd~h) T `cmd-pack)
  ((`ksd~Control-L `ksd~j) T `cmd-chop)
  
  ((`ksd~Control-L `ksd~y) T `cmd-glue-sp)
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
    (let (Line *line
          Ref (get *0 'b))
    
      # Print current line
      (println "Ref: " Ref (get Ref 'data))
      (println "*on-car: " *on-car)
      (println "Line:")
      (println Line (get Line 'data))
      #(view (getl (get Line 'line)))
      (println 'p (get Line 'line 'p) (get Line 'line 'p 'data))
      (println 'n (get Line 'line 'n) (get Line 'line 'n 'data))
      (println 'l (get Line 'line 'l) (get Line 'line 'l 'data))

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
  ((`ksd~Next)             T `cmd-zoom-cam-out)))
