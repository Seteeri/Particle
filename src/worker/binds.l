#{
  Conv Atom (Alt-L+space ctx aware)
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
    
    ((ksd~BackSpace) cmd-del)
    ((ksd~Delete   ) cmd-del)  
    
    # Make cmds only mod+ASCII? to maintain homerow
    ((ksd~Alt-R   ksd~Return)    cmd-mov-nl)
    ((ksd~Alt-R   ksd~BackSpace) cmd-mov-bl)
    
    # List
    ((ksd~Control-L ksd~r) cmd-make-nil)
    ((ksd~Control-L ksd~t) cmd-make-list)
    
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

  
(def '*binds-num
  '(
    ((ksd~d0) cmd-make-num)
    ((ksd~d1) cmd-make-num)
    ((ksd~d2) cmd-make-num)
    ((ksd~d3) cmd-make-num)
    ((ksd~d4) cmd-make-num)
    ((ksd~d5) cmd-make-num)
    ((ksd~d6) cmd-make-num)
    ((ksd~d7) cmd-make-num)
    ((ksd~d8) cmd-make-num)
    ((ksd~d9) cmd-make-num)))     


(def '*binds-latin-1
  '(
    ((ksd~space) cmd-make-char)
    ((ksd~exclam) cmd-make-char)
    ((ksd~quotedbl) cmd-make-char)
    ((ksd~numbersign) cmd-make-char)
    ((ksd~dollar) cmd-make-char)
    ((ksd~percent) cmd-make-char)
    ((ksd~ampersand) cmd-make-char)
    ((ksd~apostrophe) cmd-make-char)
    ((ksd~quoteright) cmd-make-char)
    ((ksd~parenleft) cmd-make-char)
    ((ksd~parenright) cmd-make-char)
    ((ksd~asterisk) cmd-make-char)
    ((ksd~plus) cmd-make-char)
    ((ksd~comma) cmd-make-char)
    ((ksd~minus) cmd-make-char)
    ((ksd~period) cmd-make-char)
    ((ksd~slash) cmd-make-char)
    
    ((ksd~d0) cmd-make-char)
    ((ksd~d1) cmd-make-char)
    ((ksd~d2) cmd-make-char)
    ((ksd~d3) cmd-make-char)
    ((ksd~d4) cmd-make-char)
    ((ksd~d5) cmd-make-char)
    ((ksd~d6) cmd-make-char)
    ((ksd~d7) cmd-make-char)
    ((ksd~d8) cmd-make-char)
    ((ksd~d9) cmd-make-char)
    
    ((ksd~colon) cmd-make-char)
    ((ksd~semicolon) cmd-make-char)
    ((ksd~less) cmd-make-char)
    ((ksd~equal) cmd-make-char)
    ((ksd~greater) cmd-make-char)
    ((ksd~question) cmd-make-char)
    ((ksd~at) cmd-make-char)
    ((ksd~A) cmd-make-char)
    ((ksd~B) cmd-make-char)
    ((ksd~C) cmd-make-char)
    ((ksd~D) cmd-make-char)
    ((ksd~E) cmd-make-char)
    ((ksd~F) cmd-make-char)
    ((ksd~G) cmd-make-char)
    ((ksd~H) cmd-make-char)
    ((ksd~I) cmd-make-char)
    ((ksd~J) cmd-make-char)
    ((ksd~K) cmd-make-char)
    ((ksd~L) cmd-make-char)
    ((ksd~M) cmd-make-char)
    ((ksd~N) cmd-make-char)
    ((ksd~O) cmd-make-char)
    ((ksd~P) cmd-make-char)
    ((ksd~Q) cmd-make-char)
    ((ksd~R) cmd-make-char)
    ((ksd~S) cmd-make-char)
    ((ksd~lT) cmd-make-char)
    ((ksd~U) cmd-make-char)
    ((ksd~V) cmd-make-char)
    ((ksd~W) cmd-make-char)
    ((ksd~X) cmd-make-char)
    ((ksd~Y) cmd-make-char)
    ((ksd~Z) cmd-make-char)
    ((ksd~bracketleft) cmd-make-char)
    ((ksd~backslash) cmd-make-char)
    ((ksd~bracketright) cmd-make-char)
    ((ksd~asciicircum) cmd-make-char)
    ((ksd~underscore) cmd-make-char)
    ((ksd~grave) cmd-make-char)
    ((ksd~quoteleft) cmd-make-char)
    ((ksd~a) cmd-make-char)
    ((ksd~b) cmd-make-char)
    ((ksd~c) cmd-make-char)
    ((ksd~d) cmd-make-char)
    ((ksd~e) cmd-make-char)
    ((ksd~f) cmd-make-char)
    ((ksd~g) cmd-make-char)
    ((ksd~h) cmd-make-char)
    ((ksd~i) cmd-make-char)
    ((ksd~j) cmd-make-char)
    ((ksd~k) cmd-make-char)
    ((ksd~l) cmd-make-char)
    ((ksd~m) cmd-make-char)
    ((ksd~n) cmd-make-char)
    ((ksd~o) cmd-make-char)
    ((ksd~p) cmd-make-char)
    ((ksd~q) cmd-make-char)
    ((ksd~r) cmd-make-char)
    ((ksd~s) cmd-make-char)
    ((ksd~t) cmd-make-char)
    ((ksd~u) cmd-make-char)
    ((ksd~v) cmd-make-char)
    ((ksd~w) cmd-make-char)
    ((ksd~x) cmd-make-char)
    ((ksd~y) cmd-make-char)
    ((ksd~z) cmd-make-char)
    ((ksd~braceleft) cmd-make-char)
    ((ksd~bar) cmd-make-char)
    ((ksd~braceright) cmd-make-char)
    ((ksd~asciitilde) cmd-make-char)

    ((ksd~nobreakspace) cmd-make-char)
    ((ksd~exclamdown) cmd-make-char)
    ((ksd~cent) cmd-make-char)
    ((ksd~sterling) cmd-make-char)
    ((ksd~currency) cmd-make-char)
    ((ksd~yen) cmd-make-char)
    ((ksd~brokenbar) cmd-make-char)
    ((ksd~section) cmd-make-char)
    ((ksd~diaeresis) cmd-make-char)
    ((ksd~copyright) cmd-make-char)
    ((ksd~ordfeminine) cmd-make-char)
    ((ksd~guillemotleft) cmd-make-char)
    ((ksd~notsign) cmd-make-char)
    ((ksd~hyphen) cmd-make-char)
    ((ksd~registered) cmd-make-char)
    ((ksd~macron) cmd-make-char)
    ((ksd~degree) cmd-make-char)
    ((ksd~plusminus) cmd-make-char)
    ((ksd~twosuperior) cmd-make-char)
    ((ksd~threesuperior) cmd-make-char)
    ((ksd~acute) cmd-make-char)
    ((ksd~mu) cmd-make-char)
    ((ksd~paragraph) cmd-make-char)
    ((ksd~periodcentered) cmd-make-char)
    ((ksd~cedilla) cmd-make-char)
    ((ksd~onesuperior) cmd-make-char)
    ((ksd~masculine) cmd-make-char)
    ((ksd~guillemotright) cmd-make-char)
    ((ksd~onequarter) cmd-make-char)
    ((ksd~onehalf) cmd-make-char)
    ((ksd~threequarters) cmd-make-char)
    ((ksd~questiondown) cmd-make-char)
    ((ksd~Agrave) cmd-make-char)
    ((ksd~Aacute) cmd-make-char)
    ((ksd~Acircumflex) cmd-make-char)
    ((ksd~Atilde) cmd-make-char)
    ((ksd~Adiaeresis) cmd-make-char)
    ((ksd~Aring) cmd-make-char)
    ((ksd~AE) cmd-make-char)
    ((ksd~Ccedilla) cmd-make-char)
    ((ksd~Egrave) cmd-make-char)
    ((ksd~Eacute) cmd-make-char)
    ((ksd~Ecircumflex) cmd-make-char)
    ((ksd~Ediaeresis) cmd-make-char)
    ((ksd~Igrave) cmd-make-char)
    ((ksd~Iacute) cmd-make-char)
    ((ksd~Icircumflex) cmd-make-char)
    ((ksd~Idiaeresis) cmd-make-char)
    ((ksd~ETH) cmd-make-char)
    ((ksd~Eth) cmd-make-char)
    ((ksd~Ntilde) cmd-make-char)
    ((ksd~Ograve) cmd-make-char)
    ((ksd~Oacute) cmd-make-char)
    ((ksd~Ocircumflex) cmd-make-char)
    ((ksd~Otilde) cmd-make-char)
    ((ksd~Odiaeresis) cmd-make-char)
    ((ksd~multiply) cmd-make-char)
    ((ksd~Oslash) cmd-make-char)
    ((ksd~Ooblique) cmd-make-char)
    ((ksd~Ugrave) cmd-make-char)
    ((ksd~Uacute) cmd-make-char)
    ((ksd~Ucircumflex) cmd-make-char)
    ((ksd~Udiaeresis) cmd-make-char)
    ((ksd~Yacute) cmd-make-char)
    ((ksd~THORN) cmd-make-char)
    ((ksd~Thorn) cmd-make-char)
    ((ksd~ssharp) cmd-make-char)
    ((ksd~agrave) cmd-make-char)
    ((ksd~aacute) cmd-make-char)
    ((ksd~acircumflex) cmd-make-char)
    ((ksd~atilde) cmd-make-char)
    ((ksd~adiaeresis) cmd-make-char)
    ((ksd~aring) cmd-make-char)
    ((ksd~ae) cmd-make-char)
    ((ksd~ccedilla) cmd-make-char)
    ((ksd~egrave) cmd-make-char)
    ((ksd~eacute) cmd-make-char)
    ((ksd~ecircumflex) cmd-make-char)
    ((ksd~ediaeresis) cmd-make-char)
    ((ksd~igrave) cmd-make-char)
    ((ksd~iacute) cmd-make-char)
    ((ksd~icircumflex) cmd-make-char)
    ((ksd~idiaeresis) cmd-make-char)
    ((ksd~eth) cmd-make-char)
    ((ksd~ntilde) cmd-make-char)
    ((ksd~ograve) cmd-make-char)
    ((ksd~oacute) cmd-make-char)
    ((ksd~ocircumflex) cmd-make-char)
    ((ksd~otilde) cmd-make-char)
    ((ksd~odiaeresis) cmd-make-char)
    ((ksd~division) cmd-make-char)
    ((ksd~oslash) cmd-make-char)
    ((ksd~ooblique) cmd-make-char)
    ((ksd~ugrave) cmd-make-char)
    ((ksd~uacute) cmd-make-char)
    ((ksd~ucircumflex) cmd-make-char)
    ((ksd~udiaeresis) cmd-make-char)
    ((ksd~yacute) cmd-make-char)
    ((ksd~thorn) cmd-make-char)
    ((ksd~ydiaeresis) cmd-make-char)

  ))

  
(de list-binds-ascii @
  (let L NIL
    (while (next)
      (for Bind (arg)
        (let ((Keys Rep Cmd) Bind
              Keys (mapcar '((K) (if (sym? K) (val K) @))
                           Keys)
              Rep (if (sym? Rep) (val Rep) @)
              Cmd (if (sym? Cmd) (val Cmd) @))
          (push 'L (list Keys Rep Cmd)
                   (list (cons ksd~Shift-L Keys) Rep Cmd)
                   (list (cons ksd~Shift-R Keys) Rep Cmd)))))
    L))

    
(de list-binds-num @
  (let L NIL
    (while (next)
      (for Bind (arg)
        (let ((Keys Rep Cmd) Bind
              Keys (mapcar '((K) (if (sym? K) (val K) @))
                           Keys)
              Rep (if (sym? Rep) (val Rep) @)
              Cmd (if (sym? Cmd) (val Cmd) @))
          (push 'L (list (cons ksd~Alt-L Keys) Rep Cmd)
                   (list (cons ksd~Alt-R Keys) Rep Cmd)))))
    L))
    

(de list-binds @
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
(def '*binds-latin-1-OLD

  ## Number
  '(~(let List NIL
      (for (K ksd~d0 (<= K ksd~d9) (inc K))
        # make this alt
        (push 'List (list (list 'ksd~Control-L K) 'cmd-make-num))
        (push 'List (list (list 'ksd~Control-R K) 'cmd-make-num))
      List))
    ## Char Strings
    ~(let List NIL
      (for (K 33 (<= K 255) (inc K))
        (push 'List (list (list K)             'cmd-make-char))
        (push 'List (list (list 'ksd~Shift-L K) 'cmd-make-char))
        (push 'List (list (list 'ksd~Shift-R K) 'cmd-make-char)))
      List)))      
}#
