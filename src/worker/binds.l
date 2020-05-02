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
          (push 'L (list Keys Rep Cmd)
                   (list (cons ksd~Alt-L Keys) Rep Cmd)
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


(def '*binds-num
  '(
    ((ksd~d0) T cmd-make-num)
    ((ksd~d1) T cmd-make-num)
    ((ksd~d2) T cmd-make-num)
    ((ksd~d3) T cmd-make-num)
    ((ksd~d4) T cmd-make-num)
    ((ksd~d5) T cmd-make-num)
    ((ksd~d6) T cmd-make-num)
    ((ksd~d7) T cmd-make-num)
    ((ksd~d8) T cmd-make-num)
    ((ksd~d9) T cmd-make-num)))
      

(def '*binds-latin-1
  '(
    ((ksd~space) T cmd-make-char)
    ((ksd~exclam) T cmd-make-char)
    ((ksd~quotedbl) T cmd-make-char)
    ((ksd~numbersign) T cmd-make-char)
    ((ksd~dollar) T cmd-make-char)
    ((ksd~percent) T cmd-make-char)
    ((ksd~ampersand) T cmd-make-char)
    ((ksd~apostrophe) T cmd-make-char)
    ((ksd~quoteright) T cmd-make-char)
    ((ksd~parenleft) T cmd-make-char)
    ((ksd~parenright) T cmd-make-char)
    ((ksd~asterisk) T cmd-make-char)
    ((ksd~plus) T cmd-make-char)
    ((ksd~comma) T cmd-make-char)
    ((ksd~minus) T cmd-make-char)
    ((ksd~period) T cmd-make-char)
    ((ksd~slash) T cmd-make-char)
    
    ((ksd~d0) T cmd-make-char)
    ((ksd~d1) T cmd-make-char)
    ((ksd~d2) T cmd-make-char)
    ((ksd~d3) T cmd-make-char)
    ((ksd~d4) T cmd-make-char)
    ((ksd~d5) T cmd-make-char)
    ((ksd~d6) T cmd-make-char)
    ((ksd~d7) T cmd-make-char)
    ((ksd~d8) T cmd-make-char)
    ((ksd~d9) T cmd-make-char)
    
    ((ksd~colon) T cmd-make-char)
    ((ksd~semicolon) T cmd-make-char)
    ((ksd~less) T cmd-make-char)
    ((ksd~equal) T cmd-make-char)
    ((ksd~greater) T cmd-make-char)
    ((ksd~question) T cmd-make-char)
    ((ksd~at) T cmd-make-char)
    ((ksd~A) T cmd-make-char)
    ((ksd~B) T cmd-make-char)
    ((ksd~C) T cmd-make-char)
    ((ksd~D) T cmd-make-char)
    ((ksd~E) T cmd-make-char)
    ((ksd~F) T cmd-make-char)
    ((ksd~G) T cmd-make-char)
    ((ksd~H) T cmd-make-char)
    ((ksd~I) T cmd-make-char)
    ((ksd~J) T cmd-make-char)
    ((ksd~K) T cmd-make-char)
    ((ksd~L) T cmd-make-char)
    ((ksd~M) T cmd-make-char)
    ((ksd~N) T cmd-make-char)
    ((ksd~O) T cmd-make-char)
    ((ksd~P) T cmd-make-char)
    ((ksd~Q) T cmd-make-char)
    ((ksd~R) T cmd-make-char)
    ((ksd~S) T cmd-make-char)
    ((ksd~lT) T cmd-make-char)
    ((ksd~U) T cmd-make-char)
    ((ksd~V) T cmd-make-char)
    ((ksd~W) T cmd-make-char)
    ((ksd~X) T cmd-make-char)
    ((ksd~Y) T cmd-make-char)
    ((ksd~Z) T cmd-make-char)
    ((ksd~bracketleft) T cmd-make-char)
    ((ksd~backslash) T cmd-make-char)
    ((ksd~bracketright) T cmd-make-char)
    ((ksd~asciicircum) T cmd-make-char)
    ((ksd~underscore) T cmd-make-char)
    ((ksd~grave) T cmd-make-char)
    ((ksd~quoteleft) T cmd-make-char)
    ((ksd~a) T cmd-make-char)
    ((ksd~b) T cmd-make-char)
    ((ksd~c) T cmd-make-char)
    ((ksd~d) T cmd-make-char)
    ((ksd~e) T cmd-make-char)
    ((ksd~f) T cmd-make-char)
    ((ksd~g) T cmd-make-char)
    ((ksd~h) T cmd-make-char)
    ((ksd~i) T cmd-make-char)
    ((ksd~j) T cmd-make-char)
    ((ksd~k) T cmd-make-char)
    ((ksd~l) T cmd-make-char)
    ((ksd~m) T cmd-make-char)
    ((ksd~n) T cmd-make-char)
    ((ksd~o) T cmd-make-char)
    ((ksd~p) T cmd-make-char)
    ((ksd~q) T cmd-make-char)
    ((ksd~r) T cmd-make-char)
    ((ksd~s) T cmd-make-char)
    ((ksd~t) T cmd-make-char)
    ((ksd~u) T cmd-make-char)
    ((ksd~v) T cmd-make-char)
    ((ksd~w) T cmd-make-char)
    ((ksd~x) T cmd-make-char)
    ((ksd~y) T cmd-make-char)
    ((ksd~z) T cmd-make-char)
    ((ksd~braceleft) T cmd-make-char)
    ((ksd~bar) T cmd-make-char)
    ((ksd~braceright) T cmd-make-char)
    ((ksd~asciitilde) T cmd-make-char)

    ((ksd~nobreakspace) T cmd-make-char)
    ((ksd~exclamdown) T cmd-make-char)
    ((ksd~cent) T cmd-make-char)
    ((ksd~sterling) T cmd-make-char)
    ((ksd~currency) T cmd-make-char)
    ((ksd~yen) T cmd-make-char)
    ((ksd~brokenbar) T cmd-make-char)
    ((ksd~section) T cmd-make-char)
    ((ksd~diaeresis) T cmd-make-char)
    ((ksd~copyright) T cmd-make-char)
    ((ksd~ordfeminine) T cmd-make-char)
    ((ksd~guillemotleft) T cmd-make-char)
    ((ksd~notsign) T cmd-make-char)
    ((ksd~hyphen) T cmd-make-char)
    ((ksd~registered) T cmd-make-char)
    ((ksd~macron) T cmd-make-char)
    ((ksd~degree) T cmd-make-char)
    ((ksd~plusminus) T cmd-make-char)
    ((ksd~twosuperior) T cmd-make-char)
    ((ksd~threesuperior) T cmd-make-char)
    ((ksd~acute) T cmd-make-char)
    ((ksd~mu) T cmd-make-char)
    ((ksd~paragraph) T cmd-make-char)
    ((ksd~periodcentered) T cmd-make-char)
    ((ksd~cedilla) T cmd-make-char)
    ((ksd~onesuperior) T cmd-make-char)
    ((ksd~masculine) T cmd-make-char)
    ((ksd~guillemotright) T cmd-make-char)
    ((ksd~onequarter) T cmd-make-char)
    ((ksd~onehalf) T cmd-make-char)
    ((ksd~threequarters) T cmd-make-char)
    ((ksd~questiondown) T cmd-make-char)
    ((ksd~Agrave) T cmd-make-char)
    ((ksd~Aacute) T cmd-make-char)
    ((ksd~Acircumflex) T cmd-make-char)
    ((ksd~Atilde) T cmd-make-char)
    ((ksd~Adiaeresis) T cmd-make-char)
    ((ksd~Aring) T cmd-make-char)
    ((ksd~AE) T cmd-make-char)
    ((ksd~Ccedilla) T cmd-make-char)
    ((ksd~Egrave) T cmd-make-char)
    ((ksd~Eacute) T cmd-make-char)
    ((ksd~Ecircumflex) T cmd-make-char)
    ((ksd~Ediaeresis) T cmd-make-char)
    ((ksd~Igrave) T cmd-make-char)
    ((ksd~Iacute) T cmd-make-char)
    ((ksd~Icircumflex) T cmd-make-char)
    ((ksd~Idiaeresis) T cmd-make-char)
    ((ksd~ETH) T cmd-make-char)
    ((ksd~Eth) T cmd-make-char)
    ((ksd~Ntilde) T cmd-make-char)
    ((ksd~Ograve) T cmd-make-char)
    ((ksd~Oacute) T cmd-make-char)
    ((ksd~Ocircumflex) T cmd-make-char)
    ((ksd~Otilde) T cmd-make-char)
    ((ksd~Odiaeresis) T cmd-make-char)
    ((ksd~multiply) T cmd-make-char)
    ((ksd~Oslash) T cmd-make-char)
    ((ksd~Ooblique) T cmd-make-char)
    ((ksd~Ugrave) T cmd-make-char)
    ((ksd~Uacute) T cmd-make-char)
    ((ksd~Ucircumflex) T cmd-make-char)
    ((ksd~Udiaeresis) T cmd-make-char)
    ((ksd~Yacute) T cmd-make-char)
    ((ksd~THORN) T cmd-make-char)
    ((ksd~Thorn) T cmd-make-char)
    ((ksd~ssharp) T cmd-make-char)
    ((ksd~agrave) T cmd-make-char)
    ((ksd~aacute) T cmd-make-char)
    ((ksd~acircumflex) T cmd-make-char)
    ((ksd~atilde) T cmd-make-char)
    ((ksd~adiaeresis) T cmd-make-char)
    ((ksd~aring) T cmd-make-char)
    ((ksd~ae) T cmd-make-char)
    ((ksd~ccedilla) T cmd-make-char)
    ((ksd~egrave) T cmd-make-char)
    ((ksd~eacute) T cmd-make-char)
    ((ksd~ecircumflex) T cmd-make-char)
    ((ksd~ediaeresis) T cmd-make-char)
    ((ksd~igrave) T cmd-make-char)
    ((ksd~iacute) T cmd-make-char)
    ((ksd~icircumflex) T cmd-make-char)
    ((ksd~idiaeresis) T cmd-make-char)
    ((ksd~eth) T cmd-make-char)
    ((ksd~ntilde) T cmd-make-char)
    ((ksd~ograve) T cmd-make-char)
    ((ksd~oacute) T cmd-make-char)
    ((ksd~ocircumflex) T cmd-make-char)
    ((ksd~otilde) T cmd-make-char)
    ((ksd~odiaeresis) T cmd-make-char)
    ((ksd~division) T cmd-make-char)
    ((ksd~oslash) T cmd-make-char)
    ((ksd~ooblique) T cmd-make-char)
    ((ksd~ugrave) T cmd-make-char)
    ((ksd~uacute) T cmd-make-char)
    ((ksd~ucircumflex) T cmd-make-char)
    ((ksd~udiaeresis) T cmd-make-char)
    ((ksd~yacute) T cmd-make-char)
    ((ksd~thorn) T cmd-make-char)
    ((ksd~ydiaeresis) T cmd-make-char)

  ))

    
(def '*binds-ops

  '(
    ((ksd~Return) T cmd-make-char)
    ((ksd~Tab)    T cmd-make-char)
    
    ((ksd~BackSpace) T cmd-del)
    ((ksd~Delete   ) T cmd-del)  
    
    # Make cmds only mod+ASCII? to maintain homerow
    ((ksd~Alt-R   ksd~Return)    T cmd-mov-nl)
    ((ksd~Alt-R   ksd~BackSpace) T cmd-mov-bl)
    
    ((ksd~Control-L ksd~r) T cmd-make-nil)
    ((ksd~Control-L ksd~t) T cmd-mov-nl)
    
    ((ksd~Control-L ksd~q) T cmd-ptr-ln-first)
    ((ksd~Control-L ksd~e) T cmd-ptr-ln-last)
    # ctrl + alt + q = move to start of list  
    # ctrl + alt + e = move to end of list
      
    # Tab for eval
    # eval-str to pass to str
    # also need str cmd to create data
    ((ksd~Control-L ksd~Tab)            T cmd-eval-str)
    ((ksd~Alt-L     ksd~Tab)            T cmd-str)
    ((ksd~Control-L ksd~Alt-L ksd~Tab) T cmd-eval)
    
    # other cmds...
    
    ##########
    # CUA Ops
    
    ((ksd~Control-L ksd~x) T ((Keysym) (println "Ctrl-L x: CUT")))
    ((ksd~Control-L ksd~c) T ((Keysym) (println "Ctrl-L c: COPY")))
    ((ksd~Control-L ksd~v) T ((Keysym) (println "Ctrl-L v: PASTE")))
    ((ksd~Control-L ksd~z) T ((Keysym) (println "Ctrl-L z: UNDO")))
    ((ksd~Alt-L     ksd~z) T ((Keysym) (println "Alt-L z: REDO")))

    #########
    # Str Ops
    
    # split-space (word) : glue " ", 
    # chop (letter) : glue "", pack      
    # reverse, flip
    #
    # cmd-pack-list - ref must be a list or start of list
    # cmd-glue-sp-list - ref must be a list or start of list
    
    ((ksd~Control-L ksd~h) T cmd-pack-syms)
    ((ksd~Control-L ksd~j) T cmd-chop)
    
    ((ksd~Control-L ksd~y) T cmd-glue-sp-syms)
    ((ksd~Control-L ksd~u) T cmd-split-sp)
    
    # cmd-reverse
    # cmd-flip
    
    # DEBUG
    
    #(view (getl (get *0 'b)))
    # If we lay relative to origin should stay in same place
    ((ksd~Control-R ksd~Return) T cmd-print-ptr)


    # Pointer Translation
    ((ksd~Control-L ksd~w) T cmd-ptr-up)
    ((ksd~Control-L ksd~a) T cmd-ptr-left)      
    ((ksd~Control-L ksd~s) T cmd-ptr-down)
    ((ksd~Control-L ksd~d) T cmd-ptr-right)

    ((ksd~Right) T cmd-trans-ptr-px)
    ((ksd~Left)  T cmd-trans-ptr-nx)
    ((ksd~Up)    T cmd-trans-ptr-py)
    ((ksd~Down)  T cmd-trans-ptr-ny)

    # Camera Translation
    # Prior/Next
    ((ksd~Alt-R ksd~Right) T cmd-move-cam-px)
    ((ksd~Alt-R ksd~Left)  T cmd-move-cam-nx)
    ((ksd~Alt-R ksd~Up)    T cmd-move-cam-py)
    ((ksd~Alt-R ksd~Down)  T cmd-move-cam-ny)
    ((ksd~Prior)            T cmd-zoom-cam-in)
    ((ksd~Next)             T cmd-zoom-cam-out)
  
  ))


#{
(def '*binds-latin-1-OLD

  ## Number
  '(~(let List NIL
      (for (K ksd~d0 (<= K ksd~d9) (inc K))
        # make this alt
        (push 'List (list (list 'ksd~Control-L K) T 'cmd-make-num))
        (push 'List (list (list 'ksd~Control-R K) T 'cmd-make-num))
      List))
    ## Char Strings
    ~(let List NIL
      (for (K 33 (<= K 255) (inc K))
        (push 'List (list (list K)             T 'cmd-make-char))
        (push 'List (list (list 'ksd~Shift-L K) T 'cmd-make-char))
        (push 'List (list (list 'ksd~Shift-R K) T 'cmd-make-char)))
      List)))      
}#
