(def '*binds-txt-ops

  '(
    ((ksd~Tab) cmd-chg-mode)
  
    ((ksd~Alt-L ksd~Tab)    cmd-make-char)
    ((ksd~Alt-R ksd~Tab)    cmd-make-char)
    
    ((ksd~BackSpace) cmd-bs)
    ((ksd~Delete   ) cmd-del)  
  
    ((ksd~Return) cmd-make-char)
    ((ksd~Shift-R   ksd~Return)    cmd-make-line)
    ((ksd~Shift-R   ksd~BackSpace) cmd-del-line)
    
    # DEBUG
    ((ksd~Control-R ksd~Return) cmd-print-ptr)
  
  ))

  
(de list-binds-txt-ops @
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
