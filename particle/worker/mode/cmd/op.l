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
    ((ksd~Tab) cmd-chg-mode)
    
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
