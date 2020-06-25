#{
        *
  [ ]  [ ]  [ ] .
  
  X/Y -> X/Y -> X/Y/.

  Except X car = X/Y
  
}#

# X car - UNSUPPORTED

# X cdr
(de del-pair-xb-x-x ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-make-char (char "d"))
  (cmd-make-char (char "e"))
  (cmd-ptr-left)
  (cmd-ptr-left)
  (cmd-del)
  (ver-start 
    'del-pair-xb-x-x))
  
(de del-pair-xb-x-y ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-make-char (char "d"))
  (cmd-make-pair)
  (cmd-ptr-left)
  (cmd-del)
  (ver-start 
    'del-pair-xb-x-y))
    
(de del-pair-xb-x-a ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-make-char (char "d"))
  (cmd-ptr-left)
  (cmd-alt-reg)
  (cmd-ptr-down)
  (cmd-make-char (char "e"))
  (cmd-ptr-up)
  (cmd-alt-reg)
  (cmd-del)
  (ver-start 
    'del-pair-xb-x-a))
    
(de del-pair-xb-y-x ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-make-char (char "d"))
  (cmd-make-char (char "e"))
  (cmd-ptr-left)
  (cmd-ptr-left)
  (cmd-make-pair)
  (cmd-del)
  (ver-start 
    'del-pair-xb-y-x))
    
(de del-pair-xb-y-y ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-make-char (char "d"))
  (cmd-make-char (char "e"))
  (cmd-ptr-left)
  (cmd-make-pair)
  (cmd-ptr-left)
  (cmd-make-pair)  
  (cmd-del)
  (ver-start 
    'del-pair-xb-y-y))

###        
    
(de del-pair-xb-y-a ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-make-char (char "d"))
  (cmd-make-char (char "e"))
  (cmd-ptr-left)
  (cmd-make-pair)
  (cmd-del)
  (ver-start 
    'del-pair-xb-y-a))

    
# Y car
(de del-pair-ya-x-x () T)
(de del-pair-ya-x-y () T)
(de del-pair-ya-x-a () T)

(de del-pair-ya-y-x () T)
(de del-pair-ya-y-y () T)
(de del-pair-ya-y-a () T)

# Y cdr
(de del-pair-yb-x-x () T)
(de del-pair-yb-x-y () T)
(de del-pair-yb-x-a () T)

(de del-pair-yb-y-x () T)
(de del-pair-yb-y-y () T)
(de del-pair-yb-y-a () T)
