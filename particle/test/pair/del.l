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
  (cmd-alt-reg)
  (cmd-ptr-left)
  (cmd-del)
  (ver-start 
    'del-pair-xb-x-a))

(de del-pair-xb-x-n ())
    
# For Y, use one test (disregard car since its del)    
    
(de del-pair-xb-y-x ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-make-char (char "d"))
  (cmd-make-char (char "e"))
  (cmd-ptr-left)
  (cmd-ptr-left)
  (cmd-ptr-down)
  (cmd-make-pair)
  (cmd-ptr-left)
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
  (cmd-ptr-down)
  (cmd-make-pair)
  (cmd-ptr-left)
  (cmd-del)
  (ver-start 
    'del-pair-xb-y-y))

    
(de del-pair-xb-y-a ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-make-char (char "d"))
  (cmd-ptr-left)
  (cmd-alt-reg)
  (cmd-ptr-down)
  (cmd-make-char (char "e"))
  (cmd-alt-reg)
  (cmd-ptr-left)
  (cmd-ptr-down)
  (cmd-make-pair)
  (cmd-ptr-left)
  (cmd-del)
  (ver-start 
    'del-pair-xb-y-a))

(de del-pair-xb-y-n ())
    
###            
    
# Y car - atom or pair
(de del-pair-ya-x-x ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-make-char (char "c"))
  (cmd-ptr-left)
  (cmd-del)
  (ver-start 
    'del-pair-ya-x-x))

    
(de del-pair-ya-x-y ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-del)
  (ver-start 
    'del-pair-ya-x-y))

    
(de del-pair-ya-x-a ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-down)
  (cmd-make-char (char "c"))
  (cmd-alt-reg)
  (cmd-ptr-left)
  (cmd-del)
  (ver-start 
    'del-pair-ya-x-a))
    

(de del-pair-ya-y-x ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-left)
  (cmd-del)
  (ver-start 
    'del-pair-ya-y-x))


(de del-pair-ya-ya-y ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-make-pair)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-del)
  (ver-start 
    'del-pair-ya-ya-y))

    
(de del-pair-ya-y-a ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-ptr-left)
  (cmd-del)
  (ver-start 
    'del-pair-ya-y-a))
    

# Y cdr


(de del-pair-yb-x-x ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-down)
  (cmd-make-char (char "c"))
  (cmd-make-char (char "d"))
  (cmd-ptr-left)
  (cmd-ptr-left)
  (cmd-del)
  (ver-start 
    'del-pair-yb-x-x))


(de del-pair-yb-x-y () T)


(de del-pair-yb-x-a () T)



(de del-pair-yb-y-x () T)


(de del-pair-yb-y-y () T)


(de del-pair-yb-y-a () T)
