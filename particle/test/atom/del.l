# X

(de del-x-atom-a ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-ptr-left)
  (cmd-ptr-down)
  (cmd-del)
  (ver-start
    'del-x-atom-a))
  
  
(de del-x-atom-b ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-ptr-left)
  (cmd-alt-reg)
  (cmd-ptr-right)
  (cmd-del)
  (ver-start
    'del-x-atom-b))

# Y
    
(de del-y-atom-a ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-del)
  (ver-start
    'del-y-atom-a))

    
(de del-y-atom-a-reg ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-alt-reg)
  (cmd-ptr-right)
  (cmd-del)
  (ver-start
    'del-y-atom-a-reg))
    

(de del-y-atom-a-nil ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-del)
  (ver-start
    'del-y-atom-a-nil))    
    
    
(de del-y-atom-b ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-ptr-left)
  (cmd-alt-reg)
  (cmd-ptr-down)
  (cmd-del)
  (ver-start
    'del-y-atom-b))

    
(de del-y-atom-b-nil ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-del)
  (ver-start
    'del-y-atom-b-nil))
    
