#{

  Cases:
    
    PAIR | REG 
    -----+----
    X/Y  | A/A 
    X/Y  | A/P 
    X/Y  | P/A 
    X/Y  | P/P 
               
  Some will overlap with atom, however
  atom tests when ptr is on atom,
  these tests when ptr is on pair
               
}#

# X Pair

(de alt-reg-x-pair-a-a ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-ptr-left)
  (cmd-alt-reg)
  (cmd-ptr-down)
  (cmd-make-char (char "d"))
  (cmd-ptr-up)
  (cmd-alt-reg)
  (ver-start
    'alt-reg-x-pair-a-a))

# TODO: Conv pair to Y Pair
# Compl: y-pair-px-a
(de alt-reg-x-pair-a-px ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-make-char (char "d"))
  (cmd-ptr-left)
  (cmd-ptr-left)
  (cmd-alt-reg)
  (ver-start
    'alt-reg-x-pair-a-px))

# TODO: Conv pair to Y Pair
# Compl: y-pair-py-a
(de alt-reg-x-pair-a-py ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-make-pair)
  (cmd-ptr-left)
  (cmd-alt-reg)
  (ver-start
    'alt-reg-x-pair-a-py))

# UNSUPPORTED: X-Pair car = Pair
(de alt-reg-x-pair-px-a)
(de alt-reg-x-pair-py-a)
(de alt-reg-x-pair-px-px)
(de alt-reg-x-pair-px-py)
(de alt-reg-x-pair-py-py)
(de alt-reg-x-pair-px-px)


# Y Pair

(de alt-reg-y-pair-a-a ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-ptr-left)
  (cmd-alt-reg)
  (cmd-ptr-right)
  (cmd-make-char (char "d"))
  (cmd-ptr-left)
  (cmd-alt-reg)
  (ver-start
    'alt-reg-y-pair-a-a))
    
(de alt-reg-y-pair-a-px ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-down)
  (cmd-make-char (char "c"))
  (cmd-ptr-left)
  (cmd-ptr-up)
  (cmd-alt-reg)
  (ver-start
    'alt-reg-y-pair-a-px))
    
(de alt-reg-y-pair-a-py ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-down)
  (cmd-make-pair)
  (cmd-ptr-up)
  (cmd-alt-reg)
  (ver-start
    'alt-reg-y-pair-a-py))
    
    
(de alt-reg-y-pair-px-a ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-left)
  (cmd-alt-reg)
  (ver-start
    'alt-reg-y-pair-px-a))
    
(de alt-reg-y-pair-py-a ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-make-pair)
  (cmd-alt-reg)
  (ver-start
    'alt-reg-y-pair-py-a))

    
(de alt-reg-y-pair-px-px ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-left)
  (cmd-ptr-down)
  (cmd-make-char (char "c"))
  (cmd-ptr-left)
  (cmd-ptr-up)
  (cmd-alt-reg)
  (ver-start
    'alt-reg-y-pair-px-px))
    
(de alt-reg-y-pair-px-py ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-left)
  (cmd-ptr-down)  
  (cmd-make-pair)
  (cmd-ptr-up)
  (cmd-alt-reg)  
  (ver-start
    'alt-reg-y-pair-px-py))
    
(de alt-reg-y-pair-py-py ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-make-pair)
  (cmd-ptr-down)  
  (cmd-make-pair)
  (cmd-ptr-up)
  (cmd-alt-reg)    
  (ver-start
    'alt-reg-y-pair-py-py))
    
(de alt-reg-y-pair-py-px ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-make-pair)
  (cmd-ptr-down)
  (cmd-make-char (char "c"))
  (cmd-ptr-left)
  (cmd-ptr-up)
  (cmd-alt-reg) 
  (ver-start
    'alt-reg-y-pair-a-a))
