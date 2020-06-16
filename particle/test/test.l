#{

  Test ops:

    * Verify data
    * Verify location

  Also create tests for pointer
  Also create tests for Sym, besides Pair

  Test lst cmd before str cmd
  
}#

# ATOM

(de lst-x-atom-a () T)
(de lst-y-atom-a () T)
(de lst-x-atom-b ()
  (cmd-chg-mode) # -> CMD
  (cmd-ptr-right)
  (cmd-make-pair))
(de lst-y-atom-b () T)

(de str-x-atom-a () T)
(de str-y-atom-a () T)
(de str-x-atom-b ()
  (cmd-chg-mode) # -> CMD
  (cmd-ptr-right)
  (cmd-make-char "c"))
(de str-y-atom-b () T)

(de bsp-x-atom-a () T)
(de bsp-y-atom-a () T)
(de bsp-x-atom-b () T)
(de bsp-y-atom-b () T)

(de del-x-atom-a () T)
(de del-y-atom-a () T)
(de del-x-atom-b () T)
(de del-y-atom-b () T)

(de lin-x-atom-a () T)
(de lin-y-atom-a () T)
(de lin-x-atom-b () T)
(de lin-y-atom-b () T)

# PAIR

#(de str-x-pair-x-a () T)
#(de str-x-pair-y-a () T)
(de str-y-pair-x-a () T)
(de str-y-pair-y-a () T)

(de str-x-pair-x-b () T)
(de str-x-pair-y-b () T)
(de str-y-pair-x-b () T)
(de str-y-pair-y-b () T)


#(de lst-x-pair-x-a () T)
#(de lst-x-pair-y-a () T)
(de lst-y-pair-x-a () T)
(de lst-y-pair-y-a () T)

(de lst-x-pair-x-b () T)
(de lst-y-pair-y-b () T)
(de lst-x-pair-x-b () T)
(de lst-y-pair-y-b () T)


#(de bsp-x-pair-x-a () T)
#(de bsp-x-pair-y-a () T)
(de bsp-y-pair-x-a () T)
(de bsp-y-pair-y-a () T)

(de bsp-x-pair-x-b () T)
(de bsp-y-pair-y-b () T)
(de bsp-x-pair-x-b () T)
(de bsp-y-pair-y-b () T)


#(de del-x-pair-x-a () T)
#(de del-x-pair-y-a () T)
(de del-y-pair-x-a () T)
(de del-y-pair-y-a () T)

(de del-x-pair-x-b () T)
(de del-y-pair-y-b () T)
(de del-x-pair-x-b () T)
(de del-y-pair-y-b () T)


#(de lin-x-pair-x-a () T)
#(de lin-x-pair-y-a () T)
(de lin-y-pair-x-a () T)
(de lin-y-pair-y-a () T)

(de lin-x-pair-x-b () T)
(de lin-y-pair-y-b () T)
(de lin-x-pair-x-b () T)
(de lin-y-pair-y-b () T)

  
(de run-atom ()
  (str-x-atom-a)
  (str-y-atom-a)
  
  (str-x-atom-b)
  (str-y-atom-b)

  (lst-x-atom-a)
  (lst-y-atom-a)
  
  (lst-x-atom-b)
  (lst-y-atom-b)

  (bsp-x-atom-a)
  (bsp-y-atom-a)
  
  (bsp-x-atom-b)
  (bsp-y-atom-b)

  (del-x-atom-a)
  (del-y-atom-a)
  
  (del-x-atom-b)
  (del-y-atom-b)

  (lin-x-atom-a)
  (lin-y-atom-a)
  
  (lin-x-atom-b)
  (lin-y-atom-b))
  

(de run-pair ()

  (str-y-pair-x-a)
  (str-y-pair-y-a)

  (str-x-pair-x-b)
  (str-x-pair-y-b)
  (str-y-pair-x-b)
  (str-y-pair-y-b)

  (lst-y-pair-x-a)
  (lst-y-pair-y-a)

  (lst-x-pair-x-b)
  (lst-y-pair-y-b)
  (lst-x-pair-x-b)
  (lst-y-pair-y-b)


  (bsp-y-pair-x-a)
  (bsp-y-pair-y-a)

  (bsp-x-pair-x-b)
  (bsp-y-pair-y-b)
  (bsp-x-pair-x-b)
  (bsp-y-pair-y-b)


  (del-y-pair-x-a)
  (del-y-pair-y-a)

  (del-x-pair-x-b)
  (del-y-pair-y-b)
  (del-x-pair-x-b)
  (del-y-pair-y-b)


  (lin-y-pair-x-a)
  (lin-y-pair-y-a)

  (lin-x-pair-x-b)
  (lin-y-pair-y-b)
  (lin-x-pair-x-b)
  (lin-y-pair-y-b))

  
(de run-test ()
  (run-atom)
  (run-pair))
