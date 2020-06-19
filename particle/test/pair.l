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
