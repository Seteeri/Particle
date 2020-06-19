(de lst-x-atom-b ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (ver-start
    'lst-x-atom-b
    '(((+Pair)
        (NIL)
        y
        T
        T
        NIL
        (4214357 0 0)
        T
        (2408204 2408204)
        (NIL T) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (5418459 0 0)
          T
          (1204102 0)
          (NIL NIL) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (4214357 -2408204 0)
          T
          (1204102 0)
          (T NIL) ) )))

(de lst-x-atom-a ()
  (cmd-ptr-right)
  (cmd-make-nil) # check str-x-atom-b
  (cmd-ptr-left)
  (cmd-ptr-down)
  (cmd-make-pair)
  (ver-start
    'lst-x-atom-a
    '(((+Pair)
      ((NIL))
      y
      NIL
      T
      NIL
      (4214357 0 0)
      T
      (4816408 3612306)
      (NIL T) )
      ((+Pair)
          (NIL)
          x
          T
          T
          NIL
          (5418459 0 0)
          T
          (3612306 1204102)
          (NIL NIL) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (5418459 -1204102 0)
          T
          (1204102 0)
          NIL )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (7826663 0 0)
          T
          (1204102 1204102)
          NIL )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (4214357 -3612306 0)
          T
          (1204102 0)
          (T NIL) ) )))

(de lst-y-atom-b ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-down)
  (cmd-make-pair)
  (ver-start 
    'lst-y-atom-b
    '(((+Pair)
          (NIL NIL)
          y
          T
          NIL
          NIL
          (4214357 0 0)
          T
          (2408204 4816408)
          (NIL T) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (5418459 0 0)
          T
          (1204102 0)
          (NIL NIL) )
      ((+Pair)
          (NIL)
          y
          T
          T
          NIL
          (4214357 -2408204 0)
          T
          (0 0)
          (T NIL) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (5418459 -2408204 0)
          T
          (1204102 0)
          (NIL NIL) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (4214357 -4816408 0)
          T
          (1204102 0)
          (T NIL) ) )))    

(de lst-y-atom-a ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-make-pair)
  (ver-start
    'lst-y-atom-a
    '(((+Pair)
          ((NIL))
          y
          NIL
          T
          NIL
          (4214357 0 0)
          T
          (4816408 3612306)
          (NIL T) )
      ((+Pair)
          (NIL)
          x
          T
          T
          NIL
          (5418459 0 0)
          T
          (3612306 1204102)
          (NIL NIL) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (5418459 -1204102 0)
          T
          (1204102 0)
          NIL )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (7826663 0 0)
          T
          (1204102 1204102)
          NIL )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (4214357 -3612306 0)
          T
          (1204102 0)
          (T NIL) ) )))    
    
# STR
  
(de str-x-atom-b ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (ver-start
    'str-x-atom-b
    '(((+Pair)
          ("c")
          x
          T
          T
          NIL
          (4214357 0 0)
          T
          (0 1204102)
          NIL )
      ((+Str)
          "c"
          x
          NIL
          NIL
          T
          (4214357 -1204102 0)
          T
          (0 0)
          NIL )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (5418459 0 0)
          T
          (1204102 1204102)
          NIL ) )))    

(de str-x-atom-a ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-ptr-left)
  (cmd-ptr-down)
  (cmd-make-char (char "d"))
  (ver-start
    'str-x-atom-a
    '(((+Pair)
          ("d")
          x
          T
          T
          NIL
          (4214357 0 0)
          T
          (0 1204102)
          NIL )
      ((+Str)
          "d"
          x
          NIL
          NIL
          T
          (4214357 -1204102 0)
          T
          (0 0)
          NIL )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (5418459 0 0)
          T
          (1204102 1204102)
          NIL ) )))    

(de str-y-atom-b ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-down)
  (cmd-make-char (char "c"))
  (ver-start 
    'str-y-atom-b
    '(((+Pair)
          (NIL "c")
          y
          T
          NIL
          NIL
          (4214357 0 0)
          T
          (2408204 3612306)
          (NIL T) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (5418459 0 0)
          T
          (1204102 0)
          (NIL NIL) )
      ((+Pair)
          ("c")
          x
          T
          T
          NIL
          (4214357 -2408204 0)
          T
          (2408204 1204102)
          (T NIL) )
      ((+Str)
          "c"
          x
          NIL
          NIL
          T
          (4214357 -3612306 0)
          T
          (0 0)
          NIL )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (5418459 -2408204 0)
          T
          (1204102 1204102)
          NIL ) )))    

(de str-y-atom-a ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (ver-start 
    'str-y-atom-a
    '(((+Pair)
          ("c")
          y
          T
          T
          NIL
          (4214357 0 0)
          T
          (1204102 2408204)
          (NIL T) )
      ((+Str)
          "c"
          x
          NIL
          NIL
          T
          (5418459 0 0)
          T
          (0 0)
          (NIL NIL) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (4214357 -2408204 0)
          T
          (1204102 0)
          (T NIL) ) )))    
  
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
