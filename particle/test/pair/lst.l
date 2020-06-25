#{

        *
  [ ]  [ ]  .
   .    .
  
  X/Y -> X/Y -> X/Y/A/N

  (lst-pair-xb-x-x)
  (lst-pair-xb-x-y)
  (lst-pair-xb-x-a)
   
  (lst-pair-xb-y-x)
  (lst-pair-xb-y-y)
  (lst-pair-xb-y-a)
   
  (lst-pair-ya-x-x)
  (lst-pair-ya-x-y)
  (lst-pair-ya-x-a)
   
  (lst-pair-ya-y-x)
  (lst-pair-ya-y-y)
  (lst-pair-ya-y-a)
   
  (lst-pair-yb-x-x)
  (lst-pair-yb-x-y)
  (lst-pair-yb-x-a)
  (lst-pair-yb-y-x)
  (lst-pair-yb-y-y)
  (lst-pair-yb-y-a)
  
}#


(de lst-pair-x-x-b ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-make-char (char "d"))
  (cmd-ptr-left)
  (cmd-make-pair)
  (ver-start 
    'lst-pair-x-x-b
    '(((+Pair)
        ("c" ("d"))
        x
        T
        NIL
        NIL
        (4214357 0 0)
        T
        (3612306 7224612)
        (NIL T) )
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
      ((+Pair)
          (("d"))
          y
          NIL
          T
          NIL
          (4214357 -3612306 0)
          T
          (0 0)
          (T NIL) )
      ((+Pair)
          ("d")
          x
          T
          T
          NIL
          (5418459 -3612306 0)
          T
          (2408204 1204102)
          (NIL NIL) )
      ((+Str)
          "d"
          x
          NIL
          NIL
          T
          (5418459 -4816408 0)
          T
          (0 0)
          NIL )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (6622561 -3612306 0)
          T
          (1204102 1204102)
          NIL )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (4214357 -7224612 0)
          T
          (1204102 0)
          (T NIL) ) ))) 
          
(de lst-pair-x-y-b ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-make-char (char "d"))
  (cmd-ptr-left)
  (cmd-make-pair)
  (cmd-make-pair)
  (ver-start 
    'lst-pair-x-y-b
    '(((+Pair)
      ("c" (("d")))
      x
      T
      NIL
      NIL
      (4214357 0 0)
      T
      (4816408 9632816)
      (NIL T) )
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
      ((+Pair)
          ((("d")))
          y
          NIL
          T
          NIL
          (4214357 -3612306 0)
          T
          (0 0)
          (T NIL) )
      ((+Pair)
          (("d"))
          y
          NIL
          T
          NIL
          (5418459 -3612306 0)
          T
          (3612306 3612306)
          (NIL T) )
      ((+Pair)
          ("d")
          x
          T
          T
          NIL
          (6622561 -3612306 0)
          T
          (2408204 1204102)
          (NIL NIL) )
      ((+Str)
          "d"
          x
          NIL
          NIL
          T
          (6622561 -4816408 0)
          T
          (0 0)
          NIL )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (7826663 -3612306 0)
          T
          (1204102 1204102)
          NIL )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (5418459 -7224612 0)
          T
          (1204102 0)
          (T NIL) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (4214357 -9632816 0)
          T
          (1204102 0)
          (T NIL) ) )))    

(de lst-pair-y-x-b ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-down)
  (cmd-make-char (char "c"))
  (cmd-ptr-left)
  (cmd-make-pair)
  (ver-start 
    'lst-pair-y-x-b
    '(((+Pair)
      (NIL ("c"))
      y
      T
      NIL
      NIL
      (4214357 0 0)
      T
      (3612306 6020510)
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
          (("c"))
          y
          NIL
          T
          NIL
          (4214357 -2408204 0)
          T
          (0 0)
          (T NIL) )
      ((+Pair)
          ("c")
          x
          T
          T
          NIL
          (5418459 -2408204 0)
          T
          (2408204 1204102)
          (NIL NIL) )
      ((+Str)
          "c"
          x
          NIL
          NIL
          T
          (5418459 -3612306 0)
          T
          (0 0)
          NIL )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (6622561 -2408204 0)
          T
          (1204102 1204102)
          NIL )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (4214357 -6020510 0)
          T
          (1204102 0)
          (T NIL) ) )))
          
(de lst-pair-y-y-b ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-down)
  (cmd-make-pair)
  (cmd-make-pair)
  (ver-start 
    'lst-pair-y-y-b
    '(((+Pair)
        (NIL (NIL))
        y
        T
        NIL
        NIL
        (4214357 0 0)
        T
        (3612306 7224612)
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
          ((NIL))
          y
          NIL
          T
          NIL
          (4214357 -2408204 0)
          T
          (0 0)
          (T NIL) )
      ((+Pair)
          (NIL)
          y
          T
          T
          NIL
          (5418459 -2408204 0)
          T
          (2408204 2408204)
          (NIL T) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (6622561 -2408204 0)
          T
          (1204102 0)
          (NIL NIL) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (5418459 -4816408 0)
          T
          (1204102 0)
          (T NIL) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (4214357 -7224612 0)
          T
          (1204102 0)
          (T NIL) ) )))
          
(de lst-pair-y-x-a ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-make-pair)
  (ver-start 
    'lst-pair-y-x-a
    '(((+Pair)
        (((NIL)))
        y
        NIL
        T
        NIL
        (4214357 0 0)
        T
        (6020510 6020510)
        (NIL T) )
      ((+Pair)
          ((NIL))
          y
          NIL
          T
          NIL
          (5418459 0 0)
          T
          (4816408 3612306)
          (NIL T) )
      ((+Pair)
          (NIL)
          x
          T
          T
          NIL
          (6622561 0 0)
          T
          (3612306 1204102)
          (NIL NIL) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (6622561 -1204102 0)
          T
          (1204102 0)
          NIL )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (9030765 0 0)
          T
          (1204102 1204102)
          NIL )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (5418459 -3612306 0)
          T
          (1204102 0)
          (T NIL) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (4214357 -6020510 0)
          T
          (1204102 0)
          (T NIL) ) )))    
    
    
(de lst-pair-y-y-a ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-make-pair)
  (cmd-make-pair)
  (ver-start 
    'lst-pair-y-y-a
    '(((+Pair)
        (((NIL)))
        y
        NIL
        T
        NIL
        (4214357 0 0)
        T
        (4816408 7224612)
        (NIL T) )
      ((+Pair)
          ((NIL))
          y
          NIL
          T
          NIL
          (5418459 0 0)
          T
          (3612306 4816408)
          (NIL T) )
      ((+Pair)
          (NIL)
          y
          T
          T
          NIL
          (6622561 0 0)
          T
          (2408204 2408204)
          (NIL T) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (7826663 0 0)
          T
          (1204102 0)
          (NIL NIL) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (6622561 -2408204 0)
          T
          (1204102 0)
          (T NIL) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (5418459 -4816408 0)
          T
          (1204102 0)
          (T NIL) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (4214357 -7224612 0)
          T
          (1204102 0)
          (T NIL) ) )))
