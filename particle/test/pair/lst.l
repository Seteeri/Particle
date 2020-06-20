#(de lst-x-pair-x-a () T)
#(de lst-x-pair-y-a () T)

(de lst-x-pair-x-b ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-make-char (char "d"))
  (cmd-ptr-left)
  (cmd-make-pair)
  (ver-start 
    'lst-x-pair-x-b
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
          
(de lst-x-pair-y-b ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-make-char (char "d"))
  (cmd-ptr-left)
  (cmd-make-pair)
  (cmd-make-pair)
  (ver-start 
    'lst-x-pair-y-b
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

(de lst-y-pair-x-b ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-down)
  (cmd-make-char (char "c"))
  (cmd-ptr-left)
  (cmd-make-pair)
  (ver-start 
    'lst-y-pair-x-b
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
          
(de lst-y-pair-y-b ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-down)
  (cmd-make-pair)
  (cmd-make-pair)
  (ver-start 
    'lst-y-pair-y-b
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
          
(de lst-y-pair-x-a ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-right)
  # Either char or make-pair
  (cmd-make-char (char "c"))
  (cmd-ptr-left)
  (cmd-make-pair)
  (ver-start 
    'lst-y-pair-x-a
    '(((+Pair)
        (("c"))
        y
        NIL
        T
        NIL
        (4214357 0 0)
        T
        (2408204 4816408)
        (NIL T) )
      ((+Pair)
          ("c")
          y
          T
          T
          NIL
          (5418459 0 0)
          T
          (1204102 2408204)
          (NIL NIL) )
      ((+Str)
          "c"
          x
          NIL
          NIL
          T
          (6622561 0 0)
          T
          (0 0)
          (NIL NIL) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (5418459 -2408204 0)
          T
          (1204102 0)
          (T NIL) )
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
    
(de lst-y-pair-y-a ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-make-pair)
  (cmd-make-pair)
  (ver-start 
    'lst-y-pair-y-a
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
          (NIL NIL) )
      ((+Pair)
          (NIL)
          y
          T
          T
          NIL
          (6622561 0 0)
          T
          (2408204 2408204)
          (NIL NIL) )
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
