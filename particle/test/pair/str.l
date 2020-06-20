#(de str-x-pair-x-a () T)
#(de str-x-pair-y-a () T)

(de str-x-pair-x-b ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-make-char (char "d"))
  (cmd-ptr-left)
  (cmd-make-char (char "e"))
  (ver-start 
    'str-x-pair-x-b
    '(((+Pair)
          ("c" "e" "d")
          x
          T
          NIL
          NIL
          (4214357 0 0)
          T
          (4816408 1204102)
          (NIL NIL) )
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
          ("e" "d")
          x
          T
          NIL
          NIL
          (5418459 0 0)
          T
          (0 1204102)
          NIL )
      ((+Str)
          "e"
          x
          NIL
          NIL
          T
          (5418459 -1204102 0)
          T
          (0 0)
          NIL )
      ((+Pair)
          ("d")
          x
          T
          T
          NIL
          (6622561 0 0)
          T
          (0 1204102)
          NIL )
      ((+Str)
          "d"
          x
          NIL
          NIL
          T
          (6622561 -1204102 0)
          T
          (0 0)
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
          NIL ) )))
          
(de str-x-pair-y-b ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-make-pair)
  (cmd-make-char (char "d"))
  (ver-start 
    'str-x-pair-y-b
    '(((+Pair)
      ("c" "d" NIL)
      x
      T
      NIL
      NIL
      (4214357 0 0)
      T
      (2408204 9632816)
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
        ("d" NIL)
        x
        T
        NIL
        NIL
        (4214357 -3612306 0)
        T
        (0 1204102)
        (T NIL) )
    ((+Str)
        "d"
        x
        NIL
        NIL
        T
        (4214357 -4816408 0)
        T
        (0 0)
        NIL )
    ((+Pair)
        (NIL)
        y
        T
        T
        NIL
        (4214357 -7224612 0)
        T
        (0 0)
        (T NIL) )
    ((+Nil)
        NIL
        x
        NIL
        NIL
        T
        (5418459 -7224612 0)
        T
        (1204102 0)
        (NIL NIL) )
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

(de str-y-pair-x-b ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-down)
  (cmd-make-char (char "c"))
  (cmd-ptr-left)
  (cmd-make-char (char "d"))
  (ver-start 
    'str-y-pair-x-b
    '(((+Pair)
        (NIL "d" "c")
        y
        T
        NIL
        NIL
        (4214357 0 0)
        T
        (3612306 3612306)
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
          ("d" "c")
          x
          T
          NIL
          NIL
          (4214357 -2408204 0)
          T
          (3612306 1204102)
          (T NIL) )
      ((+Str)
          "d"
          x
          NIL
          NIL
          T
          (4214357 -3612306 0)
          T
          (0 0)
          NIL )
      ((+Pair)
          ("c")
          x
          T
          T
          NIL
          (5418459 -2408204 0)
          T
          (0 1204102)
          NIL )
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
          NIL ) )))    
    
(de str-y-pair-y-b ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-down)
  (cmd-make-pair)
  (cmd-make-char (char "c"))
  (ver-start 
    'str-y-pair-y-b
      '(((+Pair)
        (NIL "c" NIL)
        y
        T
        NIL
        NIL
        (4214357 0 0)
        T
        (2408204 8428714)
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
          ("c" NIL)
          x
          T
          NIL
          NIL
          (4214357 -2408204 0)
          T
          (0 1204102)
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
      ((+Pair)
          (NIL)
          y
          T
          T
          NIL
          (4214357 -6020510 0)
          T
          (0 0)
          (T NIL) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (5418459 -6020510 0)
          T
          (1204102 0)
          (NIL NIL) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (4214357 -8428714 0)
          T
          (1204102 0)
          (T NIL) ) )))
    
(de str-y-pair-x-a ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-make-pair) 
  (cmd-make-char (char "c"))
  (ver-start 
    'str-y-pair-x-a
    '(((+Pair)
      (("c" NIL))
      y
      NIL
      T
      NIL
      (4214357 0 0)
      T
      (6020510 3612306)
      (NIL T) )
    ((+Pair)
        ("c" NIL)
        x
        T
        NIL
        NIL
        (5418459 0 0)
        T
        (4816408 1204102)
        (NIL NIL) )
    ((+Str)
        "c"
        x
        NIL
        NIL
        T
        (5418459 -1204102 0)
        T
        (0 0)
        NIL )
    ((+Pair)
        (NIL)
        x
        T
        T
        NIL
        (6622561 0 0)
        T
        (0 1204102)
        NIL )
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
        (4214357 -3612306 0)
        T
        (1204102 0)
        (T NIL) ) )))
    
(de str-y-pair-y-a ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (ver-start 
    'str-y-pair-y-a
    '(((+Pair)
      (("c" NIL))
      y
      NIL
      T
      NIL
      (4214357 0 0)
      T
      (3612306 8428714)
      (NIL T) )
    ((+Pair)
        ("c" NIL)
        x
        T
        NIL
        NIL
        (5418459 0 0)
        T
        (2408204 6020510)
        (NIL T) )
    ((+Str)
        "c"
        x
        NIL
        NIL
        T
        (5418459 -1204102 0)
        T
        (0 0)
        NIL )
    ((+Pair)
        (NIL)
        y
        T
        T
        NIL
        (5418459 -3612306 0)
        T
        (0 0)
        (T NIL) )
    ((+Nil)
        NIL
        x
        NIL
        NIL
        T
        (6622561 -3612306 0)
        T
        (1204102 0)
        (NIL NIL) )
    ((+Nil)
        NIL
        x
        NIL
        NIL
        T
        (5418459 -6020510 0)
        T
        (1204102 0)
        (T NIL) )
    ((+Nil)
        NIL
        x
        NIL
        NIL
        T
        (4214357 -8428714 0)
        T
        (1204102 0)
        (T NIL) ) )))    
