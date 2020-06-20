(de bsp-x-pair-x-b ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-make-char (char "d"))
  (cmd-make-char (char "e"))
  (cmd-ptr-left)
  (cmd-bsp)
  (ver-start
    'bsp-x-pair-x-b
    '(((+Pair)
        ("c" "e")
        x
        T
        NIL
        NIL
        (4214357 0 0)
        T
        (3612306 1204102)
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
          ("e")
          x
          T
          T
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
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (6622561 0 0)
          T
          (1204102 1204102)
          NIL ) )))

(de bsp-x-pair-y-b ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-make-char (char "c"))
  (cmd-make-char (char "d"))
  (cmd-ptr-right)
  (cmd-bsp)
  (ver-start
    'bsp-x-pair-y-b
    '(((+Pair)
        ("d" NIL)
        x
        T
        NIL
        NIL
        (4214357 0 0)
        T
        (2408204 6020510)
        (NIL T) )
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
      ((+Pair)
          (NIL)
          y
          T
          T
          NIL
          (4214357 -3612306 0)
          T
          (0 0)
          (T NIL) )
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (5418459 -3612306 0)
          T
          (1204102 0)
          (NIL NIL) )
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

#(de bsp-x-pair-x-a () T)

#(de bsp-x-pair-y-a () T)


(de bsp-y-pair-x-a ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-bsp)
  (ver-start
    'bsp-y-pair-x-a
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
  
(de bsp-y-pair-x-b ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-make-pair)
  (cmd-ptr-down)
  (cmd-make-char (char "d"))
  (cmd-ptr-left)
  (cmd-bsp)
  (ver-start
    'bsp-y-pair-x-b
    '(((+Pair)
        ("c" "d")
        x
        T
        NIL
        NIL
        (4214357 0 0)
        T
        (2408204 4816408)
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
          ("d")
          x
          T
          T
          NIL
          (4214357 -3612306 0)
          T
          (2408204 1204102)
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
      ((+Nil)
          NIL
          x
          NIL
          NIL
          T
          (5418459 -3612306 0)
          T
          (1204102 1204102)
          NIL ) )))    

(de bsp-y-pair-y-a ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-make-pair)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-ptr-right)
  (cmd-bsp)
  (ver-start
    'bsp-y-pair-y-a
    '(((+Pair)
      ((NIL))
      y
      NIL
      T
      NIL
      (4214357 0 0)
      T
      (3612306 4816408)
      (NIL T) )
    ((+Pair)
        (NIL)
        y
        T
        T
        NIL
        (5418459 0 0)
        T
        (2408204 2408204)
        (NIL T) )
    ((+Nil)
        NIL
        x
        NIL
        NIL
        T
        (6622561 0 0)
        T
        (1204102 0)
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
    
(de bsp-y-pair-y-b ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-down)
  (cmd-make-pair)
  (cmd-ptr-down)  
  (cmd-make-pair)
  (cmd-bsp)
  (ver-start
    'bsp-y-pair-y-b
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
