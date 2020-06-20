(de bsp-x-atom-a ()
  # do nothing
  T)

(de bsp-x-atom-b ()
  (cmd-ptr-right)
  (cmd-make-char (char "c"))
  (cmd-make-char (char "d"))
  (cmd-bsp)
  (ver-start
    'bsp-x-atom-b
    '(((+Pair)
        ("c")
        x
        T
        T
        NIL
        (4214357 0 0)
        T
        (2408204 1204102)
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

(de bsp-y-atom-a ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-make-pair)
  (cmd-make-pair)
  (cmd-ptr-right)
  (cmd-ptr-right)
  (cmd-bsp)
  (ver-start
    'bsp-y-atom-a
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
  
(de bsp-y-atom-b ()
  (cmd-ptr-right)
  (cmd-make-pair)
  (cmd-ptr-down)
  (cmd-make-pair)
  (cmd-ptr-down)
  (cmd-make-pair)
  (cmd-bsp)
  (ver-start
    'bsp-y-atom-b
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
