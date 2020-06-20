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
