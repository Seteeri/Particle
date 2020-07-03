(de del-x-atom-a NIL
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-ptr-left)
   (cmd-ptr-down)
   (cmd-del)
   (ver-start
      'del-x-atom-a
      '(((+Pair)
            (NIL)
            x
            T
            T
            NIL
            (4214357 0 0)
            T
            (3612306 1204102)
            (NIL NIL) )
         ((+Nil)
            NIL
            x
            NIL
            NIL
            T
            (4214357 -1204102 0)
            T
            (1204102 0)
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
            NIL ) ) ) )

(de del-x-atom-b NIL
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-ptr-left)
   (cmd-alt-reg)
   (cmd-ptr-right)
   (cmd-del)
   (ver-start
      'del-x-atom-b
      '(((+Pair)
            (NIL)
            x
            T
            T
            NIL
            (4214357 0 0)
            T
            (3612306 1204102)
            (NIL NIL) )
         ((+Nil)
            NIL
            x
            NIL
            NIL
            T
            (4214357 -1204102 0)
            T
            (1204102 0)
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
            NIL ) ) ) )

(de del-x-atom-b-nil NIL
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-del)
   (ver-start
      'del-x-atom-b-nil
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
            NIL ) ) ) )

(de del-y-atom-a NIL
   (cmd-ptr-right)
   (cmd-make-pair-y)
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-del)
   (ver-start
      'del-y-atom-a
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
            (T NIL) ) ) ) )

(de del-y-atom-a-nil NIL
   (cmd-ptr-right)
   (cmd-make-pair-y)
   (cmd-ptr-right)
   (cmd-del)
   (ver-start
      'del-y-atom-a-nil
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
            (T NIL) ) ) ) )

(de del-y-atom-b NIL
   (cmd-ptr-right)
   (cmd-make-pair-y)
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-ptr-left)
   (cmd-alt-reg)
   (cmd-ptr-down)
   (cmd-del)
   (ver-start
      'del-y-atom-b
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
            (T NIL) ) ) ) )

(de del-y-atom-b-nil NIL
   (cmd-ptr-right)
   (cmd-make-pair-y)
   (cmd-ptr-right)
   (cmd-del)
   (ver-start
      'del-y-atom-b-nil
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
            (T NIL) ) ) ) )

