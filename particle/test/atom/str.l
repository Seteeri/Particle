(de str-x-atom-b NIL
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

(de str-x-atom-a NIL
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
            (2408204 1204102)
            (NIL NIL) )
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
            NIL ) ) ) )

(de str-y-atom-b NIL
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
            NIL ) ) ) )

(de str-y-atom-a NIL
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
            (T NIL) ) ) ) )

