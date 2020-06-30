(de bsp-atom-x-x-a NIL
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-make-char (char "d"))
   (cmd-make-char (char "e"))
   (cmd-make-char (char "f"))
   (cmd-make-char (char "g"))
   (cmd-make-char (char "h"))
   (cmd-ptr-left)
   (cmd-ptr-left)
   (cmd-ptr-left)
   (cmd-ptr-down)
   (cmd-bsp)
   (ver-start
      'bsp-atom-x-x-a
      '(((+Pair)
            ("c" "d" "e" . "f")
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
            ("d" "e" . "f")
            x
            T
            NIL
            NIL
            (5418459 0 0)
            T
            (0 1204102)
            NIL )
         ((+Str)
            "d"
            x
            NIL
            NIL
            T
            (5418459 -1204102 0)
            T
            (0 0)
            NIL )
         ((+Pair)
            ("e" . "f")
            x
            T
            T
            NIL
            (6622561 0 0)
            T
            (0 1204102)
            NIL )
         ((+Str)
            "e"
            x
            NIL
            NIL
            T
            (6622561 -1204102 0)
            T
            (0 0)
            NIL )
         ((+Str)
            "f"
            x
            NIL
            NIL
            T
            (7826663 0 0)
            T
            (0 1204102)
            NIL ) ) ) )

(de bsp-atom-x-x-b NIL
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-make-char (char "d"))
   (cmd-bsp)
   (ver-start
      'bsp-atom-x-x-b
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

(de bsp-atom-x-y-a NIL
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-make-pair)
   (cmd-ptr-right)
   (cmd-make-char (char "d"))
   (cmd-bsp)
   (ver-start
      'bsp-atom-x-y-a
      '(((+Pair)
            ("c" . "d")
            x
            T
            T
            NIL
            (4214357 0 0)
            T
            (0 1204102)
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
         ((+Str)
            "d"
            x
            NIL
            NIL
            T
            (5418459 0 0)
            T
            (0 0)
            (NIL NIL) ) ) ) )

(de bsp-atom-x-y-b NIL
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-make-char (char "d"))
   (cmd-ptr-left)
   (cmd-alt-reg)
   (cmd-ptr-down)
   (cmd-make-char (char "e"))
   (cmd-alt-reg)
   (cmd-ptr-left)
   (cmd-ptr-down)
   (cmd-make-pair)
   (cmd-ptr-left)
   (cmd-ptr-down)
   (cmd-bsp)
   (ver-start
      'bsp-atom-x-y-b
      '(((+Pair)
            ("c" . "e")
            x
            T
            T
            NIL
            (4214357 0 0)
            T
            (0 3612306)
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
         ((+Str)
            "e"
            x
            NIL
            NIL
            T
            (4214357 -3612306 0)
            T
            (0 0)
            (T NIL) ) ) ) )

(de bsp-atom-y-y-a NIL
   (cmd-ptr-right)
   (cmd-make-pair)
   (cmd-make-pair)
   (cmd-ptr-right)
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-bsp)
   (ver-start
      'bsp-atom-y-y-a
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

(de bsp-atom-y-y-b NIL
   (cmd-ptr-right)
   (cmd-make-pair)
   (cmd-make-pair)
   (cmd-ptr-right)
   (cmd-ptr-down)
   (cmd-bsp)
   (ver-start
      'bsp-atom-y-y-b
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

