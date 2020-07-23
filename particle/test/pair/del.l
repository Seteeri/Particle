(de del-pair-xb-x-x NIL
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-make-char (char "d"))
   (cmd-make-char (char "e"))
   (cmd-ptr-left)
   (cmd-ptr-left)
   (cmd-del)
   (ver-start
      'del-pair-xb-x-x
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
            NIL ) ) ) )

(de del-pair-xb-x-y NIL
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-make-char (char "d"))
   (make-pair-y)
   (cmd-ptr-left)
   (cmd-del)
   (ver-start
      'del-pair-xb-x-y
      '(((+Pair)
            ("c" NIL)
            x
            T
            NIL
            NIL
            (4214357 0 0)
            T
            (2408204 6020510)
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
            (T NIL) ) ) ) )

(de del-pair-xb-x-a NIL
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-make-char (char "d"))
   (cmd-ptr-left)
   (cmd-alt-reg)
   (cmd-ptr-down)
   (cmd-make-char (char "e"))
   (cmd-alt-reg)
   (cmd-ptr-left)
   (cmd-del)
   (ver-start
      'del-pair-xb-x-a
      '(((+Pair)
            ("c" . "e")
            x
            T
            T
            NIL
            (4214357 0 0)
            T
            (1204102 1204102)
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
            "e"
            x
            NIL
            NIL
            T
            (5418459 0 0)
            T
            (0 1204102)
            NIL ) ) ) )

(de del-pair-xb-y-x NIL
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-make-char (char "d"))
   (cmd-make-char (char "e"))
   (cmd-ptr-left)
   (cmd-ptr-left)
   (cmd-ptr-down)
   (make-pair-y)
   (cmd-ptr-left)
   (cmd-del)
   (ver-start
      'del-pair-xb-y-x
      '(((+Pair)
            ("c" "e")
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
            ("e")
            x
            T
            T
            NIL
            (4214357 -3612306 0)
            T
            (2408204 1204102)
            (T NIL) )
         ((+Str)
            "e"
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
            NIL ) ) ) )

(de del-pair-xb-y-y NIL
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-make-char (char "d"))
   (cmd-make-char (char "e"))
   (cmd-ptr-left)
   (make-pair-y)
   (cmd-ptr-left)
   (cmd-ptr-down)
   (make-pair-y)
   (cmd-ptr-left)
   (cmd-del)
   (ver-start
      'del-pair-xb-y-y
      '(((+Pair)
            ("c" ("e"))
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
            (("e"))
            y
            NIL
            T
            NIL
            (4214357 -3612306 0)
            T
            (0 0)
            (T NIL) )
         ((+Pair)
            ("e")
            x
            T
            T
            NIL
            (5418459 -3612306 0)
            T
            (2408204 1204102)
            (NIL NIL) )
         ((+Str)
            "e"
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
            (T NIL) ) ) ) )

(de del-pair-xb-y-a NIL
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
   (make-pair-y)
   (cmd-ptr-left)
   (cmd-del)
   (ver-start
      'del-pair-xb-y-a
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

(de del-pair-ya-x-x NIL
   (cmd-ptr-right)
   (make-pair-y)
   (cmd-ptr-right)
   (make-pair-y)
   (cmd-make-char (char "c"))
   (cmd-ptr-left)
   (cmd-del)
   (ver-start
      'del-pair-ya-x-x
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
            (T NIL) ) ) ) )

(de del-pair-ya-x-y NIL
   (cmd-ptr-right)
   (make-pair-y)
   (cmd-ptr-right)
   (make-pair-y)
   (cmd-ptr-right)
   (make-pair-y)
   (cmd-del)
   (ver-start
      'del-pair-ya-x-y
      '(((+Pair)
            ((NIL))
            y
            NIL
            T
            NIL
            (4214357 0 0)
            T
            (2408204 6020510)
            (NIL T) )
         ((+Pair)
            (NIL)
            x
            T
            T
            NIL
            (5418459 0 0)
            T
            (1204102 3612306)
            (NIL T) )
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
            (T NIL) ) ) ) )

(de del-pair-ya-x-a NIL
   (cmd-ptr-right)
   (make-pair-y)
   (cmd-ptr-right)
   (make-pair-y)
   (cmd-ptr-down)
   (cmd-make-char (char "c"))
   (cmd-alt-reg)
   (cmd-ptr-left)
   (cmd-del)
   (ver-start
      'del-pair-ya-x-a
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

(de del-pair-ya-y-x NIL
   (cmd-ptr-right)
   (make-pair-y)
   (make-pair-y)
   (cmd-ptr-right)
   (cmd-ptr-right)
   (make-pair-y)
   (cmd-ptr-left)
   (cmd-del)
   (ver-start
      'del-pair-ya-y-x
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

(de del-pair-ya-y-y NIL
   (cmd-ptr-right)
   (make-pair-y)
   (make-pair-y)
   (make-pair-y)
   (cmd-ptr-right)
   (cmd-del)
   (ver-start
      'del-pair-ya-y-y
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
            
#{
(de del-pair-ya-y-a NIL
   (cmd-ptr-right)
   (make-pair-y)
   (make-pair-y)
   (cmd-ptr-right)
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-ptr-left)
   (cmd-del)
   (ver-start
      'del-pair-ya-y-a
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
}#

(de del-pair-yb-x-x NIL
   (cmd-ptr-right)
   (make-pair-y)
   (cmd-ptr-down)
   (cmd-make-char (char "c"))
   (cmd-make-char (char "d"))
   (cmd-ptr-left)
   (cmd-ptr-left)
   (cmd-del)
   (ver-start
      'del-pair-yb-x-x
      '(((+Pair)
            (NIL "d")
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
            ("d")
            x
            T
            T
            NIL
            (4214357 -2408204 0)
            T
            (2408204 1204102)
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

(de del-pair-yb-x-y NIL
   (cmd-ptr-right)
   (make-pair-y)
   (cmd-ptr-down)
   (cmd-make-char (char "c"))
   (cmd-make-char (char "d"))
   (cmd-ptr-left)
   (make-pair-y)
   (cmd-ptr-left)
   (cmd-del)
   (ver-start
      'del-pair-yb-x-y
      '(((+Pair)
            (NIL ("d"))
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
            (("d"))
            y
            NIL
            T
            NIL
            (4214357 -2408204 0)
            T
            (0 0)
            (T NIL) )
         ((+Pair)
            ("d")
            x
            T
            T
            NIL
            (5418459 -2408204 0)
            T
            (2408204 1204102)
            (NIL NIL) )
         ((+Str)
            "d"
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
            (T NIL) ) ) ) )

(de del-pair-yb-x-a NIL
   (cmd-ptr-right)
   (make-pair-y)
   (cmd-ptr-down)
   (cmd-make-char (char "c"))
   (cmd-ptr-left)
   (cmd-alt-reg)
   (cmd-ptr-down)
   (cmd-make-char (char "d"))
   (cmd-alt-reg)
   (cmd-ptr-left)
   (cmd-del)
   (ver-start
      'del-pair-yb-x-a
      '(((+Pair)
            (NIL . "d")
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
         ((+Str)
            "d"
            x
            NIL
            NIL
            T
            (4214357 -2408204 0)
            T
            (0 0)
            (T NIL) ) ) ) )

(de del-pair-yb-y-x NIL
   (cmd-ptr-right)
   (make-pair-y)
   (cmd-ptr-down)
   (make-pair-y)
   (cmd-ptr-down)
   (cmd-make-char (char "c"))
   (cmd-ptr-left)
   (cmd-ptr-left)
   (cmd-del)
   (ver-start
      'del-pair-yb-y-x
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

(de del-pair-yb-y-y NIL
   (cmd-ptr-right)
   (make-pair-y)
   (cmd-ptr-down)
   (make-pair-y)
   (cmd-ptr-down)
   (make-pair-y)
   (cmd-ptr-up)
   (cmd-del)
   (ver-start
      'del-pair-yb-y-y
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
            (T NIL) ) ) ) )

#{
(de del-pair-yb-y-a NIL
   (cmd-ptr-right)
   (make-pair-y)
   (cmd-ptr-down)
   (make-pair-y)
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-alt-reg)
   (cmd-ptr-up)
   (cmd-del)
   (ver-start
      'del-pair-yb-y-a
      '(((+Pair)
            (NIL . "c")
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
         ((+Str)
            "c"
            x
            NIL
            NIL
            T
            (4214357 -2408204 0)
            T
            (0 0)
            (T NIL) ) ) ) )
}#
