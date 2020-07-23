(de alt-reg-x-atom-a NIL
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-ptr-left)
   (cmd-ptr-down)
   (cmd-alt-reg)
   (ver-start
      'alt-reg-x-atom-a
      '(((+Pair)
            (NIL . "c")
            x
            T
            T
            NIL
            (4214357 0 0)
            T
            (2408204 1204102)
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
         ((+Str)
            "c"
            x
            NIL
            NIL
            T
            (6622561 0 0)
            T
            (0 1204102)
            NIL ) ) ) )

(de alt-reg-x-atom-b NIL
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-alt-reg)
   (ver-start
      'alt-reg-x-atom-b
      '(((+Pair)
            (NIL . "c")
            x
            T
            T
            NIL
            (4214357 0 0)
            T
            (2408204 1204102)
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
         ((+Str)
            "c"
            x
            NIL
            NIL
            T
            (6622561 0 0)
            T
            (0 1204102)
            NIL ) ) ) )

#{
(de alt-reg-y-atom-a NIL
   (cmd-ptr-right)
   (make-pair-y)
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-alt-reg)
   (ver-start
      'alt-reg-y-atom-a
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
      (T NIL) ) )))


(de alt-reg-y-atom-b NIL
   (cmd-ptr-right)
   (make-pair-y)
   (cmd-ptr-right)
   (cmd-make-char (char "c"))
   (cmd-alt-reg)
   (cmd-alt-reg)
   (ver-start
      'alt-reg-y-atom-b
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
      (T NIL) ) )))
}#
