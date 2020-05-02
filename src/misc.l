(de prinl-info (Name-Fn Msg)
  (prinl "[" (usec) "]" "[" *Pid "]" "[" Name-Fn "]" " " Msg))

(de prin-info (Name-Fn Msg)
  (prin "[" (usec) "]" "[" *Pid "]" "[" Name-Fn "]" " " Msg))

(de num-to-fl (Num)
  (format (pack Num ".0") *Scl))
  
(def '*cwd (pwd)) 

(de defcwdpath (Sym Path)
  (def Sym (pack *cwd Path)))

(de ld-cwd (Path)
  (load (pack *cwd Path)))
  
(de deftmppath (Sym Path)
  (def Sym (in '(mktemp) (line T))))
  
(de bind-args ()
  (setq *width     (/ 2560 2)
        *height    (/ 1080 2)
        *verts-max (** 2 18)
        *uds-ctrl  "/tmp/part-ctrl.sock"
        *uds-in    "/tmp/part-in.sock"   
        *uds-mod   "/tmp/part-mod.sock"
        *uds-rend  "/tmp/part-ren.sock"
        *path-gly  (pack (pwd) "/bin/DejaVuSansMono/")))

#{
(let Fmt (-10 -12 -12 -12 -12)
  (tab Fmt "EXP" "VERT" "TRI" "QUAD" "SZ/MB")
  (tab Fmt (pack (need 10 "-"))
           (pack (need 12 "-"))
           (pack (need 12 "-"))
           (pack (need 12 "-"))
           (pack (need 12 "-")))
  (for I 26
    (tab Fmt 
      (pack "(** 2 " I ")")
      (format (** 2 I) *Scl "." ",")
      (format (/ (** 2 I) 3) *Scl "." ",")
      (format (/ (** 2 I) 4) *Scl "." ",")
      (format (/ (* 208 (** 2 I)) 1024 1024) *Scl "." ","))))

EXP       VERT        TRI         QUAD        SZ/MB
----------------------------------------------------------
(** 2 1)  2           0           0           0
(** 2 2)  4           1           1           0
(** 2 3)  8           2           2           0
(** 2 4)  16          5           4           0
(** 2 5)  32          10          8           0
(** 2 6)  64          21          16          0
(** 2 7)  128         42          32          0
(** 2 8)  256         85          64          0
(** 2 9)  512         170         128         0
(** 2 10) 1,024       341         256         0
(** 2 11) 2,048       682         512         0
(** 2 12) 4,096       1,365       1,024       0
(** 2 13) 8,192       2,730       2,048       1
(** 2 14) 16,384      5,461       4,096       3
(** 2 15) 32,768      10,922      8,192       6
(** 2 16) 65,536      21,845      16,384      13
(** 2 17) 131,072     43,690      32,768      26
(** 2 18) 262,144     87,381      65,536      52
(** 2 19) 524,288     174,762     131,072     104
(** 2 20) 1,048,576   349,525     262,144     208
(** 2 21) 2,097,152   699,050     524,288     416
(** 2 22) 4,194,304   1,398,101   1,048,576   832
(** 2 23) 8,388,608   2,796,202   2,097,152   1,664
(** 2 24) 16,777,216  5,592,405   4,194,304   3,328
(** 2 25) 33,554,432  11,184,810  8,388,608   6,656
(** 2 26) 67,108,864  22,369,621  16,777,216  13,312

}#
