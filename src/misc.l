(de prinl-info (Name-Fn Msg)
  (prinl "[" (usec) "]" "[" *Pid "]" "[" Name-Fn "]" " " Msg))

(de prin-info (Name-Fn Msg)
  (prin "[" (usec) "]" "[" *Pid "]" "[" Name-Fn "]" " " Msg))