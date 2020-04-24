(de prinl-info (Name-Fn Msg)
  (prinl "[" (usec) "]" "[" *Pid "]" "[" Name-Fn "]" " " Msg))

(de prin-info (Name-Fn Msg)
  (prin "[" (usec) "]" "[" *Pid "]" "[" Name-Fn "]" " " Msg))

(de num-to-fl (Num)
  (format (pack Num ".0") *Scl))
  
(def '*cwd (pwd)) 

(de defcwdpath (Sym Path)
  (def Sym (pack *cwd Path)))

(de load-cwd (Path)
  (load (pack *cwd Path)))
  
(de deftmppath (Sym Path)
  (def Sym (in '(mktemp) (line T))))
  
(de bind-args ()
  (setq *width     (/ 2560 2)
        *height    (/ 1080 2)
        *verts-max (** 2 16)
        *uds-ctrl  "/tmp/part-ctrl.sock"
        *uds-in    "/tmp/part-in.sock"   
        *uds-mod   "/tmp/part-mod.sock"
        *uds-rend  "/tmp/part-ren.sock"
        *path-gly  (pack (pwd) "/src/glyphs/inconsolata/")))
