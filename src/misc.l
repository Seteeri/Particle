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
  
(de bind-args X
  (let A '(*width
           *height
           *verts-max
           PATH-UDS-CONTROL
           PATH-UDS-INPUT      
           PATH-UDS-MODEL  
           PATH-UDS-RENDER)

    (when (lst? X)
      (conc A X))
  
    (mapc set A (argv))
    
    (unless (num? (setq *width (format *width)))         (quit (pack "Quiting: Invalid width=" *width)))
    (unless (num? (setq *height (format *height)))       (quit (pack "Quiting: Invalid height=" *height)))
    (unless (num? (setq *verts-max (format *verts-max))) (quit (pack "Quiting: Invalid verts=" *verts-max)))
    
    A))
