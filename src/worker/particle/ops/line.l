        
        
(de make-line (Ref)

  #{

    [X]  [X]  [X]  NIL
    A    B    C  
  
    Mov to nl:
    

    [X]  [X]
    A    B       
    
    [X]  NIL
    C

  }#

  (println "LINE: " *line (get *line 'data))
  
  (put Ref 'line (new '(+Line)
                      *line
                      (get *line 'line 'n)
                      (if (get *line 'line 'l) @ *line)))
  (when (get *line 'line 'n)
    (put @ 'line 'p Ref))
  (put *line 'line 'n Ref)    
  
  (upd-tree)
  (mov-*1 Ref)
  
  (setq *line Ref)
  (mov-cur-abv Ref)
  (lay-part-start *0 *line 'skip-cdr)
  
  (println 'make-line "Made newline")
    
  Ref)


(de ins-line-pre (Part)

  (when (get *line 'p)
    (put @ 'line 'n Part))

  (put *line 'line 'p Part))


(de del-line (Part)

  (let? Nl (get Part 'line)

    (when (get Nl 'p)
      (put @ 'line 'n Nxt))

    (when (get Nl 'n)
      (put @ 'line 'p Prv))))

      
(de put-line (Src Dst)

  (let (Nl (get Src 'line)
        Nxt (get Nl 'n))

    (when (get Nl 'p)
      (put @ 'line 'n Dst))

    (when Nxt
      (put Nxt 'line 'p Dst))
      
    (when (= Nxt Dst)
      (put Nl 'n))
      
    # Update children's lists when Src is NIL NIL
    (when (and (not (get Nl 'p)) (not (get Nl 'l)))
      (let (Child (get Nl 'n)
            Visited NIL)
        (while (and Child
                    (not (member Child Visited)))
          (println 'put-line "Set list: " Child (get Child 'data))
          (push 'Visited Child)
          (put Child 'line 'l Dst)
          (setq Child (get Child 'line 'n))
          #(when (member Child Visited)
          #  (println 'put-line "WARNING! Cycle detected: " Child (get Child 'data)))
          )))

    (put Dst 'line Nl))

  (put Src 'line)
  
  (println 'put-line "Put line: " Src (get Src 'data) " -> " Dst (get Dst 'data))

  (setq *line Dst))      
