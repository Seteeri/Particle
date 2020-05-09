####################
        
        
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

  
###################


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
