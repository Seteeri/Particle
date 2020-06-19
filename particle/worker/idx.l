(de h64 (X)
  (native PATH-H64S "h64s" 'N X))
  
    
(de idx-pt (P)

  (with P

    ~(assert (: data))
  
    (let Node (lup-node (: data))
    
      # If node: add to list
      # else: create it
      (if Node
      
          # Create list if needed
          (prog
            (if (lst? (cdr Node))
              (push (cdr Node) P)
              (con Node (list P)))
            (when T (println 'idx-pt 'Exists! (h64 (adr (: data))) P (type P) (depth *points) *cnt-p)))

          (prog
            (idx-node P (get P 'data))
            (when (: a) (idx-node (: a) (get (: a) 'data)))
            (when (: b) (idx-node (: b) (get (: b) 'data))))))))

  
(de idx-node (X D)
  (let (A (adr D)
        H (h64 A))
    (idx '*points (cons H X) T)
    (inc '*cnt-p)
    #(println 'idx-node H A D X (depth *points) *cnt-p)
  ))
  
  
(de lup-node (D)
  (lup *points (h64 (adr D))))
  
  
(de lup-pt (D)
  (cdr (lup *points (h64 (adr D)))))


(de del-node (D)
  (idx '*points (lup-node D) NIL))
