(class +Dlst)
(dm T (Frs Las)
  (=: frs Frs)
  (=: las Las)
  (=: len 0))
  

(class +Node-Dlst)
(dm T (Dat Nxt Prv)
  (=: dat Dat)
  (=: nxt Nxt)
  (=: prv Prv))


# 1 2 3 4 5 6...
# Frs...Las

(de trav-fwd (List)
  (let Node (setq Node (get List 'frs))
    (while Node
      (println Node (get Node 'dat))
      (setq Node (get Node 'nxt)))))


(de trav-back (List)
  (let Node (setq Node (get List 'las))
    (while Node
      (println Node (get Node 'dat))
      (setq Node (get Node 'nxt)))))


(de ins-aft (List Node New)
  (put New 'prv Node)
  (if (get Node 'nxt)
    (prog
      (put New 'nxt (get Node 'nxt))
      (put (get Node 'nxt) 'prv New))
    (prog
      (put New 'nxt NIL)
      (put List 'las New)))
  (put Node 'nxt New)
  #(println (prop List 'len))
  (inc (prop List 'len)))

  
(de ins-bef (List Node New)
  (put New 'nxt Node)
  (if (get Node 'prv)
    (prog
      (put New 'prv (get Node 'prv))
      (put (get Node 'prv) 'nxt New))
    (prog
      (put New 'prv NIL)
      (put List 'frs New)))
  (put Node 'prv New)
  (inc (prop List 'len)))


(de ins-frs (List New)
  (if (get List 'frs)
    (ins-bef List (get List 'frs) New)
    (prog
      (put List 'frs New)
      (put List 'las New)
      (put New 'prv NIL)
      (put New 'nxt NIL)
      (inc (prop List 'len)))))


(de ins-las (List New)
  (if (get List 'las)
    (ins-aft List (get List 'las) New)
    (ins-frs List New)))


(de rem-dno (List Node)
  (if (get Node 'prv)
    (put (get Node 'prv) 'nxt (get Node 'nxt))
    (put List 'frs (get Node 'nxt)))
  (if (get Node 'nxt)
    (put (get Node 'nxt) 'prv (get Node 'prv))
    (put List 'las (get Node 'prv)))
  (dec (prop List 'len)))


(de rem-frs (List)
  (let Node (get *dlst 'frs)
    (when Node
      (rem-dno *dlst Node)
      Node)))


(de rem-frs-n-d (List Cnt)
  (let (L NIL
        E NIL
        C 0)
    (while (and (< C Cnt)
                (setq E (get (rem-frs *dlst) 'dat)))
      (push 'L E)
      (inc 'C))
    L))


(de rem-frs-n-d-c (List Cnt)
  (let (L NIL
        E NIL
        C 0)
    (while (and (< C Cnt)
                (setq E (get (rem-frs *dlst) 'dat)))
      (push 'L (char E))
      (inc 'C))
    L))    