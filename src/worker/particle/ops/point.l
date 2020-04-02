#{

  Str

  [+Pair]

  [+Str]]
  a (+Num)

  Sym

  [+Pair]

  [+Sym] 
  s (+Num)

  Ref will be either a pair or an atom  
  Pair will call sub> method, returning a Particle
  Set pointer to its origins
  Method will set *on-car
}#
  

(de point-prv ()
  (println 'point-prv)
  (let? Part (prv> (get *0 'b)) (point-ptr Part)))


(de point-nxt ()
  (println 'point-nxt)
  (let? Part (nxt> (get *0 'b)) (point-ptr Part)))


(de point-sup ()
  (println 'point-sup)
  (let? Part (sup> (get *0 'b)) (point-ptr Part)))


(de point-sub ()
  (println 'point-sub)
  (let? Part (sub> (get *0 'b)) (point-ptr Part)))


(de point-ptr (Part)
  (put *0 'b Part)
  (println 'point-ptr "Mov -> " Part (get Part 'data))
  (lay-part-start *0 *cur 'skip-cdr))
