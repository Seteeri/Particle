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
  

(de point-b-prv ()
  (println 'point-b-prv)
  (let? Part (prv> (get *0 'b)) (point-ptr Part)))


(de point-b-nxt ()
  (println 'point-b-nxt)
  (let? Part (nxt> (get *0 'b)) (point-ptr Part)))


(de point-a-out ()
  (println 'point-a-out)
  (let? Part (sup> (get *0 'b)) (point-ptr Part)))


(de point-a-in ()
  (println 'point-a-in)
  (let? Part (sub> (get *0 'b)) (point-ptr Part)))


(de point-ptr (Part)
  (put *0 'b Part)
  (lay-part-start *0 *cur 'skip-cdr))
