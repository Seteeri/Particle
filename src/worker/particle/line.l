(class +Line)
(dm T (P N Dims Base)
  (=: p P)
  (=: n N)
  (=: dims (if Dims Dims (list 0.0 0.0)))
  (=: base Base)) # either 'line or 'prv  
