(de make-list-cdr (Ref)
  (println 'make-list-cdr)
  
  # Pair/X or Y
  # Atom
  
  (cond ((and (lay-y?> Ref) (isa '(+Pair) Ref))
         # Make like car...
         (make-case-4 Ref)
         # Move to c instead of a
         T)
        
        ((and (lay-x?> Ref) (isa '(+Pair) Ref))
         # Make list
         T)        
        
        (T
         # End Atom
         T)))
