(de make-list-cdr (Ref)
  (println 'make-list-cdr)
  
  # Pair/X or Y
  # Atm
  
  (cond ((and (lay-y?> Ref) (is-pair? Ref))
         # Make like car...
         (make-case-4 Ref)
         # Move to c instead of a
         T)
        
        ((and (lay-x?> Ref) (is-pair? Ref))
         # Make list
         T)        
        
        (T
         # End Atm
         T)))
