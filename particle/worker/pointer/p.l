(class +Pointer)

(dm lay-x?> ()
  (let ((Pt-Pair Pt-Reg) (get-ref-pt> This))
    (lay-x?> Pt-Reg)))
      
(dm lay-y?> ()
  (let ((Pt-Pair Pt-Reg) (get-ref-pt> This))
    (lay-y?> Pt-Reg)))

(dm reg-a?> () (= (: reg) 'a))
(dm reg-b?> () (= (: reg) 'b))

(dm a-nil?>  ()
  (and (reg-a?> This) (not (cadr (get-ref> This)))))

(dm b-nil?>  ()
  (and (reg-b?> This) (not (cadr (get-ref> This)))))
  
(dm a-atom?> ()
  (and (reg-a?> This) (atom (cadr (get-ref> This)))))
    
(dm b-atom?> ()
  (and (reg-b?> This) (atom (cadr (get-ref> This)))))

(dm a-pair?> ()
  (and (reg-a?> This) (pair (cadr (get-ref> This)))))

(dm b-pair?> ()
  (and (reg-b?> This) (pair (cadr (get-ref> This)))))

(dm nil?>  () (not (cadr (get-ref> This))))
(dm atom?> () (atom (cadr (get-ref> This))))
(dm pair?> () (pair (cadr (get-ref> This))))
