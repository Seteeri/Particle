#{

                      cell
                        |
            +-----------+-----------+
            |           |           |
         Number       Symbol       Pair
                        |
                        |
   +--------+-----------+-----------+
   |        |           |           |
  NIL   Internal    Transient    External
  
}#

(class +Int +Sym)  

# Draw Car/Name after main
  
(dm lay> ()

  # Draw This
  (super)

  (when (: exp) 
    (cons
      (unless (= (: b) T)
        (list (if (: b) NIL (val (: any)))
              'b
              This))
      (list NIL
            'a
            This))))
  
        
(dm lup-a> () (: a))
(dm lup-b> ()
  (if (: b)
      (: b)
      (super)))


(dm con> (Any Pt-Any Del)
      
  (when Del
    (if (: b)
        (prog
          (del> (: b))
          (=: b)
          # do set also?
          )
        (prog
          # Print warning if non-existent
          (del> (lup-pt (val (: any))))
          ~(assert (not (: b))))))
      
  (set (: any) Any)
  
  (if (atom Any)
      (prog
        (=: b Pt-Any)
        (put Pt-Any 'c This))
      (=: b)))
      
      
(dm con-2> (Any Pt-Any Del Ln)

  (when Ln  
    # always line
    ~(assert (: line))
    (put (: line) 'n NIL)
    (if (get Pt-Any 'line)
        (put @ 'p NIL)
        (put Pt-Any 'line
          (new '(+Line)))))

  (when Del
    (if (: b)
        (prog
          (del> (: b))
          (=: b)
          # do set also?
          )
        (prog
          # Print warning if non-existent
          (del> (lup-pt (val (: any))))
          ~(assert (not (: b))))))
      
  (set (: any) Any)
  
  (if (atom Any)
      (prog
        (=: b Pt-Any)
        (put Pt-Any 'c This))
      (=: b)))      
