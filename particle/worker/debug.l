(de mov-⇲ (Ln)
  # Move to bnds of current line
  (let (Ori (get Ln 'ori)
        Dims (get Ln 'line 'dims))
    (mov-cur (+ (car Ori) (car Dims))
             (- (cadr Ori) (cadr Dims)))
    #(adj-cur-y)
    #(prinl "Cur: " "(" (format (get *cur 1) *Scl) " "
    #                   (format (get *cur 2) *Scl) ")")
    (mov> (lup-pt '⇲) *cur)))
