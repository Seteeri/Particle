#{
             3 2 1 |
  CONS:      0 0 0 0
  SYM:       1 0 0 0
  BIGNUM:    S 1 0 0
  SHORTNUM:  S 0 1 0

  STR: stored as nums
  NUM: shortnum takes entire PTR, bignum in CAR + CDR ptr
  S= pos:0 neg:1

  1. *sol-yellow  
  2. *sol-orange
  3. *sol-red
  
  4. *sol-magenta    
  5. *sol-violet
  6. *sol-blue
  7. *sol-cyan
  8. *sol-green

  Cell
  * Num - 3
    1 Ptr (builtins) = *sol-base-01 or content tone
    2 Big (interpret whole) = *sol-green
    3 Short = *sol-cyan
  * Sym - 2
    4 Internal - blue
    4 Transient
    4 External
    5 NIL - violet    
  6 Pair/List (Cell) - magenta
    
  * Accents are *Col
}#


(de decode-ptr  (Any) (>> -4 (adr Any)))
(de unpack-ptrs (Any) (struct (decode-ptr Any) '((B . 8) (B . 8))))
(de list? (Any) (and (lst? Any) (not (= Any NIL))))

# Draw ptrs
(de draw-cons-ptr (Ptr-Ar Col-Ar Ptr-Dr Col-Dr)
  (list (draw-chars Ptr-Ar Col-Ar T)
        (draw-dot T)
        (draw-chars Ptr-Dr Col-Dr T)))


(de get-col-ptr (Ptr)    

  (let ((B1 B2 B3 B4) (cdr (flip (tail 4 (chop (bin Ptr))))))
    #(println B1 B2 B3 B4)
    (cond ((= "1" B1) *sol-cyan)
          ((= "1" B2) *sol-green)
          ((= "1" B3) *sol-violet)
          ((= "0" B3) *sol-magenta)
          (T *sol-base-01))))


(de get-type-ptr (Ptr)

  # Built-ins will typically be shorts; bits are arbitrary
   
  (let ((B1 B2 B3 B4) (cdr (flip (tail 4 (chop (bin Ptr))))))
    #(println B1 B2 B3 B4)
    (cond ((= "1" B1) 'short)
          ((= "1" B2) 'big)
          ((= "1" B3) 'sym)
          ((= "0" B3) 'cons)
          (T 'ptr))))


(de gen-cell (Any Type Root)
  (cond ((= Type 'big)   (gen-big Any Root))
        ((= Type 'sym)   (gen-sym Any Root))
        ((= Type 'cons)  (gen-pair Any Root))))


(de gen-ptr-sym (Ptr Root)

  #{
  
    INTERNAL:
    CAR = Cons (property list+name), Short/Big (name)
    CDR = Any
    
    ANONYMOUS:
    CAR = 0 (long = 2)
    CDR = Any
    
  }#

  (let ((Ptr-Ar Ptr-Dr) (struct Ptr '(N . 2))
        Type-Ar         (get-type-ptr Ptr-Ar)
        Type-Dr         (get-type-ptr Ptr-Dr)
        (X Y)           *pos-cursor
        
        blah (println Ptr Ptr-Ar Ptr-Dr)
        blah (println Ptr Type-Ar Type-Dr)

        Col-Ar (cond ((= Ptr-Ar 4380504) *sol-yellow)
                     ((= Ptr-Ar 4380648) *sol-red)
                     (T (get-col-ptr Ptr-Ar)))
        Col-Dr (cond ((= Ptr-Dr 4380504) *sol-yellow)
                     ((= Ptr-Dr 4380648) *sol-red)
                     (T (get-col-ptr Ptr-Dr)))
        (Vert-Ar Dot Vert-Dr) (draw-cons-ptr (pack (tail 4 (chop (bin Ptr-Ar)))) Col-Ar
                                             (pack (tail 4 (chop (bin Ptr-Dr)))) Col-Dr)

        X-Cdr (get *pos-cursor 1)

        Pa-Ar (unless (or (= Ptr-Ar 4380504)
                          (= Ptr-Ar 4380648))
                # Move below, align left of cons
                (nl-cur) (set-cur-x X)    
                (gen-cell (cond ((= Type-Ar 'sym) (- Ptr-Ar 8))
                                ((= Type-Ar 'big) (- Ptr-Ar 4))
                                (T Ptr-Ar))
                          Type-Ar
                          Root))
                          
        Pa-Dr (unless (or (= Ptr-Dr 4380504)
                          (= Ptr-Dr 4380648)
                          (= Ptr-Dr (+ Ptr 8)))
                 # If cursor short then must adv
                 (when (<= (get *pos-cursor 1) X-Cdr)
                   (set-cur-x X-Cdr))
                 (adv-cur)
                 (set-ptr-y Y)
                 (gen-cell (cond ((= Type-Dr 'sym) (- Ptr-Dr 8))
                                 ((= Type-Dr 'big) (- Ptr-Dr 4))
                                 (T Ptr-Dr))
                            Type-Dr
                            Root)))
    (new '(+Particle)
         Ptr
         Pa-Ar Pa-Dr
         (append Vert-Ar Dot Vert-Dr))))


(de gen-ptr-big (Ptr Root)
  
  (println 'gen-big)

  (let ((Ptr-Ar Ptr-Dr) (struct Ptr '(N . 2))
        Type-Ar         'ptr # always
        Type-Dr         (get-type-ptr Ptr-Dr) # bignum or short
        (X Y)           *pos-cursor

        blah (println Ptr Ptr-Ar Ptr-Dr)
        blah (println Ptr Type-Ar Type-Dr)
        
        Col-Ar *sol-base-01
        Col-Dr (cond ((= Ptr-Dr 4380504) *sol-yellow)
                     ((= Ptr-Dr 4380648) *sol-red)
                     (T (get-col-ptr Ptr-Dr)))
        (Vert-Ar Dot Vert-Dr) (draw-cons-ptr (pack (tail 4 (chop (bin Ptr-Ar)))) Col-Ar
                                             (pack (tail 4 (chop (bin Ptr-Dr)))) Col-Dr)

        X-Cdr (get *pos-cursor 1)

        # Never draw...unless wanted...
        Pa-Ar NIL
                          
        Pa-Dr (unless (or (= Ptr-Dr 4380504)
                          (= Ptr-Dr 4380648))
                 # If cursor short then must adv
                 (when (<= (get *pos-cursor 1) X-Cdr)
                   (set-cur-x X-Cdr))
                 (adv-cur)
                 (set-ptr-y Y)
                 (gen-cell (cond ((= Type-Dr 'sym) (- Ptr-Dr 8))
                                 ((= Type-Dr 'big) (- Ptr-Dr 4))
                                 (T Ptr-Dr))
                            Type-Dr
                            Root)))
    (new '(+Particle)
         Ptr
         Pa-Ar Pa-Dr
         (append Vert-Ar Dot Vert-Dr))))


(de gen-ptr-pair (Ptr Root)
  #{
  }#

  (println 'gen-pair)

  (let ((Ptr-Ar Ptr-Dr) (struct Ptr '(N . 2))
        Type-Ar         (get-type-ptr Ptr-Ar)
        Type-Dr         (get-type-ptr Ptr-Dr)
        (X Y)           *pos-cursor
        
        blah (println Ptr Ptr-Ar Ptr-Dr)
        blah (println Ptr (bin Ptr-Ar) (bin Ptr-Dr))        
        blah (println Ptr Type-Ar Type-Dr)

        Col-Ar (cond ((= Ptr-Ar 4380504) *sol-yellow)
                     ((= Ptr-Ar 4380648) *sol-red)
                     (T (get-col-ptr Ptr-Ar)))
        Col-Dr (cond ((= Ptr-Dr 4380504) *sol-yellow)
                     ((= Ptr-Dr 4380648) *sol-red)
                     (T (get-col-ptr Ptr-Dr)))
        (Vert-Ar Dot Vert-Dr) (draw-cons-ptr (pack (tail 4 (chop (bin Ptr-Ar)))) Col-Ar
                                             (pack (tail 4 (chop (bin Ptr-Dr)))) Col-Dr)

        X-Cdr (get *pos-cursor 1)

        Pa-Ar (unless (or (= Ptr-Ar 4380504)
                          (= Ptr-Ar 4380648))
                # Move below, align left of cons
                (nl-cur) (set-cur-x X)
                (gen-cell (cond ((= Type-Ar 'sym) (- Ptr-Ar 8))
                                ((= Type-Ar 'big) (- Ptr-Ar 4))
                                (T Ptr-Ar))
                          Type-Ar
                          Root))
                          
        Pa-Dr (unless (or (= Ptr-Dr 4380504)
                          (= Ptr-Dr 4380648))
                 # If cursor short then must adv
                 (when (<= (get *pos-cursor 1) X-Cdr)
                   (set-cur-x X-Cdr))
                 (adv-cur)
                 (set-ptr-y Y)
                 (gen-cell (cond ((= Type-Dr 'sym) (- Ptr-Dr 8))
                                 ((= Type-Dr 'big) (- Ptr-Dr 4))
                                 (T Ptr-Dr))
                            Type-Dr
                            Root)))
    (new '(+Particle)
         Ptr
         Pa-Ar Pa-Dr
         (append Vert-Ar Dot Vert-Dr))))
