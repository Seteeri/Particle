(de prinl-cur ()
  (prinl "(" (format (get *cur 1) *Scl) " "
             (format (get *cur 2) *Scl)
         ")"))


#####
# mov
                       

(de mov-cur-x (X)
  (setq *cur (place 1 *cur X)))


(de mov-cur-y (Y)
  (setq *cur (place 2 *cur Y)))                

  
(de mov-cur (X Y)  
  (when X (mov-cur-x X))
  (when Y (mov-cur-y Y)))

  
(de mov-cur-abv (Part)
  # Add start/end?
  (setq *cur (get Part 'ori))
  (adj-cur-y -1.0))
  

(de mov-cur-vert (Vert)
  (setq *cur 
        (with Vert
          (ori> (: met)
                (: pos)))))   
  
  
(de mov-cur-part (Part Pos)
  (case Pos
    (start (mov-cur-vert (first-vert> Part)))
    (end   (mov-cur-vert (last-vert> Part)))))


(de mov-cur-ori (Part)
  (setq *cur (get Part 'ori)))    
 

#####
# adj
  
(de adj-cur-x (Cnt)

  (unless Cnt (setq Cnt 1.0))

  (setq *cur (place 1
                    *cur
                    (+ (car *cur)
                       (*/ (meta '(+Vertex) 'adv-vert) Cnt 1.0)))))


(de adj-cur-y (Cnt)

  (unless Cnt (setq Cnt 1.0))

  (setq *cur (place 2
                    *cur
                    (- (cadr *cur)
                       (*/ (+ (meta '(+Vertex) 'adv-vert) (meta '(+Vertex) 'adv-vert)) Cnt 1.0)))))

  
(de adj-cur (X Y)
  (when X (adj-cur-x X))
  (when Y (adj-cur-y Y)))
