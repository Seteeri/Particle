(de cmd-make-char-2 (Keysym)

  (let (Data  (cond ((= Keysym ksd~space) " ")
                    ((= Keysym ksd~Return) (char 10))
                    (T (char Keysym)))
        Verts (draw-str Data (get-color-type Data))
        Prt   (new '(+Particle) Data Verts))

    (push '*timeline Prt))

  (cond ((= Keysym ksd~Return) (nl-ptr))
        ((= Keysym ksd~Tab)    (adv-ptr 2.0))))
        

(de cmd-make-char-3 (Keysym)

  # Color car/cdr different?

  # Draw bytes
  # Draw value

  (let (Data  (cond ((= Keysym ksd~space) " ")
                    ((= Keysym ksd~Return) (char 10))
                    (T (char Keysym)))
        (Adr-Car Adr-Cdr) (struct (- (>> -4 (adr Data)) 8) '((B . 8) (B . 8)))
        Adr  (pack (glue " " Adr-Car) " " (glue " " Adr-Cdr))
        Verts-Adr (draw-str Adr (get-color-type Data))
        Dummy (nl-ptr)
        Verts-Repr (draw-str Data (get-color-type Data))
        Prt   (new '(+Particle) Data Verts))

    (push '*timeline Prt))

  (cond ((= Keysym ksd~Return) (nl-ptr))
        ((= Keysym ksd~Tab)    (adv-ptr 2.0))
        (T (adv-ptr 2.0))))


(de cmd-make-char (Keysym)

  (let Data (cond ((= Keysym ksd~space) " ")
                  ((= Keysym ksd~Return) (char 10))
                  (T (char Keysym)))

    #(draw-cell 'Data)
    #(draw-cell (adr Data))

    (let Ptr-Pos-Start (get *vert-ptr 'pos)
      #(draw-cell (>> -4 (adr (cons (cons 1 2) (cons 3 4)))))
      #(draw-cell (>> -4 (adr (list 1 2 3))))
      (draw-cell (>> -4 (adr 'Data)))

      (nl-ptr 6.0)
      (set-pos-ptr-x (get Ptr-Pos-Start 1))))
  

  (cond ((= Keysym ksd~Return) (nl-ptr))
        ((= Keysym ksd~Tab)    (adv-ptr 2.0))))


(de cmd-del (Dir)

  (prinl-info "cmd-del")

  # Currently, render will attempt to render all verts (*max-verts)
  # -> Send cmd to render to adjust max
  # - instanceCount is number of instances from beginning of array
  # *verts is LIFO, thus will always keep instances pack from beginning of array
  # -> Maintain vert cnt, instanceCount = size - used
  #
  # However, if del from arbitrary point then must zero it and instanceCount
  # remains the same
  # After del, vert returns to queue, and next vert will reuse it
  #
  # Makes most sense to simply zero vert, and not worry about instanceCount as
  # frag will be discarded

  # Process:
  # 1. Pop Prt from timeline/ptr
  # 2. For Verts: send zero-vert cmd to render
  # 3. For Verts: push Verts

  # Note, this does not necessarily delete all the vertices as it dels PARTICLES

  (let? Prt (pop '*timeline)
    (del-particle Prt)
    
    # For now, push pen back n vertices
    # TODO: handle newline and tab
    (adv-ptr (num-to-fl (- (+ (length (get Prt 'verts)) 1))))))


(de cmd-eval ()

  (println "cmd-eval")

  # Eval last item

  # Variations:
  # Eval last
  # Eval list
  # Eval str

  (let (Data (get (car *timeline) 'data)
        Str (pack Data)
        Any (any Str)
        Res (eval Any)
        Col (get-color-type Res))

    (prinl-info "cmd-eval" (pack Str))

    #(nl-ptr)
    (adv-ptr)

    (push '*timeline
          (new '(+Particle)
               Res
               (draw-str (if (num? Res) (format Res) (format (adr Res)))
                         Col)))

    (adv-ptr)))


(de cmd-eval-2 ()

  (println "cmd-eval-2")

  # Eval list (default is entire timeline)

  (let (Data (build-data)
        Res (eval Data)
        Col (get-color-type Res))

    (print-tl)
    (print-tl-2)
    (println Res)

    (push '*timeline
          (new '(+Particle)
               Res
               (draw-str (if (num? Res) (format Res) (format (adr Res)))
                         Col)))

    (adv-ptr)))


(de cmd-pack (Keysym)

  (prinl-info "cmd-pack")

  # Replace with str

  # Pop until non-str/num

  (let Data (pack (build-data))

    (println Data)

    # Adv ptr before drawing
    (adv-ptr)

    # Draw data, create particle, add to timeline
    (push '*timeline (new '(+Particle) Data (draw-str Data (get-color-type Data))))))


(de cmd-pack-alt (Keysym) T)


(de cmd-intern (Keysym)

  # Replace last item (t-sym) with i-sym

  # Ensure last item is a str

  (when (str? (get (car *timeline) 'data))

    (let Prt (pop '*timeline)

      # TODO: Assumes proper/default spacing
      (adv-ptr (format (pack (- (length (get Prt 'verts))) ".0") *Scl))

      # TODO: Reuse verts
      (intern-particle (get Prt 'data))

      (del-particle Prt)

      # TODO: Adjust ptr to right of new particle
      (adv-ptr))))


(de cmd-intern-alt (Keysym) T)


(de cmd-make-num (Keysym)

  (let (Data  (format (char Keysym))
        Verts (draw-str Data (get-color-type Data))
        Prt   (new '(+Particle) Data Verts))

    (push '*timeline Prt)

    (adv-ptr)))


(de cmd-format (Keysym)

  # Convert last num to str or last str to num

  (let Last (get (car *timeline) 'data)

    (when (or (str? Last) (num? Last))

      (let Data (format Last)
      
        (push '*timeline (new '(+Particle) Data (draw-str Data (get-color-type Data))))

        (adv-ptr)))))


(de cmd-make-cons (Keysym)

  # Make cons from last two objects

  # REPLACED:
  # A B C D
  # A B (C . D)
  # A (B . (C . D))
  # (A . (B . (C . D)))

  # APPENDED:
  # A B C D
  # A B C D -> cons -> (C . D)
  # A B C D (C . D) -> cons -> (D (C . D))
  # A B C D (C . D) (D (C . D))

  # Data: (list A B C D) = (A . (B . (C . (D . NIL))))
  # Draw: A . B . C . D (where . is colored)

  # Data: (list A (B C) D (E F) G) = (A . ((B . (C . NIL)) . (D . ((E . (F . NIL)) . (G . NIL)))))
  # Draw: A .     D .     G
  #         B . C   E . F

  (let (Car (get (car *timeline) 'data)
        Cdr (get (car (cdr *timeline)) 'data)
        Data (cons Car Cdr))

    (println Data)
  
    (push '*timeline (new '(+Particle) Data (draw-str "." (get-color-type Data)))))

  (adv-ptr))
  

(de cmd-any (Keysym)

  (let Last (get (car *timeline) 'data)

    (let Data (any Last)

      (println Data)

      # Cannot handle list yet
    
      (push '*timeline (new '(+Particle) Data (draw-str Data (get-color-type Data))))

      (adv-ptr))))


(de cmd-intern-2 (Keysym)

  # Pack then Intern
  # Same as cmd-pack except result not added to timeline
  (let ((Verts Chars) (build-str))
  
    (when Chars

      (adv-ptr)
      (intern-particle (pack Chars)))))