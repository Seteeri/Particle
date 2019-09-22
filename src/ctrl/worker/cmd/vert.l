(de car-ptr () (get *lst-ptr 'next))


(de push-ptr (Part)

  # Calculate XY adv based on verts of new part
  # Need to redraw post atoms and/or close parenthesis after each push
  # - Update needs to recurse/propogate through lists and nested lists etc
  #   -> Similar to generate particles
  # -> Later, use multiple workers to traverse graph
  #   - Send tasks to controller, wait for completion

  # Remember *lst-ptr is a Particle with a list

  # Connect data
  (with *lst-ptr
  
    (push (:: data) (get Part 'data))

    # Set Item next
    (with Part
      (=: next (get *lst-ptr 'next)))

    # Set Lst next to Particle
    (=: next Part)))


(de pop-ptr ()

  # Reverse above
  # Use prop instead

  (println (get *lst-ptr 'data) (get *lst-ptr 'next) (get (get *lst-ptr 'next) 'next))

  (let? Item (get *lst-ptr 'next)
  
    (with *lst-ptr

      # Pop actual data
      (pop (:: data))

      # Set list next to next's next Particle
      (=: next (get Item 'next)))

    (println (get *lst-ptr 'data) (get *lst-ptr 'next))

    Item))
    
  
##########################


(de cmd-make-char (Keysym)

  # Data-2 (list 1 2 3 (list 'foo 'bar (list "hello" "world") "one long string") (cons NIL T) (box))

  (if *lst-ptr
      (prog
        (let (Data (cond ((= Keysym ksd~space) " ")
                        ((= Keysym ksd~Return) (char 10))
                        (T (char Keysym)))
              Part (gen-particles Data))

          (push-ptr Part)

          #(cond ((= Keysym ksd~Return) (nl-ptr))
          #      ((= Keysym ksd~Tab)    (adv-ptr 2.0)))

          (adv-ptr)))

      (prinl-info "cmd-make-char" "MAKE LIST FIRST!")))


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
  # frag will be discarded...fix later

  # Process:
  # 1. Pop Part from timeline/ptr
  # 2. For Verts: send zero-vert cmd to render
  # 3. For Verts: push Verts

  # MODIFY COUNTER ALSO
  
  (unless (= (car-ptr) 'PARTICLE)
    (let? Part (pop-ptr)
      (del-particle Part)
      # Adv backwards over last space
      (adv-ptr -1.0))))


(de cmd-make-num (Keysym)

  (let (Data  (format (char Keysym))
        Verts (draw-chars Data (get-color-type Data) T)
        Part   (new '(+Particle)
                   Data
                   (car-ptr)
                   Verts))

    (push-ptr Part)

    (adv-ptr)))


(de cmd-format (Keysym)

  (prinl-info "cmd-format")

  # Convert num<->str

  (let Last (get (car-ptr) 'data)

    (when (or (str? Last)
              (num? Last))

      (let (Data (format Last)
            Col (get-color-type Data)
            Verts (if (num? Data)
                      (draw-chars Data Col T)
                      (draw-str Data Col T)))

        (push-ptr
              (new '(+Particle)
                   Data
                   (car-ptr)
                   Verts))

        (adv-ptr)))))


(de cmd-pack (Keysym)

  (prinl-info "cmd-pack")

  # Convert to str and push

  # This is special as it builds back until non-str/num
  # Normally, uses last item

  (let Str (pack (get (build-pack) 2))

    # Draw data, create particle, add to timeline
    (push-ptr
          (new '(+Particle)
               Str
               (car-ptr)
               (draw-str Str (get-color-type Str) T)))

    # Set next as needed

    (adv-ptr)))


(de cmd-pack-2 (Keysym)

  (prinl-info "cmd-pack-2")

  # Convert to str and push

  # Unlike OG, use last item
  # To use this normally, must create list then push chars into that

  (let Str (pack (get (car-ptr) 'data))

    # Draw data, create particle, add to timeline
    (push-ptr
          (new '(+Particle)
               Str
               (car-ptr)
               (draw-str Str (get-color-type Str) T)))

    # Set next as needed

    (adv-ptr)))


(de cmd-chop (Keysym)

  (prinl-info "cmd-chop")

  (let Last (get (car-ptr) 'data)
  
    (push-ptr (gen-particles (chop Last))))

  (adv-ptr))


(de cmd-intern (Keysym)

  (prinl-info "cmd-intern")

  # Convert last item (t-sym) with i-sym

  # Ensure last item is a str

  (let Last (get (car-ptr) 'data)

    (when (str? Last)

      (push-ptr
            (new '(+Particle)
                 (intern Last)
                 (car-ptr)
                 (draw-chars Last (get-color-type 'data) T)))

      (adv-ptr))))


(de cmd-eval ()

(prinl-info "cmd-eval")

  # Eval last item - must be data
  # Else:
  # Str (pack Data)
  # Any (any Str)

  (let (Last (get (car-ptr) 'data)
        Any (eval Last))

    (push-ptr (gen-particles Any)))

  (adv-ptr))
      

(de cmd-any (Keysym)

  (prinl-info "cmd-any")

  # Str -> Any

  (let (Last (get (car-ptr) 'data)
        Any (any Last))

    (push-ptr (gen-particles Any))

    (adv-ptr)))


(de cmd-sym ()

  (prinl-info "cmd-sym")

  # Rev of Any
  # Any -> Str

  (let (Last (get (car-ptr) 'data)
        Str (sym Last))

    (push-ptr
          (new '(+Particle)
               Str
               (car-ptr)
               (draw-str Str (get-color-type Str) T))))

  (adv-ptr))


################################################################################


(de cmd-make-list (Keysym)

  (prinl-info "cmd-make-list")

  # Produce NIL
  # Use can enter NIL, at which point is converted into "()"

  #{
  
    QUESTION: Enter list after creation or stay on same level?
  
    Default is NO so pressing new list repeatedly will create subsequent lists
    rather than nested lists
    
    Hmm...technically lists are just nested cons

    From NIL

    (NIL (NIL) (NIL))

    Then alpha

    (NIL (NIL) (NIL "a" "b" "c"))
    
  }#

  (let (List (list)
        Part (gen-particles List))

    (setq *lst-ptr Part)

    # Solely stores list trail
    (push '*lists-ptr Part))

  (adv-ptr))
