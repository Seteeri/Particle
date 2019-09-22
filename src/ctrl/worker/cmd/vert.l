(de lst-ptr () (car *lists-ptr))

(de push-ptr (Part)
  # Pop list, cons item, push new list back

  # Calculate XY adv based on verts of new part
  # Need to redraw post atoms and/or close parenthesis after each push
  # - Update needs to recurse/propogate through lists and nested lists etc
  #   -> Similar to generate particles
  # -> Later, use multiple workers to traverse graph
  #   - Send tasks to controller, wait for completion
  
  (let List (pop '*lists-ptr)
    # Traverse List to adj  
    (push '*lists-ptr (cons Part List))))

(de pop-ptr ()
  # Pop list, pop item, push new list back, return item
  (let (List (pop '*lists-ptr)
        Item (pop 'List))
     (push '*lists-ptr List)
     Item))

(de car-ptr () (car (car *lists-ptr)))
  
##########################


(de cmd-make-char (Keysym)

  # Data-2 (list 1 2 3 (list 'foo 'bar (list "hello" "world") "one long string") (cons NIL T) (box))

  (let (Data (cond ((= Keysym ksd~space) " ")
                  ((= Keysym ksd~Return) (char 10))
                  (T (char Keysym)))
        Part (gen-particles Data))

    (println Data)

    # Link Part to prev Particle or selection
    (with Part
      (=: next (car-ptr)))

    (push-ptr Part)

    (adv-ptr))

  #(cond ((= Keysym ksd~Return) (nl-ptr))
  #      ((= Keysym ksd~Tab)    (adv-ptr 2.0)))

  )


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
  # 1. Pop Part from timeline/ptr
  # 2. For Verts: send zero-vert cmd to render
  # 3. For Verts: push Verts

  # MODIFY COUNTER ALSO

  (when (car-ptr)
    (println (get (car-ptr) 'data)))

  (unless (= (car-ptr) 'PARTICLE)

    (let? Part (pop-ptr)

      # Part is either list or atom
      # Must trace next to ensure all vertices are freed - inv of gen
      (del-particle Part)

      # For now, push pen back n vertices
      # TODO: handle newline and tab
      (adv-ptr (num-to-fl (- (+ (length (get Part 'verts)) 1)))))))


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

  (push-ptr (gen-particles (list)))

  (adv-ptr))
