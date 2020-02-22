(de cmd-make-char (Keysym)

  # TODO
  # * Get list from pointer data

  # Append char to a list, er symbol containing list

  # (set (get (car *part-ptrs) 'data) Data)

  (let Data (list (cond ((= Keysym ksd~space) " ")
                        ((= Keysym ksd~Return) (char 10))
                        (T (char Keysym))))

    # This will follow pointers until b is NIL
    (let Last (last-part> *part-main)

      # Mov cursor to NIL first vert pos
      (mov-cursor (car (get Last 'verts)) T)

      # Gen part for new data; incl CAR/CDR
      (let Part (gen-pair-x-h Data
                              0.0
                              *pos-cursor
                              gen-pair-x-h
                              NIL T)

        # Connect NIL
        (with Part
          (=: b Last))
        
        # Reposition NIL in front of new Part
        (mov-cursor (last (get Part 'verts)) T)
        (adv-cursor 2.0)
        (mov> Last *pos-cursor)

        # Only if empty
        (unless (get *part-main 'b)
          (with *part-main
            # Connect symbol CDR to Part
            (=: b Part)))))

    (with *part-main          
      # Connect data; aka modify CDR of Last
      (=: data (conc (: data) Data)))

    # Data of pointer will always be the symbol itself
    # Set the symbol's value to new list
    # Set CDR, not Data?
    # TODO: Remove NIL
    (set (get (car *part-ptrs) 'data) Data)

    # TODO: Position p0 over new item
    # Rebase verts at current ptr pos
    (when T
      (nl-cursor -1.0)
      (adv-cursor -3.0) # Pos over last item (not NIL)
      (mov> (car *part-ptrs) *pos-cursor))

    # Enter/Newline should produce char + mov ptr/cur
    (cond ((= Keysym ksd~Return) T)
          ((= Keysym ksd~Tab)    T)
          (T T))))


(de cmd-del (Dir)

  (prinl-info "cmd-del")

  #{
    Currently, render will attempt to render all verts (*max-verts)
    -> Send cmd to render to adjust max
    - instanceCount is number of instances from beginning of array
    *verts is LIFO, thus will always keep instances pack from beginning of array
    -> Maintain vert cnt, instanceCount = size - used

    However, if del from arbitrary point then must zero it and instanceCount
    remains the same
    After del, vert returns to queue, and next vert will reuse it

    Makes most sense to simply zero vert, and not worry about instanceCount as
    frag will be discarded...fix later

    (NIL 1 2 3)
    Pop 1
    Connect 2 -> NIL
    ...
    (NIL 2 3)
    Pop 2
    Connect 3 -> NIL
    ...
    (NIL 3)
    Pop 3
    Replace if CDDR NIL
  }#

  #(let Last (last-part> *part-main)

  (when (get *part-main 'data)

    # Build temp list
    # Poss cache L (and use dirty bit)

    (let (L ()
          D ()
          X *part-main)

      (loop
        (push 'L X)
        (push 'D (get X 'data))
        # If CDR is NIL, end of list
        (NIL (get X 'b))
        (setq X (get X 'b)))
      
      # Delete second last particle since last is NIL
      (let (Last-2 (cadr L)
            Last-3 (caddr L)
            Part-NIL (get Last-2 'b)
            Nth-3 (cddr L))

        # Position pointer at start of Last-2
        (mov-cursor (car (get Last-2 'verts)) T)

        # Reuse NIL part so need not regenerate it
        (del-particle Last-2 T NIL)

        # Mov NIL to ptr
        (mov> Part-NIL *pos-cursor)

        (if Nth-3

            # Modify CDR of third last particle
            (with Last-3 (=: b Part-NIL))

            # Replace *part-main with NIL particle
            (setq *part-main Part-NIL))

        # Modify CDR of Last-3
        # Use con to destructively set the CDR
        (when Last-3
          (con (get Last-3 'data)))

        # Data of pointer will always be the symbol itself
        # Set the symbol's value to new end (Last-3)
        # Particles for symbols only contain the symbol itself in Data
        #   A and B are left NIL - only for list items
        (with (car *part-ptrs)
          (set (: data) Last-3))))

    (nl-cursor -1.0)
    (mov> (car *part-ptrs) *pos-cursor)))


(de cmd-make-list (Keysym)

  (prinl-info "cmd-make-list")

  # Produce NIL
  # Use can enter NIL, at which point is converted into "()"

  #{
    Enter list after creation or stay on same level?

    Default is NO so pressing new list repeatedly will create subsequent lists
    rather than nested lists
  }#

  (let Part (gen-part NIL)

    (conc-ptr Part)

    #(setq *lst-ptr Part)

    #(push '*lists-ptr Part)

    T)

  (adv-cursor))


#########################
#########################


(de cmd-make-num (Keysym)
  (let Data (format (char Keysym))
    (conc-ptr (new '(+Particle)
                    Data
                    Data
                    NIL
                    (draw-chars Data
                                (get-color-type Data)
                                T))))
  (adv-cursor))


(de cmd-format (Keysym)

  (prinl-info "cmd-format")

  # Convert num<->str

  (let Last (get (last-ptr) 'data)

    (when (or (str? Last)
              (num? Last))

      (let (Data (format Last)
            Col (get-color-type Data)
            Verts (if (num? Data)
                      (draw-chars Data Col T)
                      (draw-str Data Col T)))

        (conc-ptr (new '(+Particle)
                        Data
                        Data
                        NIL
                        Verts))

        (adv-cursor)))))


(de cmd-pack (Keysym)

  (prinl-info "cmd-pack")

  # Convert to str and push

  # This is special as it builds back until non-str/num
  # Normally, uses last item

  (let Str (pack (get (build-pack) 2))

    # Draw data, create particle, add to timeline
    (conc-ptr
          (new '(+Particle)
               Str
               Str
               NIL
               (draw-str Str (get-color-type Str) T)))

    # Set next as needed

    (adv-cursor)))


(de cmd-pack-2 (Keysym)

  (prinl-info "cmd-pack-2")

  # Convert to str and push

  # Unlike OG, use last item
  # To use this normally, must create list then push chars into that

  (let Str (pack (get (last-ptr) 'data))

    # Draw data, create particle, add to timeline
    (conc-ptr
          (new '(+Particle)
               Str
               Str
               NIL
               (draw-str Str (get-color-type Str) T)))

    # Set next as needed

    (adv-cursor)))


(de cmd-chop (Keysym)

  (prinl-info "cmd-chop")

  (let Last (get (last-ptr) 'data)

    (conc-ptr (gen-part (chop Last))))

  (adv-cursor))


(de cmd-intern (Keysym)

  (prinl-info "cmd-intern")

  # Convert last item (t-sym) with i-sym

  # Ensure last item is a str

  (let Last (get (last-ptr) 'data)

    (when (str? Last)

      (conc-ptr
            (new '(+Particle)
                 (intern Last)
                 (intern Last)
                 NIL
                 (draw-chars Last (get-color-type 'data) T)))

      (adv-cursor))))


(de cmd-eval ()

  (prinl-info "cmd-eval")

  # Eval last item - must be data
  # Else:
  # Str (pack Data)
  # Any (any Str)

  (let (Last (get (last-ptr) 'data)
        Any (eval Last))

    (conc-ptr (gen-part Any)))

  (adv-cursor))


(de cmd-any (Keysym)

  (prinl-info "cmd-any")

  # Str -> Any

  (let (Last (get (last-ptr) 'data)
        Any (any Last))

    (conc-ptr (gen-part Any))

    (adv-cursor)))


(de cmd-sym ()

  (prinl-info "cmd-sym")

  # Rev of Any
  # Any -> Str

  (let (Last (get (last-ptr) 'data)
        Str (sym Last))

    (conc-ptr
          (new '(+Particle)
               Str
               Str
               NIL
               (draw-str Str (get-color-type Str) T))))

  (adv-cursor))
