#{
(cond ((= Keysym ksd~Return) (nl-ptr))
      ((= Keysym ksd~Tab)    (adv-ptr 2.0)))
}#


(de cmd-make-char (Keysym)

  # Enter/Newline should produce character and move pointer

  (let (Data (cond ((= Keysym ksd~space) " ")
                  ((= Keysym ksd~Return) (char 10))
                  (T (char Keysym)))
        List-Data (list Data))

    (println "PART-PTR" *part-root (get *part-root 'data) (get *part-root 'a) (get *part-root 'b))

    (if (get *part-root 'data)

      (prog

        # Will return NIL also
        (let Last (last-part> *part-root)

          (println "LAST" Last (get Last 'data) (get Last 'a) (get Last 'b))

          (unless (get Last 'data)
            (setq Last *part-root))

          # Position at last
          (let (Vert (car (get Last 'verts))
                Pos (get Vert 'pos)
                Ori (get-origin> (get *metrics (get Vert 'chr)) Pos *sca-vert))
            (set-x-ptr-2 (get Ori 1))
            (set-y-ptr-2 (get Ori 2))
            (update-model-matrix> *vert-ptr)
            (cmd-update-vert *ipc *vert-ptr))

          #(nl-ptr)
          (adv-ptr 4.0)

          # Remove NIL particle
          (del-particle (get Last 'b))

          # Gen part for data
          (let Part (gen-pair-x-h List-Data
                                  0.0
                                  (get *vert-ptr 'pos)
                                  gen-pair-x-h
                                  NIL NIL)

            # Connect to last part
            (with Last (=: b Part))))

        # Append data (or use conc?)
        (with *part-root
          (=: data (conc (: data) List-Data))))

      # else replace particle
      (prog

        # Position at particle
        (let (Vert (car (get *part-root 'verts))
              Pos (get Vert 'pos)
              Ori (get-origin> (get *metrics (get Vert 'chr)) Pos *sca-vert))
          (set-x-ptr-2 (get Ori 1))
          (set-y-ptr-2 (get Ori 2))
          (update-model-matrix> *vert-ptr)
          (cmd-update-vert *ipc *vert-ptr))

        (del-particle *part-root T T)

        # Gen part for data
        (setq *part-root (gen-part List-Data
                                  0.0
                                  (get *vert-ptr 'pos)
                                  gen-pair-x-h))

        T))))


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
  }#

  (when (get *part-root 'data)

    (let (L ()
          #D ()
          X *part-root)

      (loop
        (push 'L X)
        #(push 'D (get X 'data))
        # If CDR is NIL, end of list
        (NIL (get X 'b))
        (setq X (get X 'b)))

      # Poss cache L?

      #{
        If list becomes empty, then it simply becomes NIL:

          Particle:
          data = (a b)
          a = Particle:a
          b = Particle:(b)

          ->

          Particle:
          data = NIL
          a = NIL
          b = NIL
      }#

      # Position pointer at start of cadr
      (when (cadr L)
        (let (Vert (car (get (cadr L) 'verts))
              Ori (get-origin> (get *metrics (get Vert 'chr))
                               (get Vert 'pos)
                               *sca-vert))
          (set-x-ptr-2 (get Ori 1))
          (set-y-ptr-2 (get Ori 2))
          (update-model-matrix> *vert-ptr)
          (cmd-update-vert *ipc *vert-ptr)))

      # Delete last particle, incl. NIL
      (del-particle (car L) T T)

      (println L)

      (if (cdr L)

          (prog

            # Modify CDR of second to last particle
            (with (cadr L) (=: b NIL)))

          (prog

            # Replace *part-root with NIL particle
            (setq *part-root (gen-part NIL
                                      0.0
                                      (get *vert-ptr 'pos)
                                      gen-pair-y-h)))))))


(de cmd-make-num (Keysym)
  (let Data (format (char Keysym))
    (conc-ptr (new '(+Particle)
                    Data
                    Data
                    NIL
                    (draw-chars Data
                                (get-color-type Data)
                                T))))
  (adv-ptr))


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

        (adv-ptr)))))


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

    (adv-ptr)))


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

    (adv-ptr)))


(de cmd-chop (Keysym)

  (prinl-info "cmd-chop")

  (let Last (get (last-ptr) 'data)

    (conc-ptr (gen-part (chop Last))))

  (adv-ptr))


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

      (adv-ptr))))


(de cmd-eval ()

  (prinl-info "cmd-eval")

  # Eval last item - must be data
  # Else:
  # Str (pack Data)
  # Any (any Str)

  (let (Last (get (last-ptr) 'data)
        Any (eval Last))

    (conc-ptr (gen-part Any)))

  (adv-ptr))


(de cmd-any (Keysym)

  (prinl-info "cmd-any")

  # Str -> Any

  (let (Last (get (last-ptr) 'data)
        Any (any Last))

    (conc-ptr (gen-part Any))

    (adv-ptr)))


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

  (adv-ptr))


################################################################################


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

  (adv-ptr))
