(de cmd-make-char (Keysym)
  #{
    Append char to a list, er symbol containing list
  
    Procedure:
    1. Get val of p0
      * Initially Val is *part-main
      * So Val is either symbol itself or second to last (before NIL)
        which in either case, the cdr will be NIL      
    2. Get Last (always NIL) and Last-2
    3. Mov cursor to NIL
    4. Generate new particle
    5. Connect new particle to Last and Last-2
       Last-2 -> New -> Last (NIL)
  }#   

  # Get value of p0
  # Val is Last-1
  # Nil is Last
  (let (Val (val> p0)
        Nil (get Val 'b))
  
    #(println "Val'data" (get Val 'data))
    #(println "Nil'data" (get Nil 'data))
  
    # Mov cursor to NIL first vert pos
    (mov-cursor (car (get Nil 'verts)) T)

    (let Data (list (cond ((= Keysym ksd~space) " ")
                          ((= Keysym ksd~Return) (char 10))
                          (T (char Keysym))))

      # Gen part for new data; incl CAR/CDR
      (let Part (gen-pair-x-h Data
                              0.0
                              *pos-cursor
                              gen-pair-x-h
                              NIL T)

        # Connect NIL
        (with Part (=: b Nil))
        # Reposition NIL in front of new Part
        (mov-cursor (last (get Part 'verts)) T)
        (adv-cursor 2.0)
        (mov> Nil *pos-cursor)

        # Connect prev last to new Particle
        (with Val (=: b Part))
        
        # Point to new Part
        (put p0 'b Part)

        # TODO: Position p0 over new item
        (nl-cursor -1.0)
        (adv-cursor -4.0) # Pos over last item (not NIL)
        (mov> p0 *pos-cursor))

      # Currently assume this list doesn't change
      # But upon list change, must update that specific list
      (with *part-main
        # Connect data; aka modify CDR of Val
        (=: data (conc (: data) Data)))))

  # Enter/Newline should produce char + mov ptr/cur
  (cond ((= Keysym ksd~Return) T)
        ((= Keysym ksd~Tab)    T)
        (T T)))


(de cmd-del (Dir)

  (prinl-info "cmd-del")

  #{
    Currently, render will attempt to render all verts (*max-verts)
    -> Send cmd to render to adjust max
    - instanceCount is number of instances from beginning of array
    *verts is LIFO, thus will always keep instances pack from beginning of array
    -> Maintain vert cnt, instanceCount = size - used

    However, if del from arbitrary point then must zero it
    and instanceCount remains the same
    After del, vert returns to queue
    and next vert will reuse it

    Makes most sense to simply zero vert
    and not worry about instanceCount 
    as frag will be discarded...fix later
  }#

  #{
    Procedure:
    1. Delete Val
    2. Connect Last-2/Val-1 to Last/NIL
      * Must search list
        * Currently, must traverse
        * However, when spatially mapped, that may be faster since less
        pointer chasing
      * Cache last pointer change - only valid when moving sequentially
        * Check cmd that produced it
  }#

  (when (get (get *part-main 'b) 'data)

    (let (Val (val> p0)
          Tgt (get *part-main 'b)
          Prv *part-main
          Nxt NIL)

      # Search until pointer - save previous
      (loop        
        (T (or (nil (get Tgt 'b))
                (= Tgt Val)))
        (setq Prv Tgt)
        (setq Tgt (get Tgt 'b)))
      
      # If Prv is NIL then only 1 item so attach NIL to symbol
      
      # Save next
      (setq Nxt (get Tgt 'b))

      # Position pointer at Tgt before del
      (mov-cursor (car (get Tgt 'verts)) T)

      # No del CDR
      (del-particle Tgt T NIL)

      # Mov Nxt to Tgt
      (when Nxt (mov> Nxt *pos-cursor))

      # Connect Prv to Nxt
      (with Prv (=: b Nxt))

      # Modify data/CDR of Prv
      # Use con to destructively set the CDR
      (with Prv
        (con (: data) (get Nxt 'data)))
      
      # Point to Prv
      (put p0 'b Prv)

      # TODO: Use relative to pos of Prv particle...
      (nl-cursor -1.0)
      (adv-cursor -4.0)
      (mov> p0 *pos-cursor))))


(de cmd-make-nil (Keysym)

  (prinl-info "cmd-make-nil")

  T)


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
