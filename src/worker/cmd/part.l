(de cmd-make-char (Keysym)

  #{
    Set CDR to new list with single char

    1. Create new part
    2. Set CDR of ref to part
    3. Set ptr value
    4. Move ptr

    Position new particle based on the ref's layout

    ksd~Return
    ksd~Tab
  }#

  (let (Data (when Keysym
               (cond ((= Keysym ksd~space) " ")
                     ((= Keysym ksd~Return) (char 10))
                     (T (char Keysym)))))

    (if (= (get *part-main 'data) '(NIL))

      # repl-any assumes list has y layout
      (let Part (repl-any Data)
        
        # Set new list
        (setq *part-main (get Part 'a))
        
        (put p0 'b (get Part 'a)))

      (let Part (con-any Data 'x)
        
        (put p0 'b Part)
        
        # Pointer always above regardless of layout?
        (mov-part-abv> p0 Part (cons T))))))


(de cmd-make-nil (Keysym)
  (prinl-info "cmd-make-nil")
  (cmd-make-char))


(de cmd-del (Dir)

  (prinl-info "cmd-del")

  #{
    1. Delete Ref
    2. Connect Last-2/Ref-1 to Last/NIL
      * Must search list
        * Currently, must traverse
        * However, when spatially mapped, that may be faster since less
        pointer chasing
      * Cache last pointer change - only valid when moving sequentially
        * Check cmd that produced it
  }#

  (when (get (get *part-main 'b) 'data)

    (let (Ref (val> p0)
          Tgt (get *part-main 'b)
          Prv *part-main
          Nxt NIL)

      # Search until pointer - save previous
      (loop
        (T (or (nil (get Tgt 'b))
               (= Tgt Ref)))
        (setq Prv Tgt
              Tgt (get Tgt 'b)))

      # If Prv is NIL then only 1 item so attach NIL to symbol

      # Save next particle
      (setq Nxt (get Tgt 'b))

      # Position pointer at Tgt before del
      (mov-cur-vert (car (get Tgt 'verts)) T)

      # For lists, del recursively
      (when (get Tgt 'a)
        (del> (get Tgt 'a) T T)
        (put Tgt 'a))
      # Delete target/list itself, except CDR
      (del> Tgt T NIL)

      # Mov Nxt to Tgt
      (when Nxt (mov> Nxt *cur))

      # Connect Prv to Nxt
      (put Prv 'b Nxt)

      # Modify data of Prv
      # Use con to destructively set the CDR
      (con (get Prv 'data) (get Nxt 'data))

      # Point to Prv
      (put p0 'b Prv)
      (mov-part-abv> p0 Prv (cons T)))))


(de cmd-make-num (Keysym)

  # Use cmd-make-char

  # (con-list (format (char Keysym)) 'x)
  
  (println "cmd-make-num"))


(de cmd-format (Keysym)

  # Dat (format (get (get Ref 'a) 'data))
  # Part (con-list Dat 'x))

  (prinl-info "cmd-format"))


(de cmd-pack (Keysym)

  # Dat (pack (get *part-main 'data))
  # Part (con-list Dat 'x))

  (prinl-info "cmd-pack"))


#####################


(de cmd-chop (Keysym)

  (prinl-info "cmd-chop"))


(de cmd-eval ()

  (prinl-info "cmd-eval")

  # Eval last item - must be data
  # Else:
  # Str (pack Data)
  # Any (any Str)

  (let (Last (get (last-ptr) 'data)
        Any (eval Last))

    (conc-ptr (gen-part Any)))

  (adv-cur))


(de cmd-intern (Keysym)

  (prinl-info "cmd-intern")

  (when (get (get *part-main 'b) 'data)

    (let (Ref (val> p0)
          Cdr (get Ref 'b)
          Tgt (get (get Ref 'a) 'data))

      (when (str? Tgt)

        (let Part (con-list (intern Tgt) 'x)

          # Ref will be gen'd also - move it

          # Move new part to cdr-part
          (mov-part> Part Cdr (cons T T))

          # Move cdr-part in front of Part
          (mov-part-aft> Cdr Part (cons T))

          # Set new ptr and move it above
          (put p0 'b Part)
          (mov-part-abv> p0 Part (cons T)))))))


(de cmd-any (Keysym)

  (prinl-info "cmd-any")

  # Str -> Any

  (let (Last (get (last-ptr) 'data)
        Any (any Last))

    (conc-ptr (gen-part Any))

    (adv-cur)))


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

  (adv-cur))
