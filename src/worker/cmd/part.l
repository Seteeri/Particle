(def '*mode-car)

(de cmd-make-char (Keysym)

  #{
    Set CDR to new list with single char

    1. Create new part
    2. Set CDR of ref to part
    3. Set ptr value
    4. Move ptr
    5. Update subsequent items
    6. Update superlist subsequent items

    ksd~Return
    ksd~Tab
  }#

  (let (Data (when Keysym
               (cond ((= Keysym ksd~space) " ")
                     ((= Keysym ksd~Return) (char 10))
                     (T (char Keysym))))
        Lay 'x)

    (if *mode-car

      # repl-any assumes list has y layout
      (let Part (repl-ref Data Lay)
        
        (put p0 'b (get Part 'a))
                
        # Set new list
        (setq *part-main (get Part 'a))
        
        (setq *mode-car NIL))

      (let Part (repl-cdr Data Lay)
        
        # Pointer always above regardless of layout?        
        (put p0 'b Part)
        (mov-part-abv> p0 Part (cons T))))))


(de cmd-make-nil (Keysym)
  (prinl-info "cmd-make-nil")
  (cmd-make-char))


(de cmd-del (Dir)

  (prinl-info "cmd-del")

  #{
    1. Delete Ref
    2. Connect Prv to Nxt
  }#

  (when (get (get *part-main 'b) 'data)

    (let (Ref (val> p0)
          Prv (get Ref 'c)
          Nxt (get Ref 'b))

      # For lists, del recursively
      (when (get Ref 'a)
        (del> (get Ref 'a) T T)
        (put Ref 'a))
      # Delete Ref itself, except CDR
      (del> Ref T NIL)

      # Position pointer at Ref before del
      (mov-cur-part Ref 'start)

      # Mov Nxt to Ref
      (mov> Nxt *cur)

      # Link ptrs
      (put Prv 'b Nxt)
      (put Nxt 'c Prv)

      # Link data
      # Use con to destructively set the CDR
      (unless (= Prv *part-main)
        (con (get Prv 'data) (get Nxt 'data)))

      # Link p0
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
