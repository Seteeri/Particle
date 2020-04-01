(de cmd-reset (Keysm)

  (println 'cmd-reset)

  # Clear ptr
  (put *0 'b)

  (println 'cmd-reset *master (get *master 'data))
  (del> *master T T)

  (setq *main NIL)
  (setq *main (gen-part '*main 'x))
  (with *main
    (=: nl (new '(+Line) NIL NIL (get (: a) 'dims))))
  (setq *cur (copy *pos-main))
  (lay-part-start *main *cur)
  (setq *line *main)
  (inc-line-by-part-x> *line (get *main 'b))

  # Repos ptr
  (put *0 'b (get *main 'b))
  (lay-part-start *0 *cur 'skip-cdr)
  (mov-part-abv> *0 (get *main 'b) (cons T))

  (gc))


(de cmd-make-char (Keysym)

  #{
    If pointing to Car, sublist
    If pointing to Cell/Cdr, append behind

    ksd~Return
    ksd~Tab
  }#

  (let (Data (when Keysym
               (cond ((= Keysym ksd~space) " ")
                     ((= Keysym ksd~Return) (char 10))
                     (T (char Keysym))))
        Lay 'x)

    # Push to cmd list
    #(setq p1 (repl-cdr p1 (glue " " (list (date) (time) "cmd-make-char" Data)) 'y))

    # Handle symbol

    (con-any Data Lay)))


(de cmd-make-nil (Keysym)
  (prinl-info "cmd-make-nil")
  (con-any NIL 'x))


(de cmd-del (Dir)

  (prinl-info "cmd-del")

  #{

                *0
    [ ] - [ ] - NIL
    "a"   "b"


          *0
    [ ] - NIL
    "a"   "b"

    OR

          *0
    [ ] - [ ] - [ ] - NIL
    "a"   "b"   "c"


    *0
    [ ] - [ ] - NIL
    "b"   "c"

    * Ref does not get deleted - its prv does
  }#

  # Refactor into a fn in ops?

  # Similar to adding items, must update CDR and list CDR

  (let (Ref (get *0 'b)
        Tgt (get Ref 'c)
        Cc (get Tgt 'c))

    (unless (= (get Tgt 'data) '*main)

      # Preserve Ref so only delete Car
      # Must manually do this since the flags are passed down
      (when (get Tgt 'a)
        (del> (get Tgt 'a) T T))
      (del> Tgt NIL NIL)
      (put Tgt 'b)

      # Delete Tgt
      # Connect Cc to Ref

      # Link Cc->Ref; con to destructively set Ref
      (unless (= (get Cc 'data) '*main)
        (con (get Cc 'data) (get Ref 'data)))

      #(if (isa '+Pair (get Cc 'a))
      (if (= (get Cc 'a) Tgt)

        # When deleting last item in a sublist
        # Make the Ref the Car of Cc
        # Move Ref under Cc
        (prog
          (put Cc 'a Ref)
          (mov-cur-part Cc 'start) (nl-cur)
          (mov> Ref *cur)

          # Move Cc Ref also
          # This will not move recursively
          # Use lay-part BUT lay-atom must be fixed

          (put Cc 'b Tgt)

          (align-cur-to-bnds Cc)
          (mov> Tgt *cur))

        (prog
          (put Cc 'b Ref)

          # Move Ref
          (align-cur-to-bnds Cc)
          (mov> Ref *cur)))

      (put Ref 'c Cc)

      # Mov Ref to Tgt

      # Base is origin of Pair containing Ref/Part
      # Cur is where Part should be - use Ref origin
      (mov-cur-part Tgt 'start)
      (lay-part-start Ref (get *main 'ori) 'skip-cdr)

      # Do until nl also,
      # however,
      # poss move items back to maintain line length
      # but this means update everthing
      (adj-line-to-part> *line Ref)

      # Layout remainder
      (align-cur-to-bnds Ref)
      (lay-part-start (get Ref 'b) (get *main 'ori))

      # update superlists etc

      # Ptr same; simply move it
      (mov-part-abv> *0 Ref (cons T)))))


(de cmd-make-num (Keysym)

  (println "cmd-make-num")

  (con-any (format (char Keysym)) 'x))


(de cmd-format (Keysym)

  (prinl-info "cmd-format")

  (con-any (format (get *0 'b 'c 'a 'data)) 'x))


(de cmd-pack (Keysym)

  (prinl-info "cmd-pack")

  # Pack current to prev

  (let (Ref (get *0 'b)
        Prv (get Ref 'c)
        Dat-Prv (get Prv 'a 'data)
        Cc (get Prv 'c)
        Dat-Cc (get Cc 'a 'data))

    (con-any (pack Dat-Cc Dat-Prv) 'x)))


(de cmd-swap-layout (Keysym)
  (println "Swap layout")

  # Note this causes bounds change also

  # If NIL, get prev, else do ref

  (let (Ref (get *0 'b)
        Prv (get Ref 'c)
        Tgt (if (= (get Ref 'data) NIL) Prv Ref))
    (when Tgt
      (println "Swap!" Tgt (get Tgt 'data) (get Tgt 'lay) '-> (if (= (get Tgt 'lay) 'x) 'y 'x))
      (swap-lay Tgt))))


(de cmd-mov-nl (Keysym)
  (println "Move newline")

  (let Ref (get *0 'b)

    # If already newline, don't do anything
    # TODO: Add support for this later? number of spaces...
    #(unless (get Ref 'nl))

    (mov-nl Ref)))


(de cmd-mov-bl (Keysym)

  # This moves in front of prev

  (let (Ref (get *0 'b)
        Line (get Ref 'nl)
        Prv-Line (get Line 'p)
        Nxt-Line (get Line 'n))

      # Make this a fn
      # Unlink lines

      (when Prv-Line
        (with (get Prv-Line 'nl)
          (=: n Nxt-Line)))

      (when Nxt-Line
        (with (get Nxt-Line 'nl)
          (=: p Prv-Line)))

      (setq *line Prv-Line)

      (put Ref 'nl NIL)

      (align-cur-to-bnds (get Ref 'c))
      (lay-part-start Ref *cur)

      (put *0 'b Car Ref)
      (mov-part-abv> *0 Ref (cons T))))


#####################

(de cmd-chop (Keysym) (prinl-info "cmd-chop"))
(de cmd-eval () (prinl-info "cmd-eval"))
(de cmd-intern (Keysym) (prinl-info "Use (intern <data>)"))
(de cmd-any (Keysym) (prinl-info "Use (any ...); str -> any"))
(de cmd-sym () (prinl-info "Use (sym ...); rev of (any ...)"))
