(def '*mode-repl)

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

    # Push to cmd list
    #(setq *nl-pair NIL)
    #(setq p1 (repl-cdr p1 (glue " " (list (date) (time) "cmd-make-char" Data)) 'y))
    #(setq *nl-pair T)
       
    (con-any Data Lay)))


(de cmd-make-nil (Keysym)
  (prinl-info "cmd-make-nil")
  (con-any NIL 'x))


(de cmd-del (Dir)

  (prinl-info "cmd-del")

  #{  
                p0
    [ ] - [ ] - NIL
    "a"   "b"


          p0
    [ ] - NIL
    "a"   "b"
        
    * Remove b
    * To delete NIL itself, use delete
      * That's why BackSpace is called back-space?
  }#
  
  # Refactor into a fn in ops?
  
  # Similar to adding items, must update CDR and list CDR

  (let (Ref (val> p0)
        Tgt (get Ref 'c)
        Cc (get Tgt 'c))

    (unless (= (get Tgt 'data) '*main)      

      # Preserve Ref (aka NIL) so only delete Car
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

          (base-cur Cc)
          (mov> Tgt *cur))
        
        (prog
          (put Cc 'b Ref)
          
          # Move Ref
          (base-cur Cc)
          (mov> Ref *cur)))

      (put Ref 'c Cc)

      # Reposition Ref to Tgt, recursively
      #(mov-cur-part Tgt 'start)
      #(lay-part Ref *cur)
      (let Base (get-origin> (get *main 'b))
        (mov-cur-part Tgt 'start)
        (lay-part Ref Base))

      # Ptr same; simply move it
      (mov-part-abv> p0 Ref (cons T)))))


(de cmd-make-num (Keysym)

  (println "cmd-make-num")
  
  (con-any (format (char Keysym)) 'x))


(de cmd-format (Keysym)

  (prinl-info "cmd-format")
  
  (con-any (format (get (get (get (val> p0) 'c) 'a) 'data)) 'x))


(de cmd-pack (Keysym)
  
  (prinl-info "cmd-pack")

  # Pack current to prev
  
  (let (Ref (val> p0)
        Prv (get Ref 'c)
        Dat-Prv (get (get Prv 'a) 'data)
        Cc (get Prv 'c)
        Dat-Cc (get (get Cc 'a) 'data))
        
    # (get (get *main 'b) 'data)

    (con-any (pack Dat-Cc Dat-Prv) 'x)))


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

  (when (get (get *main 'b) 'data)

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
