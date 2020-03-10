(de cmd-make-char (Keysym)

  #{
    # Append to end (cur-part) aka insert before NIL
    
    # Insert new part under cursor
    
    1. Create new part
    2. Link cur-part to new-part
    3. Link new-part to cur-part
    
    1. Move new part to cur-part
    2. Move cur-part fwd
    
    1. Set ptr value
    2. Move ptr    
  }#

  (if (car (get *part-main 'data))

    (let (Part-Ptr (val> p0)
          Cdr (get Part-Ptr 'b)
          Part (append-any
                  (cond ((= Keysym ksd~space) " ")
                        ((= Keysym ksd~Return) (char 10))
                        (T (char Keysym)))))
                        
      # Move new part to cdr-part
      (mov-part> Part Cdr (cons T))
      
      # Move cdr-part in front of Part
      (mov-part-aft> Cdr Part (cons T))
      
      # Point to new Part and mov it
      (put p0 'b Part)
      (mov-part-abv> p0 Part (cons T)))
  
    (let (Last (let Last (car *part-lsts)
                 (loop
                   # Stop at second to last
                   (NIL (get (get (get Last 'b) 'b) 'b))
                   (setq Last (get Last 'b)))
                 Last)
          Part (repl-part
                  (cond ((= Keysym ksd~space) " ")
                        ((= Keysym ksd~Return) (char 10))
                        (T (char Keysym)))
                  Last))

      # Move new part to under ptr
      #(mov-part-bel> Part p0 (cons T T))
      
      # Point to new Part
      (put p0 'b (get Part 'a))))
  
  # Enter/Newline should produce char + mov ptr/cur
  (cond ((= Keysym ksd~Return) T)
        ((= Keysym ksd~Tab)    T)
        (T T)))


(de cmd-make-nil (Keysym)

  (prinl-info "cmd-make-nil")

  (let (Cdr (get (val> p0) 'b)
        Part (append-any)) # Need dif fn?
    
    # Move new part to cdr-part
    (mov-part> Part Cdr (cons T))
    
    # Move cdr-part in front of Part
    (mov-part-aft> Cdr Part (cons T))
    
    # Set new ptr and move it above
    (put p0 'b Part)
    (mov-part-abv> p0 Part (cons T))))


(de cmd-enter-list (Keysym)

  (prinl-info "cmd-enter-list")
    
  (let (Val (val> p0)
        Data (car (get Val 'data)))
  
    # Check pointer is a list; watch for cons == list          
    (when (or (pair Data)
              (== Data NIL))
                    
      # Push Part to history
      (push '*part-lsts *part-main)
      
      # Move cursor/pointer underneath list
      (nl-cursor 3.0) (mov> p0 *pos-cursor)
    
      # Do not change pointer data - will push to it
      
      # Set main particle
      (setq *part-main Val))))
  
  
(de cmd-leave-list (Keysym)

  (prinl-info "cmd-leave-list")

  # Set main particle to old one
  (setq *part-main (pop '*part-lsts))
  
  (let Last-1 (last-1> *part-main)
    # Set ptr to last item (non-NIL)
    (put p0 'b Last-1)
    (mov-part-abv> p0 Last-1)))


(de cmd-del (Dir)

  (prinl-info "cmd-del")

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
        
    a b c NIL
    
    a b [] NIL
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
        (setq Prv Tgt
              Tgt (get Tgt 'b)))
      
      # If Prv is NIL then only 1 item so attach NIL to symbol
      
      # Save next particle
      (setq Nxt (get Tgt 'b))

      # Position pointer at Tgt before del
      (mov-cursor (car (get Tgt 'verts)) T)

      # For lists, del recursively
      (when (get Tgt 'a)
        (del> (get Tgt 'a) T T)
        (put Tgt 'a))
      # Delete target/list itself, except CDR
      (del> Tgt T NIL)

      # Mov Nxt to Tgt
      (when Nxt (mov> Nxt *pos-cursor))

      # Connect Prv to Nxt
      (put Prv 'b Nxt)

      # Modify data of Prv
      # Use con to destructively set the CDR
      (con (get Prv 'data) (get Nxt 'data))
      
      # Point to Prv
      (put p0 'b Prv)
      (mov-part-abv> p0 Prv (cons T)))))


(de cmd-make-num (Keysym)
  (let (Cdr (get (val> p0) 'b)
        Part (append-any (format (char Keysym))))
    
    # Move new part to cdr-part
    (mov-part> Part Cdr (cons T))
    
    # Move cdr-part in front of Part
    (mov-part-aft> Cdr Part (cons T))
    
    # Set new ptr and move it above
    (put p0 'b Part)
    (mov-part-abv> p0 Part (cons T))))


(de cmd-format (Keysym)

  (prinl-info "cmd-format")

  # Convert num<->str

  (when (get (get *part-main 'b) 'data)
  
    (let (Val (val> p0)
          Cdr (get Val 'b)
          Dat (format (get (get Val 'a) 'data))          
          Part (append-any Dat))
      
      # Move new part to cdr-part
      (mov-part> Part Cdr (cons T))
      
      # Move cdr-part in front of Part
      (mov-part-aft> Cdr Part (cons T))
      
      # Set new ptr and move it above
      (put p0 'b Part)
      (mov-part-abv> p0 Part (cons T)))))


(de cmd-chop (Keysym)

  (prinl-info "cmd-chop")

  (when (get (get *part-main 'b) 'data)
  
    (let (Val (val> p0)
          Cdr (get Val 'b)
          Dat (chop (get (get Val 'a) 'data))          
          Part (append-any Dat))
      
      # Move new part to cdr-part
      (mov-part> Part Cdr (cons T))
      
      # Move cdr-part in front of Part
      (mov-part-aft> Cdr Part (cons T))
      
      # Set new ptr and move it above
      (put p0 'b Part)
      (mov-part-abv> p0 Part (cons T)))))


(de cmd-intern (Keysym)

  (prinl-info "cmd-intern")

  (when (get (get *part-main 'b) 'data)
  
    (let (Val (val> p0)
          Cdr (get Val 'b)
          Tgt (get (get Val 'a) 'data))
          
      (when (str? Tgt)

        (let Part (append-any (intern Tgt))
        
          # Val will be gen'd also - move it
        
          # Move new part to cdr-part
          (mov-part> Part Cdr (cons T T))
          
          # Move cdr-part in front of Part
          (mov-part-aft> Cdr Part (cons T))
          
          # Set new ptr and move it above
          (put p0 'b Part)
          (mov-part-abv> p0 Part (cons T)))))))


#########################
#########################


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
