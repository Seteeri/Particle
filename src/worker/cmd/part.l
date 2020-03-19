(de cmd-make-char (Keysym)

  #{
    # Set CDR to new list with single char
    
    1. Create new part
    2. Set CDR of ref to part
    3. Set ptr value
    4. Move ptr    
    
    If pointing to a new list, layout is based on the type of list,
    which in this case, is strings, so x layout
    
    ksd~Return
    ksd~Tab
  }#
  
  (let (Ref (val> p0)
        Dat-Ref (car (get Ref 'data))
        Data (cond ((= Keysym ksd~space) " ")
                   ((= Keysym ksd~Return) (char 10))
                   (T (char Keysym))))

    # When entering empty list (NIL)
    # ensure new particle is positioned below
    # If not empty, particle will already be in position

    (if (= (get *part-main 'data) '(NIL))
      
      #{
            p0
      [] -> [] -> NIL
      a     NIL

            p0
      []  
      
            p0
      [] -> [] -> NIL
      a     |
            b        
            
      # Replace entire (NIL) with ("")
      }#
      
      (let (Last (let Last (car *part-lsts)
                    (loop
                      (NIL (get (get (get Last 'b) 'b) 'b))
                      (setq Last (get Last 'b)))
                    Last)
            Part (repl-list Data Last))
         
        # Mov cursor below
        (mov-cur-part Last 'end) (adv-cur 2.0)
         
        (put Part 'lay 'x)
        #(put (get Part 'b) 'lay 'x)
        (lay-part Part *cur)
        
        # Move inner list one space further down
        (mov-cur-part Last 'end) (adv-cur 2.0)
        (nl-cur 3.0)
        (put (get Part 'a) 'lay 'x)
        (lay-part (get Part 'a) *cur)
        
        (put p0 'b (get Part 'a)))
      
      
      (let Part (con-list Data)
                
        (if (get Ref 'a)
          (prog
            (mov-cur-part (get Ref 'a) 'end)
            (nl-cur -1.0))
          (prog
            (mov-cur-part Ref 'end)))
        (adv-cur 2.0)
                
        (lay-part Part *cur)
        
        # Pointer always above regardless of layout
        (put p0 'b Part)
        (mov-part-abv> p0 Part (cons T))))))


(de cmd-make-nil (Keysym)

  (prinl-info "cmd-make-nil")

  (let (Ref (val> p0)
        Part (con-list))
    
    (if (get Ref 'a)
      (prog
        (mov-cur-part (get Ref 'a) 'end)
        (nl-cur -1.0))
      (prog
        (mov-cur-part Ref 'end)))
    (adv-cur 2.0)
    
    (lay-part Part *cur)

    # Set new ptr and move it above
    (put p0 'b Part)
    (mov-part-abv> p0 Part (cons T))))


(de cmd-enter-list (Keysym)

  (prinl-info "cmd-enter-list")
    
  (let (Ref (val> p0)
        Data (car (get Ref 'data)))
  
    # Check pointer is a list; watch for cons == list          
    (when (or (pair Data)
              (== Data NIL))
                    
      # Push Part to history
      (push '*part-lsts *part-main)
      
      # DEPENDS ON LAYOUT!
      # Default is to push strings
      
      # Move cursor/pointer underneath list
      (nl-cur 3.0) (mov> p0 *cur)
    
      # Do not change pointer data - will push to it
      
      # Set main particle
      (setq *part-main Ref))))
  
  
(de cmd-leave-list (Keysym)

  (prinl-info "cmd-leave-list")

  # Set main particle to old one
  (setq *part-main (pop '*part-lsts))
  
  (let? Last-1 (last-1> *part-main)
  
    # Set ptr to last item (non-NIL)
    (put p0 'b Last-1)
    (mov-part-abv> p0 Last-1)
    
    # Move cursor
    (mov-cur-part Last-1)))


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
  
  # Same as cmd-make-char

  (let (Ref (val> p0)
        Cdr (get Ref 'b)
        Part (con-list (format (char Keysym))))
          
    (mov-cur-part (get Ref 'a) 'end) (nl-cur -1.0) (adv-cur 2.0)
    (lay-part Part *cur)
    
    # Pointer always above regardless of layout
    (put p0 'b Part)
    (mov-part-abv> p0 Part (cons T))))


(de cmd-format (Keysym)

  (prinl-info "cmd-format")

  # Convert num<->str

  (when (get (get *part-main 'b) 'data)
  
    (let (Ref (val> p0)
          Cdr (get Ref 'b)
          Dat (format (get (get Ref 'a) 'data))
          Part (con-list Dat))
      
      (mov-cur-part (get Ref 'a) 'end) (nl-cur -1.0) (adv-cur 2.0)
      (lay-part Part *cur)
      
      # Pointer always above regardless of layout
      (put p0 'b Part)
      (mov-part-abv> p0 Part (cons T)))))



(de cmd-pack (Keysym)

  (prinl-info "cmd-pack")

  # Until doubly-linked list,
  # Build up list

  (let (Ref (val> p0)
        Cdr (get Ref 'b)
        Dat (pack (get *part-main 'data))
        Part (con-list Dat))
    
    (mov-cur-part (get Ref 'a) 'end) (nl-cur -1.0) (adv-cur 2.0)
    (lay-part Part *cur)
    
    # Pointer always above regardless of layout
    (put p0 'b Part)
    (mov-part-abv> p0 Part (cons T)))
    
  (adv-cur 2.0))


#####################


(de cmd-chop (Keysym)

  (prinl-info "cmd-chop")

  (when (get (get *part-main 'b) 'data)
  
    (let (Ref (val> p0)
          Cdr (get Ref 'b)
          Dat (chop (get (get Ref 'a) 'data))          
          Part (con-list Dat))
      
      # Move new part to cdr-part
      (mov-part> Part Cdr (cons T))
      
      # Move cdr-part in front of Part
      (mov-part-aft> Cdr Part (cons T))
      
      # Set new ptr and move it above
      (put p0 'b Part)
      (mov-part-abv> p0 Part (cons T)))))


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
        
        (let Part (con-list (intern Tgt))
        
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
