# Methods of ptr
# Need not implement other commands

(de car-ptr ()
  (with *part-ptr
    (car (: a))))


(de last-ptr ()
  (with *part-ptr
    (last (: a))))
    

# Head
(de push-ptr (Part)
  (with *part-ptr
    (push (:: data) (get Part 'data))
    (push (:: a) Part))
    # Connect first item to head
    (cons> Part (cadr (: a))))


# Tail
(de conc-ptr (Part)
  (with *part-ptr
    (let Last-A (last (: a))
      (if Last-A
        (prog
          (conc (: data) (cons (get Part 'data)))
          (conc (: a) (cons Part))
          # Connect last item to tail
          (cons> Last-A Part))
        (prog
          (=: data (cons (get Part 'data)))
          (=: a (cons Part)))))))
    

(de pop-ptr ()

  (let Item (get *lst-ptr 'next)

    (with *lst-ptr

      # Pop actual data
      (pop (:: data))

      # Set list next to next's next Particle
      (=: next (get Item 'next)))

    Item))


##########################

#{
Data (list 1
           (list 2 6 7 8)
           3
           (list 4 (list 9 12))
           (list 5 10 11))
Data (list 1
           (list 2 3 (list 4) 5)
           (list 6 (list 7 (list 8))))
Data (list 1
           (list 2 3 4)
           (list 5 6 7)
           8
           (list 9 (list 10 (list 11))))

    #(push-ptr Part)

    #(cond ((= Keysym ksd~Return) (nl-ptr))
    #      ((= Keysym ksd~Tab)    (adv-ptr 2.0)))
}#


(de cmd-make-char (Keysym)

  # Enter/Newline should produce character and move pointer

  (let (Data (cond ((= Keysym ksd~space) " ")
                  ((= Keysym ksd~Return) (char 10))
                  (T (char Keysym)))
        List-Data (list Data))
                   
    (with *part-ptr
    
      # Remove NIL particle
      # - Traverse *part-ptr (current list) to get last particle
      #   or store ref to last cdr in particle
      (let C (: b)
        (while C
          (println C (get C 'data) (get C 'a) (get C 'b))
          (setq C (get C 'b))
          (call 'sleep 2)))

      # Gen part for data
      (let Part (gen-pair-y-h List-Data
                              0.0
                              (get *vert-ptr 'pos)
                              gen-pair-y-h
                              NIL NIL)
        
        # Connect part
        # - Get last item in List
        # - Set its B/CDR to new part
                              
        Part)
    
      # Append data (or use conc?)
      (=: data (append (: data) List-Data))
      (println (: data)))))


(de cmd-make-char-2 (Keysym)

  # Enter/Newline should produce character and move pointer

  (let ((X Y) (copy (get *vert-ptr 'pos)))

    (let (Data (cond ((= Keysym ksd~space) " ")
                     ((= Keysym ksd~Return) (char 10))
                     (T (char Keysym)))
          Part (let ((X Y) (get *vert-ptr 'pos))
                  (gen-part Data 0.0 X)))

      # Before drawing, align pointer to head of car of list
      
      # Gen particle
      # Push/cons particle to ptr
      #   Draw cons

      #(view (getl *part-ptr))
      
      (conc-ptr Part)

      (with *part-ptr
        (println (: a)))

      (adv-ptr)

      (when NIL
        (set-x-ptr-2 X)
        (set-y-ptr-2 Y)
        (update-model-matrix> *vert-ptr)
        (cmd-update-vert *ipc *vert-ptr))

      )))


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
  # frag will be discarded...fix later

  # Process:
  # 1. Pop Part from timeline/ptr
  # 2. For Verts: send zero-vert cmd to render
  # 3. For Verts: push Verts

  # MODIFY COUNTER ALSO

  #####################

  # Always assumed *part-ptr is a particle with a list - root particle
  # or call it god particle ;)

  (with *part-ptr
    (let? Part (last (: a))
      (=: a (head -1 (: a)))
      (=: data (head -1 (: data)))
            
      (del-particle Part)
      (with (car (get Part 'verts))
        (set-x-ptr-2 (get (: pos) 1)))
        #(set-y-ptr-2 (get (: pos) 2))
      (update-model-matrix> *vert-ptr)
      (cmd-update-vert *ipc *vert-ptr))))


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
