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
  # frag will be discarded

  # Process:
  # 1. Pop Prt from timeline/ptr
  # 2. For Verts: send zero-vert cmd to render
  # 3. For Verts: push Verts

  # Note, this does not necessarily delete all the vertices as it dels PARTICLES

  (let? Prt (pop '*timeline)
  
    (let (Sock-Render (ipc~get-fd> *ipc "RENDER")
          Verts (get Prt 'verts))

      #(println (get Prt 'data) Verts (length Verts))

      (for Vert Verts
        (let Msg (pack "(zv " (get Vert 'off-gl) '")")
          (send-msg *ipc Sock-Render Msg))
        (push '*verts Vert)
        # For now, push pen back
        (adv-ptr -1.0)))))


(de cmd-eval ()

  (println "cmd-eval")

  # Build string from ptr-prev to last newline

  # Build string from a range...

  # Also what to do if objects are not strs?
  # Eval verts->data (build list from verts)
  # Eval verts->strings->data (build string from verts)
  #
  # -> Do pack->any->push
  # Else use must do this manually

  # Read-string will flatten data/verts
  #
  # (1 "1") (2 "2") (3 "3")
  # (1 2 3)
  # pack (1 2 3) -> "123"

  (let ((Verts Data) (read-string-end 10)
        Str (pack Data)
        Any (any Str)
        Res (eval Any)
        Col (get-color-type Res))

      #(prinl-info "cmd-eval" (pack "STR=" Str))
      #(prinl-info "cmd-eval" (pack "RES=" Res))
      #(println Res)

      #(nl-ptr)
      (adv-ptr 1.0)

      (push '*timeline (new '(+Particle)
                            Res
                            (draw-str (if (num? Res) (format Res) (format (adr Res)))
                                      Col)))

      (adv-ptr 1.0)))


(de cmd-make-char (Keysym)

  # For strings either use quotations or put additional spacing between symbols
  # Problem is spaces are invisible
  # (draw-str (pack "\"" Data "\""))

  (let (Data  (cond ((= Keysym ksd~space) " ")
                    ((= Keysym ksd~Return) (char 10))
                    (T (char Keysym)))
        Verts (draw-str Data (get-color-type Data))
        Prt   (new '(+Particle) Data Verts))

    (push '*timeline Prt))

  #(for X *timeline (print (get X 'data))) (prinl)

  (cond ((= Keysym ksd~Return) (nl-ptr))
        ((= Keysym ksd~Tab)    (adv-ptr 2.0))))


(de cmd-pack (Keysym)

  (prinl-info "cmd-pack")

  # Read until non-str
  # No double-quotes

  (let ((Verts Chars) (read-string-2))

    (when Chars

      (let Data (pack Chars)

        (println Data)

        # Adv ptr before drawing
        (adv-ptr 1.0)

        # Draw data, create particle, add to timeline
        (push '*timeline (new '(+Particle) Data (draw-str Data (get-color-type Data))))))))


(de cmd-intern (Keysym)

  # Read last item
  
  (let Data (get (car *timeline) 'data)

    (when (str? Data)

      (intern-2 Data))))


(de cmd-intern-2 (Keysym)

  # Pack then Intern
  # Same as cmd-pack except result not added to timeline
  (let ((Verts Chars) (read-string-2))
  
    (when Chars
    
      (intern-2 (pack Chars)))))


(de cmd-make-num (Keysym)

  (let (Data  (format (char Keysym))
        Verts (draw-str Data (get-color-type Data))
        Prt   (new '(+Particle) Data Verts))

    (push '*timeline Prt)))


(de cmd-format (Keysym)

  # If given a list, format all nums in list
  # (1 2 3)
  # or
  # ("1" "2" "3")

  #(for Prt *timeline
  #  (with Prt
  #    (println (: data) (: verts)))))

  T)


(de cmd-make-cons (Keysym)

  # Make cons from last two objects

  # A, B, C, D
  # -> A, B, (C . D)
  # -> A, (B . (C . D))
  # -> (A . (B . (C . D)))

  # Or

  # To build:
  #
  # (de add (A B) (+ a b))

  # de -> cons(de)
  # (de . NIL)
  # add -> cons(add, (de . NIL))
  # (add . (de . NIL))

  (println (get (car *timeline) 'data))
  
  T)