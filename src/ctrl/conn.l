(de handle-keyboard (Sock Data)
  (let (Key   (get Data 4)
        State (get Data 5))
    (when (= State 1)
      (cond ((= Key 106)
             # Move camera right
             (move-cam Sock 1 '(1.0 0.0)))
            ((= Key 105)
             # Move camera left
             (move-cam Sock 1 '(-1.0 0.0)))
            ((= Key 103)
             # Move camera up
             (move-cam Sock 2 '(0.0 1.0)))
            ((= Key 108)
             # Move camera dn
             (move-cam Sock 2 '(0.0 -1.0)))

            #(= Key 30) # a
            (T
             (handle-a Key))))))


(de handle-a (Key)

  # Create vertex; data = char
  (let (Data (cons)
        I 1
        Sock-Model (ipc~get-fd> *ipc "MODEL")
        X 0.0
        Off (*/ (*/ *adv-glyph *sca-glyph 1.0) *sca-vert 1.0))

    # Get CAR/CDR ptrs/bytes
    #(println (struct (>> -4 (adr Data)) '((B . 8) (B . 8))))

    (for N (chop (format (adr Data)))
      (let (Off-Vert (get-vert-off I)
            Vert (req-recv-obj *ipc
                               Sock-Model
                               (new '(+Vertex))
                               SZ-VERT
                               Off-Vert))

        # Reset vertex
        # Pos = baseline = X,0,0
        (with Vert
          (=: pos (list X 0.0 0.0)))
         
        (update-glyph> Vert N)         
         
        (setq X (+ X Off))
                                 
        (update-model-matrix> Vert)
        
        (update-vert *ipc Vert Off-Vert)
 
        (req-send-obj *ipc
                      Sock-Model
                      Vert
                      SZ-VERT
                      Off-Vert)
 
        (when NIL
          (with Vert 
            (println N (format (get (: pos) 1) *Scl)
                       (format (get (: pos) 2) *Scl)
                       (format (get (: pos) 3) *Scl))))
        
        (inc 'I 1)

        T))))


(de move-cam (Sock Cnt Move)
(when T
   # Pull data from model
   (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
         Sock-Render (ipc~get-fd> *ipc "RENDER"))

     (with *projview

      (=: pos (list (+ (get (: pos) 1) (get Move 1))
                    (+ (get (: pos) 2) (get Move 2))
                    10.0))

      (update-mat-view> *projview)

      # Either send relevant data or send entire data
      # -> Minimize render processing time

      # Pass list of socks so need only serialize once

      (req-send-obj *ipc
                    Sock-Render
                    *projview
                    SZ-PV
                    0)

      # Push to model also
      (req-send-obj *ipc
                    Sock-Model
                    *projview
                    SZ-PV
                    0))))
)

################################################################################


(de req-send-obj (Ipc Sock Obj Sz-Obj Off-Obj)
  # Better to write all strings/bytes to buf-wr, then send in one go

  (send-msg Ipc
            (fmt-msg-send> Obj Off-Obj)
            Sock)

  (send-obj Ipc Sock Obj Sz-Obj Off-Obj))


(de req-recv-obj (Ipc Sock Obj Sz-Obj Off-Obj)
  # Either pass in object or instance new one

  (send-msg Ipc
            (fmt-msg-recv> Obj Off-Obj)
            Sock)

  (recv-obj Ipc Sock Obj Sz-Obj Off-Obj))


(de update-vert (Ipc Vert Off)
  (req-send-obj Ipc
                (ipc~get-fd> Ipc "RENDER") # Pass string and auto look up?
                Vert
                SZ-VERT
                Off))


(de update-pvm (Ipc Projview)
  (req-send-obj Ipc
                (ipc~get-fd> Ipc "RENDER")
                Projview
                SZ-PV
                0))