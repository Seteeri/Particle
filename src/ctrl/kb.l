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

            ((= Key 104) # pg-up
             (zoom-cam Sock -3.0))
             
            ((= Key 109) # pg-dn
             (zoom-cam Sock 3.0))
  
            (T
             (handle-ascii Key))))))
   

(def '*pen-v (list 0.0 0.0 0.0))
(def '*cnt-v 1)


(de handle-ascii (Key)

  (let (Data (cons Key "ABC")
        Ptrs (struct (>> -4 (adr Data)) '((B . 8) (B . 8)))
        Off (*/ (*/ *adv-glyph *sca-glyph 1.0) *sca-vert 1.0))
        
    (println Ptrs)
        
    (draw-bytes (get Ptrs 1))
    (draw-bytes (get Ptrs 2))
    
    # For each ptr-vert, push into list and store in sym prop-list 'verts
    # [PL][VAL], VAL="a" etc.
    # [PL][VAL], VAL="abc" etc., there would be three verts in pl 'verts
    # Then push sym into master list or timeline
    
    # Produce vertices for representation/val (decoded pointer)
    
    # Link all together
    
    (setq *pen-v (list 0.0
                       (- (get *pen-v 2) (+ Off Off))
                       0.0))))


(de draw-bytes (Bytes)
  # Bytes is list of numbers

  # Create vertex; data = char
  (let (Sock-Model (ipc~get-fd> *ipc "MODEL")
        Off (*/ (*/ *adv-glyph *sca-glyph 1.0) *sca-vert 1.0))
    
    (for Byte Bytes
    
      (let Str (format Byte)
      
        (for N (chop Str)
        
          (let (Off-Vert (get-vert-off *cnt-v)
                Vert (req-recv-obj *ipc
                                   Sock-Model
                                   (new '(+Vertex))
                                   SZ-VERT
                                   Off-Vert))

            # Reset vertex
            # Pos = baseline = X,0,0
            (with Vert
              (=: pos (list (get *pen-v 1) (get *pen-v 2) 0.0)))
             
            #(with Vert
            #  (println Vert (: pos)))
             
            (update-glyph> Vert N)
             
            (setq *pen-v (list (+ (get *pen-v 1) Off)
                               (get *pen-v 2)
                               0.0))
                                     
            (update-model-matrix> Vert)
            
            (update-vert *ipc Vert Off-Vert)
     
            (req-send-obj *ipc
                          Sock-Model
                          Vert
                          SZ-VERT
                          Off-Vert)

            (inc '*cnt-v 1)))
            
          # Inc byte spacing depending on length
          (when T
            (setq *pen-v (list (+ (get *pen-v 1) (* Off (- 4 (length Str))))
                               (get *pen-v 2)
                               0.0)))
                             
                             T))))


(de move-cam (Sock Cnt Move)

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


(de zoom-cam (Sock Delta)

   (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
         Sock-Render (ipc~get-fd> *ipc "RENDER"))

     (with *projview

      (inc (:: scale-ortho) Delta)

      (update-mat-proj> *projview)

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