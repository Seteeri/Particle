(de read-pvm (Sock)
  # Get raw bytes -> Reconstruct object
  (ipc~send-msg> *ipc "(get-pvm)" Sock)
  (let Bytes-Recv (ipc~recv-bytes> *ipc Sock T 128)
    (if (= Bytes-Recv 128)
      (struct (get *ipc 'ipc~buf-rd) '(-1.0 . 32)) # Use structure
      (prinl (usec) " | read-pvm | Bytes-Recv=" Bytes-Recv " errno=" (errno)))))


(de write-pvm (Pvm Sock)
  # Serialize pvm
  (ipc~send-msg> *ipc "(mc projview NIL 128 0)" Sock)
  (struct (get *ipc 'ipc~buf-wr) 'N (cons -1.0 Pvm))
  #(println "write-pvm" (struct (get *ipc 'ipc~buf-wr) '(B . 128)))  
  (ipc~send-bytes> *ipc
                   Sock
                   T
                   128))


(de read-vert (Sock Cnt)
  # Get raw bytes -> Reconstruct object
  (ipc~send-msg> *ipc (pack "(get-vert " Cnt ")") Sock)
  (let Bytes-Recv (ipc~recv-bytes> *ipc Sock T 208)
    (if (= Bytes-Recv 208)
      (struct (get *ipc 'ipc~buf-rd) '((-1.0 . 16) (-1.0 . 16) (-1.0 . 16) (I . 4)))
      (prinl (usec) " | read-vert | Bytes-Recv=" Bytes-Recv " errno=" (errno)))))


(de write-vert (Vert Cnt Sock)
  # Serialize pvm
  (ipc~send-msg> *ipc (pack "(write-vert " Cnt ")") Sock)
  (let P (get *ipc 'ipc~buf-wr)
    (struct P           'N (cons -1.0 (get Vert 1)))
    (struct (inc 'P 64) 'N (cons -1.0 (get Vert 2)))
    (struct (inc 'P 64) 'N (cons -1.0 (get Vert 3)))
    (struct (inc 'P 64) 'N (cons (get (get Vert 4) 1) 4)
                           (cons (get (get Vert 4) 2) 4)
                           (cons (get (get Vert 4) 3) 4)
                           (cons (get (get Vert 4) 4) 4)))  
  (ipc~send-bytes> *ipc
                   Sock
                   T
                   208))

                   
################################################################################


(de handle-keyboard (Sock Data)
  (let (Key   (get Data 4)
        State (get Data 5))
    (cond ((and (= Key 106) (= State 1)) # Move camera right
           (move-cam Sock 29 -1.0))
          ((and (= Key 105) (= State 1)) # Move camera left
           (move-cam Sock 29 1.0))
          ((and (= Key 103) (= State 1)) # Move camera up
           (move-cam Sock 30 -1.0))
          ((and (= Key 108) (= State 1)) # Move camera dn
           (move-cam Sock 30 1.0)))))

                         
(de move-cam (Sock Cnt Delta)
   # Pull data from model
   (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
         Sock-Render (ipc~get-fd> *ipc "RENDER")
         Cam         (if *cam *cam (read-pvm Sock-Model)))
      #(println Cam)
      
      # Modify position
     (let Cam-New (place Cnt Cam (+ (get Cam Cnt) Delta))
       
       # Push data to model
       #(write-pvm Cam Sock-Model)
       
       # Push update to render
       (write-pvm Cam-New Sock-Render)
       
       #(prinl (usec))
       
       (setq *cam Cam-New))))