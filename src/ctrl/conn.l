(de read-pvm (Sock Events)
  # Get raw bytes -> Reconstruct object
  (ipc~send-msg> *ipc "(get-pvm)" Sock)
  (let Bytes-Recv (ipc~recv-bytes> *ipc Sock Events T 128)
    (if (= Bytes-Recv 128)
      (struct (get *ipc 'ipc~buf-rd) '(-1.0 . 32))
      (println "(= Bytes-Recv 128)"))))


(de write-pvm (Pvm Sock Events)
  # Serialize pvm
  (ipc~send-msg> *ipc "(memcpy projview NIL 128 0)" Sock)
  (struct (get *ipc 'ipc~buf-wr) 'N (cons -1.0 Pvm))
  #(println "write-pvm" (struct (get *ipc 'ipc~buf-wr) '(B . 128)))  
  (ipc~send-bytes> *ipc
                   Sock
                   Events
                   (get *ipc 'ipc~buf-wr)
                   128))

                   
################################################################################


(de handle-keyboard (Sock Events Data)
  (let (Key   (get Data 4)
        State (get Data 5))
    (cond ((and (= Key 106) (= State 1)) # Move camera right
           (move-cam Sock Events 29 -1.0))
          ((and (= Key 105) (= State 1)) # Move camera left
           (move-cam Sock Events 29 1.0))
          ((and (= Key 103) (= State 1)) # Move camera up
           (move-cam Sock Events 30 -1.0))
          ((and (= Key 108) (= State 1)) # Move camera dn
           (move-cam Sock Events 30 1.0)))))

                         
(de move-cam (Sock Events Cnt Delta)
   # Pull data from model
   (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
         Sock-Render (ipc~get-fd> *ipc "RENDER")
         Cam         (if *cam *cam (read-pvm Sock-Model)))
      #(println Cam)
      # Modify position
     (let Cam-New (place Cnt Cam (+ (get Cam Cnt) Delta))
       # Push data to model
       #(write-pvm Cam Sock-Model Events)
       # Push update to render
       (write-pvm Cam-New Sock-Render Events)
       (prinl (usec))
       (setq *cam Cam-New))))