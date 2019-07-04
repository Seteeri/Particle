(de read-pvm (Sock)
  # Get raw bytes -> Reconstruct object
  (ipc~send-msg> *ipc "(get-pvm)" Sock)
  (let Bytes-Recv (ipc~recv-bytes> *ipc T Sock T 128)
    (if (= Bytes-Recv 128)
      (struct (get *ipc 'ipc~buf-rd) '(-1.0 . 32))
      (println "(= Bytes-Recv 128)"))))


(de write-pvm (Pvm Sock)
  # Serialize pvm
  (ipc~send-msg> *ipc "(memcpy projview NIL 128 0)" Sock)
  (struct (get *ipc 'ipc~buf-wr) 'N (cons -1.0 Pvm))
  #(println "write-pvm" (struct (get *ipc 'ipc~buf-wr) '(B . 128)))  
  (ipc~send-bytes> *ipc
                   T
                   Sock
                   (get *ipc 'ipc~buf-wr)
                   128))