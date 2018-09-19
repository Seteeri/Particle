(in-package :protoform.model)

(defclass conn-server ()
  ((sock :accessor sock :initarg :sock :initform nil)
   (clients :accessor clients :initarg :clients :initform (make-hash-table :size 8))

   ;; TEMP
   (view :accessor view :initarg :view :initform nil)
   (controller :accessor controller :initarg :controller :initform nil)))

;; add nonblock option
(defun init-conn-server (path-server)
  (make-instance 'conn-server
		 :sock (init-socket-server nil path-server))) ; blocking

(defun accept-client (conn-server msdf)
  (let ((sock (sock conn-server)))
    (multiple-value-bind (sock-client-accept addr-client) (accept4 sock) ;;+sock-nonblock+)
      (when sock-client-accept
	(format t "[accept-client] Accepted connection: ~a, ~a~%" sock-client-accept addr-client)
	(let ((conn-client (init-conn-client sock-client-accept)))
	  (setf (id conn-client) (recv-message sock-client-accept
					       (buffer-ptr-recv conn-client)
					       (buffer-arr-recv conn-client)))
	  conn-client)))))

;; These requests modify model data

(defun serve-client (conn-client msdf)
  (with-slots (sock
	       buffer-ptr-recv
	       buffer-arr-recv)
      conn-client

    (loop

       :with requests-served = 0
       :do (multiple-value-bind (ret-recv len-recv) (recv-message sock
								  buffer-ptr-recv
								  buffer-arr-recv)
	     (if ret-recv

		 (progn
		   (incf requests-served)
		   ;; (format t "[serve-client] ~a~%" ret-recv)
		   (cond ((eq (first ret-recv) :insert)
			  (serve-insert msdf
					conn-client
					(second ret-recv)
					(third ret-recv)))
			 ((eq (first ret-recv) :delete)
			  (serve-delete msdf
					conn-client
					(second ret-recv)
					(third ret-recv)))
			 ((eq (first ret-recv) :modify-rgba)
			  (serve-modify-rgba msdf
					     conn-client
					     (second ret-recv)
					     (third ret-recv)
					     (fourth ret-recv)))
			 ((eq (first ret-recv) :view)
			  t)
			 ((eq (first ret-recv) :sync)
			  (serve-sync conn-client)
			  (return requests-served)
			  t)
			 ((eq (first ret-recv) :exit)
			  ;; pass both conns
			  (serve-exit msdf conn-client))
			 (t
			  (format t "[serve-client] Unknown request: ~a~%" (first ret-recv)))))

		 ;; Check for client disconnect
		 ;; Use epoll to handle this also
		 (cond ((= len-recv 0)
			;; 11=EAGAIN
			;; (format t "[serve-client] Client disconnected: ~a, ~a~%" sock-client (sb-alien:get-errno))
			;; (serve-exit msdf conn-server)

			;; Remove fd from epoll
			
			t)
		       
		       (t ; -1=no-data,>0=invalid-data
			  (return requests-served)
			t)))))))

(defun serve-sync (conn-client)
  (send-message (sock conn-client)
		(buffer-ptr-send conn-client)
		"t"))

(defun serve-exit (msdf conn-model)
  (error "TODO: serve-exit")
  
  ;; create ipc-view function
  (with-slots (sock) conn-model
    ;; Shutdown clients also
    (c-shutdown sock +shut-rdwr+)
    (c-close sock-client))
  (clean-up-msdf msdf)
  (sb-ext:exit))

;; Fix layout
;; - Refactor chars to allow individual layout (used with compute shader later)

;; How to Interpolate the Update
;; - Instead of draw-blank-draw (frame-frame-frame essentially)
;;   use this same pattern in chunks
;;
;; For each batch:
;; - zero batch-sized memory (blank characters)
;; - ensure frame through sync
;;   - switch server to blocking possibly
;; - fill memory
;; - upload to GPU
;;
;; Like a domino or slot effect
;; Or do a fade-in/fade-out effect?

(defun serve-insert (msdf
		     conn-client
		     chr
		     ret)
  
  ;; Insert single/range of chr instances  
  (with-slots (conn-model
	       mapping-base
 	       sl-chrs
 	       metrics
 	       dpi-glyph
 	       scale-glyph
 	       cursor
 	       queue-chrs
	       lock-sl-chrs
	       inst-max)
      msdf

    (when (= (skip-list:sl-length sl-chrs) inst-max)
      (format t "[serve-insert] List full!~%")
      (return-from serve-insert))

    ;; TODO
    ;; 1. Seralize first char and send update to view immediately
    ;; 2. Make sure to pass starting position to queue
    
    (let ((sl-chr (make-instance 'chr
				 :chr (code-char chr)))
	  (cursor-layout (vec3 (* 9.375 scale-glyph (1- (vx2 cursor)))
			       0.0
			       0.0)))
  
      ;; If update running, cancel it
      ;; When item consumed, lock will release
      (push-queue -1 queue-chrs)

      ;; Insert behind cursor
      (bt:with-lock-held (lock-sl-chrs)
	(skip-list:insert sl-chrs
			  (1- (vx2 cursor))
			  sl-chr)

	;; Serialize chr for view before adding to skip-list
	(update-shm-from-chr sl-chr
			     mapping-base
			     metrics
			     dpi-glyph
			     scale-glyph
			     cursor-layout
			     (round (* (/ 208 4) (1- (vx2 cursor)))))
	
	;; Synchronize chr with view
	(request-memcpy (view conn-model)
			"instance" "instance"
			208
			t
			:offset-dest (round (* 208 (1- (vx2 cursor))))
			:offset-src  (round (* 208 (1- (vx2 cursor)))))
      
	;; Increment cursor position
	(incf (vx2 cursor))))
        
    ;; Update remainder of shm starting from position
    (push-queue (vx2 cursor) queue-chrs))

  (when ret
    (send-message (sock conn-client)
		  (buffer-ptr-send conn-client)
		  "t")))

(defun serve-delete (msdf
		     conn-client
		     pos
		     ret)
  
  ;; Delete single/range of chr instances
  (with-slots (conn-model
	       mapping-base
 	       sl-chrs
 	       metrics
 	       dpi-glyph
 	       scale-glyph
 	       cursor
 	       queue-chrs
	       lock-sl-chrs)
      msdf

    (when (= (skip-list:sl-length sl-chrs) 0)
      (format t "[serve-delete] List empty~%")
      (return-from serve-delete))
    
    ;; If running, cancel it
    ;; When item consumed, lock will release
    (push-queue -1 queue-chrs)

    ;; Insert @ cursor
    
    (let ((node-del nil)
	  (node-del-fwd nil))
      
      (bt:with-lock-held (lock-sl-chrs)

	(setf node-del (skip-list:delete sl-chrs
					 (+ (vx2 cursor) pos))))

      ;; Get next node
      ;; Create function in skip-list
      (setf node-del-fwd (aref (skip-list:ne-forwards node-del) 0))

      ;; TODO: verify node is not null (end of list)
      ;; If null, zero out memory segment
      
      (format t "Delete, node-del-fwd data: ~a~%" (skip-list:ne-data node-del-fwd))

      ;; TODO: Move before the lock
      ;; Update node layout so it is drawn where deleted node is
      (update-shm-from-chr (skip-list:ne-data node-del-fwd)
			   mapping-base
			   metrics
			   dpi-glyph
			   scale-glyph
			   (vec3 (* 9.375 scale-glyph (+ (vx2 cursor) pos))
				 0.0
				 0.0)
			   (round (* (/ 208 4) (+ (vx2 cursor) pos)))))

    ;; Synchronize with view
    (request-memcpy (view conn-model)
		    "instance" "instance"
		    208
		    t
		    :offset-dest (round (* 208 (+ (vx2 cursor) pos)))
		    :offset-src  (round (* 208 (+ (vx2 cursor) pos))))

    ;; For backspace (-2) dec cursor
    ;; Refactor this to pass flag to optionally adjust cursor after delete
    (when (= pos -2)
      (decf (vx2 cursor)))
    
    ;; Update shm
    (push-queue (vx2 cursor) queue-chrs)
    
    (when ret
      (send-message (sock conn-client)
		    (buffer-ptr-send conn-client)
		    "t"))))

(defun serve-modify-rgba (msdf
			  conn-client
			  pos
			  rgba
			  ret)  
  ;; Modify rgba of chr instance

  (with-slots (sl-chrs
	       cursor)
      msdf
  
    ;; set current chr color to default
    ;; set new chr color to yellow

    (let ((chr (skip-list:get-nth-data sl-chrs (+ (vx2 cursor) pos))))
      (dotimes (i 16)
	(setf (aref (rgba chr) i)
	      (aref rgba i)))))

  (when ret
    (send-message (sock conn-client)
		  (buffer-ptr-send conn-client)
		  "t")))

(defun serve-view ()
  ;; Explicitly send request to view to memcpy
  t)
