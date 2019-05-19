(in-package :protoform.model)

(defun init-threads ()
  ;; Seems easiest to implement and understand this model
  
  ;; Must be dedicated threads:
  ;; * libinput - pull from queue ASAP
  ;; * controller - process events regardless of model (maintain responsiveness)
  ;; -> define two types of events:
  ;;    * immediate - execute in controller
  ;;    * normal - execute in model
  (setf *thread-libinput*   (bordeaux-threads:make-thread #'poll-fd-li)
	*thread-controller* (bordeaux-threads:make-thread #'run-controller))

  ;; For thread-IO and SSDs/NANDs, could use a thread pool?

  ;; * async - CPU-bound tasks
  ;; * io - IO-bound tasks
  ;; * These threads interdependent
  ;;
  ;; * socket - responding to socket (exclusive of io - for explicit tasks that would block model)
  ;; * This is essentially the main thread since all model does is wait on view messages

  ;; Could run these as processes and use DBUS etc?
  ;; Process require serialization which increases latency so easier to run in same address space
  ;; to pass objects around
  
  (setf	*thread-async*      (bordeaux-threads:make-thread #'run-async)
	*thread-io*         (bordeaux-threads:make-thread #'run-io)
	*thread-socket*     (bordeaux-threads:make-thread #'serve-socket))

  (bordeaux-threads:join-thread *thread-libinput*)
  (bordeaux-threads:join-thread *thread-controller*)
  (bordeaux-threads:join-thread *thread-async*)
  (bordeaux-threads:join-thread *thread-io*)
  (bordeaux-threads:join-thread *thread-socket*))

(defun init-nodes-default ()
  ;; *mutex-main*    (sb-thread:make-mutex)
  (setf *digraph-main* (digraph:make-digraph)
	*digraph-vcs*  (digraph:make-digraph))

  (setf *node-ptr-main* (init-node-ptr-shm *digraph-main*
					   *vertices-main*
					   (vec3 0 0 0))
	
	*node-ptr-vcs* (init-node-ptr-shm *digraph-vcs*
					  *vertices-vcs*
					  (vec3 0 10 0)))

  (send-node *node-ptr-main* nil)
  (send-node *node-ptr-vcs* nil)

  ;; Default nodes:
  ;; * Special variables (globals)
  ;;   * Tasks (globals also?)
  
  ;; Create nodes with objects for special variables
  ;; Don't link cursor
  ;; For non-chars link to beginning of node(s)
  (loop
     :with baseline := (get-origin-from-node-pos *node-ptr-main*)
     :with pen := (vcopy3 baseline)
     :with node-prev := nil
     :for var :in *vars-special*
     :do (progn
	   (setf node-prev (make-string-to-node (string var)
						pen
						:data (symbol-value var)
						:node-prev node-prev))
	   (move-pen-newline pen baseline))))

(defun set-projview ()
  (setf *projview* (make-instance 'projview
				  :width *width*
				  :height *height*
				  :type-proj 'orthographic)))

(defun set-controller ()  
  (setf *controller* (init-controller *channel-input*
				      *queue-tasks-sync*
				      #'translate-node-rel))
  (register-keyboard-callbacks))

(defun set-metrics ()
  (setf *metrics* (init-metrics (merge-pathnames #P"glyphs-msdf/"
						 (asdf:system-source-directory :protoform))
				+scale-msdf+)))

(defun set-cc ()
  (setf *queue-tasks-sync*  (sb-concurrency:make-queue)
	*queue-tasks-async* (sb-concurrency:make-queue)

	*mb-async*          (sb-concurrency:make-mailbox)
	*mb-io*             (sb-concurrency:make-mailbox)
	
	*tasks-inactive*    (make-hash-table :size 64)
	*tasks-active*      (make-hash-table :size 64)
	
	*ht-timing-fn*      (make-hash-table :size 64)))

(defun set-nodes-model ()
  (setf *nodes-model*
	(loop
  	   :with cursor := (vec3 0 0 0)
  	   :for i :from 0 :below (floor (/ 134217728 4 +size-struct-instance+)) ; buffer size / node struct size
  	   :collect (init-node-msdf cursor
  				    *scale-node*
  				    i ; use offset?
  				    nil
  				    nil))))

(defun set-nodes-view ()
  (setf *stack-i-nodes*
	(loop
  	   :with cursor := (vec3 0 0 0)
  	   :for i :from 0 :below (floor (/ 134217728 4 +size-struct-instance+)) ; buffer size / node struct size
  	   :collect (init-node-msdf cursor
  				    *scale-node*
  				    i ; use offset?
  				    nil
  				    nil))))

(defun set-rtree-model ()
  (setf *rtree-model* (spatial-trees:make-spatial-tree :r
						       :rectfun #'node-rect)))

(defun set-rtree-view ()
  (setf *r-tree* (spatial-trees:make-spatial-tree :r
						  :rectfun #'node-rect)))

(defun set-shm-projview (projview)
  (setf *shm-projview* (init-shm-projview)))

(defun set-shm-nodes ()
  (setf *shm-nodes* (init-shm-nodes)))

(defun set-shm-atomic-counter ()
  (setf *shm-atomic-counter* (init-shm-atomic-counter)))

(defun set-shm-vertices ()
  (setf *shm-vertices* (init-shm-vertices)))

(defun set-shm-element ()
  (setf *shm-element* (init-shm-element)))

(defun set-shm-draw-indirect ()
  (setf *shm-draw-indirect* (init-shm-draw-indirect)))

(defun set-shm-texture-glyphs ()
  (setf *shm-texture-glyphs* (init-shm-texture-glyphs)))

(defun register-keyboard-callbacks ()
  
  ;; protoform.controller::+xk-return+    
  ;; protoform.controller::+xk-escape+
  ;; protoform.controller::+xk-backspace+
  ;; protoform.controller::+xk-delete+
  ;; protoform.controller::+xk-left+
  ;; protoform.controller::+xk-right+
  ;; protoform.controller::+xk-up+
  ;; protoform.controller::+xk-down+
  ;; protoform.controller::+xk-minus+
  ;; protoform.controller::+xk-equal+
  ;; protoform.controller::+xk-up+
  ;; protoform.controller::+xk-down+
  ;; protoform.controller::+xk-left+
  ;; protoform.controller::+xk-right+

  ;; protoform.controller::+xk-shift-l+ #xffe1   ;  Left shift 
  ;; protoform.controller::+xk-shift-r+ #xffe2   ;  Right shift 
  ;; protoform.controller::+xk-control-l+ #xffe3   ;  Left control 
  ;; protoform.controller::+xk-control-r+ #xffe4   ;  Right control 
  ;; protoform.controller::+xk-caps-lock+ #xffe5   ;  Caps lock 
  ;; protoform.controller::+xk-shift-lock+ #xffe6   ;  Shift lock 
  ;; protoform.controller::+xk-meta-l+ #xffe7   ;  Left meta 
  ;; protoform.controller::+xk-meta-r+ #xffe8   ;  Right meta 
  ;; protoform.controller::+xk-alt-l+ #xffe9   ;  Left alt 
  ;; protoform.controller::+xk-alt-r+ #xffea   ;  Right alt 
  ;; protoform.controller::+xk-super-l+ #xffeb   ;  Left super 
  ;; protoform.controller::+xk-super-r+ #xffec   ;  Right super 
  ;; protoform.controller::+xk-hyper-l+ #xffed   ;  Left hyper 
  ;; protoform.controller::+xk-hyper-r+ #xffee   ;  Right hyper 

  ;; protoform.controller::+xk-left+ #xff51   ;  Move left, left arrow 
  ;; protoform.controller::+xk-up+ #xff52   ;  Move up, up arrow 
  ;; protoform.controller::+xk-right+ #xff53   ;  Move right, right arrow 
  ;; protoform.controller::+xk-down+ #xff54   ;  Move down, down arrow 
  
  (when t
    (register-callback `(,protoform.controller::+xk-escape+ (:press))
    		       :exclusive
    		       (lambda (seq-key)
			 
			 (send-clean-up-render)
			 
			 ;; cleanup mmaps
			 (dolist (mmap (list *shm-projview*
					     *shm-vertices*
					     *shm-nodes*
					     *shm-texture-glyphs*
					     *shm-element*
					     *shm-draw-indirect*
					     *shm-atomic-counter*))
			   (clean-up-mmap mmap t))

			 (foreign-free *buffer-sock-ptr*)
			 
			 (when *sock-render*
    			   (c-shutdown *sock-render* +shut-rdwr+)
    			   (c-close *sock-render*))

			 ;; (end-kernel *kernel*)
			 ;; kill threads
			 ;; (ignore-errors
			 ;;   (bordeaux-threads:destroy-thread *thread-libinput*)
			 ;;   (bordeaux-threads:destroy-thread *thread-controller*)
			 ;;   (bordeaux-threads:destroy-thread *thread-async*)
			 ;;   (bordeaux-threads:destroy-thread *thread-io*)
			 ;;   (bordeaux-threads:destroy-thread *thread-socket*))
			 
    			 (fmt-model "handle-escape" "Model process exiting!~%")
			 (force-output)
    			 (sb-ext:exit))))
  
  (when t
    ;; handlers in node
    (loop
       :for keysym :from 32 :to 255
       :do (progn
	     (register-callback `(,keysym (:press :repeat))
	 			:exclusive
	 			#'add-node-ascii-cb)
	     ;; Better way to handle below?
	     (register-callback `(,protoform.controller::+xk-shift-l+ (:press :down)
	       					,keysym       (:press :repeat))
	       			:exclusive
	       			#'add-node-ascii-cb)
	     (register-callback `(,protoform.controller::+xk-shift-r+ (:press :down)
	       					,keysym       (:press :repeat))
	       			:exclusive
	       			#'add-node-ascii-cb)
	     t)))
  
  (when t
    ;; handlers in node
    (dolist (seq-event `((,protoform.controller::+xk-backspace+ ,#'backspace-node-ascii-cb)
			 (,protoform.controller::+xk-return+    ,#'add-node-newline-cb)
			 (,protoform.controller::+xk-tab+       ,#'add-node-tab-cb)))
      (register-callback `(,(first seq-event) (:press :repeat))
			 :exclusive
			 (second seq-event))))

  (when t
    ;; cut/copy/paste
    (dolist (seq-event `((,protoform.controller::+xk-x+ ,#'cut-node-cb)
			 (,protoform.controller::+xk-c+ ,#'copy-node-cb)
			 (,protoform.controller::+xk-v+ ,#'paste-node-cb)))
      (register-callback `(,protoform.controller::+xk-control-l+    (:press :down)
					      ,(first seq-event) (:press :repeat))
			 :exclusive
			 (second seq-event))
      (register-callback `(,protoform.controller::+xk-control-r+    (:press :down)
					      ,(first seq-event) (:press :repeat))
			 :exclusive
			 (second seq-event))))	
  
  (when t
    
    (dolist (seq-event `((,protoform.controller::+xk-left+       ,#'move-node-ptr-in-cb)
			 (,protoform.controller::+xk-up+         ,#'translate-node-ptr-up-cb)
			 (,protoform.controller::+xk-right+      ,#'move-node-ptr-out-cb)
			 (,protoform.controller::+xk-down+       ,#'translate-node-ptr-down-cb)))
      (register-callback `(,(first seq-event) (:press :repeat))
			 :exclusive
			 (second seq-event))))
  
  (when t
    ;; handlers in projview
    (dolist (seq-event `((,protoform.controller::+xk-left+       ,#'translate-camera-left-cb)
			 (,protoform.controller::+xk-up+         ,#'translate-camera-up-cb)
			 (,protoform.controller::+xk-right+      ,#'translate-camera-right-cb)
			 (,protoform.controller::+xk-down+       ,#'translate-camera-down-cb)))
      (register-callback `(,protoform.controller::+xk-control-l+    (:press :down)
					      ,(first seq-event) (:press :repeat))
			 :exclusive
			 (second seq-event))
      (register-callback `(,protoform.controller::+xk-control-r+    (:press :down)
					      ,(first seq-event) (:press :repeat))
			 :exclusive
			 (second seq-event)))

    (dolist (seq-event `((,protoform.controller::+xk-left+       ,#'scale-ortho-up-cb)
			 (,protoform.controller::+xk-up+         ,#'scale-ortho-down-cb)
			 (,protoform.controller::+xk-right+      ,#'scale-ortho-down-cb)
			 (,protoform.controller::+xk-down+       ,#'scale-ortho-up-cb)))
      (register-callback `(,protoform.controller::+xk-control-l+    (:press :down)
					      ,protoform.controller::+xk-shift-l+      (:press :down)
					      ,(first seq-event) (:press :repeat))
			 :exclusive
			 (second seq-event))
      (register-callback `(,protoform.controller::+xk-control-r+    (:press :down)
					      ,protoform.controller::+xk-shift-r+      (:press :down)
					      ,(first seq-event) (:press :repeat))
			 :exclusive
			 (second seq-event))))

  ;; Eval
  (register-callback `(,protoform.controller::+xk-shift-r+ (:press :down)
				     ,protoform.controller::+xk-return+  (:press :repeat))
		     :exclusive
		     #'eval-node-cb)
  (register-callback `(,protoform.controller::+xk-shift-l+ (:press :down)
		       ,protoform.controller::+xk-return+  (:press :repeat))
		     :exclusive
		     #'eval-node-cb)

  ;; When used with modifiers, need to use iso-left-tab
  (register-callback `(,protoform.controller::+xk-shift-r+      (:press :down)
		       ,protoform.controller::+xk-iso-left-tab+ (:press))
		     :exclusive
		     #'load-file-cb)
  (register-callback `(,protoform.controller::+xk-shift-l+ (:press :down)
		       ,protoform.controller::+xk-iso-left-tab+     (:press))
		     :exclusive
		     #'load-file-cb)
  
  ;; for testing
  (when t
    (register-callback `(,protoform.controller::+xk-alt-r+ (:press :down)
			 ,protoform.controller::+xk-1+     (:press))
		       :exclusive
		       #'pause-task-load-char-from-file)
    (register-callback `(,protoform.controller::+xk-alt-r+ (:press :down)
			 ,protoform.controller::+xk-2+     (:press))
		       :exclusive
		       #'resume-task-load-char-from-file)
    (register-callback `(,protoform.controller::+xk-alt-r+ (:press :down)
			 ,protoform.controller::+xk-3+     (:press))
		       :exclusive
		       #'stop-task-load-char-from-file))
  
  ;; Print hashtable of callbacks
  ;;(when nil
  ;;  (maphash (lambda (key value)
  ;;		(fmt-model "register-keyboard..." "Seq-event: ~S = ~S~%" key value))
  ;;	       key-callbacks))
  
  t)

(defun register-callback-down (keysym cb)
  (register-callback `(,keysym :press)
		     ()
		     :exclusive
		     cb)
  (register-callback `(,keysym :repeat)
		     ()
		     :exclusive
		     cb))
