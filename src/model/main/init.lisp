(in-package :protoform.model)

(defun init-threads ()
  (let ((thread-view       (bordeaux-threads:make-thread #'serve-client))
	(thread-model      (bordeaux-threads:make-thread #'run-model))
	(thread-libinput   (bordeaux-threads:make-thread #'poll-fd-li))
	(thread-controller (bordeaux-threads:make-thread #'run-controller)))
    (bordeaux-threads:join-thread thread-view)
    (bordeaux-threads:join-thread thread-model)
    (bordeaux-threads:join-thread thread-libinput)
    (bordeaux-threads:join-thread thread-controller)))

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
	*mb-model*          (sb-concurrency:make-mailbox)

	*tasks-inactive*    (make-hash-table :size 64)
	*tasks-active*      (make-hash-table :size 64)
	
	*ht-timing-fn*      (make-hash-table :size 64)))

(defun set-stack-node ()   
  (setf *stack-i-nodes*     (loop ; run in parallel?
  			       :with cursor := (vec3 0 0 0)
  			       :for i :from 0 :below (floor (/ 134217728 4 +size-struct-instance+)) ; buffer size / node struct size
  			       :collect (init-node-msdf cursor
  							*scale-node*
  							i
  							nil
  							nil))
  	;; *mutex-stack-nodes* (sb-thread:make-mutex)
	))

(defun set-r-tree ()
  (setf *r-tree*        (spatial-trees:make-spatial-tree :r
							 :rectfun #'node-rect)
	;; *mutex-r-tree*  (sb-thread:make-mutex)
	))

(defun set-digraph ()
  (setf *digraph-main* (digraph:make-digraph)
	;; *mutex-main*    (sb-thread:make-mutex)
	*digraph-vcs*  (digraph:make-digraph)
	;; *mutex-vcs*     (sb-thread:make-mutex)
	
	*digraph-clip* (digraph:make-digraph)
	*digraph-repl* (digraph:make-digraph)))

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

(defun set-node-pointer (&rest deps)
  (declare (ignore deps))
  (setf *node-ptr-main* (init-node-ptr-shm *digraph-main*
					   *vertices-main*
					   (vec3 0 0 0))
	
	*node-ptr-vcs* (init-node-ptr-shm *digraph-vcs*
					  *vertices-vcs*
					  (vec3 0 -10 0))))

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
    			 ;; (clean-up-handles-shm)
    			 (c-shutdown *sock-view*)
    			 (c-close *sock-view*)
    			 (fmt-model t "handle-escape" "Model process exiting!~%")
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

    ;; (dolist (seq-event `((,protoform.controller::+xk-left+       ,#'move-node-ptr-in-cb)
    ;; 			   (,protoform.controller::+xk-up+         ,#'translate-node-ptr-up-cb)
    ;; 			   (,protoform.controller::+xk-right+      ,#'move-node-ptr-out-cb)
    ;; 			   (,protoform.controller::+xk-down+       ,#'translate-node-ptr-down-cb)))
    ;; 	(register-callback `(,(first seq-event) (:press :repeat))
    ;; 			   :exclusive
    ;; 			   (second seq-event)))
    
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
		     #'print-graph-cb)
  (register-callback `(,protoform.controller::+xk-shift-l+ (:press :down)
				     ,protoform.controller::+xk-iso-left-tab+     (:press))
		     :exclusive
		     #'print-graph-cb)
  
  ;; for testing
  (when t
    (register-callback `(,protoform.controller::+xk-f5+ (:press))
		       :exclusive
		       #'pause-task-load-char-from-file)    
    (register-callback `(,protoform.controller::+xk-f6+ (:press))
		       :exclusive
		       #'resume-task-load-char-from-file)
    (register-callback `(,protoform.controller::+xk-f7+ (:press))
		       :exclusive
		       #'stop-task-load-char-from-file))
  
  ;; Print hashtable
  ;;(when nil
  ;;  (maphash (lambda (key value)
  ;;		(fmt-model t "register-keyboard..." "Seq-event: ~S = ~S~%" key value))
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

;;;;;;;;;;;;;;;;;;;;;;;

(defun run-graph-dep ()
  ;; If user modifies, any related functionality during runtime
  ;; this needs to be ran again
  ;; TODO: user needs to be able to run the analyzer during runtime
  ;; for code that runs in the main loop (rpc)
  
  (let* ((path-lisp (merge-pathnames (make-pathname :name "deps-model"
						    :type "lisp")
				     (merge-pathnames #P"src/model/" (asdf:system-source-directory :protoform))))
	 (path-tasks (merge-pathnames (make-pathname :name "tasks-model"
						     :type "lisp")
				      (merge-pathnames #P"src/model/" (asdf:system-source-directory :protoform))))
	 (tasks (if (probe-file path-tasks) ; If tasks exist, load it else generate it
		    (read-from-string (read-file-string path-tasks))
		    (multiple-value-bind (digraph root tasks-new)
			(analyze-file path-lisp
				      path-tasks
				      :init-conn-rpc-view)
		      tasks-new))))

    ;; Convert to ptree and execute
    ;; Build ptree manually    
    
    ;; TODO: Recurse for more complicated setups, i.e. nested lists
    (submit-receive-graph tasks)))

(defun submit-receive-graph (tasks)
  (loop
     :for nodes :across tasks
     :do (loop
	    :for node :across nodes
	    :do (progn
		  (fmt-model t "submit-receive-graph" "task: ~a~%" (string node))
		  (submit-task *channel*
			       (symbol-function (find-symbol (string node)
							     :protoform.model))))
	    :finally (dotimes (i (length nodes))
		       (receive-result *channel*)))))
