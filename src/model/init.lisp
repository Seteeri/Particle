(in-package :protoform.model)

(defun init-threads ()
  (let ((thread-view  (bordeaux-threads:make-thread #'serve-client))
	;; (thread-model (bordeaux-threads:make-thread #'process-queue-input))
	(thread-input (bordeaux-threads:make-thread #'run-controller)))
    (bordeaux-threads:join-thread thread-view)
    ;; (bordeaux-threads:join-thread thread-model)
    (bordeaux-threads:join-thread thread-input)))

(defun set-projview ()
  (setf *projview* (make-instance 'projview
				  :width *width*
				  :height *height*
				  :type-proj 'orthographic)))

(defun set-controller ()  
  (setf *controller* (init-controller))
  (register-keyboard-callbacks))

(defun set-metrics ()
  (setf *metrics* (init-metrics)))

(defun set-digraph ()
  (setf *digraph* (digraph:make-digraph)))

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

(defun set-node-pointer (digraph
			 metrics
			 shm-nodes)
  (setf *node-pointer* (init-node-pointer-graph-shm)))

(defun register-keyboard-callbacks ()

  (with-slots (key-callbacks)
      *controller*
    
    ;; +xk-return+    
    ;; +xk-escape+
    ;; +xk-backspace+
    ;; +xk-delete+
    ;; +xk-left+
    ;; +xk-right+
    ;; +xk-up+
    ;; +xk-down+
    ;; +xk-minus+
    ;; +xk-equal+
    ;; +xk-up+
    ;; +xk-down+
    ;; +xk-left+
    ;; +xk-right+

    ;; +xk-shift-l+ #xffe1   ;  Left shift 
    ;; +xk-shift-r+ #xffe2   ;  Right shift 
    ;; +xk-control-l+ #xffe3   ;  Left control 
    ;; +xk-control-r+ #xffe4   ;  Right control 
    ;; +xk-caps-lock+ #xffe5   ;  Caps lock 
    ;; +xk-shift-lock+ #xffe6   ;  Shift lock 
    ;; +xk-meta-l+ #xffe7   ;  Left meta 
    ;; +xk-meta-r+ #xffe8   ;  Right meta 
    ;; +xk-alt-l+ #xffe9   ;  Left alt 
    ;; +xk-alt-r+ #xffea   ;  Right alt 
    ;; +xk-super-l+ #xffeb   ;  Left super 
    ;; +xk-super-r+ #xffec   ;  Right super 
    ;; +xk-hyper-l+ #xffed   ;  Left hyper 
    ;; +xk-hyper-r+ #xffee   ;  Right hyper 

    ;; +xk-left+ #xff51   ;  Move left, left arrow 
    ;; +xk-up+ #xff52   ;  Move up, up arrow 
    ;; +xk-right+ #xff53   ;  Move right, right arrow 
    ;; +xk-down+ #xff54   ;  Move down, down arrow 

    (when t
      (register-callback `(,+xk-escape+ (:press))
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
	 			  #'add-node-cb)
	       ;; Better way to handle below?
	       (register-callback `(,+xk-shift-l+ (:press :down)
	       			    ,keysym       (:press :repeat))
	       			  :exclusive
	       			  #'add-node-cb)
	       (register-callback `(,+xk-shift-r+ (:press :down)
	       			    ,keysym       (:press :repeat))
	       			  :exclusive
	       			  #'add-node-cb)
	       t)))
    
    (when t
      ;; handlers in node
      (dolist (seq-event `((,+xk-backspace+ ,#'backspace-node-cb)
			   (,+xk-return+    ,#'insert-node-newline-cb)
			   (,+xk-tab+       ,#'insert-node-tab-cb)))
	(register-callback `(,(first seq-event) (:press :repeat))
			   :exclusive
			   (second seq-event))))

    (when t
      ;; cut/copy/paste
      (dolist (seq-event `((,+xk-x+ ,#'cut-node-cb)
			   (,+xk-c+ ,#'copy-node-cb)
			   (,+xk-v+ ,#'paste-node-cb)))
	(register-callback `(,+xk-control-l+    (:press :down)
			     ,(first seq-event) (:press :repeat))
			   :exclusive
			   (second seq-event))
	(register-callback `(,+xk-control-r+    (:press :down)
			     ,(first seq-event) (:press :repeat))
			   :exclusive
			   (second seq-event))))	
    
    (when t
      ;; handlers in node
      (dolist (seq-event `((,+xk-left+       ,#'translate-pointer-left-cb)
			   (,+xk-up+         ,#'translate-pointer-up-cb)
			   (,+xk-right+      ,#'translate-pointer-right-cb)
			   (,+xk-down+       ,#'translate-pointer-down-cb)))
	(register-callback `(,(first seq-event) (:press :repeat))
			   :exclusive
			   (second seq-event))))
    
    (when t
      ;; handlers in projview
      (dolist (seq-event `((,+xk-left+       ,#'translate-camera-left-cb)
			   (,+xk-up+         ,#'translate-camera-up-cb)
			   (,+xk-right+      ,#'translate-camera-right-cb)
			   (,+xk-down+       ,#'translate-camera-down-cb)))
	(register-callback `(,+xk-control-l+    (:press :down)
			     ,(first seq-event) (:press :repeat))
			   :exclusive
			   (second seq-event))
	(register-callback `(,+xk-control-r+    (:press :down)
			     ,(first seq-event) (:press :repeat))
			   :exclusive
			   (second seq-event)))

      (dolist (seq-event `((,+xk-left+       ,#'scale-ortho-up-cb)
			   (,+xk-up+         ,#'scale-ortho-down-cb)
			   (,+xk-right+      ,#'scale-ortho-down-cb)
			   (,+xk-down+       ,#'scale-ortho-up-cb)))
	(register-callback `(,+xk-control-l+    (:press :down)
			     ,+xk-shift-l+      (:press :down)
			     ,(first seq-event) (:press :repeat))
			   :exclusive
			   (second seq-event))
	(register-callback `(,+xk-control-r+    (:press :down)
			     ,+xk-shift-r+      (:press :down)
			     ,(first seq-event) (:press :repeat))
			   :exclusive
			   (second seq-event))))

    ;; Eval
    (register-callback `(,+xk-shift-r+ (:press :down)
			 ,+xk-return+  (:press :repeat))
		       :exclusive
		       #'eval-node-cb)
    (register-callback `(,+xk-shift-l+ (:press :down)
			 ,+xk-return+  (:press :repeat))
		       :exclusive
		       #'eval-node-cb)
    
    ;; for testing
    (when nil
      (register-callback `(,+xk-f7+ (:press))
			 :exclusive
			 (lambda (seq-event ptree queue)
			   t)))
    
    ;; Print hashtable
    (when nil
      (maphash (lambda (key value)
		 (fmt-model t "register-keyboard..." "Seq-event: ~S = ~S~%" key value))
	       key-callbacks))
    
    t))

(defun register-callback-down (keysym cb)
  (with-slots (key-callbacks)
      *controller*
    (register-callback `(,keysym :press)
		       ()
		       :exclusive
		       cb)
    (register-callback `(,keysym :repeat)
		       ()
		       :exclusive
		       cb)))

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
