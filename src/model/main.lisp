(in-package :protoform.model)

(defun fmt-model (dst ctx-str ctl-str &rest rest)
  ;; Add space opt
  (apply #'format
	 dst
	 (str:concat (format nil "[MODEL:~a][~a] " (sb-posix:getpid) ctx-str)
		     ctl-str)
	 rest))

(defun run-model (width height
		  inst-max
		  addr-swank-view)
  
  (init-kernel-lparallel)
  (fmt-model t "main-model" "Initialized kernel lparallel~%")

  ;; Faster to do this here since they aren't functions
  (setf *width* width
	*height* height
	*inst-max* inst-max)
  
  (exec-graph-dep)
  (fmt-model t "main-init" "Finished model initialization~%")

  ;; Create kernel for each?

  ;; epoll/wait sock -> dispatch/submit tasks -> loop...
  ;; s - p - s

  ;; - events rely on frame time
  ;;   - controller can trigger single-frame or multi-frame (per-frame) calculations
  ;; - would make sense to do controller iteration when frame sent
  ;;   -> need to read from event queue ASAP
  ;; - if locks on controller structures, cannot block view...
  ;; - poss event loop place callbacks in a list/queue for frame-callback to execute
  ;;   - two types of callbacks - sync or async

  (defparameter *queue* (sb-concurrency:make-queue))
  
  ;; RPC loop
  (submit-task *channel*
	       #'serve-client)
  
  ;; Input event loop
  (submit-task *channel*
	       (lambda ()
		 (loop
		    (dispatch-events-input)
		    (dispatch-all-seq-event)
		    (update-states-keyboard-continuous))))
  
  ;; Block forever
  (loop (receive-result *channel*)))  

(defun init-kernel-lparallel ()

  ;; Swank, Input, Wayland
  ;; (bordeaux-threads:make-thread (lambda () (sleep 1)))

  (setf *kernel*     (make-kernel (+ 0 4))
	*channel*    (make-channel)
	*chan-anim*  (make-channel)
	*queue-anim* (make-queue)))

(defun exec-graph-dep ()
  (let ((path-lisp (merge-pathnames (make-pathname :name "deps-model"
						   :type "lisp")
				    (merge-pathnames #P"src/model/" (asdf:system-source-directory :protoform))))
	(path-tasks (merge-pathnames (make-pathname :name "tasks-model"
						    :type "lisp")
				     (merge-pathnames #P"src/model/" (asdf:system-source-directory :protoform))))
	(tasks nil))

    ;; Refactor below into functions and place in analyzer module
    
    ;; If tasks exist, load it else generate it

    (if (probe-file path-tasks)
	(setf tasks (read-from-string (read-file-string path-tasks)))
	
	(multiple-value-bind (digraph root tasks-new)
	    (analyze-file path-lisp :init-conn-rpc-view)
	  (with-open-file (stream path-tasks
				  ;; :external-format charset:iso-8859-1
				  :direction :output
				  :if-exists :overwrite
				  :if-does-not-exist :create)
	    (format stream "#(~%")
	    (loop
	       :for task :across tasks-new
	       :do (if (listp task)
		       (progn
			 (format stream "(~%")
			 (dolist (task-2 task)
			   (format stream "~S~%" (protoform.analyzer-dep::data task-2)))
			 (format stream ")~%"))
		      (format stream "~S~%" (protoform.analyzer-dep::data task))))
	    (format stream ")~%")
	    t)
	  (setf tasks tasks-new)))

    ;; Then execute it

    ;; TODO: Recurse for more complicated setups, i.e. nested lists
    (loop
       :for i :from (1- (length tasks)) :downto 0
       :for nodes := (aref tasks i)
       :do (progn
	     (loop
		:for node :in nodes
		:do (progn
		      (submit-task *channel*
				   (symbol-function (find-symbol (string node) :protoform.model)))
		      (fmt-model t "main-init" "Submitted task: ~a~%" node)
		      ;; (submit-task *channel*
		      ;; 		   (symbol-function (find-symbol (string (protoform.analyzer-dep::data node)) :protoform.model)))
		      ;; (fmt-model t "main-init" "Submitted task: ~a~%" (protoform.analyzer-dep::data node))
		      ;; (receive-result *channel*)
		      t))
	     (dotimes (i (length nodes)) (receive-result *channel*))
	     t))))

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

(defun set-shm-projview ()
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

(defun set-node-pointer ()
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

    (when nil
      (register-callback `(,+xk-escape+ (:press))
    			 :exclusive
    			 (lambda (seq-key)
    			   (clean-up-handles-shm)
    			   (c-shutdown *sock-view*)
    			   (c-close *sock-view*))
    			 (fmt-model t "handle-escape" "Model process exiting!~%")
    			 (sb-ext:exit)))
    
    (when t
      ;; handlers in node
      (loop
	 :for keysym :from 32 :to 255
	 :do (register-callback `(,keysym (:press :repeat))
				:exclusive
				#'add-node-msdf)))

    (when t
      ;; handlers in node
      (dolist (seq-event `((,+xk-left+       ,#'move-pointer-left)
			   (,+xk-up+         ,#'move-pointer-up)
			   (,+xk-right+      ,#'move-pointer-right)
			   (,+xk-down+       ,#'move-pointer-down)
			   (,+xk-backspace+  ,#'backspace-node-msdf)
			   (,+xk-return+     ,#'return-node-msdf)))
	(register-callback `(,(first seq-event) (:press :repeat))
			   :exclusive
			   (second seq-event))))
    
    (when t
      ;; handlers in projview
      (dolist (seq-event `((,+xk-left+       ,#'move-camera-left)
			   (,+xk-up+         ,#'move-camera-up)
			   (,+xk-right+      ,#'move-camera-right)
			   (,+xk-down+       ,#'move-camera-down)))
	(register-callback `(,+xk-control-l+ (:press :down)
			     ,(first seq-event) (:press :repeat))
			   :exclusive
			   (second seq-event)))

      (dolist (seq-event `((,+xk-left+       ,#'update-scale-ortho-out)
			   (,+xk-up+         ,#'update-scale-ortho-in)
			   (,+xk-right+      ,#'update-scale-ortho-in)
			   (,+xk-down+       ,#'update-scale-ortho-out)))
	(register-callback `(,+xk-control-l+ (:press :down)
			     ,+xk-shift-l+ (:press :down)
			     ,(first seq-event) (:press :repeat))
			   :exclusive
			   (second seq-event))))

    (register-callback `(,+xk-f7+ (:press))
		       :exclusive
		       (lambda (seq-event)
			 (setf *fn-anim* #'easing:in-cubic)
			 (setf *value-start* (vx3 (pos *projview*)))
			 (setf *time-start* (osicat:get-monotonic-time))
			 (setf *time-end* (+ *time-start* 4)) ; (/ frame count fps)
			 (setf *time-duration* (- *time-end* *time-start*)) ; (/ frame-count fps)
			 (setf *time-elapsed* 0.0)
			 (setf *time-run* t)))
    ;; (submit-task *chan-anim*
    ;; 	      #'produce-frames-anim)))

    
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
