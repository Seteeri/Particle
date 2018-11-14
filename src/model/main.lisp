(in-package :protoform.model)

(defun fmt-model (dst ctx-str ctl-str &rest rest)
  ;; Add space opt
  (apply #'format
	 dst
	 (str:concat (format nil "[MODEL:~a][~a] " (sb-posix:getpid) ctx-str)
		     ctl-str)
	 rest))

(defparameter *queue-front* (sb-concurrency:make-queue))
(defparameter *queue-back* (sb-concurrency:make-queue))
(defparameter *queue-input* *queue-back*)
(defparameter *queue-view* (sb-concurrency:make-queue))

(defun run-model (width height
		  inst-max
		  addr-swank-view)
  
  (init-kernel-lparallel)
  (fmt-model t "main-model" "Initialized kernel lparallel~%")

  ;; Faster to do this here since they aren't functions
  (setf *width*    width
	*height*   height
	*inst-max* inst-max)
  
  (run-graph-dep)
  (fmt-model t "main-init" "Finished model initialization~%")

  (let ((thread-view  (bordeaux-threads:make-thread #'serve-client))
	;; (thread-model (bordeaux-threads:make-thread #'process-queue-input))
	(thread-input (bordeaux-threads:make-thread #'run-controller)))
    (bordeaux-threads:join-thread thread-view)
    ;; (bordeaux-threads:join-thread thread-model)
    (bordeaux-threads:join-thread thread-input)))
  

(defun init-kernel-lparallel ()
  (setf *kernel*        (make-kernel 4)
	*channel*       (make-channel)
	*channel-input* (make-channel)))

(defun run-graph-dep ()
  (let* ((path-lisp (merge-pathnames (make-pathname :name "deps-model"
						    :type "lisp")
				     (merge-pathnames #P"src/model/" (asdf:system-source-directory :protoform))))
	 (path-tasks (merge-pathnames (make-pathname :name "tasks-model"
						     :type "lisp")
				      (merge-pathnames #P"src/model/" (asdf:system-source-directory :protoform))))
	 (tasks (if (probe-file path-tasks) ; If tasks exist, load it else generate it
		    (read-from-string (read-file-string path-tasks))
		    (multiple-value-bind (digraph root tasks-new)
			(analyze-file path-lisp path-tasks :init-conn-rpc-view)
		      tasks-new))))

    ;; TODO: Recurse for more complicated setups, i.e. nested lists
    (submit-receive-graph tasks)))

(defun submit-receive-graph (tasks)
  (loop
     :for i :from (1- (length tasks)) :downto 0
     :for nodes := (aref tasks i)
     :do (progn
	   (loop
	      :for node :in nodes
	      :do (progn
		    (submit-task *channel*
				 (symbol-function (find-symbol (string node) :protoform.model))))
	      :finally (dotimes (i (length nodes)) (receive-result *channel*))))))

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
	 :do (progn
	       (register-callback `(,keysym (:press :repeat))
	 			  :exclusive
	 			  #'add-node-msdf)
	       ;; Better way to handle below?
	       (register-callback `(,+xk-shift-l+ (:press :down)
	       			    ,keysym (:press :repeat))
	       			  :exclusive
	       			  #'add-node-msdf)
	       (register-callback `(,+xk-shift-r+ (:press :down)
	       			    ,keysym (:press :repeat))
	       			  :exclusive
	       			  #'add-node-msdf)
	       t)))
    
    (when t
      ;; handlers in node
      (dolist (seq-event `((,+xk-backspace+  ,#'backspace-node-msdf)
			   (,+xk-return+     ,#'return-node-msdf)))
	(register-callback `(,(first seq-event) (:press :repeat))
			   :exclusive
			   (second seq-event))))
    
    (when nil
      ;; handlers in node
      (dolist (seq-event `((,+xk-left+       ,#'move-pointer-left)
			   (,+xk-up+         ,#'move-pointer-up)
			   (,+xk-right+      ,#'move-pointer-right)
			   (,+xk-down+       ,#'move-pointer-down)))
	(register-callback `(,(first seq-event) (:press :repeat))
			   :exclusive
			   (second seq-event))))
    
    (when nil
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

    ;; How to handle situation if animation running
    (register-callback `(,+xk-f7+ (:press))
		       :exclusive
		       (lambda (seq-event)
			 (setf *fn-anim* #'easing:in-cubic)
			 (setf *value-start* (vx3 (pos *projview*)))
			 (setf *time-start* (osicat:get-monotonic-time))
			 (setf *time-end* (+ *time-start* 4)) ; (/ frame count fps)
			 (setf *time-duration* (- *time-end* *time-start*)) ; (/ frame-count fps)
			 (setf *time-elapsed* 0.0)
			 (ease-camera-x seq-event)))
    
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

(defun process-queue-input ()
  (loop
     :for task := (pop-queue *queue-input-model*)
     :do (funcall (first task)
		  (second task))))
