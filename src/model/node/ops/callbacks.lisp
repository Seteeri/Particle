(in-package :protoform.model)

(defmacro defcb-sync (name-fn id-fn fn)
  `(defun ,name-fn (seq-key)
     (fmt-model t (format nil "~a" (quote ,name-fn)) "~a~%" seq-key)
     (enqueue-task-sync (make-instance 'task
				       :id ,id-fn
				       :fn-play ,fn))))

(defmacro defcb-async (name-fn id-fn fn)
  `(defun ,name-fn (seq-key)
     (fmt-model t (format nil "~a" (quote ,name-fn)) "~a~%" seq-key)
     (enqueue-task-async (make-instance 'task
					:id ,id-fn
					:fn-play ,fn))))

(defcb-async
    add-node-ascii-cb
    'add-node-ascii
  (lambda ()
    (funcall #'add-node-ascii (code-char (second (reverse (second seq-key)))))))

(defcb-async
    backspace-node-ascii-cb
    'backspace-node-ascii
  (lambda ()
    (funcall #'backspace-node-ascii)))

(defcb-async
    add-node-tab-cb
    'add-node-tab
  (lambda ()
    (funcall #'add-node-tab)))

(defcb-async
    add-node-newline-cb
    'add-node-newline
  (lambda ()
    (funcall #'add-node-newline)))
  
(defcb-async
    eval-node-cb
    'eval-node
  (lambda ()
    (funcall #'eval-node)))

(defun cut-node-cb (seq-key)
  (fmt-model t "cut-node" "~a~%" seq-key))

(defun copy-node-cb (seq-key)
  (fmt-model t "copy-node" "~a~%" seq-key))

(defun paste-node-cb (seq-key)
  (fmt-model t "paste-node" "~a~%" seq-key))

(defun show-node-ids-cb (seq-key)
  (fmt-model t "show-node-ids" "~a~%" seq-key))

;; Anims so these use queue-anim

(defcb-sync
    move-node-ptr-in-cb
    'move-node-ptr-in
  (lambda ()
    (funcall #'move-node-ptr :in)))

(defcb-sync
    move-node-ptr-out-cb
    'move-node-ptr-out
  (lambda ()
    (funcall #'move-node-ptr :out)))

(defun translate-node-ptr-left-cb (seq-event)
  (with-slots (model-matrix)
      *node-ptr-main*
    (translate-node-ptr seq-event
		       (lambda (value-new) ; update fn
			 (setf (vx3 (translation model-matrix)) value-new)
			 (send-node *node-ptr-main*))
		       (vx3 (translation model-matrix)) ; start
		       (- (* 96 *scale-node*))          ; delta
		       'move-pointer-x)))

(defun translate-node-ptr-right-cb (seq-event)
  (with-slots (model-matrix)
      *node-ptr-main*
    (translate-node-ptr seq-event
		       (lambda (value-new)
			 (setf (vx3 (translation model-matrix)) value-new)
			 (send-node *node-ptr-main*))
		       (vx3 (translation model-matrix))
		       (* 96 *scale-node*)
		       'move-pointer-x)))

(defun translate-node-ptr-up-cb (seq-event)
  (with-slots (model-matrix)
      *node-ptr-main*
    (translate-node-ptr seq-event
		       (lambda (value-new)
			 (setf (vy3 (translation model-matrix)) value-new)
			 (send-node *node-ptr-main*))
		       (vy3 (translation model-matrix))
		       (* +linegap+ *scale-node* 4)
		       'move-pointer-y)))

(defun translate-node-ptr-down-cb (seq-event)
  (with-slots (model-matrix)
      *node-ptr-main*
    (translate-node-ptr seq-event
		       (lambda (value-new)
			 (setf (vy3 (translation model-matrix)) value-new)
			 (send-node *node-ptr-main*))
		       (vy3 (translation model-matrix))
		       (- (* +linegap+ *scale-node* 4))
		       'move-pointer-y)))

;; (defun print-graph-cb (seq-key)
;;   (fmt-model t "print-graph" "~a~%" seq-key)
;;   (sb-concurrency:enqueue (list nil
;; 				'print-graph
;; 				'()
;; 				(lambda ()
;; 				  (digraph:mapc-vertices
;; 				   (lambda (node)
;; 				     (format t "~S = ~S~%" node (data node)))
;; 				   *digraph-main*)
;; 				  (funcall #'draw-graph)))
;; 			  *queue-tasks-sync*))

(defun print-graph-cb (seq-key)
  (fmt-model t "print-graph" "~a~%" seq-key)

  ;; Break task into nodes -> 8 enqueues

  ;; PROBLEM:
  ;; - each char dependent on previous being drawn
  ;; - order not guaranteed since later char might finish before earlier char
  ;; SOLUTION:
  ;; - pp: init-node; calc positions based on char index
  ;; - insert vertices over all || insert tree over all
  ;; - once:update pointer last (or after each node)
  ;;   - attach ptr
  ;;   - advance ptr

  ;; store nodes in hashtable - ix : node
  ;; maintain stack to determine used and unused
  
  ;; (defun add-node (data
  ;; 		   &optional
  ;; 		     (ix (pop *stack-i-nodes*))
  ;; 		     (baseline (get-origin-from-node-pos *node-ptr-main*)))
  ;;   (let ((node (init-node-msdf baseline
  ;; 				*scale-node*
  ;; 				ix
  ;; 				data)))
  ;;     (insert-vertex node)
  ;;     (spatial-trees:insert node *r-tree*)    
  ;;     node))

  ;; symbol can do about 800 chars in 16.7 ms

  ;; move opening file handle to load-chunk-file (model thread)?
  ;; should be okay in controller thread for now...
  
  (when-let ((in (open (make-pathname :directory '(:absolute "home" "user" "quicklisp" "local-projects" "protoform")
				      :name "README" :type "md")
		       :external-format :utf-8)))
	    (sb-concurrency:send-message
	     *mb-model*
	     (make-instance 'task
			    :id 'load-chunk-file
			    :fn-play (lambda (task)
				       
				       (setf (gethash 'load-chunk-file *tasks-active*) task)
				       
    	      			       (funcall #'load-chunk-file
						in
						0
						4096
						0
						0
						(get-origin-from-node-pos *node-ptr-main*)
						(get-origin-from-node-pos *node-ptr-main*)))))))

;; use fn keys for testing

(defun load-chunk-file (in
			start
			length
			;; create structure for below
			index-data
			index-char
			baseline-start
			baseline)
  ;; Read data
  ;; Return params for next frame
  (let* ((data (make-string length))
	 (pos (read-sequence data in)))
    (format t "pos = ~a~%" pos)
    (when (plusp pos)
      ;; Enqueue in async so it can create nodes
      ;; When it is done with the chunk, it will request another here
      ;; And this will repeat...
      (let* ((in-2 in)
	     (start-2 start)
	     (pos-2 pos)
	     (data-2 data)
	     (task (make-instance 'task
				  :id 'load-char-from-file
				  :fn-play (lambda (task)				   
					     (load-char-from-file in-2
								  start-2
								  pos-2
								  data-2
								  index-data
								  index-char
								  baseline-start
								  baseline)))))
	(enqueue-task-async task)
	task))))

(defun load-char-from-file (in
			    start
			    length
			    data
			    ;; create structure for below
			    index-data
			    index-char
			    baseline-start
			    baseline)

  ;; pass loop count
  (dotimes (i 1)
    
    (when (>= index-data (length data))
      ;; Read new chunk
      (sb-concurrency:send-message
       *mb-model*
       (make-instance 'task
		      :id 'load-chunk-file
		      :fn-play (lambda (task)
				 
				 (setf (gethash 'load-chunk-file *tasks-active*) task)
				 
    	      			 (funcall #'load-chunk-file
					  in
					  (+ start length)
					  4096
					  0 ; reset
					  index-char
					  baseline-start
					  baseline))))
      (return-from load-char-from-file))
    
    (let ((char (schar data index-data)))

      (when (char-equal char #\Nul)
	;; EOF
	;; (format t "FOUND EOF~%")
	(close in)
	(remhash 'load-chunk-file *tasks-active*)
	(remhash 'load-char-from-file *tasks-active*)
	(return-from load-char-from-file))
      
      ;; calc pos relative to line rather than prev char
      (let* ((node (pop *stack-i-nodes*))
	     (pos (v+ (if (char-equal char #\Newline)
			  (progn
			    (setf (vx3 baseline) (vx3 baseline-start))
			    (decf (vy3 baseline) (* +linegap+ *scale-node*))
			    (setf index-char 0))
			  (progn
			    baseline))
		      (vec3 (* 9.375 +scale-msdf+ *scale-node* index-char)
			    0
			    0))))
	(update-translation-node node pos)
	(update-glyph-node node char)
	(update-transform-node node)
	(insert-vertex node)
	(spatial-trees:insert node *r-tree*)
	(send-node node nil))

      (add-node-vcs)

      (incf index-data)
      (incf index-char)))
	
  ;; Continue to consume data
  ;; Depends how many chars to create at once
  
  (let ((task (make-instance 'task
			     :id 'load-char-from-file
			     :fn-play (lambda (task)			    
					(load-char-from-file in
							     start
							     length
							     data
							     index-data
							     index-char
							     baseline-start
							     baseline)))))
    ;; Must do this here so a subsequent stop task can stop the new task
    (setf (gethash 'load-char-from-file *tasks-active*) task)
    ;; (format t "[load/make...] NEW TASK = ~a~%" task)
    task))

(defun stop-task-load-char-from-file (event)
  (stop-task-mb 'load-chunk-file)
  (stop-task 'load-char-from-file))

(defun pause-task-load-char-from-file (event)
  (pause-task-mb 'load-chunk-file)
  (pause-task 'load-char-from-file))

(defun resume-task-load-char-from-file (event)
  (resume-task 'load-char-from-file))

;; (with-open-file (in filename)
;;   (let ((scratch (make-string 4096)))
;;     (loop for read = (read-sequence scratch in)
;;        while (plusp read) sum read)))

(defun print-text ()
  ;; Make ptree version

  ;; 0: string | baseline
  ;; 1: node,main,r-tree,enqueue | ...
  
  (let ((string "HELLO_WORLD!")
	(baseline (get-origin-from-node-pos *node-ptr-main*)))
    (loop
       :for ch :across string
       :for i :upfrom 0
       :do (let* ((node (pop *stack-i-nodes*))
		  (bl baseline)
		  (i-2 i)
		  (ch-2 ch)
		  (pos (v+ bl
			   (vec3 (* 9.375 +scale-msdf+ *scale-node* i-2)
				 0
				 0))))
	     (sb-concurrency:enqueue (list 'print-text
  					   (lambda ()
					     (update-translation-node node pos)
					     (update-glyph-node node ch-2)
					     (update-transform-node node)
					     
					     (insert-vertex node)
					     (spatial-trees:insert node *r-tree*)
					     (send-node node nil)
					     t))
				     *queue-tasks-async*)))))
