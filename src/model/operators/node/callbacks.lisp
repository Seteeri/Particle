(in-package :protoform.model)

(defmacro define-cb-async (name-fn id-fn fn)
  `(defun ,name-fn (seq-key)
     (fmt-model t (format nil "~a" (quote ,name-fn)) "~a~%" seq-key)
    (enqueue-task-async (make-instance 'task
				      :id ,id-fn
				      :fn-play ,fn))))

(define-cb-async
    add-node-ascii-cb
    'add-node-ascii
  (lambda ()
    (funcall #'add-node-ascii (code-char (second (reverse (second seq-key)))))))

(define-cb-async
    backspace-node-ascii-cb
    'backspace-node-ascii
  (lambda ()
    (funcall #'backspace-node-ascii)))

(define-cb-async
    add-node-tab-cb
    'add-node-tab
  (lambda ()
    (funcall #'add-node-tab)))

(define-cb-async
    add-node-newline-cb
    'add-node-newline
  (lambda ()
    (funcall #'add-node-newline)))
  
(define-cb-async
    eval-node-cb
    'eval-node
  (lambda ()
    (funcall #'eval-node)))

(defun show-node-ids-cb (seq-key)
  (fmt-model t "show-node-ids" "~a~%" seq-key))

(defun cut-node-cb (seq-key)
  (fmt-model t "cut-node" "~a~%" seq-key))

(defun copy-node-cb (seq-key)
  (fmt-model t "copy-node" "~a~%" seq-key))

(defun paste-node-cb (seq-key)
  (fmt-model t "paste-node" "~a~%" seq-key))

;; Anims so these use queue-anim

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

(defun move-node-ptr-in-cb (seq-event)
  (fmt-model t "move-node-ptr-in" "~a~%" seq-event)
  (enqueue-task-async (make-instance 'task
				     :id 'move-node-ptr-in
				     :fn-play (lambda ()
						(funcall #'move-node-ptr :in)))))

(defun move-node-ptr-out-cb (seq-event)
  (fmt-model t "move-node-ptr-out" "~a~%" seq-event)
  (enqueue-task-async (make-instance 'task
				     :id 'move-node-ptr-out
				     :fn-play (lambda ()
						(funcall #'move-node-ptr :out)))))

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

  (when-let ((in (open (make-pathname :directory '(:absolute "home" "user" "quicklisp" "local-projects" "protoform")
				      :name "README" :type "md")
		       :external-format :utf-8)))
    (sb-concurrency:send-message *mb-model*
				 (lambda ()
    	      			   (funcall #'load-chunk-file
					    in
					    0
					    4096
					    0
					    0
					    (get-origin-from-node-pos *node-ptr-main*)
					    (get-origin-from-node-pos *node-ptr-main*))))))

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
	     (data-2 data))
	(enqueue-task-async (make-instance 'task
					   :id 'load-char-from-file
					   :fn-play (lambda ()
						      (load-char-from-file in-2
									   start-2
									   pos-2
									   data-2
									   index-data
									   index-char
									   baseline-start
									   baseline))))))))

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
  (dotimes (i 8)
    
    (when (>= index-data (length data))
      ;; Read new chunk
      (sb-concurrency:send-message *mb-model*
				   (lambda ()
    	      			     (funcall #'load-chunk-file
					      in
					      (+ start length)
					      4096
					      0 ; reset
					      index-char
					      baseline-start
					      baseline)))
      (return-from load-char-from-file))
    
    (let ((char (schar data index-data)))

      (when (char-equal char #\Nul)
	;; EOF
	;; (format t "FOUND EOF~%")
	(close in)
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

      (incf index-data)
      (incf index-char)))
	
  ;; Continue to consume data
  ;; Depends how many chars to create at once

  ;; Technically could add task here to execute on same frame
  ;; Or return to execute on next frame
  ;;
  ;; Maybe return async or sync option

  (make-instance 'task
		 :id 'load-char-from-file
		 :fn-play (lambda ()
			    (load-char-from-file in
						 start
						 length
						 data
						 index-data
						 index-char
						 baseline-start
						 baseline))))

(defun cancel-task (event)
  
  ;; Solution A:
  ;; * Cancel task by ID
  ;; * Create cancel list, canceling will add ID to list, as queue is
  ;;   consumed, check each
  ;;
  ;; Solution B:
  ;; * Dump entire queue, check all, merge new items unto old items
  ;;   * O(n) + O(n) + O(n)+O(n)
  ;;   * Parallelize search -> O(n) + O(n/4) + O(n)

  (format t "CANCEL~%")
  
  (push 'load-char-from-file *stop-qta*))


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
