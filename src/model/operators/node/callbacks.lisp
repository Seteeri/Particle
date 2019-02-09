(in-package :protoform.model)

(defun add-node-ascii-cb (seq-key)
  (fmt-model t "add-node-ascii" "~a~%" seq-key)
  (let ((ptree (make-ptree)))
    (ptree-fn 'add-node ;; (make-symbol (format nil "~a~%" seq-key))
	      '()
	      (lambda ()
		(funcall #'add-node-ascii (code-char (second (reverse (second seq-key))))))
	      ptree) ; create aux fn for this
    (sb-concurrency:send-message *mailbox-model*
				 (list ptree
				       'add-node))))

(defun backspace-node-ascii-cb (seq-key)
  (fmt-model t "backspace-node-ascii" "~a~%" seq-key)
  (let ((ptree (make-ptree)))
    (ptree-fn 'backspace-node ; verify
	      '()
	      (lambda ()
		(funcall #'backspace-node-ascii))
	      ptree)
    (sb-concurrency:send-message *mailbox-model*
				 (list ptree
				       'backspace-node))))

(defun add-node-tab-cb (seq-key)
  (fmt-model t "add-node-tab" "~a~%" seq-key)
  (let ((ptree (make-ptree)))
    (ptree-fn 'add-node-tab
	      '()
  	      (lambda ()
  		(funcall #'add-node-tab))
	      ptree)
    (sb-concurrency:send-message *mailbox-model*
				 (list ptree
				       'add-node-tab-node))))

(defun add-node-newline-cb (seq-key)
  (fmt-model t "add-node-newline" "~a~%" seq-key)
  (let ((ptree (make-ptree)))
    (ptree-fn 'add-node-newline
	      '()
  	      (lambda ()
  		(funcall #'add-node-newline))
	      ptree)
    (sb-concurrency:send-message *mailbox-model*
				 (list ptree
				       'add-node-newline))))
  
(defun eval-node-cb (seq-key)
  (fmt-model t "eval-node" "~a~%" seq-key)
  (let ((ptree (make-ptree)))
    (ptree-fn 'eval-node
	      '()
	      (lambda ()
		(funcall #'eval-node))
	      ptree)
    (sb-concurrency:send-message *mailbox-model*
				 (list ptree
				       'eval-node))))

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
			 (enqueue-node-ptr))
		       (vx3 (translation model-matrix)) ; start
		       (- (* 96 *scale-node*))          ; delta
		       'move-pointer-x)))

(defun translate-node-ptr-right-cb (seq-event)
  (with-slots (model-matrix)
      *node-ptr-main*
    (translate-node-ptr seq-event
		       (lambda (value-new)
			 (setf (vx3 (translation model-matrix)) value-new)
			 (enqueue-node-ptr))
		       (vx3 (translation model-matrix))
		       (* 96 *scale-node*)
		       'move-pointer-x)))

(defun translate-node-ptr-up-cb (seq-event)
  (with-slots (model-matrix)
      *node-ptr-main*
    (translate-node-ptr seq-event
		       (lambda (value-new)
			 (setf (vy3 (translation model-matrix)) value-new)
			 (enqueue-node-ptr))
		       (vy3 (translation model-matrix))
		       (* +linegap+ *scale-node* 4)
		       'move-pointer-y)))

(defun translate-node-ptr-down-cb (seq-event)
  (with-slots (model-matrix)
      *node-ptr-main*
    (translate-node-ptr seq-event
		       (lambda (value-new)
			 (setf (vy3 (translation model-matrix)) value-new)
			 (enqueue-node-ptr))
		       (vy3 (translation model-matrix))
		       (- (* +linegap+ *scale-node* 4))
		       'move-pointer-y)))

(defun move-node-ptr-in-cb (seq-event)
  (fmt-model t "move-node-ptr-in" "~a~%" seq-event)
  (let ((ptree (make-ptree)))
    (ptree-fn 'move-node-ptr-in
	      '()
	      (lambda ()
		(funcall #'move-node-ptr :in))
	      ptree)
    (sb-concurrency:send-message *mailbox-model*
				 (list ptree
				       'move-node-ptr-in))))

(defun move-node-ptr-out-cb (seq-event)
  (fmt-model t "move-node-ptr-out" "~a~%" seq-event)
  (let ((ptree (make-ptree)))
    (ptree-fn 'move-node-ptr-out
	      '()
	      (lambda ()
		(funcall #'move-node-ptr :out))
	      ptree)
    (sb-concurrency:send-message *mailbox-model*
				 (list ptree
				       'move-node-ptr-out))))

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
;; 			  *queue-anim*))

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

  (let ((ptree (make-ptree)))
    (ptree-fn 'print-text
	      '()
  	      (lambda ()
  		(funcall #'print-text))
	      ptree)
    (sb-concurrency:send-message *mailbox-model*
				 (list ptree
				       'print-text))))

(defun print-text ()
  ;; Make ptree version

  ;; 0: string | baseline
  ;; 1: node,main,r-tree,enqueue | ...
  
  (let ((string "HELLO_WORLD!")
	(baseline (get-origin-from-node-pos *node-ptr-main*)))
    (loop
       :for ch :across string
       :for i :upfrom 0
       :do (let* ((ix (sb-thread:with-mutex (*mutex-stack-nodes*)
			(pop *stack-i-nodes*)))
		  (node (init-node-msdf (v+ baseline
					   (vec3 (* 9.375 +scale-msdf+ *scale-node* i)
						 0
						 0))
  				       *scale-node*
				       ix
  				       ch)))
	     (sb-thread:with-mutex ((mutex node))
	       (sb-thread:with-mutex (*mutex-main*)
		 (insert-vertex node))
	       (sb-thread:with-mutex (*mutex-r-tree*)
		 (spatial-trees:insert node *r-tree*))
	       (enqueue-node node))))))
