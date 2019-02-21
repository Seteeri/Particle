(in-package :protoform.model)

(defmacro define-cb-async (name-fn id-fn fn)
  `(defun ,name-fn (seq-key)
       (fmt-model t (format nil "~a" (quote ,name-fn)) "~a~%" seq-key)
       (sb-concurrency:enqueue (list ,id-fn ,fn)
			       *queue-tasks-async*)))

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
  (let ((ptree (make-ptree)))
    (ptree-fn 'move-node-ptr-in
	      '()
	      (lambda ()
		(funcall #'move-node-ptr :in))
	      ptree)
    (sb-concurrency:enqueue *queue-tasks-async*
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
    (sb-concurrency:enqueue *queue-tasks-async*
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
  
  (sb-concurrency:enqueue (list 'print-text
				(lambda ()
    	      			  (funcall #'test-load-file "/home/user/quicklisp/local-projects/protoform/src/protoform.lisp" 0 4096)))
			  *queue-tasks-async*))

;; use fn keys for testing

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

(defun test-load-file (path start length)
  ;; Enqueue task
  (let* ((in (open path
		   :external-format :utf-8)))
    (when in
      (let ((data (make-string length)))
	(read-sequence data stream)
	data)
      (close in)))
  (format t "DONE~%"))

;; (with-open-file (in filename)
;;   (let ((scratch (make-string 4096)))
;;     (loop for read = (read-sequence scratch in)
;;        while (plusp read) sum read)))
