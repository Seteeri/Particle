(in-package :protoform.model)

(defun build-nodes-from-string (string)
  (loop
     :for char :across string
     :do (let ((node (add-node-ascii char)))
	   t)))

(defun build-string-from-nodes ()
  ;; Pass starting node else use node-pointer
  ;; To eval, build string from predecessors until newline
  
  (let ((chrs nil))
    (loop
       ;; currently assuming linear
       :for pred := (digraph:successors *digraph-main* *node-ptr-main*)
       :then (digraph:predecessors *digraph-main* pred)
       :while pred
       :do (loop
	      ;; Leave on first non-ptr node
	      :for node :in pred
	      :do (unless (eq node *node-ptr-main*)
		    (when (char-equal (char-glyph node) #\Newline)
		      (return))
		    (push (char-glyph node) chrs)
		    (setf pred node)
		    (return))))
    
    (with-output-to-string (stream)
      (dolist (c chrs)
	(write-char c stream)))))

(defun find-node-line-start (node-end dir)
  ;; Find end, specify successor or predecessor direction
  (let ((node-start node-end)
	(fn (cond ((eq dir :in)
		   #'digraph:predecessors)
		  ((eq dir :out)
		   #'digraph:successors))))
    
    ;; Create fn - loop until specified character or pass lambda as predicate
    ;; Start with newline char instead of pointer or it will terminate immediately
    (loop
       :for pred := (funcall fn *digraph-main* node-end)
       :then (funcall fn *digraph-main* pred)
       :while pred
       :do (loop :for node :in pred
	      :do (unless (equal node *node-ptr-main*) ; skip pointer
		    ;; (format t "node: ~S = ~S~%" node (char-glyph node))
		    (when (char-equal (char-glyph node) #\Newline)
		      (return))
		    ;; Else set node to get preds and goto next iteration
		    (setf pred node
			  node-start node)
		    (return))))
    
    node-start))

;;;;;;;;
;; WIP

;; Need do-node macro
(defmacro do-node ((var node-default dir-1 dir-2) &body body)
  `(let ((fn-1 (cond ((eq ,dir-1 :in)
		      #'digraph:predecessors)
		     ((eq ,dir-1 :out)
		      #'digraph:successors)
		     (t
		      t)))
	 (fn-2 (cond ((eq ,dir-2 :in)
		      #'digraph:predecessors)
		     ((eq ,dir-2 :out)
		      #'digraph:successors)
		     (t
		      t))))
     ;; Could detect first char to see which direction text is in...
     ;; - Might introduce bugs -> print warning?
     
     ;; Create fn - loop until specified character or pass lambda as predicate
     ;; Start with newline char instead of pointer or it will terminate immediately
     (loop
	:for pred := (funcall fn-1 *digraph-main* ,node-default)
	:then (funcall fn-2 *digraph-main* pred)
	:while pred
	:do (loop
	       :for node-pred :in pred
	       :do (unless (equal node-pred *node-ptr-main*) ; skip pointer
		     (let ((,var node-pred))
		       ,@body)
		     (setf pred node-pred))))))

(defun nth-node (node-end nth)
  ;; This assumes a non-branching graph
  t)
