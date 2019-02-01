(in-package :protoform.model)

(defun print-node-dirs (&key
			  (node-ptr *node-pointer*)
			  (dir-ptr :out))
  ;; Print node
  (when-let ((node-ref (get-node-ptr-out)))	    
	    (let ((nodes-ref-in (loop :for n :in (get-nodes-in node-ref) :collect (data n)))
		  (nodes-ref-out (loop :for n :in (get-nodes-out node-ref) :collect (data n))))
	      (format t "ptr: ~a~%" *node-pointer*)
	      (format t "ptr-ref (ptr out): ~a~%" node-ref)
	      (format t "in: ~a~%" nodes-ref-in)
	      (format t "out: ~a~%" nodes-ref-out))))

(defun randomize-color-node (vert)
  ;; random color
  (setf (aref (rgba vert) 0)  (coerce (random 1.0) 'single-float)
	(aref (rgba vert) 1)  (coerce (random 1.0) 'single-float)
	(aref (rgba vert) 2)  (coerce (random 1.0) 'single-float)
	;; (aref (rgba vert) 3)  (coerce (/ 255 255)  'single-float)
	
	(aref (rgba vert) 4)  (coerce (random 1.0) 'single-float)
	(aref (rgba vert) 5)  (coerce (random 1.0) 'single-float)
	(aref (rgba vert) 6)  (coerce (random 1.0) 'single-float)
	;; (aref (rgba vert) 7)  (coerce (/ 255 255)  'single-float)
	
	(aref (rgba vert) 8)  (coerce (random 1.0) 'single-float)
	(aref (rgba vert) 9)  (coerce (random 1.0) 'single-float)
	(aref (rgba vert) 10) (coerce (random 1.0) 'single-float)
	;; (aref (rgba vert) 11) (coerce (/ 255 255)  'single-float)
	
	(aref (rgba vert) 12) (coerce (random 1.0) 'single-float)
	(aref (rgba vert) 13) (coerce (random 1.0) 'single-float)
	(aref (rgba vert) 14) (coerce (random 1.0) 'single-float)
	;; (aref (rgba vert) 15) (coerce (/ 255 255)  'single-float)
	)
  (enqueue-node vert))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-origin-from-node-pos (node)
  ;; Get origin from node position
  ;; l b r t
  (v+ (translation (model-matrix node))
      (let ((bounds-origin (bounds-origin (gethash (char-code (data node)) *metrics*))))
	(vec3 (- (* (aref bounds-origin 0) *scale-node*))
	      (- (* (aref bounds-origin 1) *scale-node*))
	      0.0))))

(defun advance-node-of-node (node-a
			     node-b
			     n-adv
			     &key
			       (offset (vec3 0 0 0)))
  (let* ((origin-b (get-origin-from-node-pos node-b))
	 (bounds-a (bounds-origin (gethash (char-code (data node-a)) *metrics*)))
	 (new-pos (v+ origin-b
		      offset
		      (vec3 (+ (* 9.375 +scale-msdf+ *scale-node* n-adv)
			       (* (aref bounds-a 0) *scale-node*))
			    (* (aref bounds-a 1) *scale-node*)
			    0.0))))
    (setf (translation (model-matrix node-a)) new-pos)
    (update-transform (model-matrix node-a))
    new-pos))

(defun translate-node-to-node (node-a
			       node-b
			       &key
				 (offset (vec3 0 0 0)))
  ;; Move node-a to node-b position; don't change links
  ;; Adjust for baseline
  (let* ((origin-b (get-origin-from-node-pos node-b))
	 (bounds-a (bounds-origin (gethash (char-code (data node-a)) *metrics*)))
	 (new-pos (v+ origin-b
		      offset
		      (vec3 (* (aref bounds-a 0) *scale-node*)
			    (* (aref bounds-a 1) *scale-node*)
			    0.0))))
    (setf (translation (model-matrix node-a)) new-pos)
    (update-transform (model-matrix node-a))
    new-pos))

(defun build-string-from-nodes ()
  ;; Pass starting node else use node-pointer
  ;; To eval, build string from predecessors until newline

  ;; (fmt-model t "build-string-from-nodes" "Pointer: ~a~%" *node-pointer*)
  
  (let ((chrs nil))
    (loop
       ;; currently assuming linear
       :for pred := (digraph:successors *digraph* *node-pointer*)
       :then (digraph:predecessors *digraph* pred)
       :while pred
       :do (loop
	      ;; Leave on first non-ptr node
	      :for node :in pred
	      :do (unless (eq node *node-pointer*)
		    (when (char-equal (data node) #\Newline)
		      (return))
		    (push (data node) chrs)
		    (setf pred node)
		    (return))))
    
    (with-output-to-string (stream)
      (dolist (c chrs)
	(write-char c stream)))))

(defun remove-all-nodes ()
  ;; Exclude pointer
  (digraph:mapc-vertices (lambda (v)
			   (unless (eq v *node-pointer*)
			     (enqueue-node-zero (index v))
			     (remove-vertex v)))
			 *digraph*)
  (digraph:mapc-edges (lambda (e)
			(remove-edge e))
		      *digraph*))

(defun nth-node (node-end nth)
  ;; This assumes a non-branching graph
  t)

(defun copy-node-to-node (node-src)
  (let* ((baseline (get-origin-from-node-pos node-src))
	 (node (init-node-msdf baseline
			       *scale-node* ; get from node-src
			       (digraph:count-vertices *digraph*)
			       (data node-src))))
    (insert-vertex node)
    node))

;; Find end, specify successor or predecessor direction
(defun find-node-line-start (node-end dir)
  (let ((node-start node-end)
	(fn (cond ((eq dir :in)
		   #'digraph:predecessors)
		  ((eq dir :out)
		   #'digraph:successors))))
    
    ;; Create fn - loop until specified character or pass lambda as predicate
    ;; Start with newline char instead of pointer or it will terminate immediately
    (loop :for pred := (funcall fn *digraph* node-end)
       :then (funcall fn *digraph* pred)
       :while pred
       :do (loop :for node :in pred
	      :do (unless (equal node *node-pointer*) ; skip pointer
		    ;; (format t "node: ~S = ~S~%" node (data node))
		    (when (char-equal (data node) #\Newline)
		      (return))
		    ;; Else set node to get preds and goto next iteration
		    (setf pred node
			  node-start node)
		    (return))))
    
    node-start))

(defun replace-node (node-src
		     node-dest
		     dir-src)
  
  ;; Procedure
  ;;
  ;; Insert d:
  ;;
  ;; a <- b <- * <- a | GIVEN
  ;;
  ;; 1. Unlink src
  ;; 1. Get all the ins of dest
  ;;    2. Unlink old, link new
  ;; 3. Get all the outs of dest
  ;;    4. Unlink old, link new

  ;; unlink old
  
  (let ((nodes-in (get-nodes-in node-dest))
	(nodes-out (get-nodes-out node-dest)))

    ;; Unlink dest
    (loop
       :for node :in nodes-in
       :do (remove-edge node node-dest))
    (loop
       :for node :in nodes-out
       :do (remove-edge node-dest node))

    ;; Link src
    (loop
       :for node :in nodes-in
       :do (insert-edge node node-src))
    (loop
       :for node :in nodes-out
       :do (insert-edge node-src node)))

  ;; Could easily do swap function
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	:for pred := (funcall fn-1 *digraph* ,node-default)
	:then (funcall fn-2 *digraph* pred)
	:while pred
	:do (loop
	       :for node-pred :in pred
	       :do (unless (equal node-pred *node-pointer*) ; skip pointer
		     (let ((,var node-pred))
		       ,@body)
		     (setf pred node-pred))))))

(defun get-node-type (node-ref)
  (let ((node-ref-in (get-node-in node-ref))
	(node-ref-out (get-node-out node-ref)))
    (cond ((and node-ref-in
		node-ref-out)
	   :intra)
	  ((and node-ref-in
		(not node-ref-out))
	   :end)
	  ((and (not node-ref-in)
		node-ref-out)
	   :start)
	  ((and (not node-ref-in)
		(not node-ref-out))
	   :iso))))

(defun update-transform-line (node-start offset)
  ;; If assumed, glyphs are already aligned,
  ;; can use offset in line to update nodes simultaneously
  (let ((pos-start (translate (model-matrix node-start)))
	(node-prev nil))
    (do-node (node node-start :out :out)
      (setf (translation (model-matrix node))
	    (v+ (translation (model-matrix node))))
      (update-transform (model-matrix node))
      (enqueue-node node))))
