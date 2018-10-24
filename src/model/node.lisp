(in-package :protoform.model)

(defparameter *uv-default-node* (list 1.0 1.0  0.0 0.0
			  	      1.0 0.0  0.0 0.0
			  	      0.0 0.0  0.0 0.0
			  	      0.0 1.0  0.0 0.0))

(defparameter *color-default-node* (list  (coerce (/ 131 255) 'single-float)
					  (coerce (/ 148 255) 'single-float)
					  (coerce (/ 155 255) 'single-float)
					  (coerce (/ 255 255) 'single-float)

					  (coerce (/ 131 255) 'single-float)
					  (coerce (/ 148 255) 'single-float)
					  (coerce (/ 155 255) 'single-float)
					  (coerce (/ 255 255) 'single-float)

					  (coerce (/ 131 255) 'single-float)
					  (coerce (/ 148 255) 'single-float)
					  (coerce (/ 155 255) 'single-float)
					  (coerce (/ 255 255) 'single-float)

					  (coerce (/ 131 255) 'single-float)
					  (coerce (/ 148 255) 'single-float)
					  (coerce (/ 155 255) 'single-float)
					  (coerce (/ 255 255) 'single-float)))

(defparameter +linegap+ (+ 96 (* 8 5.8239365)))

(defclass node ()
  ((data :accessor data :initarg :data :initform nil)
   (index :accessor index :initarg :index :initform nil)
   (origin :accessor origin :initarg :origin :initform (vec3 0 0 0))
   (model-matrix :accessor model-matrix :initarg :model-matrix :initform (make-instance 'model-matrix))
   (rgba :accessor rgba :initarg :rgba :initform (make-array (* 4 4) ; or use vec4
							     :adjustable nil
							     :fill-pointer nil
							     :element-type 'single-float
							     :initial-contents *color-default-node*))
   
   (offset-texel-texture :accessor offset-texel-texture :initarg :offset-texel-texture :initform 0)
   (dims-texture :accessor dims-texture :initarg :dims-texture :initform (vec2 0 0))
   (uv :accessor uv :initform (make-array 16
					  :adjustable nil
					  :fill-pointer nil
					  :element-type 'single-float
					  :initial-contents *uv-default-node*))
   (flags :accessor flags :initarg :flags :initform 1)))

(defun copy-node-to-shm (node &optional (offset-ptr 0))
  
  (with-slots (ptr size)
      *shm-nodes*
    
    (with-slots (data
		 model-matrix
		 rgba
		 offset-texel-texture
		 dims-texture
		 uv
		 flags)
	node
      
      (loop
	 :for c :across (marr (matrix model-matrix))
	 :for c-i :upfrom 0
	 :do (setf (mem-aref ptr :float (+ offset-ptr c-i))
		   c))
      (incf offset-ptr 16)
      
      (loop
	 :for c :across rgba
	 :for c-i :upfrom 0
	 :do (setf (mem-aref ptr :float (+ offset-ptr c-i))
		   c))
      (incf offset-ptr 16)

      (loop
	 :for c :across uv
	 :for c-i :upfrom 0
	 :do (setf (mem-aref ptr :float (+ offset-ptr c-i))
		   c))
      (incf offset-ptr 16)
      
      ;; Glyph, Flags, pad, pad
      ;; (setf (mem-aref ptr :int (+ offset-ptr 0)) (- (char-code data) 32))
      ;; http://www.lispworks.com/documentation/lcl50/aug/aug-90.html#HEADING90-0
      (setf (mem-aref ptr :int (+ offset-ptr 0)) offset-texel-texture) ; tex offset
      (setf (mem-aref ptr :int (+ offset-ptr 1)) (truncate (vx2 dims-texture))) ; tex dim x
      (setf (mem-aref ptr :int (+ offset-ptr 2)) (truncate (vy2 dims-texture))) ; tex dim y
      (setf (mem-aref ptr :int (+ offset-ptr 3)) flags) ; draw
      (incf offset-ptr 4)))

  ;; (fmt-model t "copy-node-to-shm" "offset: ~S, bytes: ~S~%" offset-ptr (* offset-ptr 4))

  t)

;; TODO: Refactor to pass offsets, range, etc.
(defun copy-nodes-to-shm ()
  (digraph:mapc-vertices (lambda (node)
			   (copy-node-to-shm node
					     (* (index node)
						(/ +size-struct-instance+ 4))))
			 *digraph*))

(defun zero-node-to-shm (&optional (offset-ptr 0))
  
  (with-slots (ptr size)
      (gethash "nodes" *handles-shm*)

    (dotimes (i (/ +size-struct-instance+ 4))
      (setf (mem-aref ptr :int (+ offset-ptr i)) 0))
    
    ;; (fmt-model t "zero-node-to-shm" "offset: ~S, bytes: ~S~%" offset-ptr (* offset-ptr 4))

    t))


(defun init-node-msdf (cursor
		       scale-glyph
		       ix
		       data
		       &optional
			 (color nil))

  (let ((node (make-instance 'node
			     :data data
			     :index ix
			     :model-matrix (make-instance 'model-matrix
							  :scale (vec3 scale-glyph scale-glyph scale-glyph)
							  :translation (vcopy3 cursor))))
	(metrics-glyph (gethash (char-code data) *metrics*)))
    
    ;; Set color
    (when color
      (let ((rgba (rgba node)))
	(dotimes (i 16)
	  (setf (aref rgba i) (nth i color)))))

    ;; ascii - 1
    (setf (offset-texel-texture node) (* (- (char-code data) 1) 96 96))
    (setf (dims-texture node) (vec2 96 96))
    
    ;; Set UVs
    (with-slots (advance
		 translate
		 bounds
		 scale
		 dims-glyph)
	metrics-glyph

      (let ((translation-mm (translation (model-matrix node))))
	;; (setf (vx3 translation-mm) (- (vx3 cursor) (* (vx2 translate) scale scale-glyph)))
	(setf (vy3 translation-mm) (- (vy3 cursor) (* (vy2 translate) scale scale-glyph)))
	(setf (vz3 translation-mm) (vz3 cursor)))

      ;; (fmt-model t "init-node-msdf" "advance: ~a~%" (* advance scale-glyph))

      (let ((scale-mm (scale (model-matrix node))))
	(setf (vx3 scale-mm) (* (vx2 dims-glyph) scale-glyph))
	(setf (vy3 scale-mm) (* (vy2 dims-glyph) scale-glyph))))
    
    ;; Update transform
    (update-transform (model-matrix node))
    
    node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Separate callbacks into callbacks-node

(defun add-node-msdf (seq-key)
  ;; Add node to pointer position
  ;; Move pointer right
  ;; Maybe have pointer appear below/above so edge will show

  (fmt-model t "add-node-msdf" "~a~%" seq-key)
  
  ;; Advance - origin to origin
  ;; 1. Find glyph A origin
  ;;    1. Model trans + glyph trans
  ;; 2. Set glyph B origin
  ;;    1. origin A + advance - glyph trans  
    (let* ((metrics-space (gethash 32 *metrics*))
	   (spacing (* (advance metrics-space) (scale metrics-space) *scale-node*))
	   (cursor (translation (model-matrix *node-pointer*)))
	   (key-first (first (second seq-key)))
	   (data (if (= key-first +xk-return+)
		     #\Newline
		     (code-char key-first)))
	   (node (init-node-msdf (vcopy3 cursor)
				 *scale-node*
				 (digraph:count-vertices *digraph*)
				 data)))
      
      (update-transform (model-matrix node))
      
      ;; Make new node a child of pointer node
      ;; and child of previous
      ;; Old:
      ;; [a]-[*]
      ;; graph
      ;; [*]
      ;;  |
      ;; [a]
      ;;
      ;; New:
      ;; [a]-[b]-[*]
      ;; graph
      ;; [*]--+
      ;;      |
      ;; [a]-[b]

      (digraph:insert-vertex *digraph* node)
      
      (when (first (digraph:successors *digraph* *node-pointer*))
	;; Insert edge a-b
	(digraph:insert-edge *digraph*
			     (first (digraph:successors *digraph* *node-pointer*))
			     node)
	;; Remove edge edge *-a
	(digraph:remove-edge *digraph*
			     *node-pointer*
			     (first (digraph:successors *digraph* *node-pointer*))))
      ;; Insert edge *-b
      (digraph:insert-edge *digraph*
			   *node-pointer*
			   node)
      ;; Move pointer node to right
      (move-node-x *node-pointer*
		   (* 96 *scale-node*)
		   :relative
		   t
		   nil)

      ;; (fmt-model t "init-node-msdf" "cursor: ~a~%" cursor)

      ;; Copy only this node
      (copy-node-to-shm node
			(* (index node)
			   (/ +size-struct-instance+ 4)))

      ;; Copy all nodes
      (memcpy-shm-to-cache-flag* (list (list "nodes"
				       	     0
      				       	     (* +size-struct-instance+ (+ (digraph:count-vertices *digraph*)
				       					  (digraph:count-edges *digraph*))))))

      node))

(defun backspace-node-msdf (seq-key)
  
    (let ((node-tgt (first (digraph:successors *digraph* *node-pointer*))))
      (when node-tgt

	;; Remove node data
	(zero-node-to-shm (* (index node-tgt)
			     (/ +size-struct-instance+ 4)))
	
	;; Remove node from graph
	;; 1. Insert edge: ptr-pred
	;; 2. Remove edges: ptr-node, pred-node
	(let ((preds (digraph:predecessors *digraph* node-tgt)))
	  ;; Find non-ptr edge and create edge from ptr to pred
	  (dolist (pred preds)
	    (if (eq pred *node-pointer*)
		t
		(progn
		  (digraph:insert-edge *digraph*
				       *node-pointer*
				       pred)
		  ;; Move pointer node to right of pred and up a line
		  (move-node-x *node-pointer*
			       (+ (vx3 (translation (model-matrix pred)))
				  (* 96 *scale-node*))
			       :absolute
			       t ; do on move-node-y
			       nil)
		  ;; Only move if end of line - REFACTOR ENTER
		  (when nil
		    (move-node-y *node-pointer*
				 (* +linegap+ scale-node) ; add more spacing due to bl adjustments
				 :relative
				 t
				 nil)))))
	  ;; Now can remove edges
	  (dolist (pred preds)
	    (digraph:remove-edge *digraph*
				 pred
				 node-tgt)))
	;; Remove vertex
	(digraph:remove-vertex *digraph*
			       node-tgt)

	(memcpy-shm-to-cache-flag* (list (list "nodes"
				       	       0
      				       	       (* +size-struct-instance+ (+ (digraph:count-vertices *digraph*)
				       					    (digraph:count-edges *digraph*)))))))))

(defun return-node-msdf (seq-key)
  ;; Add node
  ;; Move pointer back
    (let ((node (add-node-msdf seq-key)))
      (move-node-x *node-pointer*
		   -11.5199995
		   :absolute)
      (move-node-y *node-pointer*
		   (- (* +linegap+ *scale-node*))
		   :relative) ; add more spacing due to bl adjustments    
      (fmt-model t "move-pointer-*" "~a~%" (translation (model-matrix *node-pointer*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun move-node-x (node
		    displacement
		    type-displace
		    &optional
		      (copy-to-shm t)
		      (copy-to-cache t))
    (with-slots (model-matrix
		 index)
	node
      (cond ((eq type-displace :absolute)
	     (setf (vx3 (translation model-matrix)) displacement))
	    ((eq type-displace :relative)
	     (incf (vx3 (translation model-matrix)) displacement))
	    (t
	     (error "Unknown type-displace")))
      (update-transform model-matrix)
      (when copy-to-shm
	(copy-node-to-shm node
			  (* index
			     (/ +size-struct-instance+ 4))))
      (when copy-to-cache
	(memcpy-shm-to-cache-flag* (list (list "nodes"
				       	       0
      				       	       (* +size-struct-instance+ (+ (digraph:count-vertices *digraph*)
				       					    (digraph:count-edges *digraph*)))))))))

(defun move-node-y (node
		    displacement
		    type-displace
		    &optional
		      (copy-to-shm t)
		      (copy-to-cache t))
    (with-slots (model-matrix
		 index)
	node
      (cond ((eq type-displace :absolute)
	     (setf (vy3 (translation model-matrix)) displacement))
	    ((eq type-displace :relative)
	     (incf (vy3 (translation model-matrix)) displacement))
	    (t
	     (error "Unknown type-displace")))
      (update-transform model-matrix)
      (when copy-to-shm
	(copy-node-to-shm node
			  (* index
			     (/ +size-struct-instance+ 4))))
      (when copy-to-cache
	(memcpy-shm-to-cache-flag* (list (list "nodes"
				       	       0
      				       	       (* +size-struct-instance+ (+ (digraph:count-vertices *digraph*)
				       					    (digraph:count-edges *digraph*)))))))))

(defun eval-node-msdf (seq-key)
  ;; To eval, build up string from predecessors
    (let ((node-tgt (first (digraph:successors *digraph* *node-pointer*)))
          (chrs nil))
      (loop
	 :for pred := node-tgt :then (digraph:predecessors *digraph* pred)
	 :while pred
	 :do (progn
	       (when (listp pred)
		 (if (eq (first pred) *node-pointer*)
		     (setf pred (second pred))
		     (setf pred (first pred))))
	       (push (data pred) chrs)
	       (when nil (format t "~a: ~a~%" pred (data pred)))))

      (fmt-model t "eval-node-msdf" "Eval: ~a~%" (eval (read-from-string (with-output-to-string (stream)
									   (dolist (c chrs)
									     (write-char c stream))))))
      (when nil
	(digraph.dot:draw digraph :filename "digraph.png" :format :png))))
