(in-package :protoform.opengl)

(defclass buffer-object ()
  ((target :accessor target :initarg :target :initform nil)
   (binding-layout :accessor binding-layout :initarg :binding-layout :initform nil)
   (fn-bind :accessor fn-bind :initarg :fn-bind :initform nil)
   
   (buffers :accessor buffers :initarg :buffers :initform nil)
   (count-buffers :accessor count-buffers :initarg :count-buffers :initform nil)
   (size-buffer :accessor size-buffer :initarg :size-buffer :initform nil)

   ;; get rid of this and store nil or array in ptrs-buffer to indicate mapping
   (mapped-persistent :accessor mapped-persistent :initarg :mapped-persistent :initform nil)
   (ptrs-buffer :accessor ptrs-buffer :initarg :ptrs-buffer :initform nil)))

(defun format-buffer-object (buffer)
  (with-slots (target
	       binding-layout
	       size-buffer)
      buffer
    (with-output-to-string (stream)
      (format stream "Buffer Object: ~a~%" buffer)
      (format stream "  target: ~a~%" target)
      (format stream "  size: ~a bytes~%" buffer)
      (format stream "  bind: ~a~%" binding-layout))))

(defun init-buffer-object (target
			   name
			   size
			   binding-layout
			   mapped-persistent
			   &key
			     (buffering :triple)     ; 'single 'double 'triple, make this required
			     (usage :static-draw) ; dynamic-draw :stream-draw
			     (data nil))

  ;; https://stackoverflow.com/questions/47563995/double-buffering-vs-triple-buffering-for-vertex-buffers
  ;; As long as you protect buffer memory areas from overwrites with fences,
  ;; you can even use a single buffered vertex buffer.
  ;; This will however give much slower performance
  ;; because you will lose the benefits of asynchronous memory transfers.
  ;; The write operation will wait for the previous rendering operation
  ;; to complete which serializes the update and draw stages.
  ;;
  ;; Again, all this double and triple buffering is only relevant
  ;; for streamed vertex data that changes each frame.
  
  (let ((buffer (make-instance 'buffer-object
			       :target target
			       :binding-layout binding-layout
			       :fn-bind (get-fn-bind target binding-layout)
			       :mapped-persistent mapped-persistent)))
	;; (gl-type (foreign-enum-value '%gl:enum type)) ; this as an enum/int, pass to %gl functions
	;; (type-size (foreign-type-size (type-cffi-to-gl type)))) ; convert attrib-type to glenum type to get size
    
    (with-slots (fn-bind
		 buffers
		 count-buffers
		 size-buffer
		 ptrs-buffer)
	buffer
      
      (cond ((eq buffering :single) (setf count-buffers 1))
	    ((eq buffering :double) (setf count-buffers 2))
	    ((eq buffering :triple) (setf count-buffers 3))
	    ((eq buffering :quad)   (setf count-buffers 4))
	    (t ;; default to single?
	     (error (format nil "[init-buffer-object] Unknown argument: ~a" buffering))))
      
      (setf size-buffer size)
      (setf buffers (make-array count-buffers :initial-contents (gl:gen-buffers count-buffers)))
      (setf ptrs-buffer (make-array count-buffers :initial-element (null-pointer)))

      (map-persistent buffer data usage)

      ;; Unbind...
      
      buffer)))

(defun get-fn-bind (target binding-layout)
  (cond ((or (eq target :draw-indirect-buffer)
	     (eq target :element-array-buffer)
	     (eq target :texture-buffer))
	  (lambda (buffer)
	    (%gl:bind-buffer target
			     buffer)))
	 (t
	  (lambda (buffer)
	    (%gl:bind-buffer-base target
				  binding-layout
				  buffer)))))

(defun map-persistent (buffer data usage)

  (with-slots (target
	       fn-bind
	       buffers
	       count-buffers
	       size-buffer
	       ptrs-buffer
	       mapped-persistent)
      buffer
    
    ;; If map persistent, get the pointer to the pinned memory
    ;; Otherwise check if there is data and upload to the GPU
    ;;
    ;; Add assertion for data?
    ;;
    ;; * glBufferStorage - only alloc once
    ;; * glBufferData - alloc/load/orphan data as needed
    ;; * glBufferSubData - load partial data as needed
    (if mapped-persistent

	(dotimes (i count-buffers)
	  (funcall fn-bind (aref buffers i))
	  (setf (aref ptrs-buffer i) (map-buffer-range target
						    size-buffer
						    :data data)))


	(progn
	    (dotimes (i count-buffers)
	      (funcall fn-bind (aref buffers i))
	      (if data	   
		  ;; Currently only used by single element-array-buffer
		  ;; REFACTOR: bind, iterate
		  (cffi-sys:with-pointer-to-vector-data (ptr data)
		    (%gl:buffer-data target
				     size-buffer
				     ptr
				     usage))
		  (%gl:buffer-data target
				   size-buffer
				   (null-pointer)
				   usage)))))))

(defun map-buffer-range (target
			 size
			 &key
			   (data nil)
			   (offset 0))  
  (if data
      (cffi-sys:with-pointer-to-vector-data (ptr data)
	(%gl:buffer-storage target
			    size
			    ptr
			    *map-buffer-range-access*))
      (%gl:buffer-storage target
			  size
			  (null-pointer)
			  *map-buffer-range-access*))
  (%gl:map-buffer-range target
			offset
			size
			*map-buffer-range-access*))

(defun update-binding-buffer (buffer index)
  (with-slots (fn-bind buffers) buffer
    (funcall fn-bind (aref buffers index))))

(defun clean-up-buffer-object (buffer)
  ;; For each boa: bind->unmap->delete
  (loop 
     :for b :across (buffers buffer)
     :do (progn
	   ;; (format t "[clean-up-buffer-object] Cleaning ~a: ~a~%" buffer b)
	   (funcall (fn-bind buffer) b)
	   (when (mapped-persistent buffer)
	     (gl:unmap-buffer (target buffer)))
	   (funcall (fn-bind buffer) 0)))
  (gl:delete-buffers (buffers buffer))
  (format t "[clean-up-buffer-object] Cleaned-up ~a~%" buffer))

(defun set-bo-draw-indirect (bo-indirect
			     count
			     prim-count
			     first-index
			     base-vertex
			     base-instance)

  ;; count: the number of elements to be rendered.
  ;; type: the type of the values in indices. Must be one of GL_UNSIGNED_BYTE, GL_UNSIGNED_SHORT, or GL_UNSIGNED_INT.
  ;; indices: a byte offset (cast to a pointer type) into the buffer bound to GL_ELEMENT_ARRAY_BUFFER to start reading indices from.
  ;; primcount: the number of instances of the indexed geometry that should be drawn.
  ;; basevertex: a constant that should be added to each element of indices​ when chosing elements from the enabled vertex arrays.
  ;; baseinstance: the base instance for use in fetching instanced vertex attributes.
  
  (with-slots (count-buffers ptrs-buffer) bo-indirect
    (loop
       :for i :from 0 :below count-buffers
       :for ptr := (aref ptrs-buffer i)
       :do (progn
	     (setf (mem-aref ptr :uint 0) count)             ; count (# of elements to render, i.e. number of indices to render from ebo)
	     (setf (mem-aref ptr :uint 1) prim-count)        ; prim-count (# of instances)
	     (setf (mem-aref ptr :uint 2) first-index)       ; first-index  (# offset into indices vbo)
	     (setf (mem-aref ptr :int  3) base-vertex)       ; base-vertex (added to element indices, i.e. stride)
	     (setf (mem-aref ptr :uint 4) base-instance))))) ; base-instance


(defun set-bo-element (bo-element data)
  ;; Better way to do this...
  (with-slots (count-buffers ptrs-buffer) bo-element
    (loop
       :for i :from 0 :below count-buffers
       :for ptr := (aref ptrs-buffer i)
       :do (dotimes (k 6)
	     (setf (mem-aref ptr :uint k) (aref data k))))))
