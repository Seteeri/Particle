(in-package :protoform.opengl)

;; count
;;     Specifies the number of elements to be rendered.
;; type
;;     Specifies the type of the values in indices. Must be one of GL_UNSIGNED_BYTE, GL_UNSIGNED_SHORT, or GL_UNSIGNED_INT.
;; indices
;;     Specifies a byte offset (cast to a pointer type) into the buffer bound to GL_ELEMENT_ARRAY_BUFFER to start reading indices from.
;; primcount
;;     Specifies the number of instances of the indexed geometry that should be drawn.
;; basevertex
;;     Specifies a constant that should be added to each element of indices​ when chosing elements from the enabled vertex arrays.
;; baseinstance
;;     Specifies the base instance for use in fetching instanced vertex attributes.

;; maybe remove size and just do count * type?
(defun init-buffer-draw-indirect (program
				  target
				  name ; 
				  type ; :int
				  size ; 5
				  count ; 1
				  binding-layout
				  mapped-persistent
				  &key
				    (buffering 'triple)     ; 'single 'double 'triple, make this required
				    (usage :static-draw) ; dynamic-draw :stream-draw
				    (data nil))
  
  (let ((buffer (init-buffer-object program
				    target
				    name
				    type
				    size
				    count
				    binding-layout
				    mapped-persistent
				    :buffering buffering
				    :usage usage
				    :data data)))
    buffer))

(defun set-bo-indirect (bo-indirect
			count
			prim-count
			first-index
			base-vertex
			base-instance)
  
  (with-slots (count-buffers ptrs-buffer) bo-indirect
    (loop
       :for i :from 0 :below count-buffers
       :for ptr := (aref ptrs-buffer i)
       :do (progn
	     (setf (mem-aref ptr :uint 0) count)        ; count (# of elements to render, i.e. number of indices to render from ebo)
	     (setf (mem-aref ptr :uint 1) prim-count) ; prim-count (# of instances)
	     (setf (mem-aref ptr :uint 2) first-index)        ; first-index  (# offset into indices vbo)
	     (setf (mem-aref ptr :int 3) base-vertex)         ; base-vertex (added to element indices, i.e. stride)
	     (setf (mem-aref ptr :uint 4) base-instance)))))      ; base-instance
