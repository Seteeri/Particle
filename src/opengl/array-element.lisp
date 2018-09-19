(in-package :protoform.opengl)

;; To create new models during runtime
;; - modify static vbo (would be dynamic in this case)
;; - modify element buffer

;; To create new models during runtime
;; - modify static vbo (would be dynamic in this case)
;; - modify element buffer

(defun init-boa-element (program
			 target ; :element-array-buffer:
			 name ; "element-array-buffer"
			 type ; int
			 size ; 1 per vertex
			 count ; 6 ints per vertex (sq = 2 tris)
			 binding-layout
			 mapped-persistent
			 &key
			   (buffering 'triple)    ; 'single 'double 'triple, make this required
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
