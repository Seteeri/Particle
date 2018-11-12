(in-package :protoform.model)

(defparameter *time-last* 0)

;; For now, determine these through view - maybe model can request from view?
;; GL_MAX_SHADER_STORAGE_BLOCK_SIZE = 134217728 = 134.217728 MBs
;; GL_MAX_TEXTURE_BUFFER_SIZE       = 134217728 = 134.217728 MBs
;;
;; Cache/compute will use cs-in
;; Step/raster will use vs-in

(defparameter *params-shm* (list :projview *params-projview-shm*
				 :vertices *params-vertices-shm*
				 :nodes *params-nodes-shm*
				 :texture *params-texture-shm*
				 :element *params-element-shm*
				 :draw-indirect *params-draw-indirect-shm*
				 :atomic-counter *params-atomic-counter-shm*))

;;;;;;;;;;;;;

(defparameter *width* nil)
(defparameter *height* nil)
(defparameter *inst-max* nil)

(defparameter *sock-view* nil)
(defparameter *buffer-sock-ptr* (foreign-alloc :unsigned-char :count 212992))
(defparameter *buffer-sock-array* (make-array 212992
					      :adjustable nil
					      :fill-pointer nil
					      :element-type '(unsigned-byte 8)))
(defparameter *projview* nil)

(defparameter *digraph* nil)
(defparameter *node-pointer* nil)

(defparameter *offset-texel-textures* nil) ; sum of WxH
(defparameter *offset-bytes-textures* nil) ; sum of bytes
;; Textures - list of Texture instances wich store tex parameters
;; Use skip list? -> For now use vector
;; Hmm, when texture is removed need to recopy all (to "defragment")
;; (defparameter *textures* (make-array 64 :adjustable t :fill-pointer 0))
(defparameter *metrics* nil)
(defparameter *dpi-glyph* (/ 1 90))
(defparameter *scale-node* 0.008)

(defparameter *controller* nil)
