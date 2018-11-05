(in-package :protoform.model)

;; For now, determine these through view - maybe model can request from view?
;; GL_MAX_SHADER_STORAGE_BLOCK_SIZE = 134217728 = 134.217728 MBs
;; GL_MAX_TEXTURE_BUFFER_SIZE       = 134217728 = 134.217728 MBs
;;
;; Cache/compute will use cs-in
;; Step/raster will use vs-in
(defparameter *params-projview-shm* (list :uniform-buffer
					  "projview"
					  "/protoform-projview"
					  (* (+ 16 16) 4)
					  0 0  ; cs-in (cache), vs-in (raster)
					  :triple
					  -1)) ; copy every frame

(defparameter *params-vertices-shm* (list :uniform-buffer
					  "vertices"
					  "/protoform-vertices"
					  (* 16 4)
					  1 1
					  :triple
					  0))

(defparameter *params-nodes-shm* (list :shader-storage-buffer
				       "nodes"
				       "/protoform-nodes"
				       (/ 134217728 4)
				       2 3
				       :triple
				       0))

;; (defparameter *glyphs-msdf-shm* (list :texture-buffer
;; 				      "glyphs-msdf"
;; 				      "/protoform-glyphs-msdf"
;; 				      16465920 ; size of all ppm glyphs
;; 				      -1 -1
;; 				      :triple
;; 				      0
;; 				      :rgba8))

(defparameter *params-texture-shm* (list :texture-buffer
					 "texture"
					 "/protoform-texture"
					 (/ 134217728 4)
					 -1 -1
					 :triple
					 0
					 :rgba8)) ; requires fmt type

(defparameter *params-element-shm* (list :element-array-buffer
					 "element"
					 "/protoform-element"
					 (* 4 6)  ; 4 bytes/int * 6 ints or indices
					 -1 -1
					 :triple
					 0))

(defparameter *params-draw-indirect-shm* (list :draw-indirect-buffer
					       "draw-indirect"
					       "/protoform-draw-indirect"
					       (* 4 6)  ; 6 ints/params
					       -1 -1
					       :triple
					       0))

(defparameter *params-atomic-counter-shm* (list :atomic-counter-buffer
						"atomic-counter"
						"/protoform-atomic-counter"
						(* 4 6)  ; 6 ints/params
						4 -1
						:triple
						0))

(defparameter *params-shm* (list :projview *params-projview-shm*
				 :vertices *params-vertices-shm*
				 :nodes *params-nodes-shm*
				 :texture *params-texture-shm*
				 :element *params-element-shm*
				 :draw-indirect *params-draw-indirect-shm*
				 :atomic-counter *params-atomic-counter-shm*))

(defconstant +size-struct-instance+ 208)

;; From Solaris Red
;; 220  50  47
(defparameter *color-default-ptr* (list  (coerce (/ 220 255) 'single-float)
					 (coerce (/ 50 255)  'single-float)
					 (coerce (/ 47 255)  'single-float)
					 (coerce (/ 255 255) 'single-float)
					 
					 (coerce (/ 220 255) 'single-float)
					 (coerce (/ 50 255)  'single-float)
					 (coerce (/ 47 255)  'single-float)
					 (coerce (/ 255 255) 'single-float)
					 
					 (coerce (/ 220 255) 'single-float)
					 (coerce (/ 50 255)  'single-float)
					 (coerce (/ 47 255)  'single-float)
					 (coerce (/ 255 255) 'single-float)
					 
					 (coerce (/ 220 255) 'single-float)
					 (coerce (/ 50 255)  'single-float)
					 (coerce (/ 47 255)  'single-float)
					 (coerce (/ 255 255) 'single-float)))

(defparameter *sock-view* nil)
(defparameter *buffer-sock-ptr* (foreign-alloc :unsigned-char :count 212992))
(defparameter *buffer-sock-array* (make-array 212992
					      :adjustable nil
					      :fill-pointer nil
					      :element-type '(unsigned-byte 8)))
(defparameter *handles-shm* nil)
(defparameter *projview* nil)
(defparameter *width* nil)
(defparameter *height* nil)
(defparameter *inst-max* nil)
(defparameter *digraph* nil)
(defparameter *offset-texel-textures* nil) ; sum of WxH
(defparameter *offset-bytes-textures* nil) ; sum of bytes
;; Textures - list of Texture instances wich store tex parameters
;; Use skip list? -> For now use vector
;; Hmm, when texture is removed need to recopy all (to "defragment")
;; (defparameter *textures* (make-array 64 :adjustable t :fill-pointer 0))
(defparameter *metrics* nil)
(defparameter *node-pointer* nil)
(defparameter *dpi-glyph* (/ 1 90))
(defparameter *scale-node* 0.008)

(defparameter *shm-projview* nil)
(defparameter *shm-nodes* nil)
(defparameter *shm-atomic-counter* nil)
(defparameter *shm-vertices* nil)
(defparameter *shm-element* nil)
(defparameter *shm-draw-indirect* nil)
(defparameter *shm-texture* nil)

(defparameter *controller* nil)
