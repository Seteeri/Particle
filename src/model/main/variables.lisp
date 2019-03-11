(in-package :protoform.model)

;; For now, determine these through view - maybe model can request from view?
;; GL_MAX_SHADER_STORAGE_BLOCK_SIZE = 134217728 = 134.217728 MBs
;; GL_MAX_TEXTURE_BUFFER_SIZE       = 134217728 = 134.217728 MBs
;;
;; Cache/compute will use cs-in
;; Step/raster will use vs-in

(defparameter *params-shm* (list '*shm-projview* *params-projview-shm*
				 '*shm-vertices* *params-vertices-shm*
				 '*shm-nodes* *params-nodes-shm*
				 '*shm-texture-glyphs* *params-texture-glyphs-shm*
				 '*shm-element* *params-element-shm*
				 '*shm-draw-indirect* *params-draw-indirect-shm*
				 '*shm-atomic-counter* *params-atomic-counter-shm*))

(defparameter *sym-to-shm* nil)

;;;;;;;;;;;;;

(defparameter *width* nil)
(defparameter *height* nil)
(defparameter *inst-max* nil)

(defparameter *queue-tasks-sync* nil)
(defparameter *queue-tasks-async* nil)
(defparameter *ht-timing-fn* nil)

(defparameter *stack-i-nodes* nil)
(defparameter *r-tree* nil)

(defparameter *digraph-main* nil)
(defparameter *node-ptr-main* nil)
(defparameter *vertices-main* (cons 0 nil))
(defparameter *edges-main*(cons 0 nil))

(defparameter *digraph-vcs* nil)
(defparameter *node-ptr-vcs* nil)
(defparameter *vertices-vcs* (cons 0 nil))
(defparameter *edges-vcs* (cons 0 nil))

(defparameter *offset-texel-textures* nil) ; sum of WxH
(defparameter *offset-bytes-textures* nil) ; sum of bytes
;; Textures - list of Texture instances wich store tex parameters
;; Use skip list? -> For now use vector
;; Hmm, when texture is removed need to recopy all (to "defragment")
;; (defparameter *textures* (make-array 64 :adjustable t :fill-pointer 0))
(defparameter *metrics* nil)

(defparameter *controller* nil)

(defparameter *sock-view* nil)
(defparameter *buffer-sock-ptr* nil)
(defparameter *buffer-sock-array* nil)
(defparameter *projview* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *vars-special*
  '(*params-shm*
    *sym-to-shm*
    *width*
    *height*
    *inst-max*

    *queue-tasks-sync*
    *queue-tasks-async*
    *ht-timing-fn*

    *stack-i-nodes*
    *r-tree*

    *digraph-main*
    *node-ptr-main*
    *vertices-main*
    *edges-main*

    *digraph-vcs*
    *node-ptr-vcs*
    *vertices-vcs*
    *edges-vcs*

    *offset-texel-textures*
    *offset-bytes-textures*
    
    *metrics*

    *controller*

    *sock-view*
    *buffer-sock-ptr*
    *buffer-sock-array*
    *projview*))
