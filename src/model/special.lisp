(in-package :protoform.model)

(defparameter *time-gc-last* 0.0)

;; For now, determine these through view - maybe model can request from view?
;; GL_MAX_SHADER_STORAGE_BLOCK_SIZE = 134217728 = 134.217728 MBs
;; GL_MAX_TEXTURE_BUFFER_SIZE       = 134217728 = 134.217728 MBs
;;
;; Cache/compute will use cs-in
;; Step/raster will use vs-in

(defparameter *params-shm*
  (list '*shm-projview* *params-projview-shm*
	'*shm-vertices* *params-vertices-shm*
	'*shm-nodes* *params-nodes-shm*
	'*shm-texture-glyphs* *params-texture-glyphs-shm*
	'*shm-element* *params-element-shm*
	'*shm-draw-indirect* *params-draw-indirect-shm*
	'*shm-atomic-counter* *params-atomic-counter-shm*))
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; READ ONLY
(defparameter *width* nil)
(defparameter *height* nil)
(defparameter *inst-max* nil)

(defparameter *metrics* nil)
(defparameter *controller* nil)

(defparameter *thread-libinput*   nil)
(defparameter *thread-controller* nil)
(defparameter *thread-async*      nil)
(defparameter *thread-io*         nil)
(defparameter *thread-socket*     nil)

(defparameter *nodes-model* nil)
(defparameter *stack-i-nodes* nil)
(defparameter *rtree-model* nil)
(defparameter *r-tree* nil)
(defparameter *graphs* nil)
(defparameter *projview* nil)

(defparameter *mutex-sin* nil)
(defparameter *mutex-rt* nil)
(defparameter *mutex-graphs* nil)

;; Link to *graphs*
;; Make graph class
(defparameter *digraph-main* nil)
(defparameter *node-ptr-main* nil)
(defparameter *vertices-main* (cons 0 nil))
(defparameter *edges-main*(cons 0 nil))
(defparameter *digraph-vcs* nil)
(defparameter *node-ptr-vcs* nil)
(defparameter *vertices-vcs* (cons 0 nil))
(defparameter *edges-vcs* (cons 0 nil))

;; Move to task manager inst?
(defparameter *queue-tasks-sync* nil)
(defparameter *queue-tasks-async* nil)
(defparameter *ht-timing-fn* nil)
(defparameter *mb-async* nil)
(defparameter *mb-io* nil)

(defparameter *mutex-qts* nil)
(defparameter *mutex-qta* nil)
(defparameter *mutex-htf* nil)

;; Move to conn/socket inst?
;; Don't need locks...
(defparameter *sock-render* nil)
(defparameter *buffer-sock-ptr* nil)
(defparameter *buffer-sock-array* nil)

;; Used by pango
;; (defparameter *offset-texel-textures* nil) ; sum of WxH
;; (defparameter *offset-bytes-textures* nil) ; sum of bytes
;; Textures - list of Texture instances wich store tex parameters
;; Hmm, when texture is removed need to recopy all (to "defragment")
;; (defparameter *textures* (make-array 64 :adjustable t :fill-pointer 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *vars-special*
  '(*params-shm*

    *width*
    *height*
    *inst-max*

    *thread-libinput*
    *thread-controller*
    *thread-async*
    *thread-io*
    *thread-socket*
    
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
    
    *metrics*

    *controller*

    *sock-render*
    *buffer-sock-ptr*
    *buffer-sock-array*
    *projview*))
