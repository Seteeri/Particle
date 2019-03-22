(in-package :protoform.model)

(defmacro defcb-sync (name-fn id-fn fn)
  `(defun ,name-fn (seq-key)
     (fmt-model t (format nil "~a" (quote ,name-fn)) "~a~%" seq-key)
     (enqueue-task-sync (make-instance 'task
				       :id ,id-fn
				       :fn-play ,fn))))

(defmacro defcb-async (name-fn id-fn fn)
  `(defun ,name-fn (seq-key)
     (fmt-model t (format nil "~a" (quote ,name-fn)) "~a~%" seq-key)
     (enqueue-task-async (make-instance 'task
					:id ,id-fn
					:fn-play ,fn))))

(defcb-async
    add-node-ascii-cb
    'add-node-ascii
  (lambda (task)
    (funcall #'add-node-ascii (code-char (second (reverse (second seq-key)))))))

(defcb-async
    backspace-node-ascii-cb
    'backspace-node-ascii
  (lambda (task)
    (funcall #'backspace-node-ascii)))

(defcb-async
    add-node-tab-cb
    'add-node-tab
  (lambda (task)
    (funcall #'add-node-tab)))

(defcb-async
    add-node-newline-cb
    'add-node-newline
  (lambda (task)
    (funcall #'add-node-newline)))
  
(defcb-async
    eval-node-cb
    'eval-node
  (lambda (task)
    (funcall #'eval-node)))

(defun cut-node-cb (seq-key)
  (fmt-model t "cut-node" "~a~%" seq-key))

(defun copy-node-cb (seq-key)
  (fmt-model t "copy-node" "~a~%" seq-key))

(defun paste-node-cb (seq-key)
  (fmt-model t "paste-node" "~a~%" seq-key))

(defun show-node-ids-cb (seq-key)
  (fmt-model t "show-node-ids" "~a~%" seq-key))

(defcb-async
    load-file-cb
    'load-file
  (lambda (task)
    (funcall #'load-file-to-nodes)))

;;;;;;;;
;; Anims

(defcb-sync
    move-node-ptr-in-cb
    'move-node-ptr-in
  (lambda (task)
    (funcall #'move-node-ptr :in)))

(defcb-sync
    move-node-ptr-out-cb
    'move-node-ptr-out
  (lambda (task)
    (funcall #'move-node-ptr :out)))

;; (defun translate-node-ptr-left-cb (seq-event)
;;   (with-slots (model-matrix)
;;       *node-ptr-main*
;;     (translate-node-ptr seq-event
;; 		       (lambda (value-new) ; update fn
;; 			 (setf (vx3 (translation model-matrix)) value-new)
;; 			 (send-node *node-ptr-main*))
;; 		       (vx3 (translation model-matrix)) ; start
;; 		       (- (* 96 *scale-node*))          ; delta
;; 		       'move-pointer-x)))

;; (defun translate-node-ptr-right-cb (seq-event)
;;   (with-slots (model-matrix)
;;       *node-ptr-main*
;;     (translate-node-ptr seq-event
;; 		       (lambda (value-new)
;; 			 (setf (vx3 (translation model-matrix)) value-new)
;; 			 (send-node *node-ptr-main*))
;; 		       (vx3 (translation model-matrix))
;; 		       (* 96 *scale-node*)
;; 		       'move-pointer-x)))

(defun translate-node-ptr-up-cb (seq-event)
  (with-slots (model-matrix)
      *node-ptr-main*
    (translate-node-ptr seq-event
		       (lambda (value-new)
			 (setf (vy3 (translation model-matrix)) value-new)
			 (send-node *node-ptr-main*))
		       (vy3 (translation model-matrix))
		       (* +linegap+ *scale-node* 8)
		       'move-pointer-y)))

(defun translate-node-ptr-down-cb (seq-event)
  (with-slots (model-matrix)
      *node-ptr-main*
    (translate-node-ptr seq-event
		       (lambda (value-new)
			 (setf (vy3 (translation model-matrix)) value-new)
			 (send-node *node-ptr-main*))
		       (vy3 (translation model-matrix))
		       (- (* +linegap+ *scale-node* 8))
		       'move-pointer-y)))
