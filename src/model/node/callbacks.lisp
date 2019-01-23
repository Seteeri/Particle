(in-package :protoform.model)

(defun add-node-cb (seq-key ptree queue)
  (fmt-model t "add-node" "~a~%" seq-key)
  (sb-concurrency:enqueue (list nil
				'add-node
				'()
				(lambda ()
				  (funcall #'add-node (second (reverse (second seq-key)))))) ; create aux fn for this
			  *queue-anim*))

(defun backspace-node-cb (seq-key ptree queue)
  (fmt-model t "backspace-node" "~a~%" seq-key)
  (sb-concurrency:enqueue (list nil
				'backspace-node
				'()
				(lambda ()
				  (funcall #'backspace-node)))
			  *queue-anim*))
  
(defun insert-node-newline-cb (seq-key ptree queue)
  (fmt-model t "insert-node-newline" "~a~%" seq-key)
  (sb-concurrency:enqueue (list nil
				'insert-node-newline
				'()
  				(lambda ()
  				  (funcall #'insert-node-newline)))
			  *queue-anim*))
  
(defun eval-node-cb (seq-key ptree queue)  
  (fmt-model t "eval-node" "~a~%" seq-key)
  (sb-concurrency:enqueue (list nil
				'eval-node
				'()
				(lambda ()
				  (funcall #'eval-node)))
			  *queue-anim*))
