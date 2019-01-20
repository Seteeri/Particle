(in-package :protoform.model)

;; Create macro

(defun add-node-callback (seq-key ptree queue)
  ;; Add node to pointer position
  ;; Move pointer right
  ;; Maybe have pointer appear below/above so edge will show

  (fmt-model t "add-node" "~a~%" seq-key)

  (ptree-fn 'add-node
	    '()
	    (lambda ()
	      (funcall #'add-node (second (reverse (second seq-key))))) ;; create macro/func for this
	    ptree)

  (sb-concurrency:enqueue 'add-node queue))

(defun backspace-node-callback (seq-key ptree queue)

  (fmt-model t "backspace-node" "~a~%" seq-key)

  (ptree-fn 'backspace-node
	    '()
	    (lambda ()
	      (funcall #'backspace-node seq-key))
	    ptree)

  (sb-concurrency:enqueue 'backspace-node queue))

(defun insert-node-newline-callback (seq-key ptree queue)
  
  (fmt-model t "insert-node-newline" "~a~%" seq-key)

  (ptree-fn 'insert-node-newline
	    '()
	    (lambda ()
	      (funcall #'insert-node-newline seq-key))
	    ptree)

  (sb-concurrency:enqueue 'insert-node-newline queue))

(defun eval-node-callback (seq-key ptree queue)
  
  (fmt-model t "eval-node" "~a~%" seq-key)

  (ptree-fn 'eval-node
	    '()
	    (lambda ()
	      (funcall #'eval-node seq-key))
	    ptree)

  (sb-concurrency:enqueue 'eval-node queue))
