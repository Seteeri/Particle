(in-package :protoform.model)

(defun add-node-vcs ()
  (let ((node (pop *stack-i-nodes*)))
    
    (update-translation-node node (get-origin-from-node-pos *node-ptr-vcs*))
    (update-glyph-node node #\*)
    (update-transform-node node)

    (insert-vertex node *digraph-vcs* *vertices-vcs*)
    (spatial-trees:insert node *r-tree*)
    
    (insert-node node
      		 *node-ptr-vcs*
		 :out
		 *digraph-vcs*
		 *edges-vcs*)
    
    (advance-node-of-node *node-ptr-vcs*
			  node
  			  1.0)
    
    (send-node *node-ptr-vcs* nil)
    (send-node node)))
