(in-package :protoform.model)

(defcb-async
    load-file-cb
    'load-file
  (lambda (task)
    (funcall #'load-file-to-nodes)))
;; (defun load-file-cb (seq-key)
;;   (sb-concurrency:send-message *mb-async*
;; 			       #'load-file-to-nodes-ptree))