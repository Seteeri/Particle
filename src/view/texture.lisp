(in-package :protoform.view)

(defun init-buffer-texture (program
			    target ; :texture-2d-array
			    name ; "texture-buffer"
			    type ; rgba8 :char
			    size ; * 4
			    count ; * w h
			    binding-layout
			    mapped-persistent
			    &key
			      (buffering 'triple)    ; 'single 'double 'triple, make this required
			      (usage :static-draw) ; dynamic-draw :stream-draw
			      (data nil))
  
  (let* ((buffer (init-buffer-object program
				     target
				     name
				     type
				     size
				     count
				     binding-layout
				     mapped-persistent
				     :buffering buffering
				     :usage usage
				     :data data))	
	 (texture (aref (buffers buffer) 0)))

    ;; https://www.opengl.org/discussion_boards/showthread.php/173917-samplerBuffer-example-needed
    ;; (gl:bind-texture :texture-buffer texture)

    ;; Set format type
    (%gl:tex-buffer :texture-buffer :rgba8 texture)

    buffer))
