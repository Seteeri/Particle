(in-package #:protoform.render)

;; (glfw:def-key-callback quit-on-escape (window key scancode action mod-keys)
;;   (declare (ignore window scancode mod-keys))
;;   (when (and (eq key :escape) (eq action :press))
;;     (glfw:set-window-should-close)))

(glfw:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (gl:viewport 0 0 w h))

;; (let* ((buffer-pixel (foreign-alloc :unsigned-char :count (* 2880 1920 4))))

;; (proc-ffmpeg (sb-ext:run-program "/usr/bin/ffmpeg"
;; 				      '("-r" "60" "-f" "rawvideo" "-pix_fmt" "rgba" "-s" "1280x720" "-i" "-"
;; 					"-threads" "0" "-preset" "fast" "-y" "-pix_fmt" "yuv420p" "-crf" "21" "-vf" "vflip" "output.mp4")
;; 				      :wait nil
;; 				      :input :stream
;; 				      :output t
;; 				      :error t))
;; (out-ffmpeg (sb-ext:process-output proc-ffmpeg))
;; (in-ffmpeg (sb-ext:process-input proc-ffmpeg)))

;;(%gl:read-pixels 0 0 width height :rgba :unsigned-byte buffer-pixel)
;; write to output
;; (mem-aref buffer-pixel :char 0)
;; (dotimes (i (* 2880 1920 4))
;; 	 (write-byte 0;;(mem-aref buffer-pixel :unsigned-char i)
;; 		     in-ffmpeg))

;;(sb-ext:process-close proc-ffmpeg)
