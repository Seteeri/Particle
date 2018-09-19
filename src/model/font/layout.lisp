(in-package :protoform.model)

(defun update-layout-chars (sl-chrs
			    metrics
			    dpi-glyph
			    scale-glyph)
  (let* ((metrics-space (gethash 32 metrics))
	 (cursor (vec3 0.0 0.0 0.0)))

    (skip-list:doskiplist (sl-chr sl-chrs)
       (with-slots (chr)
	   sl-chr

	 ;; 0.029 ms
	 (update-transform-chr cursor
			       (if (or (char-equal chr #\Tab) (char-equal chr #\Newline))
				   metrics-space
				   (gethash (char-code chr) metrics))
			       scale-glyph
			       dpi-glyph
			       sl-chr)

	 (when (and (char-equal chr #\Newline) t)
	   ;; After new chr, move down, reset x
	   (setf (vx3 cursor) 0.0)
	   (decf (vy3 cursor) (* 9.375 2.0 scale-glyph)))))))

(defun layout-char (metrics
		    cursor
		    ch
		    dpi-glyph
		    scale-glyph)

  ;; Load-file -> layout-text
  ;; Layout-text return a char
      
  (let* ((metrics-glyph (gethash (char-code ch) metrics))
	 (metrics-space (gethash 32 metrics))
	 (chr nil))
    
    ;; Use space for tabs for now
    	      
    (setf chr (init-chr cursor
			(if (or (char-equal ch #\Tab)
				(char-equal ch #\Newline))
			    metrics-space
			    metrics-glyph)
			scale-glyph
			dpi-glyph
			ch))
	
    (when (and (char-equal ch #\Newline) t)
      ;; After new chr, move down, reset x
      (setf (vx3 cursor) 0.0)
      (decf (vy3 cursor) (* 9.375 2.0 scale-glyph)))

    chr))
  
;; Problem - using z offset to determine blending is not robust enough
;; because depth buffer has limited precision - can only draw so many
;; glyphs in a row; easier to extend XY than Z

;; Storage:
;; Atlas
;; - One UV per glyph, multiple draw calls
;; - Important to stick to single draw call
;;   - For windows, will need to bind textures
;; Array
;; - Reuse UV, pass UV-Z, single draw call
;; - Adjust UV, adjust scale
;;
;; Drawing:
;; - Kerning
;;   - monospaced (fixed-width) fonts have no kerning -> problem solved
;;   - proportional fonts -> require a BVH
;;     - prevent overlap for the most part...
;; - Cannot move to multiple draw calls until mutli draw is available
;;
;; - Crop images but results in uneven tiles
