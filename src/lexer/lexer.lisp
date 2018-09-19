(in-package :protoform)

;; These all read with some variation of
;; - list or string
;; - pop start token or not
;; - pop end token or not
;; - recursive

(defun read-until-non-whitespace (buffer len)
  (iter (while (< (file-position buffer) len))
        (for next-char = (peek-char nil buffer))
      (if (char-equal next-char #\ )
        (read-char buffer)
        (finish))))

;; single-line comment
;; i.e. read until newline
(defun read-for-single-line-comment (buffer len)
  (list (string (read-char buffer))
        (with-output-to-string (stream)
          (iter (while (< (file-position buffer) len))
                (for chr = (read-char buffer))
              (if (char-equal chr #\Newline)
                (finish) ;; Don't write NewLine
                (write-char chr stream))))))

;; list
(defun read-for-list (buffer len)
  ;; Recursive
  (read-char buffer) ; pop (, since we use . for the indicator
  (cons "."
        (iter (while (< (file-position buffer) len))
              (for chr = (peek-char nil buffer))
            (if (char-equal chr #\))
              (progn
                (read-char buffer) ; pop parenthesis but dont write
                (finish))
              ;; Technically either an atom or a list but in this context
              ;; it can be called an atom
              (let ((atom (lex-sexp buffer len)))
                (when atom  ; could be nil if this list is empty, e.g. (    )
                  (collect atom)))))))

;; atoms

;; string - the start and end delimiters the same so must read it or the loop
;; will terminate immediately
(defun read-for-atom-string (buffer len)
  (with-output-to-string (stream)
    (write-char (read-char buffer) stream)
    (iter (while (< (file-position buffer) len))
          (for first-char = (read-char buffer))
          (for next-char = (peek-char nil buffer))
        (when (and (char-equal first-char #\\)
                   (char-equal next-char #\"))
            ;; Backslash was written on the previous loop so only write "
            ;; to skip over it for next iteration.
            ;; In this iteration, don't check for "
            (write-char (read-char buffer) stream)
            (next-iteration))
        (write-char first-char stream)
        (when (char-equal first-char #\")
            (finish)))))

(defun read-for-atom (buffer len)
  ;; Read until space/NG or parenthesis
  (with-output-to-string (stream)
    (iter (while (< (file-position buffer) len))
          (for chr = (peek-char nil buffer))
        (if (or (char-equal chr #\Space)
                (not (graphic-char-p chr))
                (char-equal chr #\)))
          (finish)  ; don't pop end delimiter
          (write-char (read-char buffer) stream)))))

;; quote, backquote, comma, at
(defun read-for-quote (buffer len)
  ;; Filter whitespace after ', then read like an s-expression
  ;; We treat as a separate token like . for list
  ;; Seek until non-space encountered
  (cons (string (read-char buffer))
        (lex-sexp buffer len)))


;; main function		
(defun lex-sexp (buffer len)

  (iter (while (< (file-position buffer) len))
        (for chr = (peek-char nil buffer)) ; Should use peek instead?

    ;; Raise error for end delimiters?
    
    ;; For some of the macro dispatch characters they contain s-expressions
    ;; which we lex
    ;; Or turn all of them into subtrees?
    ;; subtree will either be an s-expression or a string (atom)
    ;; #x0001 = {"#x"}-{"0001"}

    (cond ((or (char-equal chr #\Space)
               (not (graphic-char-p chr)))
           (read-char buffer)
           ;;(format t "Skip space~%")
           (next-iteration))

          ((char-equal chr #\))
           (error "Found unexpected )"))
		   
          ; Handle |#
          ((char-equal chr #\|)
           (let ((c (read-char buffer)))
              (if (char-equal (peek-char nil buffer) #\#)
                (error "Found unexpected |#")
                (unread-char c buffer))))
                 
          ; ...raise for any token that gets popped off

          ((char-equal chr #\()
           (format t "Found list!~%")           
           (return-from lex-sexp
                        (read-for-list buffer len))) ; recursive

          ((char-equal chr #\;)
           (format t "Found single-line comment!~%")
           (return-from lex-sexp
                        (read-for-single-line-comment buffer len)))

          ;; For these, similar to atoms, but they need to skip whitespace
          ((char-equal chr #\')
           (format t "Found quote!~%")           
           (return-from lex-sexp
                        (read-for-quote buffer len))) ; recursive
          ((char-equal chr #\`)
           (format t "Found backquote!~%")
           (return-from lex-sexp
                        (read-for-quote buffer len)))
          ((char-equal chr #\,)
           (format t "Found comma!~%")
           (return-from lex-sexp
                        (read-for-quote buffer len)))
          ((char-equal chr #\@)
           (format t "Found @!~%")
           (return-from lex-sexp
                        (read-for-quote buffer len)))
                                                                        
          ;; atom - determine type of atom by first char
          (t
           (cond ((char-equal chr #\#) ; # non-terminating dispatching macro char
                  ;; need only handle cases where there is whitespace
                  (format t "Found sharpsign~%")
                  ;; Read next char if possible
                  (when (= (file-position buffer) len)
                     (error "Unexpected EOF"))
                  (return-from lex-sexp
                               (lex-sharpsign buffer len)))

                 ((char-equal chr #\") ; string
                  (format t "Found string~%")
                  (return-from lex-sexp
                               (read-for-atom-string buffer len)))

                 (t ; non-whitespaced atom like numbers, symbols etc.
                  ;; read until whitespace
                  (format t "Found non-whitespace atom: ~a~%" chr)
                  (return-from lex-sexp
                               (read-for-atom buffer len))))))))

(defun lex-path (path)
  (let ((stream (open path)))
                      ;:element-type 'unsigned-byte)))
    (iter (while (< (file-position stream) (file-length stream)))
          (collect (lex-sexp stream (file-length stream))))))
