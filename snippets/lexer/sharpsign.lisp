(in-package :protoform)

#|
Consists of +feature form
3 Options:
1. {a}-{#+}-{exp1}-{exp2}-{b}
2. {a}-{#+}-{b}                   #2 <- Use
         |
      {exp1}-{exp2}
3. {a}-{#+}-{exp2}                ; Not visually cohesive
        |
      {exp1}
      
Implement this as an option later. Same goes with other sharpsigns
like multicomment:
(... ("" "blah") ...)
or
(... "" "blah" ...)

They all use s-expression syntax so should just parse like an s-expression

When more than 1 atom or list, use a list, e.g. ("#+" "nil" ...)
When 1 atom, no list - return string, e.g "#x0001"
  rather than ("#x" "0001"), former is more readable and familiar
  However would create more branching when parsing
  Or instead when displaying, can keep them on one line instead.
  For char, it would be - ("#\" "a")
  Make an option
  
TODO:
- Make option for keeping head and body together
- Make option for displaying on one line or branching
|#

;; sharpsign

;; generic function
(defun read-for-sharpsign-sexp (buffer len)
  ;; pass fn instead of cons, how many chars to read
  (cons (let ((str (make-string 2)))
          (read-sequence str buffer)
          seq)
        (lex-sexp buffer len)))
        
(defun read-for-sharpsign-vector (buffer len)
  (read-for-sharpsign-sexp buffer len))

(defun read-for-sharpsign-plus-minus (buffer len)
  ;; First sexp
  ;; Second sexp
  ;; cons or append with read-for-sharpsign-sexp?...
  (list (let ((str (make-string 2)))            ;; #+ or #-
                    (read-sequence str buffer)
                    str)
        (lex-sexp buffer len)
        (lex-sexp buffer len)))

(defun read-for-sharpsign-pipe (buffer len)
  ;; Only place where comments are not valid
  (list (let ((str (make-string 2)))            ;; #|
                    (read-sequence str buffer)
                    str)
        (with-output-to-string (stream)
          (iter (while (< (file-position buffer) len))
                (for first-char = (read-char buffer))
                (for next-char = (peek-char nil buffer))
                (if (and (char-equal first-char #\|)
                         (char-equal next-char #\#))
                    (progn (read-char buffer) (finish))         ; dont write last two chars
                    (progn (write-char first-char stream)))))))

(defun read-for-sharpsign-until-whitespace (buffer len)
  (list (let ((str (make-string 2)))            ;; #?
                    (read-sequence str buffer)
                    str)
        (with-output-to-string (stream)
          (iter (while (< (file-position buffer) len))
                (for chr = (peek-char nil buffer))
              (if (or (char-equal chr #\Space)
                      (not (graphic-char-p chr))
                      (char-equal chr #\)))
                (finish)  ; don't pop end delimiter
                (write-char (read-char buffer) stream))))))

(defun read-for-sharpsign-char (buffer len)
  (list (let ((str (make-string 2)))            ;; #\
                    (read-sequence str buffer)
                    str)
        (with-output-to-string (stream)
          ;; Handle end delimiters first or it iteration will end prematurely
          ;; <Space> )
            
          ;; Any number of spaces is just a space, e.g. (list #\ )
                  
          (when (char-equal (peek-char nil buffer) #\))
            (write-char (read-char buffer) stream))

          (when (char-equal (peek-char nil buffer) #\ )
            (write-char (read-char buffer) stream))
                    
          ;; Below will find end delimiter - space or )
          (iter (while (< (file-position buffer) len))
                (for next-char = (peek-char nil buffer))
              (when (or (char-equal next-char #\ )
                        (char-equal next-char #\)))
                (finish))  ; don't pop end delimiter
              (write-char (read-char buffer) stream)))))

(defun read-for-sharpsign-single-quote (buffer len)
  (read-for-sharpsign-sexp buffer len))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lex-sharpsign (buffer len)
   ;; Caller should check buffer position prior

   (let* ((front-char (read-char buffer))  ; Remove # char temporarily
          (next-char (peek-char nil buffer)))

      ;; Put back char before processing
      (unread-char front-char buffer)

      (cond ((char-equal next-char #\\) ; chars
             (format t "Found sharpsign backslash expression~%")
             (return-from lex-sharpsign
                          (read-for-sharpsign-char buffer len)))

            ((char-equal next-char #\') ; function - similar to quote
             (format t "Found sharpsign single-quote expression~%")
             (return-from lex-sharpsign
                          (read-for-sharpsign-single-quote buffer len)))

            ((char-equal next-char #\() ; vectors
             (format t "Found sharpsign open parenthesis expression~%")
             (return-from lex-sharpsign
                          (read-for-sharpsign-vector buffer len)))

            ((char-equal next-char #\*) ; * n <> n <>
             t)
             
            ((char-equal next-char #\:) ; symbol
             t)
             
            ((char-equal next-char #\.) ; #.()
             (format t "Found sharpsign . expression atom~%")
             (return-from lex-sharpsign
                          (read-for-sharpsign-until-whitespace buffer len)))
               
            ((or (char-equal next-char #\B)   ; #b123
                 (char-equal next-char #\b))
             (format t "Found sharpsign b expression~%")
             (return-from lex-sharpsign
                          (read-for-sharpsign-until-whitespace buffer len)))
                          
            ((or (char-equal next-char #\O)   ; #o1234
                 (char-equal next-char #\o))
             (format t "Found sharpsign o expression~%")
             (return-from lex-sharpsign
                          (read-for-sharpsign-until-whitespace buffer len)))
                          
            ((or (char-equal next-char #\X)   ; #x001
                 (char-equal next-char #\x))
             (format t "Found sharpsign x expression atom~%")
             (return-from lex-sharpsign
                          (read-for-sharpsign-until-whitespace buffer len)))
                          
            ((or (char-equal next-char #\C)   ; #c() - list
                 (char-equal next-char #\c))
             (format t "Found sharpsign c expression atom~%")
             (return-from lex-sharpsign
                          (read-for-sharpsign-until-whitespace buffer len)))
                          
            ((or (char-equal next-char #\S)   ; #s()  - 
                 (char-equal next-char #\s))
             (format t "Found sharpsign s expression atom~%")
             (return-from lex-sharpsign
                          (read-for-sharpsign-until-whitespace buffer len)))
                          
            ((or (char-equal next-char #\P)   ; #P<<exp>>
                 (char-equal next-char #\p))
             (format t "Found sharpsign p expression atom~%")
             (return-from lex-sharpsign
                          (read-for-sharpsign-until-whitespace buffer len)))
             
            ((or (char-equal next-char #\+)
                 (char-equal next-char #\-))
             (return-from lex-sharpsign
                          (read-for-sharpsign-plus-minus buffer len)))
             
            ((char-equal next-char #\|) ; multiline comment
             (format t "Found multi-line comment!~%")
             (return-from lex-sharpsign
                          (read-for-sharpsign-pipe buffer len)))

            ;; For R,A,#,=,(
            ;; Must read next-char
            ((digit-char-p next-char)
              t)

            ;; error for ) <
            
            (t ; non-whitespaced sharpsign type
             (format t "Found non-whitespace sharpsign atom~%")
             (return-from lex-sharpsign
                          (read-for-sharpsign-until-whitespace buffer len))))))
 
