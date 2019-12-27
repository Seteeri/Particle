#unistd

(de close-fd (Fd)
    (native "@" "close" 'I Fd))


(de ftruncate (Fd Length)
    (native "@" "ftruncate" 'I Fd Length))

#{
(de write-int-to-bytes (N)
    (list (& (>> 24 N) (hex "FF"))
	  (& (>> 16 N) (hex "FF"))
	  (& (>> 8  N) (hex "FF"))
	  (&  N        (hex "FF"))))

(de read-int-from-bytes (B1 B2 B3 B4)
    (| (>> -24 B1)
       (>> -16 B2)
       (>> -8  B3)
       B4))

(de write-short-to-bytes (N)
    (list (& (>> 8  N) (hex "FF"))
	  (&  N        (hex "FF"))))

(de read-short-from-bytes (B1 B2)
      (| (>> -8 B1)
      B2))
}#

(de unlink-fd (Pathname)
    (native "@" "unlink" 'I Pathname))
