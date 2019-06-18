#mman.h
#stat.h -> mod constants
#fcntl.h -> O_* constants

(def 'O-CREAT (oct "0100"))
(def 'O-RDWR (oct "02"))
(def 'O-EXCL (oct "0200"))

(def 'S-IRUSR (oct "00400"))
(def 'S-IWUSR (oct "00200"))

(def 'Prot-READ (hex "1"))
(def 'Prot-WRITE (hex "2"))

(def 'MAP-SHARED (hex "01"))
(def 'MAP-LOCKED (hex "2000"))
(def 'MAP-POPULATE (hex "8000"))

#https://stackoverflow.com/questions/21311080/linux-shared-memory-shmget-vs-mmap
#https://stackoverflow.com/questions/13703079/does-mmap-need-shm-open
(de memfd-create (Name Flags)
    (native "@" "memfd_create" 'I Name Flags))

(de mmap (Addr Length Prot Flags Fd Offset)
    (native "@" "mmap" 'N Addr Length Prot Flags Fd Offset))

(de msync (Addr Length Flags)
    (native "@" "mmap" 'I Addr Length Flags))

(de munmap (Addr Length)
    (native "@" "munmap" 'I Addr Length))

(de mlock (Addr Len)
    (native "@" "mlock" 'I Addr Len))
 
(de munlock (Addr Len)
    (native "@" "munlock" 'I Addr Len))

(de mlockall (Flags)
    (native "@" "mlockall" 'I Flags))

(de munlockall ()
    (native "@" "munlockall" 'I))

(de madvise (Addr Length Advise)
    (native "@" "madvise" 'I Addr Length Advise))


#string.h

(de memcpy (Dest Src N)
    (native "@" "memcpy" NIL Dest Src N))

(de memmove (Dest Src N)
    (native "@" "memmove" NIL Dest Src N))


#stdio

(de popen (Command Type)
    (native "@" "popen" 'I Command Type))

(de pclose (Stream)
    (native "@" "pclose" 'I Stream))


#unistd

(de c-close (Fd)
    (native "@" "close" 'I Fd))


(de ftruncate (Fd Length)
    (native "@" "ftruncate" 'I Fd Length))
