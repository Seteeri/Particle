#mman.h
#stat.h -> mod constants
#fcntl.h -> O_* constants

(def 'O-CREAT (oct "0100"))
(def 'O-RDWR (oct "02"))
(def 'O-EXCL (oct "0200"))

(def 'S-IRUSR (oct "00400"))
(def 'S-IWUSR (oct "00200"))

(def 'PROT-READ (hex "1"))
(def 'PROT-WRITE (hex "2"))

(def 'MAP-SHARED (hex "01"))
(def 'MAP-LOCKED (hex "2000"))
(def 'MAP-POPULATE (hex "8000"))

#Maybe need wrapper for these?
#https://stackoverflow.com/questions/21311080/linux-shared-memory-shmget-vs-mmap
#https://stackoverflow.com/questions/13703079/does-mmap-need-shm-open
#(de shm-open (NAME OFLAG MODE)
    (native "@" "shm_open" 'I NAME OFLAG MODE))
#(de shm-unlink (NAME)
#    (native "@" "shm_unlink" 'I NAME))

(de memfd-create (NAME FLAGS)
    (native "@" "memfd_create" 'I NAME FLAGS))

(de mmap (ADDR LENGTH PROT FLAGS FD OFFSET)
    (native "@" "mmap" 'N ADDR LENGTH PROT FLAGS FD OFFSET))

(de msync (ADDR LENGTH FLAGS)
    (native "@" "mmap" 'I ADDR LENGTH FLAGS))

(de munmap (ADDR LENGTH)
    (native "@" "munmap" 'I ADDR LENGTH))

(de mlock (ADDR LEN)
    (native "@" "mlock" 'I ADDR LEN))
 
(de munlock (ADDR LEN)
    (native "@" "munlock" 'I ADDR LEN))

(de mlockall (FLAGS)
    (native "@" "mlockall" 'I FLAGS))

(de munlockall ()
    (native "@" "munlockall" 'I))

(de madvise (ADDR LENGTH ADVISE)
    (native "@" "madvise" 'I ADDR LENGTH ADVISE))


#string.h

(de memcpy (DEST SRC N)
    (native "@" "memcpy" NIL DEST SRC N))

(de memmove (DEST SRC N)
    (native "@" "memmove" NIL DEST SRC N))


#stdio

(de popen (COMMAND TYPE)
    (native "@" "popen" 'I COMMAND TYPE))

(de pclose (STREAM)
    (native "@" "pclose" 'I STREAM))


#unistd

(de c-close (FD)
    (native "@" "close" 'I FD))


(de ftruncate (FD LENGTH)
    (native "@" "ftruncate" 'I FD LENGTH))
