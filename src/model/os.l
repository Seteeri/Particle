#mman.h
#stat.h -> mod constants
#fcntl.h -> O_* constants

(de shm-open (NAME OFLAG MODE)
    (native "@" "shm_open" 'I NAME OFLAG MODE))

(de shm-unlink (NAME)
    (native "@" "shm_unlink" 'I NAME))

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

(de close (FD)
    (native "@" "close" 'I FD))


(de ftruncate (FD LENGTH)
    (native "@" "ftruncate" 'I FD LENGTH))
