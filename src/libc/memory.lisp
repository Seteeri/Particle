(in-package :protoform.libc)

(defcfun ("memcpy" c-memcpy) :void
  (dest :pointer)
  (src :pointer)
  (n :uint))

(defcfun ("memmove" c-memmove) :void
  (dest :pointer)
  (src :pointer)
  (n :uint))

(defcfun ("popen" c-popen) :int
  (command (:pointer :char))
  (type (:pointer :char)))

(defcfun ("pclose" c-pclose) :int
  (stream (:pointer :int)))
  
(defcfun ("shm_open" c-shm-open) :int
  (name (:pointer :char))
  (oflag :int)
  (mode :int))

(defcfun ("shm_unlink" c-shm-unlink) :int
  (name :string))

(defcfun ("ftruncate" c-ftruncate) :int
  (fd :int)
  (length :long)) ;; off_t = slong

(defcfun ("mmap" c-mmap) :pointer
  (addr :pointer)
  (length :long)
  (prot :int)
  (flags :int)
  (fd :int)
  (offset :long))

(defcfun ("msync" c-msync) :int
  (addr :pointer)
  (length :long)
  (flags :int))

(defcfun ("munmap" c-munmap) :int
  (addr :pointer)
  (length :long))

(defcfun ("close" c-close) :int
  (fd :int))		  

(defcfun ("mlock" c-mlock) :int
  (addr :pointer)
  (len :long))

(defcfun ("munlock" c-munlock) :int
  (addr :pointer)
  (len :long))

(defcfun ("mlockall" c-mlockall) :int
  (flags :int))

(defcfun ("munlockall" c-munlockall) :int)

(defcfun ("madvise" c-madvise) :int
  (addr :pointer)
  (length :long)
  (advise :int))

(defclass mmap ()
  ((path :accessor path :initarg :path :initform nil)
   (fd :accessor fd :initarg :fd :initform nil)
   (ptr :accessor ptr :initarg :ptr :initform nil)
   (size :accessor size :initarg :size :initform nil)
   (lock :accessor lock :initarg :lock :initform nil)))

(defun fill-mmap-2 (ptr-mmap arr)
  (declare (type (array (unsigned-byte 8)) arr))
  (cffi-sys:with-pointer-to-vector-data (ptr-arr arr)
    (c-memcpy ptr-mmap
	      ptr-arr
	      (length arr))))

(defun fill-mmap (ptr-mmap arr)
  (declare (type (array (unsigned-byte 8)) arr))
  (loop
     :for i :from 0 :below (length arr)
     :for c :across arr
     :do (setf (mem-aref ptr-mmap :uchar i) c)))

(defun clean-up-mmap (mmap &optional (unlink nil))
  (with-slots (path
	       fd
	       ptr
	       size
	       lock)
      mmap    
    (when lock
      (c-munlock ptr size))
    (c-munmap ptr size)
    (c-close fd)
    ;; only creator should unlink
    (when unlink
      (c-shm-unlink path))))

;; pass path
(defun init-mmap (path
		  size
		  create
		  &key
		    (mlock nil))
  
  (when create
    (c-shm-unlink path))
  
  (let* ((fd-shm (with-foreign-string (ptr path) ; #\/ required
		   (c-shm-open ptr
			       (if create
				   (logior #o100 #o2 #o200)
				   (logior #o2)) ;; O_CREAT | O_RDWR | O_EXCL
			       (logior #o0400 #o0200)))))  ;; S_IRUSR | S_IWUSR
    
    (when (= fd-shm -1)
      (error (get-str-errno)))
    
    ;; Configure the size of the shared memory segment - only on create
    (when create 
      (c-ftruncate fd-shm size))
    
    ;; Map shm to address space of process
    (let ((ptr-shm-base (c-mmap (null-pointer)
				size
				(logior #x1 #x2) ; PROT_READ PROT_WRITE,
				(logior #x01) ; ;#x2000) ; #x8000) ; MAP_SHARED, MAP_LOCKED, MAP_POPULATE
				fd-shm
				0)))
      
      (when (null-pointer-p ptr-shm-base)
	(error (get-str-errno)))

      ;; (c-madvise ptr-shm-base
      ;; 		 length-shm
      ;; 2) ;MADV_SEQUENTIAL
      ;; (c-madvise ptr-shm-base
      ;; 		 length-shm
      ;; 		 3) ;MADV_WILLNEED
      
      ;; (when mlock
      ;; 	(c-mlock ptr-shm-base size))
      
      ;; if (shm_base == MAP_FAILED) {
      ;;   printf("prod: Map failed: %s\n", strerror(errno));
      ;;   // close and shm_unlink?
      ;;   exit(1);
      ;; }
      
      (make-instance 'mmap
		     :path path
		     :fd fd-shm
		     :ptr ptr-shm-base
		     :size size))))

(defun mmap-file (path)
  (let ((fd (osicat-posix:open path (logior osicat-posix:o-rdonly))))
    (unwind-protect
         (let* ((size (osicat-posix:stat-size (osicat-posix:fstat fd)))
                (addr (osicat-posix:mmap (cffi:null-pointer) size
                                         (logior osicat-posix:prot-read)
                                         (logior osicat-posix:map-private)
                                         fd 0)))
           (values addr size))
      (osicat-posix:close fd))))

(defun munmap-file (addr size)
  (osicat-posix:munmap addr size))

(defmacro with-mmapped-file ((file addr size) &body body)
  (let ((original-addr (gensym "ADDR-"))
        (original-size (gensym "SIZE-")))
    `(multiple-value-bind (,addr ,size)
         (mmap-file ,file)
       (let ((,original-addr ,addr)
             (,original-size ,size))
         (unwind-protect
              (progn ,@body)
           (munmap-file ,original-addr ,original-size))))))

#|

/usr/include/bits/fcntl-linux.h
#define O_ACCMODE	   0003
#define O_RDONLY	     00
#define O_WRONLY	     01
#define O_RDWR		     02
#ifndef O_CREAT
# define O_CREAT	   0100	/* Not fcntl.  */
#endif
#ifndef O_EXCL
# define O_EXCL		   0200	/* Not fcntl.  */
#endif
#ifndef O_NOCTTY
# define O_NOCTTY	   0400	/* Not fcntl.  */
#endif
#ifndef O_TRUNC
# define O_TRUNC	  01000	/* Not fcntl.  */
#endif
#ifndef O_APPEND
# define O_APPEND	  02000
#endif
#ifndef O_NONBLOCK
# define O_NONBLOCK	  04000
#endif
#ifndef O_NDELAY
# define O_NDELAY	O_NONBLOCK
#endif
#ifndef O_SYNC
# define O_SYNC	       04010000
#endif
#define O_FSYNC		O_SYNC
#ifndef O_ASYNC
# define O_ASYNC	 020000
#endif
#ifndef __O_LARGEFILE
# define __O_LARGEFILE	0100000
#endif

#ifndef __O_DIRECTORY
# define __O_DIRECTORY	0200000
#endif
#ifndef __O_NOFOLLOW
# define __O_NOFOLLOW	0400000
#endif
#ifndef __O_CLOEXEC
# define __O_CLOEXEC   02000000
#endif
#ifndef __O_DIRECT
# define __O_DIRECT	 040000
#endif
#ifndef __O_NOATIME
# define __O_NOATIME   01000000
#endif
#ifndef __O_PATH
# define __O_PATH     010000000
#endif
#ifndef __O_DSYNC
# define __O_DSYNC	 010000
#endif
#ifndef __O_TMPFILE
# define __O_TMPFILE   (020000000 | __O_DIRECTORY)
#endif


/usr/include/linux/stat.h
#define S_IRWXU 00700
#define S_IRUSR 00400
#define S_IWUSR 00200
#define S_IXUSR 00100

#define S_IRWXG 00070
#define S_IRGRP 00040
#define S_IWGRP 00020
#define S_IXGRP 00010

#define S_IRWXO 00007
#define S_IROTH 00004
#define S_IWOTH 00002
#define S_IXOTH 00001

/* Protections are chosen from these bits, OR'd together.  The
   implementation does not necessarily support PROT_EXEC or PROT_WRITE
   without PROT_READ.  The only guarantees are that no writing will be
   allowed without PROT_WRITE and no access will be allowed for PROT_NONE. */

#define PROT_READ	0x1		/* Page can be read.  */
#define PROT_WRITE	0x2		/* Page can be written.  */
#define PROT_EXEC	0x4		/* Page can be executed.  */
#define PROT_NONE	0x0		/* Page can not be accessed.  */
#define PROT_GROWSDOWN	0x01000000	/* Extend change to start of
					   growsdown vma (mprotect only).  */
#define PROT_GROWSUP	0x02000000	/* Extend change to start of
					   growsup vma (mprotect only).  */

/* Sharing types (must choose one and only one of these).  */
#define MAP_SHARED	0x01		/* Share changes.  */
#define MAP_PRIVATE	0x02		/* Changes are private.  */
#ifdef __USE_MISC
# define MAP_TYPE	0x0f		/* Mask for type of mapping.  */
#endif

/* Other flags.  */
#define MAP_FIXED	0x10		/* Interpret addr exactly.  */
#ifdef __USE_MISC
# define MAP_FILE	0
# ifdef __MAP_ANONYMOUS
#  define MAP_ANONYMOUS	__MAP_ANONYMOUS	/* Don't use a file.  */
# else
#  define MAP_ANONYMOUS	0x20		/* Don't use a file.  */
# endif
# define MAP_ANON	MAP_ANONYMOUS
/* When MAP_HUGETLB is set bits [26:31] encode the log2 of the huge page size.  */
# define MAP_HUGE_SHIFT	26
# define MAP_HUGE_MASK	0x3f
#endif

/* Flags to `msync'.  */
#define MS_ASYNC	1		/* Sync memory asynchronously.  */
#define MS_SYNC		4		/* Synchronous memory sync.  */
#define MS_INVALIDATE	2		/* Invalidate the caches.  */

/* Flags for `mremap'.  */
#ifdef __USE_GNU
# define MREMAP_MAYMOVE	1
# define MREMAP_FIXED	2
#endif

/* Advice to `madvise'.  */
#ifdef __USE_MISC
# define MADV_NORMAL	  0	/* No further special treatment.  */
# define MADV_RANDOM	  1	/* Expect random page references.  */
# define MADV_SEQUENTIAL  2	/* Expect sequential page references.  */
# define MADV_WILLNEED	  3	/* Will need these pages.  */
# define MADV_DONTNEED	  4	/* Don't need these pages.  */
# define MADV_FREE	  8	/* Free pages only if memory pressure.  */
# define MADV_REMOVE	  9	/* Remove these pages and resources.  */
# define MADV_DONTFORK	  10	/* Do not inherit across fork.  */
# define MADV_DOFORK	  11	/* Do inherit across fork.  */
# define MADV_MERGEABLE	  12	/* KSM may merge identical pages.  */
# define MADV_UNMERGEABLE 13	/* KSM may not merge identical pages.  */
# define MADV_HUGEPAGE	  14	/* Worth backing with hugepages.  */
# define MADV_NOHUGEPAGE  15	/* Not worth backing with hugepages.  */
# define MADV_DONTDUMP	  16    /* Explicity exclude from the core dump,
                                   overrides the coredump filter bits.  */
# define MADV_DODUMP	  17	/* Clear the MADV_DONTDUMP flag.  */
# define MADV_HWPOISON	  100	/* Poison a page for testing.  */
#endif

|#
