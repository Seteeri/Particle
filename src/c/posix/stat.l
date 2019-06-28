#{

struct stat {
    dev_t     st_dev;     /* ID of device containing file */
    ino_t     st_ino;     /* inode number */
    mode_t    st_mode;    /* protection */
    nlink_t   st_nlink;   /* number of hard links */
    uid_t     st_uid;     /* user ID of owner */
    gid_t     st_gid;     /* group ID of owner */
    dev_t     st_rdev;    /* device ID (if special file) */
    off_t     st_size;    /* total size, in bytes */
    blksize_t st_blksize; /* blocksize for file system I/O */
    blkcnt_t  st_blocks;  /* number of 512B blocks allocated */
    time_t    st_atime;   /* time of last access */
    time_t    st_mtime;   /* time of last modification */
    time_t    st_ctime;   /* time of last status change */
};
#144 bytes

}#

(de stat-2 (Path)
    (let (Buf (native "@" "malloc" 'N 144))
      (native "@" "stat" 'I Path Buf)
      #Convert before free
      (native "@" "free" Buf)))

(de fstat-2 (Fd)
    (let (Buf (native "@" "malloc" 'N 144))
      (native "@" "stat" 'I Fd Buf)
      #Convert before free
      (native "@" "free" Buf)))

(de lstat-2 (Path)
    (let (Buf (native "@" "malloc" 'N 144))
      (native "@" "stat" 'I Path Buf)
      #Convert before free
      (native "@" "free" Buf)))
