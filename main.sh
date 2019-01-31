#taskset 0x1 
#sbcl --dynamic-space-size 4096 --eval "(protoform:main)"

# (width (/ 2560 2))     ; 1280
# (height 1600)          ; 1600
# (inst-max (expt 2 16))

sudo pkill -f sbcl
sudo pkill -f sbcl
sbcl --eval "(protoform:main (/ 2560 2) 1080 (expt 2 16))"
#sbcl --eval "(protoform:main (/ 2560 1) 1080 (expt 2 16))"
