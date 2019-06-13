gcc -march=native -O3 -fpic -c mathc.c
gcc -shared -o libmathc.so mathc.o
