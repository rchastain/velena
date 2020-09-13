gcc -m32 -c *.c -Wall -Wextra -O2
gcc -m32 *.o -o ../connect4
rm *.o
