echo == 1 ==
gcc -m32 -c *.c -Wall -Wextra -O2
echo == 2 ==
gcc -m32 *.o -o ../connect4
