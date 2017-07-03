GCC="g++ -m64 -g --std=c++14 -pipe -Wall -O3 -ffast-math -fno-finite-math-only -march=native -mavx -fopenmp"

$GCC -o reference.out reference/reference.cpp
$GCC -o avx.out avx/avx.cpp
$GCC -o avx4.out avx4/avx4.cpp

