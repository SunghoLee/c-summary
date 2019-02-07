#include <stdio.h>

int g1;
int* g_ptr = &g1;
int kkk;

int swap(int* x, int* y, int* z){
	int tmp = *x;
    *z = 999;
	*x = *y;
	*y = tmp;
    g1 = 9;
    *g_ptr = g1;
    return *g_ptr;
}

int foo(int y){
    return y;
}

int main(int x){
	int k = 1;
	int v = swap(&k, &x, &kkk);
    int z = foo(v);
}

