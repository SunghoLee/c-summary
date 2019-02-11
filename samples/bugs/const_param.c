#include <stdio.h>

int foo(int y){
    return y + 1;
}

int main(){
    int x = 3;
    int z = foo(x);
    printf("%d\n", z);
    return 1;
}

