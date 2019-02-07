#include <stdio.h>

struct A {
    int b;
};

struct A k(struct A g){
    g.b = 9;
    return g;
}

int main(){
    struct A a;
    a.b = 3;
    a = k(a);
    printf("val: %d\n", a.b);
    return 1;
}

