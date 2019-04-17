#include <stdio.h>

typedef struct S{
  int *p;
}S;

int f(S *a, S *b){
  *(a->p) = 42;
  return *(b->p);
}

int main(){
  int x = 1;
  S s = { &x };
  return f(&s, &s);
}
