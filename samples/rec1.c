#include <stdio.h>

int rec(int i){
    if(i==0)
        return 1;
    else
        return i+rec(i-1);
}

int main(){
    int x = rec(5);
    printf("Ret: %d\n", x);
    return x;
}
