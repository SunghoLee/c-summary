#include <stdio.h>

struct Apple {
    int rate;
    int price;
};

int assign(struct Apple* a){
    a->rate=7;
    a->price=9;
}
/*
int get(struct Apple a){
    return a.price;
}
*/
int main(){
    struct Apple app;
    int x;
    assign(&app);
    //x = get(app);
    x = app.rate;
    printf("Ret: %d\n", x);
    return x;
}
