#include <stdio.h>

struct Kakao {
    int menu;
    int spicy;
};

struct Apple {
    struct Kakao kakao;
    int price;
};

int main(){
    struct Apple app;
    app.kakao.spicy = 7;
    printf("value: %d\n", app.kakao.spicy);
    return app.kakao.spicy;
}
