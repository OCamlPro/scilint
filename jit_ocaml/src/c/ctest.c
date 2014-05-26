#include <stdlib.h>
#include <stdio.h>

int ctest(int i){
     char *v[1];
     v[0] = "";
     printf("We are in it boys\n");
     caml_startup(v);
     return -1;
}
