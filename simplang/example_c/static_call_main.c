
// static_call_main.c
#include <stdio.h>
#include "static_func.h"

int main(int argc, char ** argv) {

  int answer = getanswer();

  double fraca = signedfrac(3.2);
  double fracb = signedfrac(-2.3);

  printf("answer should be 42;                  was %d\n", answer);
  printf("signedfrac 3.2 should yield 0.2;      was %f\n", fraca);
  printf("signedfrac -2.3 should yield -0.3;    was %f\n", fracb);

  return 0;
}