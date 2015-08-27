
// static_call_main.c
#include <stdio.h>
#include "static_func.h"

int main(int argc, char ** argv) {

  int answer = getanswer();

  double fraca = signedfrac(3.2);
  double fracb = signedfrac(-2.3);

  tensor_double_3 vecx;
  vecx.d[0] = 1.0;
  vecx.d[1] = 2.0;
  vecx.d[2] = 3.0;
  double xlen = len2(vecx);

  tensor_double_3_3 diag42 = diag3(42);

  printf("answer should be 42;                  was %d\n", answer);
  printf("signedfrac 3.2 should yield 0.2;      was %f\n", fraca);
  printf("signedfrac -2.3 should yield -0.3;    was %f\n", fracb);
  printf("len2 {1.0,2.0,3.0} should yield 14.0; was %f\n", xlen);
  printf("diag3(42) should yield 42.0 and 0;    were\n"
         "  %f %f %f\n"
         "  %f %f %f\n"
         "  %f %f %f\n",
         diag42.d[0], diag42.d[1], diag42.d[2],
         diag42.d[3], diag42.d[4], diag42.d[5],
         diag42.d[6], diag42.d[7], diag42.d[8]);

  return 0;
}