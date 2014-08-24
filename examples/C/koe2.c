#include <stdio.h>
#define printInt(k) printf("%d\n", k)

/* print the first 12 factorials */

int main () {

  int i ;
  int k ;
  i = 1 ;
  k = i ;

  while (i < 13) {
    k = k * i ;
    i++ ;
    printInt (k) ;
    } ;

  return 0 ;

  }
