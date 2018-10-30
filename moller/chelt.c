#include <stdio.h>

main()
{
  int i,j,n;
  int bit24, bit23, bit22, bit17, newbit;

  printf(" Start ..\n");

  bit24 = 1;
  bit23 = 1;
  bit22 = 0;
  bit17 = 0;
  newbit = ( bit24 ^ bit23 ^ bit22 ^ bit17 ) & 0x1;
  
  printf(" End  %d\n",newbit);
}
