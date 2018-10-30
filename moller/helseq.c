#include <stdio.h>
#include <stdlib.h>
#define NBIT 24
int hbits[NBIT];         // The NBIT shift register
int iseed;  // Random Seed

main(){
  int i,j;
  unsigned int getSeed();
  int nextHel(int);
  unsigned int kk;

  i=2;
  j=i^8;
  printf(" j=%d \n",j);
  j=4;
  j <<= 1;
  j <<= 1;
  printf(" j=%d \n",j);
  j=1;
  for (i=0;i<24;i++) {
    hbits[i]=j; 
    j=!j; 
    printf("%d",hbits[i]);
  };
 printf("\n");
 iseed=getSeed();
 printf(" Seed=%d\n",iseed);
 i=nextHel(2);
 printf(" next0=%d\n",i);
 for (j=0;j<100;j++) { 
   i=nextHel(i);
   printf(" %d next=%d\n",j,i);
 }
}

unsigned int getSeed() {
  int seedbits[NBIT];
  int i;
  unsigned int ranseed = 0;

  for (i=0; i < 20; i++) {
    seedbits[23-i] = hbits[i];
  }
  seedbits[3] = hbits[20]^seedbits[23];
  seedbits[2] = hbits[21]^seedbits[22]^seedbits[23];
  seedbits[1] = hbits[22]^seedbits[21]^seedbits[22];
  seedbits[0] = hbits[23]^seedbits[20]^seedbits[21]^seedbits[23];
  printf(" seedbits=%d %d %d %d \n",seedbits[0],seedbits[1],seedbits[2],seedbits[3]);
  for (i=NBIT-1; i >= 0; i--) ranseed = ranseed<<1|(seedbits[i]&1);
  ranseed = ranseed&0xFFFFFF;
  return ranseed;
}


int nextHel(int hRead)
{
#define IB1 1        // Bit 1 of Shift Register 000000000000000000000001
#define IB3 4        // Bit 3 of Shift Register 000000000000000000000100
#define IB4 8        // Bit 4 of Shift Register 000000000000000000001000
#define IB24 8388608 // Bit 24 of Shift Register 100000000000000000000000
#define MASK IB1+IB3+IB4+IB24   // 100000000000000000001101
  /*  Returns the next predicted helicity 
   iseed    the seed
   hRead     the current value, or 2 
  */
  int hPred;
            
  hPred = (iseed & IB24 ) ? 1 : 0;
  if ((hRead == 2 ? hPred : hRead) == 1)
    iseed=((iseed^MASK)<<1) | IB1;
  else
    iseed <<= 1;

  return hPred;
}
