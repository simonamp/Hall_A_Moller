//*************************************************************************
// R. Michaels  July 29, 2002  modified (simplified) from the original by
// Eric Stangland 7-01 seqgen24.C
// G0 helicity predictor algorithm.
// This code generates a pseudorandom sequence of bits of length 16777215.
// After this the sequence repeats itself.
//*************************************************************************

#include <stdio.h>
#include <stdlib.h>

#define IB1 1	     // Bit 1 of Shift Register 000000000000000000000001
#define IB3 4	     // Bit 3 of Shift Register 000000000000000000000100
#define IB4 8	     // Bit 4 of Shift Register 000000000000000000001000
#define IB24 8388608 // Bit 24 of Shift Register 100000000000000000000000
#define MASK IB1+IB3+IB4+IB24	// 100000000000000000001101

void main()
{
	unsigned int iseed;    // initial value of 24-bit number
	int bit_counter = 0;   // count the number of bits generated

	FILE *f1;
	
	iseed = 11184810;  // Initial value of iseed; 101010101010101010101010

	// Creating output file of helicity pattern
	f1 = fopen("f1.txt", "w");

	while(bit_counter < 16777216)  // loop 16777216 to allow all 
	      // 1677215 bits to be printed to .txt files; the last bit 
	      // generated (bit 16777216) is where the pseudorandom 
	      // sequence begins to repeat itself
	{
          
	  // Following algorithm taken from "Numerical Recipes in C" 
	  // Press, Flannery, et al., 1988
               
		if(iseed & IB24)
		{
			iseed = ((iseed ^ MASK) << 1) | IB1;
                        fprintf(f1,"%d\n",1);
		}
		else
		{	
			iseed <<= 1;
                        fprintf(f1,"%d\n",0);
		}
		bit_counter++;
	}

	fclose(f1);
}


