//******************************************************************
//
// g0helicity.c    R. Michaels   July 29, 2002
//
// This code performs the following tasks as would be
// expected of an analysis code:
//
//   1. Reads the helicity info -- readHelicity()
//      In this case the helicity comes from a text file 
//      for test purposes, but one can imagine it coming
//      from the datastream.
//
//   2. Loads the shift register to calibrate it. 
//      The shift register is a word of NBIT bits of 0's and 1's.
//      After loading is done, helicity predictions are available
//      as global variables like "present_helicity".
//        -- loadHelicity(int)
//            returns 0  if loading not finished yet
//            returns 1  if loading finished (helicity available)
//
//   3. If the helicity is wrong, the code sets up recovery.
//      by setting global variable recovery_flag.
//
//  Other routines used:
//
//   ranBit(unsigned int& seed)  -- returns helicity based on seed value
//                                 also modifies the seed (arg)
//   getSeed()  -- returns seed value based on string of NBIT bits.
//
//******************************************************************

#define DEBUG 1   // Debug output (if =1)
#include <iostream>
#include <stdio.h>
#include <stdlib.h>

// Functions to perform  ----------------------------------

int readHelicity();
int loadHelicity();
int ranBit(unsigned int& seed);   
unsigned int getSeed();

// Global variables  --------------------------------------

#define NBIT 24
int hbits[NBIT];         // The NBIT shift register

int present_reading;     // present reading of helicity
int predicted_reading;   // prediction of present reading (should = present_reading)
int present_helicity;    // present helicity (using prediction)

#define NDELAY 2         // number of quartets between
                         // present reading and present helicity 
                         // (i.e. the delay in reporting the helicity)

int recovery_flag = 0;   // flag to determine if we need to recover
                         // from an error (1) or not (0)
unsigned int iseed;            // value of iseed for present_helicity
unsigned int iseed_earlier;    // iseed for predicted_reading


// *****************************************************
// main()
// This code displays the normal flow of data analysis.
// Data are skipped if we are calibrating the shift register.
// See also comments at top of file.
// *****************************************************

void main() {

  int iev=0;
  while (readHelicity()) {  // read data until no more data

    iev++;
    if (loadHelicity()) {

      if (present_reading != predicted_reading) {

	cout << "DISASTER:  The helicity is wrong !!"<<endl;
        recovery_flag = 1;  // ask for recovery

      }

    } else {
 
      cout << "Skipping event "<<iev<<endl;

    }
  }
}


// *****************************************************
// readHelicity()
// This reads the helicity from somewhere -- in this case
// a text file. 
// In a real analysis code this would be replaced by 
// something that fetches from decoding.
// *****************************************************

int readHelicity() {
  static char strin[100];
  static FILE *fd;
  static int isopen = 0;
  if ( !isopen ) {
    isopen = 1;
    fd = fopen("helicity.data","r");
    if (fd == NULL) {
      cout << "Error:  helicity.data not found."<<endl;  
      return 0;
    }
  }
  if (fgets(strin,100,fd) == NULL) return 0;
  sscanf(strin,"%d",&present_reading);
  if (DEBUG) cout << "present reading "<<present_reading<<endl<<flush;
  return 1;
}


// *************************************************************
// loadHelicity()
// Loads the helicity to determine the seed.
// After loading (nb==NBIT), predicted helicities are available.
// *************************************************************

int loadHelicity() {
  int i;
  static int nb;
  if (recovery_flag) nb = 0;
  recovery_flag = 0;
  if (nb < NBIT) {
      hbits[nb] = present_reading;
      nb++;
      return 0;
  } else if (nb == NBIT) {   // Have finished loading
      iseed_earlier = getSeed();
      for (i = 0; i < NBIT+1; i++) 
          predicted_reading = ranBit(iseed_earlier);
      iseed = iseed_earlier;
      for (i = 0; i < NDELAY; i++)
          present_helicity = ranBit(iseed);
      nb++;
      return 1;
  } else {      
      predicted_reading = ranBit(iseed_earlier);
      present_helicity = ranBit(iseed);
      if (DEBUG) {
        cout << "helicities  "<<predicted_reading<<"  "<<
              present_reading<<"  "<<present_helicity<<endl;
      }
      return 1;
  }

}


// *************************************************************
// This is the random bit generator according to the G0
// algorithm described in "G0 Helicity Digital Controls" by 
// E. Stangland, R. Flood, H. Dong, July 2002.
// Argument:
//        ranseed = seed value for random number. 
//                  This value gets modified.
// Return value:
//        helicity (0 or 1)
// *************************************************************

int ranBit(unsigned int& ranseed) {

  static int IB1 = 1;           // Bit 1
  static int IB3 = 4;           // Bit 3
  static int IB4 = 8;           // Bit 4
  static int IB24 = 8388608;    // Bit 24 
  static int MASK = IB1+IB3+IB4+IB24;

  cout << " hel ranseed "<< ((ranseed & IB24)>0) <<" "<<ranseed <<endl;
  if(ranseed & IB24) {    
      ranseed = ((ranseed^MASK)<<1) | IB1;
      return 1;
  } else  { 
      ranseed <<= 1;
      return 0;
  }

};



// *************************************************************
// getSeed
// Obtain the seed value from a collection of NBIT bits.
// This code is the inverse of ranBit.
// Input:
//       int hbits[NBIT]  -- global array of bits of shift register
// Return:
//       seed value
// *************************************************************


unsigned int getSeed() {
  int seedbits[NBIT];
  unsigned int ranseed = 0;
  if (NBIT != 24) {
    cout << "ERROR: NBIT is not 24.  This is unexpected."<<endl;
    cout << "Code failure..."<<endl;  // admittedly awkward, but
    return 0;                         // you probably won't care.
  }
  for (int i = 0; i < 20; i++) seedbits[23-i] = hbits[i];
  seedbits[3] = hbits[20]^seedbits[23];
  seedbits[2] = hbits[21]^seedbits[22]^seedbits[23];
  seedbits[1] = hbits[22]^seedbits[21]^seedbits[22];
  seedbits[0] = hbits[23]^seedbits[20]^seedbits[21]^seedbits[23];
  for (int i=NBIT-1; i >= 0; i--) ranseed = ranseed<<1|(seedbits[i]&1);
  ranseed = ranseed&0xFFFFFF;
  cout << "ranseed "<<ranseed<<endl;
  return ranseed;
}







