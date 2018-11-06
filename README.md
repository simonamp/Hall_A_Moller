# Hall_A_Moller
Hall A Moller analysis code

## Decoder 
The decoder is ~/analysis/raw-ntup/rawrd.com (located in the ~/bin/ as well).

This .com (command) file calls ~/analysis/raw-ntup/rawread_Linux.exe executable.

The executable is compiled from rawread.f via the "lnkr" shell script which does the following (assumes the user is logged into moller@hamoller - same credentials): 

gfortran rawread.f /site/coda/2.5/Linux/lib/libcoda.a -L /apps/cernlib/i386_rhel5/2005/lib -lpacklib -o rawread_test.exe

## Analysis
The analysis macro is in ~/paw/moller/asym18.kumac. It should be used as follows:

paw

paw> exec run run=15625

paw> exec lg_spectra run=15625

paw> exec time run=15625

paw> lcd mag_optim

paw> exec anpow_set e0=7.37 (E0 - beam energy in GeV)

paw> exec asym11 anpow=0.7735 gate=0.033 run=15625 (gate=0.033 for 30Hz helicity frequency and gate=0.0008 for 1kHz)
