      SUBROUTINE RATEANGL(NORMAL,IRUN1,IRUN2,IRUN3,IRUN4)
C
C===     Calculate the target angle: from the rates
C===     The data are taken from a vector RESRUN
C===     Input: the run range IRUN1:IRUN2, for 90 degrees, the index=IRUN-IOFFSET 
C===                          IRUN3:IRUN4 - for the angle in question
C===                          NORMAL>0 normalize to the BCM
C
      IMPLICIT NONE
      INTEGER IRUN1,IRUN2,IRUN3,IRUN4,NORMAL
      VECTOR RESRUN(30,1000)
C
      INTEGER i,irun,ir,ir1,ir2,ir3,ir4,ioffset
      REAL    rate1,rate2,angl,anorm
C
      ir1=MOD(IRUN1,1000)
      ir2=MOD(IRUN2,1000)
      ir3=MOD(IRUN3,1000)
      ir4=MOD(IRUN4,1000)
      ioffset=IRUN2-ir2
C
      IF(ir1.LT.1.OR.ir1.GT.1000.OR.
     +   ir2.LT.1.OR.ir2.GT.1000) THEN
         WRITE(6,*) ' *** Error: the input runs/offset are out of '
     +        ,'range:',IRUN1,IRUN2,IOFFSET
         GO TO 999
      ENDIF
C
      rate1=0.
      rate2=0.
C
      i=0
      DO ir=ir1,ir2
         i=i+1
         anorm=1.
         IF(NORMAL.GT.0) anorm=AMAX1(RESRUN(5,ir),1.)
         rate1=rate1+RESRUN(3,ir)/anorm
      END DO
      rate1=rate1/REAL(i)
C
      i=0
      DO ir=ir3,ir4
         i=i+1
         anorm=1.
         IF(NORMAL.GT.0) anorm=AMAX1(RESRUN(5,ir),1.)
         rate2=rate2+RESRUN(3,ir)/anorm
      END DO
      rate2=rate2/REAL(i)
C
      IF(rate2.GT.0..AND.rate2.GT.rate1) THEN
         angl=ASIN(rate1/rate2)*180./3.1415
         WRITE(6,*) 'The angle=',angl
      ELSE
         WRITE(6,*) 'Can not calculate the angle from the rates:'
     +             ,rate1,rate2
      ENDIF
C
C
 999  CONTINUE
      END

