      REAL FUNCTION SPIN(THETA)
C
C===      Calculates the longitudinal spin component for the injector angle THETA
C===      Input: THETA    - spin angle at the injector (w. filter) in degrees
C                SPIND(1) - the hall number (A=1.,B=2.,C=3.) 
C                SPIND(2) - energy per pass (2 linacs) in GeV
C                SPIND(3) - the number of passes
C
      IMPLICIT NONE
      REAL     THETA
C
      VECTOR SPIND(4)
C
      INTEGER ihall
      REAL a,gfac,am,b,pi,fac,preces,pass,ang
      DATA a/0.1125/             ! energy  injector/linac
      DATA gfac/0.00115965/      ! (g-2)/2
      DATA am/0.000511/          ! mass of electron
C
C     ------------------------------------------------------------------
C
      pi=ACOS(0.)*2.
      ihall=INT(SPIND(1)+.1)
      pass=SPIND(3)
      fac=SPIND(2)/am*gfac
      IF(ihall.EQ.1) THEN
         b=-1./2.4
      ELSE IF(ihall.EQ.2) THEN
         b=0.
      ELSE IF(ihall.EQ.3) THEN
         b= 1./2.4
      ENDIF
C
      preces=fac*(2.*pass**2-pass*(1.-2.*a+b)-a*(1.+b/2.))*pi
      ang=preces+THETA*pi/180.
      SPIN=COS(ang)
      IF(SPIND(4).GT.0.1) SPIN=ang*180./pi
C
      END
