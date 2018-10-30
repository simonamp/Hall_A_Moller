      REAL FUNCTION CALIB_F(IFLA)
C
C=== Fill histograms of the full energies
C===  for different combinations of hit maxima in L and R arms
C
      IMPLICIT REAL(A-H,O-Z),INTEGER(J-N)
      INCLUDE ?
C
      VECTOR IDD0(1)
      VECTOR PEDES(12)
      VECTOR CALIB(12)
C
      DATA ievv/0/
C
      id0=IDD0(1)
      ievv=ievv+1
C
C---    Find the max
C 
      CALIB_F=0.
      ener=0.
C
      earm1=0.
      imx1=0
      DO i=1,4
        a=IADC(i)-PEDES(i)
        IF(a.GT.earm1) THEN
          earm1=a
          imx1=i
        ENDIF
        ener=ener+a*CALIB(i)
      END DO
C
      earm2=0.
      imx2=0
      DO i=5,8
        a=IADC(i)-PEDES(i)
        IF(a.GT.earm2) THEN
          earm2=a
          imx2=i
        ENDIF
        ener=ener+a*CALIB(i)
      END DO
C
      IF(earm1.GT.200.and.earm2.GT.200.) THEN
          id=id0+imx1*10+imx2-4
          CALL HF1(id,ener,1.)
          CALIB_F=ener
      ENDIF
C
 999  CONTINUE
      END


