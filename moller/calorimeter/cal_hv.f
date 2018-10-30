      REAL FUNCTION CAL_HV(NCAL,ITIM1,ITIM2,ID)
C
C---   Calorimeter: HV dependence
C                   ID - histogram start up
C                   ITIM1,ITIM2 - time limits for the hodoscope
C
      IMPLICIT NONE
      INTEGER NCAL,ITIM1,ITIM2,ID
      INCLUDE ?
C
      VECTOR PEDES(36)
C
      INTEGER i,imx,irowmx

      INTEGER    mxcal
      PARAMETER (mxcal=25)
      REAL    enlg(mxcal)
      REAL    enpar,emax,etot
C
      CAL_HV=0.
      IF(NADC.LE.0) GO TO 999 
      IF(NTDC.EQ.0) GO TO 999
C
C
C---     Find the max module
C
      emax=0.
      etot=0.
      imx=0
      DO i=1,NCAL
         enlg(i)=(IADC(12+i)-PEDES(12+i))
         IF(enlg(i).GT.emax) THEN
            imx=i
            emax=enlg(i)
         ENDIF
         etot=etot+enlg(i)
      ENDDO
C
      IF(emax.LT.etot*0.98) GO TO 999
C
      CALL HF1(ID+imx,emax,1.)
C
      CAL_HV=emax
C
 999  CONTINUE
      END
