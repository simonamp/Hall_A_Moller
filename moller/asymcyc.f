      REAL FUNCTION ASYMCYC(ID)
C
C===     Returns the scaler event number
C
      IMPLICIT NONE
      INTEGER ID
C
      INCLUDE ?
C
      INTEGER ievv,itick0
C
      DATA ievv/1/
      DATA itick0/0/
C
      IF(IDNEVT.EQ.1) ievv=1
      IF(NSCA.GT.0) THEN
         ievv=ievv+1
         IF(itick0.GT.0) THEN
            IF(ID.NE.0) CALL HF1(ID,itick-itick0+.1,1.) 
         ENDIF
         itick0=ITICK
      ENDIF
      ASYMCYC=ievv
C
      END
