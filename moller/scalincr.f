      REAL FUNCTION SCALINCR(ISC)
C
C===     Returns the increment in the scaler number ISC 
C===     (with respect to the previous scaler reading)
C
      IMPLICIT NONE
      INTEGER ISC           !  the scaler channel
      INCLUDE ?
C
      INTEGER jnew           !  current scaler value
     +       ,jold(32)       !  previous scaler value
     +       ,nsc            !  number of scalers
     +       ,i
C
      SAVE jold
      INTEGER   ini1
      PARAMETER (ini1=-1)
      DATA jold/32*ini1/
C
C     ------------------------------------------------------------------
C
      SCALINCR=0.
      IF(NSCA.LE.0) GO TO 999
C
      nsc=MIN(NSCA,32)
      IF(ISC.LT.1.OR.ISC.GT.nsc) GO TO 999
C      WRITE(6,FMT='(1X,I5,I4,32(I8,1X))') IDNEVT,ISC,(jold(i),i=1,14)
      jnew=ISCA(ISC)
C      WRITE(6,FMT='(1X,I5,I5,32(I8,1X))') IDNEVT,ISC,jnew,jold(ISC)
      IF(jold(ISC).GT.0) THEN
         SCALINCR=jnew-jold(ISC)
      ENDIF
      jold(ISC)=jnew
C
 999  RETURN
      END




