      REAL FUNCTION A_SPREAD(IVAR)
C
C ===  Analyze the SCALER ntuple (one quad per entry)
C ==   Returns max-min of the values in the quad, normalized to the mean.
C       If mean is 0 - return 1E6
C ==    IVAR > 0 - scaler number (1-Left, 2-Right, 3-Coin, 4-accid, 5- BCM...)
C
      IMPLICIT NONE
      INTEGER IVAR
      INCLUDE ?
C
      INTEGER nel,iq
      REAL a,an,aa,sum,qmin,qmax
C
      A_SPREAD=1.E6
      IF(IFZUG.NE.0) GO TO 999  ! remove faulty trains (quads)
      IF(IVAR.LT.1.OR.IVAR.GT.NSCAL) THEN
         WRITE(6,*) ' *** Error: zug=',IZUG,' IVAR=',IVAR
     +              ,' is out of range ',1,NSCAL
         GO TO 999
      ENDIF
C
      nel=NELEM
      sum=0.
C
      qmin= 1.E20
      qmax=-1.E20
C      
      DO iq=1,nel
         a=JCNT(IVAR,iq)
         sum=sum+a
         qmin=MIN(qmin,a)
         qmax=MAX(qmin,a)
      ENDDO
C
      sum=sum/nel
C
      IF(sum.GT.0.) A_SPREAD=(qmax-qmin)/sum
C
 999  RETURN
      END

