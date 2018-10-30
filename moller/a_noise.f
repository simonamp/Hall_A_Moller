      REAL FUNCTION A_NOISE(IVAR,NORM,IHEL)
C
C ===  Analyze the SCALER ntuple (one quad per entry)
C ==   Returns signal (scaler values) noise: diff between the same helicities (a-b)/(a+b):
C ==    IVAR > 0 - scaler number (1-Left, 2-Right, 3-Coin, 4-accid, 5- BCM...)
C       NORM = 0 - not normalized
C            = 1 - normalized to BCM 
C       IHEL = 1/2 - select helicities 0/1
C              
C
      IMPLICIT NONE
      INTEGER IVAR,NORM,IHEL
      INCLUDE ?
C
      INTEGER iq,n,nel
      REAL a,an,sum,b(2)
C
      A_NOISE=-2.
      IF(IHEL.LT.1.OR.IHEL.GT.2) THEN
         WRITE(6,*) ' *** IHEL=',IHEL,' should be 1 or 2'
         GO TO 999  
      ENDIF
      IF(IFZUG.NE.0) GO TO 999  ! remove faulty trains (quads)
      IF(NELEM.LT.4) GO TO 999  ! full trains are needed
      IF(IVAR.LT.1.OR.IVAR.GT.NSCAL) THEN
         WRITE(6,*) ' *** Error: zug=',IZUG,' IVAR=',IVAR
     +              ,' is out of range ',1,NSCAL
         GO TO 999
      ENDIF
C
      nel=NELEM
      sum=0.
      n=0
      DO iq=1,4
         IF(jhel(iq).EQ.IHEL-1) THEN
            a=JCNT(IVAR,iq)
            an=1.
            IF(NORM.EQ.1) THEN
               an=JCNT(5,iq)
            ENDIF
            IF(an.GT.0.01) THEN
               a=a/an
               IF(n.LT.2) THEN
                  n=n+1
                  b(n)=a
               ENDIF
            ENDIF
         ENDIF
      ENDDO
C
      IF(n.EQ.2) THEN
         sum=b(1)+b(2)
         IF(sum.GT.0.) THEN
            A_NOISE=(b(1)-b(2))/sum
         ENDIF
      ENDIF
C
 999  RETURN
      END

