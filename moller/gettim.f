      REAL FUNCTION GETTIM(NCH,ID)
C
C=== Fills a histogram ID for the time hits in the channel NCH
C
      IMPLICIT NONE
      INTEGER NCH,ID
      INCLUDE ?
C
      VECTOR PEDES(12)
C
      LOGICAL HEXIST
C
      INTEGER ievv,ifirst,i,ich
C
      DATA ievv/0/
      DATA ifirst/1/
C
      GETTIM=1.
      ievv=ievv+1
C
      IF(ifirst.NE.0) THEN
         ifirst=0
         IF(ID.NE.0.AND.HEXIST(ID)) CALL HRESET(ID,'    ')
      ENDIF
C
      DO i=1,NTDC
         ich=ITCH(i)
         IF(ich.EQ.NCH) THEN
C            IF(iadc(1)+iadc(2)+iadc(3)+iadc(4)-270.GT.500) THEN
              CALL HF1(ID,ITIM(i)+0.1,1.)
C            ENDIF
         ENDIF
      END DO
C
 999  CONTINUE
      END

