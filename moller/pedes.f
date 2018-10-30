      REAL FUNCTION PEDES(DUMMY)
C
C===       Checks if the event comes from the pulser
C
C
      IMPLICIT REAL(A-H,O-Z),INTEGER(J-N)
      INCLUDE ?
C
      VECTOR IDD0(1)
C
      id0=IDD0(1)
      PEDES=0.
C
      DO i=NTDC,1,-1
         ich=ITCH(i)
         IF(ich.EQ.8) THEN
C     12 Nov 2014 r.p. Change time limit from 205 to 250
C           IF(ITIM(i).GE.195.AND.ITIM(i).LT.205) PEDES=1.
           IF(ITIM(i).GE.195.AND.ITIM(i).LT.250) PEDES=1.
           GO TO 999
         ENDIF
      END DO
C
 999  CONTINUE
      END





