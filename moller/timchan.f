      REAL FUNCTION TIMCHAN(ICH,ITIM1,ITIM2)
C
C---   Number of hits in a given channel within a given time range
C
      IMPLICIT NONE
      INTEGER ICH,ITIM1,ITIM2
      INCLUDE ?
C
      INTEGER i
C
      TIMCHAN=0.
      IF(ICH.LE.0) GO TO 999
      IF(NTDC.EQ.0) GO TO 999
C      
      DO i=1,NTDC
         IF(ITCH(i).EQ.ICH) THEN
            IF(ITIM(i).GE.ITIM1.AND.ITIM(i).LE.ITIM2) THEN
               TIMCHAN=TIMCHAN+1.
            ENDIF
         ENDIF
      ENDDO
C
 999  CONTINUE
      END

