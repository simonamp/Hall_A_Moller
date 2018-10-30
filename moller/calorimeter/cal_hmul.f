      REAL FUNCTION CAL_HMUL(ICH1,ICH2,ITIM1,ITIM2)
C
C---   Number of hits in a given channel within a given time range
C
      IMPLICIT NONE
      INTEGER ICH1,ICH2,ITIM1,ITIM2
      INCLUDE ?
C
      INTEGER i
C
      CAL_HMUL=0.
      IF(ICH1.LE.0) GO TO 999
      IF(NTDC.EQ.0) GO TO 999
C      
      DO i=1,NTDC
         IF(ITCH(i).GE.ICH1.AND.ITCH(i).LE.ICH2) THEN
            IF(ITIM(i).GE.ITIM1.AND.ITIM(i).LE.ITIM2) THEN
               CAL_HMUL=CAL_HMUL+1.
            ENDIF
         ENDIF
      ENDDO
C
 999  CONTINUE
      END

