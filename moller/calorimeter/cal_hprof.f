      REAL FUNCTION CAL_HPROF(ITIM1,ITIM2,ID)
C
C---   Hodoscope hit profile
C
      IMPLICIT NONE
      INTEGER ITIM1,ITIM2,ID
      INCLUDE ?
C
      INTEGER i,ich,nh(32),jch
C
      CAL_HPROF=1.
      IF(NTDC.EQ.0) GO TO 999
C      
      DO i=1,32
         nh(i)=0
      ENDDO
C
      DO i=1,NTDC
         ich=ITCH(i)
         jch=ich-16
         IF(jch.GE.1.AND.jch.LE.32) THEN
            IF(ITIM(i).GE.ITIM1.AND.ITIM(i).LE.ITIM2) THEN
               nh(jch)=nh(jch)+1
               IF(nh(jch).EQ.1) THEN 
                  CALL HF1(ID,jch+0.1,1.)
               ENDIF   
            ENDIF
         ENDIF
      ENDDO
C
 999  CONTINUE
      END

