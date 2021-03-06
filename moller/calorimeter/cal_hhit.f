      REAL FUNCTION CAL_HHIT(ICH1,ICH2,ITIM1,ITIM2,KHIT,JHIT)
C
C---   Hodoscope: = channel number for hit JHIT if there are KHIT hits
C                   0 - no hits, -1 - wrong number of hits
C
      IMPLICIT NONE
      INTEGER ICH1,ICH2,ITIM1,ITIM2,KHIT,JHIT
      INCLUDE ?
C
      INTEGER i,nh,ih(16),i1,i2
C
      nh=0
      i1=ICH1+16
      i2=ICH2+16
      CAL_HHIT=0.
      IF(ICH1.LE.0) GO TO 999
      IF(NTDC.EQ.0) GO TO 999
      IF(JHIT.LT.1.OR.JHIT.GT.32) GO TO 999
C      
      DO i=1,NTDC
         IF(ITCH(i).GE.i1.AND.ITCH(i).LE.i2) THEN
            IF(ITIM(i).GE.ITIM1.AND.ITIM(i).LE.ITIM2) THEN
               nh=nh+1
               ih(nh)=ITCH(i)-16
            ENDIF
         ENDIF
      ENDDO
      IF(nh.EQ.KHIT) THEN
         CAL_HHIT=ih(JHIT)
      ELSE IF(nh.GT.0) THEN
         CAL_HHIT=-1
      ENDIF
C
 999  CONTINUE
      END

