      REAL FUNCTION TIMCHAN2(ITIM1,ITIM2,ICH2,ITIM3,ITIM4,ID)
C
C---   Number of hits in a given channel range(17-48) 
C---   within a given time range that is correlated with
C---   another channel and time interval (ICH2, ITIM3,ITIM4) 
C---   ICH is filled to histo with ID
C
      IMPLICIT NONE
      INTEGER ITIM1,ITIM2
      INTEGER ICH2,ITIM3,ITIM4,ID
      INCLUDE ?
C
      INTEGER i,k,ich,jch
C

      TIMCHAN2=0.
C      IF(ICH.LE.0) GO TO 999
C      IF(ICH2.LE.0) GO TO 999
      IF(NTDC.EQ.0) GO TO 999
C-- scan by tdc channels for hodoscope(17-48)

      IF(ICH2.EQ.0) THEN 
         TIMCHAN2=1.
      ENDIF
         
      DO i=1,NTDC
         IF(ITCH(i).EQ.ICH2) THEN
            IF(ITIM(i).GE.ITIM3.AND.ITIM(i).LE.ITIM4) THEN
               TIMCHAN2=TIMCHAN2+1.
C     WRITE (6,*) 'hit ',ITCH(i)
            ENDIF
         ENDIF
      ENDDO
C---  lookup a hit in another channel
      DO i=1,NTDC
         ich=ITCH(i)
         jch=ich-16
         IF(jch.GE.1.AND.jch.LE.32) THEN
            IF(ITIM(i).GE.ITIM1.AND.ITIM(i).LE.ITIM2) THEN
               IF(TIMCHAN2>0)  THEN
                  CALL HF1(ID,jch+0.1,1.)
               ENDIF
            ENDIF
         ENDIF
      ENDDO



C
 999  CONTINUE
      END

