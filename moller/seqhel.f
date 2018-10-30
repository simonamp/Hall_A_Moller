      SUBROUTINE TSTSEQ
C
      INTEGER i,j
      REAL x
      j=0
      DO i=1,24
         x=SEQHEL(j)
         j=j+1
         IF(j.GT.1) j=0
      ENDDO
C
      END
      REAL FUNCTION SEQHEL(IHEL)
C
C===     Initialize the helicity pseudo-random sequence 
C===     Input: IHEL - the current helicity
C===     Output: fill the seed
C===             return: 0 - seed not yet filled, =1 - IHEL - OK, -1 IHEL wrong  
C               
C
      IMPLICIT NONE
      INTEGER IHEL
C
C      INCLUDE ?
C
C      INTEGER IAND
C
      INTEGER i,j,jhel 
      INTEGER    mxseed
      PARAMETER (mxseed=24)
      INTEGER iseed,nseed,iseqhel(mxseed)
      SAVE iseed,nseed
      DATA nseed/0/
C
C     ------------------------------------------------------------------
C
      SEQHEL=0.
      IF(nseed.EQ.0) THEN
        iseed=0
        DO i=1,mxseed
           iseqhel(i)=0
        ENDDO
      ENDIF
C
      jhel=IAND(ihel,1)
      IF(nseed.LE.mxseed) THEN
        iseed=iseed*2+jhel
        nseed=nseed+1
      ENDIF
      
C
      WRITE(6,*) 'iseed=',iseed,nseed
C
      RETURN
      END
