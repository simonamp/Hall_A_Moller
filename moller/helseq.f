      REAL FUNCTION HELSEQ(IFL)
      IMPLICIT NONE
C
      INCLUDE ?
      INTEGER IFL
C
      REAL SEQHEL
C
      INTEGER ihel
      REAL x
C
      HELSEQ=1.
C
      ihel=jhel(1)
      HELSEQ=SEQHEL(ihel)
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
      INTEGER NEXTSEQ
C
      INTEGER i,j,khel
      INTEGER    mxseed
      PARAMETER (mxseed=24)
      INTEGER iseed,nseed,ish(mxseed)
      INTEGER m1,m3,m4,m24,msk,msk24
      PARAMETER (m1=1,m3=4,m4=8,m24=2**23,msk=m1+m3+m4+m24
     +          ,msk24=2**24-1)
      SAVE iseed,nseed
      DATA nseed/0/
C
C     ------------------------------------------------------------------
C
      SEQHEL=0.
      IF(nseed.EQ.0) THEN
        iseed=0
      ENDIF
      nseed=nseed+1
C
      khel=IAND(ihel,1)
C
      khel=NEXTSEQ(iseed,IHEL)
      IF(nseed.GT.mxseed+1) THEN
         IF(khel.NE.IHEL) THEN
C            WRITE(6,*) 'read,next,iseed=',nseed,IHEL,khel
C     +        ,IHEL-khel,iseed
            nseed=1
            SEQHEL=-1.
         ELSE
            SEQHEL=1.
         ENDIF
      ENDIF
C
      RETURN
      END
C
      INTEGER FUNCTION NEXTSEQ(ISEED,IHEL)
C
C---    Returns the next preudo-random number using the ISEED, compares
C---    with the observed number IHEL
C---    IHEL=2 
C
      IMPLICIT NONE
      INTEGER ISEED,IHEL
C
      INTEGER m1,m3,m4,m24,msk,msk24
      PARAMETER (m1=1,m3=4,m4=8,m24=2**23,msk=m1+m3+m4+m24
     +          ,msk24=2**24-1)
C
      INTEGER k
C
      NEXTSEQ=0
      IF(IAND(ISEED,m24).NE.0) NEXTSEQ=1
      k=IHEL
      IF(IHEL.EQ.2) k=NEXTSEQ
      IF(k.EQ.1) THEN
         ISEED=IEOR(ISEED,msk)*2+1
      ELSE
         ISEED=ISEED*2
      ENDIF
      ISEED=IAND(ISEED,msk24)      
C
      RETURN
      END
