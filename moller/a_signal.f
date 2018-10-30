      REAL FUNCTION A_SIGNAL(IVAR,NORM,IHEL,IPAIR,NPAIRS,ID,ID0)
C
C ===  Analyze the SCALER ntuple (one quad per entry)
C ==   Fills ID with signals (scaler values):
C ==    IVAR > 0 - scaler number (1-Left, 2-Right, 3-Coin, 4-accid, 5- BCM...)
C       NORM = 0 - not normalized
C            = 1 - normalized to BCM 
C           = -2 - not normalized, the minimum of the quad is returned 
C           = -1 - not normalized, the maximum of the quad is returned 
C       IHEL = 0 - all helicities
C            1/2 - select helicities: 0/1 for helmh<0 or 1/0 for helmh>0 
C      IPAIR = 0 - all pairs
C            > 0 - only the pair number
C     NPAIRS > 1 - accumulate NPAIRS of pairs before histogramming into ID
C            =-1 - take the difference with the prev. cycle
C         ID <>0 - histogram for sums of NPAIRS     
C        ID0 <>0 - histogram for each widnow (nelem per one zug - entry)     
C      Returns the signal average per quad
C
      IMPLICIT NONE
      INTEGER IVAR,NORM,IHEL,IPAIR,NPAIRS,ID,ID0
      INCLUDE ?
C
      LOGICAL HEXIST
      REAL A_BEAM
      EXTERNAL HEXIST,A_BEAM
C
      DOUBLE PRECISION dsig(2),dnorm(2)
C
      INTEGER iq,n,ifirst,nel,kpairs,mpairs,i,mhel
      REAL a,an,aa,sum,qmin,qmax
C
      DATA ifirst/1/
      DATA ipairs/0/


C
      IF(ifirst.NE.0) THEN
         ifirst=0
         IF(ID.NE.0.AND.HEXIST(ID)) CALL HRESET(ID,'    ')
         IF(ID0.NE.0.AND.HEXIST(ID0)) CALL HRESET(ID0,'    ')
      ENDIF
C
      A_SIGNAL=0.
      IF(IFZUG.NE.0) GO TO 999  ! remove faulty trains (quads)
      IF(IVAR.LT.1.OR.IVAR.GT.NSCAL) THEN
         WRITE(6,*) ' *** Error: zug=',IZUG,' IVAR=',IVAR
     +              ,' is out of range ',1,NSCAL
         GO TO 999
      ENDIF
C
      nel=NELEM
      sum=0.
      n=0
      kpairs=NPAIRS
      IF(NPAIRS.LT.1) kpairs=1
      IF(NPAIRS.EQ.-1) kpairs=2
      IF(ipairs.EQ.0) THEN
         DO i=1,2
            dsig(i)=0.D0
            dnorm(i)=0.D0
         ENDDO
      ENDIF
      mpairs=0
C
      qmin= 1.E20
      qmax=-1.E20
C      
      DO iq=1,nel
         mhel=JHEL(iq)
         IF(HELMH.GT.0) mhel=1-mhel
         IF(IHEL.EQ.0.OR.mhel.EQ.IHEL-1) THEN
            IF(IPAIR.EQ.0.OR.INT((iq+1)/2).EQ.IPAIR) THEN
               a=JCNT(IVAR,iq)
               an=1.
               IF(NORM.EQ.1) THEN
C                  an=JCNT(5,iq)
                  an=A_BEAM(iq)
               ENDIF
C
               IF(an.GT.0.01) THEN
                  aa=a/an
                  IF(ID0.NE.0) CALL HF1(ID0,aa,1.)
                  n=n+1
                  sum=sum+aa
                  qmin=MIN(qmin,aa)
                  qmax=MAX(qmin,aa)
               ENDIF
C
               i=1
               IF(jhel(iq).LT.1) i=2
               IF(ipairs.EQ.1.AND.NPAIRS.EQ.-1) THEN
                  i=3-i
               ENDIF
               dsig(i)=dsig(i)+DBLE(a)
               dnorm(i)=dnorm(i)+DBLE(an)
C
               mpairs=mpairs+1
            ELSE
               WRITE(6,*) iq,INT((iq+1)/2),IPAIR
            ENDIF
         ENDIF
      ENDDO
C
      IF(NORM.GE.0) THEN
         IF(n.GT.0) A_SIGNAL=sum/n
      ELSE IF(NORM.EQ.-1) THEN
         A_SIGNAL=qmax
      ELSE
         A_SIGNAL=qmin
      ENDIF
C
      ipairs=ipairs+mpairs
C      write(6,*) mpairs,ipairs
      IF(ipairs.GE.2*kpairs) THEN
         sum=-2.
         IF(dnorm(1)+dnorm(2).GT.0.D0) 
     +              sum=(dsig(1)+dsig(2))/(dnorm(1)+dnorm(2))
         IF(ID.NE.0) CALL HF1(ID,sum,1.)
         ipairs=0
      ENDIF
C
        open(1,file='signal.dat',status='unknown')
	write(1,10) A_SIGNAL

 999  RETURN

 10     format(2x,f10.6)
       close(1)

      END
C
      INCLUDE 'a_beam.f'

