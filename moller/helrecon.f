      SUBROUTINE HELRECON
C
C---      Analyze the helicity sequence for the run and reconstruct the helicity mode
C---      Compares the exected helicity with the observed one
C
      IMPLICIT NONE
C      INTEGER IHEL           ! INPUT : the current helicity
C     +       ,ITICK          ! INPUT : the tick (time mark, 120Hz) of the entry
C     +       ,IHELPRED       ! OUTPUT: the predicted helicity
C     +       ,MODEHEL        ! OUTPUT: helicity mode: 0 - unknown, =1 - pseudorandom, 2 - toggle
C                                        +10 - double (oversampling: 2 gates for 1 helicity window) 
      INCLUDE 'inc/v_helw.inc'
C
      INTEGER  KHELPRED
      EXTERNAL KHELPRED 
C
      INTEGER i,k,m,nhel(2),i1,i1mx,i2,i2mx,idel,mxipri,jpair
     +       ,ibeg           ! start of the 1-st helicity cycle  in the register 
     +       ,ifin           ! end   of the last helicity cycle  in the register 
     +       ,ioversmp       !  oversampling ( =1 - normal - 1 readout per helicity window, = 2 - double, <5 )
     +       ,itoggle        !  >0 - toggle mode
     +       ,jhelpred       !  predicted helicity 
     +       ,ipair          !  =1 - 1st wave in the cycle, =2 - 2nd wave, =0 -?
     +       ,idir           !  +1 or -1 - direction of the reconstruction
C
      CHARACTER  cprtog(2)*16
      DATA    ipri/0/
      DATA    cprtog/'pseudorandom    '
     +              ,'toggle          '/
C
C     ------------------------------------------------------------------
C
C
C---      Find the longest non-interrupted sequence
C
      i1=1
      i1mx=1
      i2mx=0
C
      DO i=1,NCOWIN(1)
         IF(KTICWIN(i).GT.0.AND.KHELWIN(i).GT.0) THEN
            IF(i-i1+1.GT.i2mx-i1mx+1) THEN
               i1mx=i1
               i2mx=i
            ENDIF
         ELSE
            i1=i+1
         ENDIF
      ENDDO
C
      IF(i2mx-i1mx+1.LT.20) THEN
         WRITE(6,*) ' *** Error: uninterrupted helicity sequence'
     +      ,' is too short (1): from ',i1mx,' to ',i2mx
         GO TO 999
      ENDIF
C
C---      Find the oversampling mode (=1 - no oversampling)
C
C---        Find a subsequence of full windows 
      i1=i1mx
      DO i=i1mx,i2mx-1
         IF(KHELWIN(i).NE.KHELWIN(i+1)) THEN
            i1=i+1
            GO TO 10
         ENDIF
      ENDDO
 10   i1mx=i1
C
      i1=i2mx
      DO i=i2mx,i1mx+1,-1
         IF(KHELWIN(i-1).NE.KHELWIN(i)) THEN
            i1=i-1
            GO TO 20
         ENDIF
      ENDDO
 20   i2mx=i1
C
      nhel(1)=0
      nhel(2)=0
      DO i=i1mx,i2mx
         IF(KHELWIN(i).LE.2) nhel(KHELWIN(i))=nhel(KHELWIN(i))+1
      ENDDO
C
      IF(i2mx-i1mx+1.LT.20) THEN
         WRITE(6,*) ' *** Error: uninterrupted helicity sequence'
     +      ,' is too short (2): from ',i1mx,' to ',i2mx
         GO TO 999
      ENDIF
      IF(nhel(1).LT.8.OR.nhel(2).LT.8) THEN
         WRITE(6,*) ' *** Error: uninterrupted helicity sequence'
     +      ,' too few different helicities :',nhel
         GO TO 999
      ENDIF
C
      ioversmp=999
      i1=i1mx
      DO i=i1mx+1,i2mx
         IF(KHELWIN(i1).NE.KHELWIN(i)) THEN
            ioversmp=MIN(ioversmp,i-i1)
            i1=i
         ENDIF
      ENDDO
C
      k=MOD(i2mx-i1mx+1,ioversmp)
      IF(k.GT.0) THEN
         WRITE(6,*) ' *** Error: wrong oversampling period in the'
     +          ,' selected sequence:',ioversmp,i1mx,i2mx
     +          ,' period adjusted'
         i2mx=i2mx-k
      ENDIF
C
      IF(ioversmp.GT.4) THEN
         WRITE(6,*) ' *** Error: wrong oversampling :',ioversmp
         GO TO 999
      ENDIF
      IF(i2mx-i1mx+1.LT.ioversmp*20) THEN
         WRITE(6,*) ' *** Error: uninterrupted helicity sequence'
     +      ,' is too short (3): from ',i1mx,' to ',i2mx
     +      ,' oversampling=',ioversmp
         GO TO 999
      ENDIF
C
C---       Is it the toggle mode?
C
      itoggle=1
      DO i=i1mx,i2mx-ioversmp,ioversmp
         IF(KHELWIN(i).EQ.KHELWIN(i+ioversmp)) THEN
            itoggle=0
            GO TO 30
         ENDIF
      ENDDO
 30   CONTINUE
C
C---    Find the beginning of a cycle (1 cycle contains 2 windows with opposite helicities)
C---      In the toggle mode it can be any window beginning
C---      In pseudorandom mode one should find a border between 2 windows with the same helicities
C
      ibeg=i1mx     
C
      IF(itoggle.EQ.0) THEN
         DO i=i1mx,i2mx-ioversmp,ioversmp
            ibeg=i1mx+(i1mx-ibeg+ioversmp)
            IF(KHELWIN(i).EQ.KHELWIN(i+ioversmp)) GO TO 40
         ENDDO
 40      CONTINUE
      ENDIF
C     
      ifin=ibeg-1+((i2mx-ibeg+1)/(2*ioversmp))*2*ioversmp
C
      IF(itoggle.EQ.0.AND.ifin-ibeg+1.LT.ioversmp*60) THEN
         WRITE(6,*) ' *** Error: uninterrupted helicity sequence'
     +      ,' is too short (4) for pseudorandom: from ',ibeg,' to '
     +      ,ifin,' oversampling=',ioversmp
         GO TO 999
      ENDIF
C
      NCOWIN(2)=ioversmp
      NCOWIN(3)=itoggle
C      WRITE(6,*) 'ibeg,ifin,i1mx,i2mx ',ibeg,ifin,i1mx,i2mx 
C
C---     Check the helicity forward from the selected region
C
      DO idir=-1,1,2      ! forward and backward reconstruction
         IF(idir.EQ.1) THEN
            i1=ibeg
            i2=NCOWIN(1)
            idel=1
         ELSE
            i1=ifin-1
            i2=1
            idel=-1
         ENDIF
         DO jpair=1,2     ! 1-st and 2-nd window in a cycle
            ipair=1
            k=0
            DO i=i1,i2,idel
               k=k+1
               IF(k.GT.ioversmp) THEN
                  ipair=3-ipair
                  k=1
               ENDIF
               IF(ipair.EQ.jpair) THEN
                  IF(KPAIWIN(i).EQ.0) KPAIWIN(i)=ipair 
                  jhelpred=KHELPRED(i,itoggle,ioversmp,ipair,idir)
C                  WRITE(6,*) 'i=',idir,jpair,i
C                  jhelpred=KHELWIN(i)
                  IF(jhelpred.GT.0) THEN
                     IF(KHELWIN(i).GT.0) THEN
                        IF(KHELWIN(i).NE.jhelpred) THEN
                           WRITE(6,*) ' *** Error: wrong helicity '
     +                        ,'predicted'
     +                        ,' at the sample',i,' , hel=',KHELWIN(i)
     +                        ,jhelpred,' , pair=',ipair,idir
                           KFLAWIN(i)=2
                        ENDIF
                     ELSE
                        KHELWIN(i)=jhelpred
                        WRITE(6,*) ' --- Warning:     helicity defined'
     +                ,'   at the sample',i,' , hel=',KHELWIN(i),idir
                     ENDIF
                     IF(KRECWIN(i).GT.0) THEN
                        IF(KRECWIN(i).NE.jhelpred) THEN
                           WRITE(6,*) ' *** Error: different helicities'
     +                        ,' predicted'
     +                        ,' at the sample',i,' , hel=',KRECWIN(i)
     +                        ,jhelpred,' , pair=',ipair,idir
                        ENDIF
                     ELSE
                        KRECWIN(i)=jhelpred
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
      ENDDO
C
      k=0
      m=0
      DO i=1,NCOWIN(1)
         IF(KRECWIN(i).NE.KHELWIN(i)) THEN
            k=k+1
            write(6,*) 'i=',i,KRECWIN(i),KHELWIN(i)
         ENDIF
         IF(KFLAWIN(i).GT.0) m=m+1
      ENDDO
      IF(k.GT.0) WRITE(6,*) ' --- Warning: ',k,' helicity samples not'
     +              ,' properly recontructed'
C
      WRITE(6,1000) NCOWIN(1),m,cprtog(itoggle+1),ioversmp
 1000 FORMAT(/' === ',I6,'  helicity samples',5X,I6,'  rejected,'
     +      ,6X,' === mode: ',A16,' with oversampling of ',I2)
C
 999  RETURN
      END
C
      INTEGER FUNCTION KHELPRED(IS,ITOGGLE,IOVERSMP,IPAIR,IDIR)
C
C===     Predict the helicity of a given sample:
C
      IMPLICIT NONE
      INTEGER  IS            ! INPUT: the sample number
     +        ,ITOGGLE       ! INPUT: =1 - toggle mode, =0 - pseudorandom
     +        ,IOVERSMP      ! INPUT: oversampling ( 1 if there is no oversampling)
     +        ,IPAIR         ! INPUT: =1 - 1st wave in the cycle, =2 - 2nd wave, =0 -?
     +        ,IDIR          ! INPUT: =1 - forward, -1 - backward prediction
C
      INCLUDE 'inc/v_helw.inc'
C
      INTEGER i,j,jhel,jhelpred,ierr
     +       ,mbit(4)        ! the bits used for exclusive OR (XOR)
     +       ,kbit(4)        ! the helicities at the XOR bits
     +       ,is0            ! the smaple number for the "result" helicity
     +       ,is1            ! the sample number for the 1-st XOR window (17-th)
      DATA    mbit/17,22,23,24/
C
C     ------------------------------------------------------------------
C
      KHELPRED=0
C
      IF(ITOGGLE.NE.0) THEN     ! toggle - invert the adjacent helicity
         IF(IPAIR.EQ.2) THEN
            IF(IS-IOVERSMP.LT.1.AND.IDIR.EQ.-1) THEN 
               i=IS+IOVERSMP
            ELSE
               i=IS-IOVERSMP
            ENDIF
            IF(i.GE.1.AND.i.LE.NCOWIN(1)) THEN
               jhel=KHELWIN(i)
               IF(jhel.GT.0) KHELPRED=3-jhel
            ENDIF
         ELSE
            i=IS-IDIR*IOVERSMP*2
            IF(i.GE.1.AND.i.LE.NCOWIN(1)) THEN
               jhel=KHELWIN(i)
               IF(jhel.GT.0) KHELPRED=jhel
            ENDIF
         ENDIF
      ELSE                      ! pseudorandom
C
         IF(IPAIR.EQ.2) THEN    ! the 2-nd window in a cycle: take the hel. opposite to the 1-st window
            i=IS-IOVERSMP
            IF(i.GE.1.AND.i.LE.NCOWIN(1)) THEN
               jhel=KHELWIN(i)
               IF(jhel.GT.0) KHELPRED=3-jhel
            ENDIF
         ELSE                   ! the 1-st window in a cycle: predict the helicity
            ierr=0
            is0=IS
            is1=IS-mbit(4)*IOVERSMP*2
            IF(IDIR.EQ.-1) THEN
               is0=IS+mbit(4)*IOVERSMP*2
               is1=IS
            ENDIF
            jhel=KHELWIN(is0)
            DO j=1,4
               i=is1-(mbit(j)-mbit(4))*IOVERSMP*2
               IF(i.GT.0.AND.i.LE.NCOWIN(1)) THEN
                  kbit(j)=KHELWIN(i)
                  IF(kbit(j).LT.1) THEN
                     ierr=MAX(ierr,1)
                     kbit(j)=1
                     IF(IDIR.EQ.1) THEN
                        ierr=MAX(ierr,2)
                     ELSE
                        IF(j.NE.4) ierr=MAX(ierr,2)
                     ENDIF
                  ENDIF
               ELSE
                  ierr=MAX(ierr,3)
               ENDIF
C               IF(ierr.NE.0) WRITE(6,*) IS,is1,j,i,ierr
            ENDDO
C            IF(ierr.NE.0) WRITE(6,*) 'IS,ierr=',IS,ierr
            IF(ierr.LT.2) THEN
C
C---                XOR of 4 bits
C
               jhelpred=kbit(1)
               DO j=2,4
                  IF(jhelpred.EQ.kbit(j)) THEN
                     jhelpred=1
                  ELSE
                     jhelpred=2
                  ENDIF
               ENDDO
C
               IF(IDIR.EQ.1) THEN
                  KHELPRED=jhelpred
               ELSE
                  IF(ierr.NE.0) THEN
                     IF(jhel.EQ.jhelpred) THEN
                        KHELPRED=1
                     ELSE
                        KHELPRED=2
                     ENDIF
                  ELSE
                     KHELPRED=KHELWIN(is1)
                  ENDIF
               ENDIF
C
            ENDIF
         ENDIF
            
      ENDIF
C
C      WRITE(6,*) IS,KHELPRED,KHELWIN(IS),KFLAWIN(IS)
C     
 999  RETURN
      END








