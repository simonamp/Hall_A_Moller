      REAL FUNCTION ASYMMETP(ID)
C
C===     Asymmetry/polarization calculation 
C===     for the new setup 
C
      IMPLICIT NONE
      INTEGER ID            !  starting ID
C
      INCLUDE ?
C
      INCLUDE 'inc/crunpol.inc'           ! output COMMON
      INCLUDE 'inc/v_asym.inc'            ! auxill. vectors
C
      LOGICAL HEXIST
C
      DOUBLE PRECISION da(32,2),df(2),dsum2(32),dsumh2(32,2),de(2)
     +      ,ddb,dnrm,dds
C
      VECTOR IDD0(1)
      VECTOR ICHIS(10)
      VECTOR NNLIM(2)
      VECTOR BCMLIN(3)
C
      INTEGER jsca(64),nsumh(32,2),jdif(64),jdifold(64)
C
      INTEGER id0
      INTEGER ievv,ieva,ihelic,ierr,ierr1,i,nsc,iflip,nflip,k,jinc
     +       ,ncyclusf,ierr2,itick1,itick2,iticksc,iticksc0
     +       ,nhelcycl(2),jd3(2),jas,jab,idifchk,ihelicold
     +       ,jhel           !  helicity of the current cycle
     +       ,jhelold        !  helicity of the previous cycle, =0 - unknown
     +       ,jheloldo       !  helicity of the second to previous cycle, =0 - unknown
     +       ,jhelcyc        !  =1 for the 1-st 0.5sec of helicity, =2 - 2nd, =0 ?
     +       ,jnorm          !  normalization pointer
      REAL asym,err,fac,pol,epol,diffav(32,2),qq,asymbcm,angl1
      INTEGER ihelseq(2)     ! helicities in 2 previous cycles
C
      DATA ievv/0/
      DATA ieva/0/
      DATA ihelic/-1/
      DATA jsca/64*0/
      DATA nsumh/64*0/
      DATA ierr/0/
      DATA nflip/0/
      DATA jhelold/0/
      DATA jhelcyc/0/
      DATA iticksc0/0/
      DATA ncyclusf/0/
      DATA dsum2/32*0.D0/
      DATA dsumh2/64*0.D0/
      DATA nhelcycl/2*0/
      DATA idifchk/-1/
      DATA ihelseq/-1,-1/
      DATA ihelicold/-1/
C
C     ------------------------------------------------------------------
C
C===    Copy the parameters from vectors to the CRUNPOL COMMON'
C
      INCLUDE 'inc/v_asym_a.inc'          ! vector ==> output COMMON 
C
      angl1=TANGL
      IF(ABS(angl1).GT.90.) angl1=180.-angl1
C
      nsc=MIN(NSCA,32)
      id0=IDD0(1)
      ASYMMETP=1.
C
      IF(idifchk.EQ.-1) THEN
C
         DO i=1,100
            IF(HEXIST(ID+i-1)) CALL HRESET(ID+i-1,' ')
         END DO
C
         idifchk=1
         DO i=1,3
            k=0
C            WRITE(6,*) 'CASYM,EASYM', CASYM(i),EASYM(i)
            IF(CASYM(i).GT.100.) THEN
               k=1
               diffav(i,1)=CASYM(i)
               diffav(i,2)=CASYM(i)*0.4
            ENDIF
            IF(k.EQ.0) idifchk=0
C            WRITE(6,*) ' diffav ',diffav(i,1),diffav(i,2)
         END DO 
         WRITE(6,*) 'Start asymmetp.f, idifchk=',idifchk
      ENDIF
C
      IF(ITRIG(1).NE.0) THEN
         ieva=ieva+1
         IF(ieva.EQ.1) THEN
            itick1=ITICK
            itick2=ITICK
            ihelseq(2)=ihelseq(1)
            ihelseq(1)=ihelicold
            ihelic=ITRIG(6)
            ihelicold=ihelic
C            write(6,*) 'cyc=',ievv,ihelseq,ihelic
            IF(ihelic.EQ.ihelseq(1).AND.
     +         ihelic.EQ.ihelseq(2)) THEN
C !               ierr=1
C               NASYM(3)=NASYM(3)+1
C               WRITE(6,*) ' Helicity sequence error,at ev='
C     +              ,idnevt,ieva,ievv,ihelic,ihelseq
            ENDIF
         ELSE
            itick2=ITICK
            IF(ihelic.NE.ITRIG(6).AND.ierr.EQ.0) THEN
               ierr=1
               NASYM(3)=NASYM(3)+1
               WRITE(6,*) ' Helicity flip at ev=',idnevt,ieva,ievv
     +              ,ihelic,ITRIG(6),ihelseq
            ENDIF
         ENDIF
      ENDIF
C
      IF(NSCA.LE.0) GO TO 999
C
      ievv=ievv+1
C
      iflip=0
      ierr1=0
C
      iticksc=ITICK
      IF(iticksc0.NE.0) CALL HF1(ID+50,iticksc-iticksc0+.1,1.)
      IF(itick2.NE.0) CALL HF1(ID+51,iticksc-itick2+.1,1.)
      IF(itick1.NE.0) CALL HF1(ID+52,itick2-itick1+.1,1.)
C
      IF(ITRIG(6).EQ.1) THEN
         DO i=1,3
            CALL HF1(ID+53+i,
     +           ISCA(i)-jsca(i)-ISCA(i+16)+jsca(i+16)+0.1,1.)
         END DO
C         CALL HF1(ID+54,ISCA(3)-jsca(3)-ISCA(3+16)+jsca(3+16)+0.1,1.)
C         CALL HF2(ID+59,ISCA(3)-jsca(3)-ISCA(3+16)+jsca(3+16)+0.1
C     +        ,ISCA(3)-jsca(3)+0.1,1.)
C     +              ,ISCA(1)-jsca(2)-ISCA(1+16)+jsca(1+16)+0.1,1.)
      ENDIF
C
C---     Reset the ticks for the ADC triggers
C
      itick1=0
      itick2=0
C
      IF(ievv.LT.5) ierr1=1
      DO i=1,nsc
C         IF(ISCA(i).LT.jsca(i)) ierr1=2
      END DO
      IF(ierr1.EQ.2) NASYM(4)=NASYM(4)+1
      IF(ierr1.EQ.0.AND.
C     +     IABS(iticksc-iticksc0-5).GT.2) THEN
     +     IABS(iticksc-iticksc0-60).GT.5) THEN
         jhelold=0               ! erase the history 
         ierr1=1
         NASYM(12)=NASYM(12)+1
      ENDIF
C      IF(ierr1.EQ.0.AND.
C     +     ISCA(11)-jsca(11).NE.1) THEN
      IF(ierr1.EQ.0.AND.
     +      REAL(ISCA(4)-jsca(4)).GT.REAL(ISCA(3)-jsca(3))*0.2) THEN
C         ierr1=1
C         NASYM(5)=NASYM(5)+1
      ENDIF
      IF(ierr1.EQ.0.AND.
C     +   ISCA(5)-jsca(5).LT.30000) THEN
C         .OR.ISCA(5)-jsca(5).GT.80000) THEN
     +   ISCA(3)-jsca(3).LT.1) THEN
         ierr1=1
         NASYM(6)=NASYM(6)+1
      ENDIF
      IF(ierr1.EQ.0.AND.
     +     ieva.NE.NEVADC.AND.NEVADC.GE.0) THEN
         ierr1=1
         NASYM(7)=NASYM(7)+1
         WRITE(6,*) 'ieva=',ieva
      ENDIF
C
C      IF(ierr1.EQ.0.AND.ieva.GT.0.AND.ihelic.NE.ITRIG(6)) THEN
C         ierr1=1
C!         NASYM(8)=NASYM(8)+1
C         WRITE(6,*) ' Helicity flip at ev',idnevt,ievv,ihelic,ITRIG(6)
C      ENDIF
C
      IF(ierr1.EQ.0.AND.ieva.GT.0.AND.jhelold.EQ.0) THEN
         ierr1=1
         NASYM(8)=NASYM(8)+1
      ENDIF
C
C---     Learn the helicity of the current cycle 
C
      jhel=jhelold
      IF(jhel.EQ.0) ierr1=1
C
C---     Try to learn if this is the first or the 1/sec second cycle of 
C---       the given helicity
C
      IF(jhelcyc.EQ.0) THEN
C        WRITE(6,*) 'jhel,jhelold,jheloldo',jhel,jhelold,jheloldo
        IF(jheloldo.NE.0.AND.jhel.NE.0) THEN
            IF(jhel.NE.jheloldo) jhelcyc=1
         ENDIF
      ELSE
         jhelcyc=3-jhelcyc
         IF(jhel.NE.jheloldo.AND.jhelcyc.EQ.2) THEN
C            WRITE(6,*) 'Wrong cycle mark',jhelcyc,jhel,jhelold,jheloldo
         ENDIF
      ENDIF
C      IF(ierr1.EQ.0.AND.jhelcyc.NE.1) THEN
C         ierr1=1
C         NASYM(8)=NASYM(8)+1
C      ENDIF
C
C---     Store the helicity - it belongs to the NEXT scaler readout (cycle) 
C
      jheloldo=jhelold
      jhelold=1
      IF(ITRIG(6).EQ.1) jhelold=2
C
C      
C
      CALL HF1(ID+30,ievv+.1,REAL(jhel))
      CALL HF1(ID+99,ievv+.1,REAL(jhelcyc))
      CALL HF1(ID+53,ievv+.1,REAL(iticksc-iticksc0+.1))
C      IF(ierr1.EQ.0.AND.ieva.GT.0.AND.jhel.EQ.jhelold) THEN
C         ierr1=1
C         NASYM(9)=NASYM(9)+1
C      ENDIF
      IF(ierr1.EQ.0) THEN
         k=1
         DO i=3,4
            IF(IABS(ISCA(i)-jsca(i)-ISCA(i+16)+jsca(i+16)).GT.mxdiff)
     +          k=0
         END DO
         IF(k.EQ.0) THEN
            ierr1=1
            NASYM(9)=NASYM(9)+1
         ENDIF
      ENDIF
      ihelic=-1
C
C---   Time generator
C
      IF(ierr1.EQ.0) THEN
         IF(ISCA(12).LT.10) THEN
            ierr1=1
            NASYM(10)=NASYM(10)+1
         ENDIF
      ENDIF
C
      IF(ierr1.EQ.0) THEN
         jinc=0
C
         IF(nhelcycl(jhel).GT.nhelcycl(3-jhel)+8) THEN
            ierr1=1
            NASYM(13)=NASYM(13)+1
         ENDIF
C
         IF(idifchk.NE.0) THEN
            k=1
            DO i=1,3
               qq=ISCA(i)-jsca(i)
               IF(ABS(qq-diffav(i,1)).GT.diffav(i,2)) k=0
            END DO
            IF(k.EQ.0) THEN
               ierr1=2
               NASYM(14)=NASYM(14)+1
            ENDIF
         ENDIF
C
      ENDIF
C
      IF(ierr1.GT.1) THEN
         WRITE(6,*) 'Error in difference calculation iev=',ievv,NSCA
         WRITE(6,1000) (i,jsca(i),ISCA(i),ISCA(i)-jsca(i),i=1,nsc)
 1000    FORMAT(' error ',I4,3I11)
      ENDIF
C
C---      Ignore the cuts?
C
      IF(NOCUT.NE.0) THEN
         ierr1=0
         ierr=0
      ENDIF
C
      IF(jhel.GT.0) THEN
         NASYM(jhel)=NASYM(jhel)+1
      ELSE
C         WRITE(6,*) 'JJJJJ HEL',ievv,jhel
      ENDIF
C
      IF(ierr.EQ.0.AND.ierr1.EQ.0.AND.
     +   ievv.GE.NNLIM(1).AND.ievv.LE.NNLIM(2)) THEN
         nhelcycl(jhel)=nhelcycl(jhel)+1
         NASYM(15+jhel)=nhelcycl(jhel)
         DO i=1,nsc
            jdif(i)=ISCA(i)-jsca(i) 
         END DO
         IF(NORBCM.NE.0) THEN
            jdif(5)=BCMLIN(1)+jdif(5)*BCMLIN(2)+jdif(5)**2*BCMLIN(3)
         ENDIF
         DO i=1,5
            dsum2(i)=dsum2(i)+(DBLE(jdif(i+jinc)))**2
            nsumh(i,jhel)=nsumh(i,jhel)+jdif(i+jinc)
            dsumh2(i,jhel)=dsumh2(i,jhel)+(DBLE(jdif(i+jinc)))**2
C            WRITE(6,*) 'i,jdef ',i,jdif(i+jinc)
            NASYM(17+jhel)=nsumh(i,jhel)
         END DO
         nsumh(6 ,jhel)=nsumh( 6,jhel)+jdif(12+jinc)
         nsumh(7 ,jhel)=nsumh( 7,jhel)+jdif( 6+jinc)
         nsumh(19,jhel)=nsumh(19,jhel)+jdif(19+jinc)
         dsum2(6)=dsum2(6)+(DBLE(jdif(12+jinc)))**2
         dsum2(7)=dsum2(7)+(DBLE(jdif( 6+jinc)))**2
         dsumh2(6,jhel)=dsumh2(6,jhel)+(DBLE(jdif(12+jinc)))**2
C
         DO i=1,10
            k=ICHIS(i)
            IF(k.GT.0) THEN
               CALL HF1(ID+k,IABS(jdif(k))+0.1,1.)
               IF(jhel.EQ.1) THEN
                  CALL HF1(ID+k+60,jdif(k)+0.1,1.)
               ELSE
                  CALL HF1(ID+k+80,jdif(k)+0.1,1.)
               ENDIF
               CALL HF1(ID+k+30,ievv+.1,REAL(jdif(k)))
               IF(k.EQ.3) THEN
                  IF(jhelold.NE.jhel) THEN
                     IF(jhel.EQ.1) THEN
                        jd3(1)=jdif(3)
                        jd3(2)=jdifold(3)
                     ELSE
                        jd3(2)=jdif(3)
                        jd3(1)=jdifold(3)
                     ENDIF
                     CALL HF2(ID+88,jd3(1)+.1,jd3(2)+.1,1.)
                  ENDIF
               ENDIF
            ENDIF
         END DO
C
         CALL HF2(ID+25,jdif(5)+0.1,jdif(3)+0.1,1.)
         CALL HF2(ID+26,jdif(12)+0.1,jdif(3)+0.1,1.)
C
      ENDIF
C
      CALL HF1(ID+20,ievv+0.1,REAL(ISCA(11)-jsca(11)))
C      CALL HF1(ID+21,ievv+0.1,ieva+.1)
C      IF(ieva.NE.10) WRITE(6,*) 'event',IDNEVT,' adc event ',ieva
C
      DO i=1,nsc
         jsca(i)=ISCA(i)
         jdifold(i)=jdif(i)
      END DO
C
      iticksc0=iticksc
C     
      IF(ierr.NE.0) NASYM(11)=NASYM(11)+1
      ierr2=MAX(ierr,ierr1)
      ierr=ierr1

      ieva=0

      IF(ierr2.NE.0) GO TO 999
      IF(ievv.LT.NNLIM(1).OR.ievv.GT.NNLIM(2)) GO TO 999
C
      DO i=1,nsc
         da(i,1)=nsumh(i,1)
         da(i,2)=nsumh(i,2)
      END DO
C      da(3,1)=nsumh(19,1)
C      da(3,2)=nsumh(19,2)
C
      ncyclusf=ncyclusf+1
C      NASYM(2)=ncyclusf
      DO i=1,7
        CASYM(i)=(da(i,1)+da(i,2))/DBLE(ncyclusf)
        err=dsum2(i)/DBLE(ncyclusf)-CASYM(i)**2
        IF(err.LT.0.) THEN
           IF(i.NE.6) WRITE(6,2100) ievv,i,CASYM(i),err
 2100      FORMAT(' *** Error in calculating the error:',2I6,2F12.2)
           err=0.
        ENDIF
        err=SQRT(err)
        EASYM(i)=err
C        write(6,*) i,dsum2(i),CASYM(i),err,ncyclusf
      END DO 
C
      asym=0.
C
      IF(NOCUT.GT.0.OR.
     +  (da(3,1).GT.0.D0.AND.
     +   da(3,2).GT.0.D0.AND.
     +   da(6,1).GT.0.D0.AND.
     +   da(6,1).GT.0.D0)) THEN
         IF(MOD(nhelcycl(1)+nhelcycl(2),2).EQ.0.AND.
     +          nhelcycl(1)*nhelcycl(2).GT.9) THEN
            fac=1./PTARG/ANPOW/COS(angl1*3.1415/180.) 
            DO jas=1,6
C
              jnorm=6
              IF(NORM.NE.0) jnorm=5
C
              jab=0
              IF(jas.EQ.3.AND.IACSUB.GT.0) jab=4
              DO i=1,2
                 ddb=0.
                 IF(jab.GT.0) ddb=da(jab,i)
                 dnrm=da(jnorm,i)
                 IF(jas.EQ.5) THEN
                    dnrm=da(6,i)
                    dnrm=1.
                    ddb=0.
                 ENDIF
C                    ddb=0.
                 IF(dnrm.GT.0.) THEN
                    df(i)=(da(jas,i)-ddb)/dnrm
                    de(i)=(da(jas,i)+ddb)/dnrm**2 ! the scaler/BCM da(5..) is ignored
                    IF(jas.EQ.5) THEN                                  !
                       ddb=da(jas,i)/REAL(nhelcycl(i))
                       dds=(dsumh2(jas,i)/REAL(nhelcycl(i))-ddb**2)
                       df(i)=ddb
                       de(i)=dds/REAL(nhelcycl(i))/dnrm**2
C     +                      *REAL(nhelcycl(i))
C                       WRITE(6,6100) i,nhelcycl(i),df(i),dds,de(i),ddb
C     +                      ,dsumh2(jas,i),dnrm
 6100                  FORMAT(2I4,8G12.4)
                    ENDIF
                 ELSE
                    df(i)=-2.
                    de(i)=1.
                 ENDIF
              END DO
C
              asym=-2.
              err=1.
              IF(df(1)+df(2).GT.0.) THEN
                 asym=REAL((df(1)-df(2))/(df(1)+df(2)))
                 err=2.D0/(df(1)+df(2))**2
     +              *DSQRT(df(1)**2*de(2)+df(2)**2*de(1))
              ENDIF
C              WRITE(6,*) jas,df(1),df(2),asym
C
              IF(jas.NE.5) THEN
                 pol=asym*fac
                 epol=err*fac
              ELSE
                 pol=asym
                 epol=err
              ENDIF
              IF(jas.EQ.3) THEN
                 CASYM(21)=asym
                 EASYM(21)=err
                 CASYM(22)=pol
                 EASYM(22)=epol
              ELSE
                 CASYM(22+jas)=pol
                 EASYM(22+jas)=epol
              ENDIF
              IF(ABS(asym).GT.1.001) THEN
                 WRITE(6,*) 'Error: asym=',asym
C                 WRITE(6,1000) (i,jsca(i),ISCA(i)
C     +              ,ISCA(i)-jsca(i),i=1,nsc)
              ENDIF
            END DO
         ENDIF
         CALL HF1(ID,ievv+0.1,CASYM(21))
C         CALL HF1(ID,ievv+0.1,CASYM(27))
      ELSE
         IF(ievv.GT.20) THEN
            WRITE(6,*) 'Error in asymmetry calculation iev=',ievv
            WRITE(6,1000) (i,jsca(i),ISCA(i),ISCA(i)-jsca(i),i=1,nsc)
         ENDIF
      ENDIF
C
      IF(MOD(nhelcycl(1)+nhelcycl(2),2000).EQ.0.AND.ierr.EQ.0) THEN
         IF(nflip.NE.0) THEN
            WRITE(6,*) ' *** Warning: Helicity sync. wave flip '
         ENDIF
         WRITE(6,2000) ievv,CASYM(21),EASYM(21),CASYM(22),EASYM(22)
C         WRITE(6,*) 'ihelseq',ihelseq,ihelic
 2000    FORMAT(' Cycle=',I6,'  Asymmetry=',F11.7,' +/- ',F11.7
     +          ,5X,'Polarization=',F8.4,' +/-',F8.4)
      ENDIF
C      ASYMMETP=ievv
C
 999  CONTINUE
      END




