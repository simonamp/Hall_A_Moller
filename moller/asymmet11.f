      REAL FUNCTION ASYMMET11(ANGL,ANPOW,PTAR,IHELADC,NOCOCUT)
C
C===     Asymmetry/polarization calculation 
C===     for the new setup 
C
      IMPLICIT NONE
      INTEGER ID            !  starting ID
     +       ,NEVADC        !  number of adc triggers per window
     +       ,INORM         !  normalization: =0 - generator, =1 - BCM
     +       ,NORBCM        !  normalization of the BCM
     +       ,NOCUT         !  >0 - no cuts
     +       ,IDELAY        !  >0 - delay by 8 cycles
     +       ,KTYP          !  run type
     +       ,IHELADC       !  >0 - take the helicity from ADC
     +       ,NOCOCUT       !  >0 - no cut on coincidence counting rate
     +       ,NOSUBBG       !  >0 - no subtraction of the BG (accidentals)
C
      REAL    ANGL          !  target angle
     +       ,ANPOW         !  analyzing power
     +       ,PTAR          !  target polarization
C
C
      INCLUDE ?
C
      INCLUDE 'inc/v_asym.inc'
C
      VECTOR ACUTS(20)
      VECTOR NASPAR(32)     ! call formal parameters
C
      LOGICAL HEXIST
C
      DOUBLE PRECISION da(32,2),df(2),dsum2(32),dsumh2(32,2),de(2)
     +      ,ddb,dnrm,dds,denom,dra(2),dera(2)
C
      VECTOR IDD0(1)
      VECTOR ICHIS(10)
      VECTOR NNLIM(2)
      VECTOR BCMLIN(3)
C
      VECTOR LIMSCA(2,32)
C
      INTEGER    NCDEL
      PARAMETER (NCDEL=129)
      INTEGER jsca(64),nsum(64),nsumh(32,2)
     +     ,jdif(64,NCDEL),jhelst(NCDEL)
     +     ,jtick(NCDEL),jcycer(NCDEL)
     +     ,jdifold(64),jcdif(64)
C
      INTEGER id0
      INTEGER ievv,ieva,ihelic,ierr,ierr1,i,nsc,iflip,nflip,k,jinc,m,j
     +       ,ncyclusf,ierr2,itick1,itick2,iticksc,iticksc0
     +       ,nhelcycl(2),jd3(2),jas,jab,idifchk,ihelicold,idel
     +       ,mtick          !  distance between the current tick and the delayed tick 
     +       ,idelcur        !  the number of the cycle delayed, =1 - for no delay =9 for delayed  
     +       ,jhel           !  helicity of the current cycle
     +       ,jhelold        !  helicity of the previous cycle, =0 - unknown
     +       ,jheloldo       !  helicity of the second to previous cycle, =0 - unknown
     +       ,jhelcyc        !  =1 for the 1-st 0.5sec of helicity, =2 - 2nd, =0 ?
     +       ,jnorm          !  normalization pointer
     +       ,mxdiff         !  max difference between 2 scalers
     +       ,mapscal(32)    !  mapping scaler --> CASYM  : mapscal(i)=iscal
     +       ,massub(32)     !  final asymmetry calculation:  subtract this scaler   
     +       ,masnor(32)     !  final asymmetry calculation:  normalize to this scaler   
     +       ,iascal         !  >0 - orrected asymmetry calculated
C
      REAL asym,err,fac,pol,epol,diffav(32,2),qq,asymbcm,angl1
      INTEGER ihelseq(2)     ! helicities in 2 previous cycles
C
      DATA ievv/0/
      DATA ieva/0/
      DATA ihelic/-1/
      DATA jsca/64*0/
      DATA nsum/64*0/
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
      DATA jcycer/NCDEL*2/
      DATA mapscal/ 1, 2, 3, 4, 5,12, 6, 7, 8, 9
     +            ,17,18,19,20,21,22,23,24,25,26
     +            ,12*0/
      DATA massub/ 0, 0, 4, 13*0
     +           , 0, 0,20, 13*0/
      DATA masnor/ 5, 5, 5, 5,12, 5, 5, 5, 5, 5, 6*0
     +           ,16*5/
C      DATA masnor/ 32*0/
C
C     ------------------------------------------------------------------
C
      ID    =NASPAR(1)
      NEVADC=NASPAR(2)
      INORM =NASPAR(3)
      NORBCM=NASPAR(4)
      NOCUT =NASPAR(5)
      IDELAY=NASPAR(6)
      KTYP  =NASPAR(7)
      NOSUBBG=NASPAR(8)
C
      idelcur=1
      mtick=0
      IF(IDELAY.NE.0) THEN
         idelcur=IDELAY
      ENDIF
C      mtick=(idelcur-1)*120./30.
C      mtick=(idelcur-1)*120./240.
      mtick=(idelcur-1)*120./1000.

C
       angl1=ANGL
       IF(ABS(angl1).GT.360.) angl1=MOD(angl1,360.) 
       angl1=MOD(angl1+360.,360.)                  ! angle in 0-360 range 
       IF(ABS(angl1-180.).LT.90.) angl1=angl1-180. ! -90 - 90 range
C       write(6,*) ' Target angle=',angl1
       IF(ABS(angl1).LT.85..AND.ABS(PTAR).GT.1.E-6) THEN
          fac=1./PTAR/ANPOW/COS(angl1*3.1415/180.)
       ELSE
          fac=0.
C         WRITE(6,*) '=== The target angle is  ',ANGL
C     +     ,':target polarization is ignored' 
       ENDIF

C      fac=1./PTAR/ANPOW
      COASYM(9)=fac
C
      mxdiff=ACUTS(1)
C
      nsc=MIN(NSCA,32)
      id0=IDD0(1)
      ASYMMET11=1.
C
      IF(idifchk.EQ.-1) THEN
C
         DO i=1,100
            IF(HEXIST(ID+i-1)) CALL HRESET(ID+i-1,' ')
         END DO
C
         idifchk=1
         DO i=1,5
            k=0
C            WRITE(6,*) 'CASYM,ECASYM', CASYM(i),ECASYM(i)
            IF(CASYM(i).GT.100.) THEN
               k=1
               diffav(i,1)=CASYM(i)
               diffav(i,2)=CASYM(i)*0.4
            ENDIF
            IF(k.EQ.0) idifchk=0
C            WRITE(6,*) ' diffav ',diffav(i,1),diffav(i,2)
         END DO 
         WRITE(6,*) 'Start asymmet11.f, idifchk=',idifchk
      ENDIF
C
      IF(ITRIG(1).NE.0) THEN
         ieva=ieva+1
         IF(ieva.EQ.1) THEN
            itick1=ITICK
            itick2=ITICK
            ihelseq(2)=ihelseq(1)
            ihelseq(1)=ihelicold
            IF(IHELADC.EQ.0) THEN
               ihelic=ITRIG(6)
            ELSE
               ihelic=0
               IF(IADC(12).GT.150) ihelic=1
            ENDIF
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
            IF(IHELADC.EQ.0) THEN
               i=ITRIG(6)
            ELSE
               i=0
               IF(IADC(12).GT.150) i=1
            ENDIF
            IF(ihelic.NE.i.AND.ierr.EQ.0) THEN
               ierr=1
               NASYM(3)=NASYM(3)+1
               WRITE(6,*) ' Helicity flip at ev=',idnevt,ieva,ievv
     +              ,ihelic,i,ihelseq
            ENDIF
         ENDIF
      ENDIF
C
      IF(NSCA.LE.0) GO TO 999
C
      ievv=ievv+1
      NASYM(1)=ievv
C
      iflip=0
      ierr1=0
C
      iticksc=ITICK
      IF(iticksc0.NE.0) CALL HF1(ID+50,iticksc-iticksc0+.1,1.)
      IF(itick2.NE.0) CALL HF1(ID+51,iticksc-itick2+.1,1.)
      IF(itick1.NE.0) CALL HF1(ID+52,itick2-itick1+.1,1.)
C
      IF(ITRIG(6).GE.0) THEN
         DO i=1,4
            CALL HF1(ID+53+i,
     +           ISCA(i)-jsca(i)-ISCA(i+16)+jsca(i+16)+0.1,1.)
         END DO
CC         CALL HF1(ID+54,ISCA(3)-jsca(3)-ISCA(3+16)+jsca(3+16)+0.1,1.)
CC         CALL HF2(ID+59,ISCA(3)-jsca(3)-ISCA(3+16)+jsca(3+16)+0.1
CC     +        ,ISCA(3)-jsca(3)+0.1,1.)
CC     +              ,ISCA(1)-jsca(2)-ISCA(1+16)+jsca(1+16)+0.1,1.)
      ENDIF
C
C---     Reset the ticks for the ADC triggers
C
      itick1=0
      itick2=0
C
      IF(ievv.LT.5) ierr1=1
      DO i=1,nsc
         jcdif(i)=ISCA(i)-jsca(i)
          IF(ISCA(i).LT.jsca(i)) THEN
C             ierr1=2
             IF(i.LE.16) THEN
                j=ISCA(i+16)-jsca(i+16)
             ELSE
                j=ISCA(i-16)-jsca(i-16)
             ENDIF
             WRITE(6,*) ' Wrong increments ',ievv,i,ISCA(i)-jsca(i),j
          ENDIF
      END DO
      IF(ierr1.EQ.2) NASYM(4)=NASYM(4)+1
      IF(ierr1.EQ.0.AND.
     +     IABS(iticksc-iticksc0-4).GT.4) THEN ! correction for 240 Hz (was .GT.2)
         jhelold=0               ! erase the history 
         ierr1=1
         NASYM(12)=NASYM(12)+1
      ENDIF
      IF(ierr1.EQ.0.AND.
     +      REAL(ISCA(4)-jsca(4)).GT.REAL(ISCA(3)-jsca(3))*0.2) THEN
C         ierr1=1
         NASYM(5)=NASYM(5)+1
      ENDIF
      IF(ierr1.EQ.0.AND.NOCOCUT.EQ.0.AND.
     +  (ISCA(3)-jsca(3).LT.1.OR.
     +   ISCA(3)-jsca(3).GT.ISCA(1)-jsca(1))) THEN
         ierr1=1
         NASYM(6)=NASYM(6)+1
      ENDIF
      IF(ierr1.EQ.0.AND.
     +     ieva.NE.NEVADC.AND.NEVADC.GE.0) THEN
         ierr1=1
         NASYM(7)=NASYM(7)+1
C         WRITE(6,*) 'ieva=',ieva
      ENDIF
C
      IF(ierr1.EQ.0.AND.ieva.GT.0.AND.jhelold.EQ.0) THEN
         ierr1=1
         NASYM(8)=NASYM(8)+1
      ENDIF
C
C---     Learn the helicity of the current cycle 
C
      IF(IRUN.LT.10950) THEN  ! before the scaler is added
         IF(IHELADC.EQ.0) THEN
            jhel=jhelold
         ELSE
            jhel=1
            IF(ihelic.EQ.0) jhel=2
         ENDIF
      ELSE
         jhel=2
C*** 28 Oct, 2010 Helicity 1000Hz: jcdif(10)~100?, but  jcdif(12)=85
C***        IF(IABS(jcdif(10)-jcdif(12)).LT.jcdif(12)/10) jhel=1
         IF(IABS(jcdif(10)-jcdif(12)).LT.jcdif(12)/4) jhel=1
      ENDIF
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
C
C---     Store the helicity - it belongs to the NEXT scaler readout (cycle) 
C
      jheloldo=jhelold
      jhelold=1
      IF(IHELADC.EQ.0) THEN
         IF(ITRIG(6).EQ.0) jhelold=2
      ELSE
         IF(ihelic.EQ.0) jhelold=2
      ENDIF
C      write(6,*) 'jhel',jhel,jhelold,ITRIG(6)
C
      CALL HF1(ID+30,ievv+.1,REAL(jhel))
      CALL HF1(ID+99,ievv+.1,REAL(jhelcyc))
      CALL HF1(ID+53,ievv+.1,REAL(iticksc-iticksc0+.1))
C
      IF(ierr1.EQ.0) THEN
         k=1
         DO i=3,4
            IF(IABS(ISCA(i)-jsca(i)-ISCA(i+16)+jsca(i+16)).GT.mxdiff) 
     +          THEN
               k=0
C               WRITE(6,*) ievv,i,ISCA(i)-jsca(i)-ISCA(i+16)+jsca(i+16)
            ENDIF
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
C         IF(ISCA(2)-jsca(2).EQ.0) jinc=5
C         jhel=1
C         IF(ITRIG(6).EQ.1) jhel=2
         
C
C        WRITE(6,*) 'jhel1=',nhelcycl(jhel),nhelcycl(3-jhel)
 
        IF(nhelcycl(jhel).GT.nhelcycl(3-jhel)+16) THEN

            ierr1=1
            NASYM(13)=NASYM(13)+1
C        WRITE(6,*) 'jhel1=',nhelcycl(jhel),nhelcycl(3-jhel)
         ENDIF
C
         IF(idifchk.NE.0) THEN
            k=1
            DO i=3,5,2
               qq=ISCA(i)-jsca(i)
               IF(diffav(i,2).GT.0.) THEN
                 IF(ABS(qq-diffav(i,1)).GT.diffav(i,2)) k=0
               ENDIF
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
         WRITE(6,1000) (i,jsca(i),ISCA(i),ISCA(i)-jsca(i)
     +                ,diffav(i,1),diffav(i,2),i=1,nsc)
 1000    FORMAT(' error ',I4,3I11,3X,2F10.1)
      ENDIF
C
C---      Fill the stack for increments 
C
      DO idel=idelcur,2,-1
         DO i=1,nsc
            jdif(i,idel)=jdif(i,idel-1)
         ENDDO
         jcycer(idel)=jcycer(idel-1)
         jtick(idel)=jtick(idel-1)
         jhelst(idel)=jhelst(idel-1)
      ENDDO
      ierr=jcycer(idelcur)
C
      DO i=1,nsc
         jdif(i,1)=ISCA(i)-jsca(i)
C---       Limits: for histogramming    
         IF(jdif(i,1).GT.0.AND.jdif(i,1).LT.4000000) THEN
            LIMSCA(1,i)=MIN(LIMSCA(1,i),jdif(i,1))
            LIMSCA(2,i)=MAX(LIMSCA(2,i),jdif(i,1))
         ENDIF
      ENDDO
C
      jtick(1)=ITICK
      jhelst(1)=jhel
      jcycer(1)=ierr1
      IF(IABS(jtick(1)-jtick(idelcur)-mtick).GT.2) THEN
C !!         ierr=1
         NASYM(14)=NASYM(14)+1
      ENDIF
         
C
C---      Ignore the cuts?
C
      IF(NOCUT.NE.0) THEN
         ierr1=0
         ierr=0
         ierr2=0
      ENDIF
C
C      WRITE(6,*) ierr,ierr1,jhel,jcycer(1),jcycer(idelcur)
C     +          ,jtick(1)-jtick(idelcur),CASYM(21)
C
      IF(ierr.EQ.0.AND.ierr1.EQ.0.AND.
     +   ievv.GE.NNLIM(1).AND.ievv.LE.NNLIM(2)) THEN
         nhelcycl(jhel)=nhelcycl(jhel)+1
         NASYM(15+jhel)=nhelcycl(jhel)
         IF(NORBCM.NE.0) THEN
C            IF(jdif(5,idelcur).GT.1000) THEN
CCC      WRITE(6,*) jdif(5,idelcur),BCMLIN(1),BCMLIN(2),BCMLIN(3)
               jdif(5,idelcur)=BCMLIN(1)+jdif(5,idelcur)*BCMLIN(2)
     +                     +jdif(5,idelcur)**2*BCMLIN(3)
C            ENDIF
         ENDIF
         DO i=1,32
            nsum(i)=nsum(i)+jdif(i+jinc,idelcur)
            dsum2(i)=dsum2(i)+(DBLE(jdif(i+jinc,idelcur)))**2
            m=jdif(i+jinc,idelcur)
            nsumh(i,jhel)=nsumh(i,jhel)+m
            dsumh2(i,jhel)=dsumh2(i,jhel)+(DBLE(m))**2
C            WRITE(6,*) 'i,jdef ',i,jdif(i+jinc,idelcur)
         ENDDO
         NASYM(17+jhel)=nsumh(5,jhel)
C         WRITE(6,*) 'jhel..',nhelcycl(1)+nhelcycl(2)
C     +         ,jhelst(1),jhelst(idelcur)
C     +         ,jdif(3+jinc,1),jdif(3+jinc,idelcur)
C     +         ,jtick(1),jtick(idelcur)
C
         DO i=1,10
            k=ICHIS(i)
            IF(k.GT.0) THEN
               CALL HF1(ID+k,IABS(jdif(k,idelcur))+0.1,1.)
               IF(jhel.EQ.1) THEN
                  CALL HF1(ID+k+60,jdif(k,idelcur)+0.1,1.)
               ELSE
                  CALL HF1(ID+k+80,jdif(k,idelcur)+0.1,1.)
               ENDIF
               CALL HF1(ID+k+30,ievv+.1,REAL(jdif(k,idelcur)))
               IF(k.EQ.3) THEN
                  IF(jhelold.NE.jhel) THEN
                     IF(jhel.EQ.1) THEN
                        jd3(1)=jdif(3,idelcur)
                        jd3(2)=jdifold(3)
                     ELSE
                        jd3(2)=jdif(3,idelcur)
                        jd3(1)=jdifold(3)
                     ENDIF
                     CALL HF2(ID+88,jd3(1)+.1,jd3(2)+.1,1.)
                  ENDIF
               ENDIF
            ENDIF
         END DO
C
         CALL HF2(ID+25,jdif(5,idelcur)+0.1,jdif(3,idelcur)+0.1,1.)
         CALL HF2(ID+26,jdif(12,idelcur)+0.1,jdif(3,idelcur)+0.1,1.)
C
      ENDIF
C
      CALL HF1(ID+20,ievv+0.1,REAL(ISCA(11)-jsca(11)))
C      CALL HF1(ID+21,ievv+0.1,ieva+.1)
C      IF(ieva.NE.10) WRITE(6,*) 'event',IDNEVT,' adc event ',ieva
C
      DO i=1,nsc
         jsca(i)=ISCA(i)
         jdifold(i)=jdif(i,idelcur)
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
      NASYM(2)=ncyclusf
C
      asym=0.
C
      DO i=1,32
C
C---       Rate calculation
C
         CASYM(i)=(da(i,1)+da(i,2))/DBLE(ncyclusf)
         dds=0.
         DO j=1,2
            dra(j)=0.
            dera(j)=0.
            IF(nhelcycl(j).GT.0) THEN
               dra(j)=da(i,j)/DBLE(nhelcycl(j))
C
C---             Dispersion is taken individually for both helicities
C
               dera(j)=dsumh2(i,j)/DBLE(nhelcycl(j))-dra(j)**2
               dds=dds+dera(j)*DBLE(nhelcycl(j))
            ENDIF
         ENDDO
         err=dds/DBLE(ncyclusf)
         IF(err.LT.0.) THEN
            IF(i.NE.6) WRITE(6,2100) ievv,i,CASYM(i),err
 2100       FORMAT(' *** Error in calculating the error:',2I6,2F12.2)
            err=0.
         ENDIF
         err=SQRT(err)
         ECASYM(i)=err
C         write(6,*) i,dsum2(i),CASYM(i),err,ncyclusf
C
C---            Asymmetry calculation
C
         IF(nhelcycl(1).GT.1.AND.nhelcycl(2).GT.1.AND.
     +      da(12,1).GT.0.D0.AND.da(12,2).GT.0.D0.AND.
     +      da( i,1).GT.0.D0.AND.da( i,2).GT.0.D0) THEN
            IF(MOD(nhelcycl(1)+nhelcycl(2),2).EQ.0.AND.
     +             nhelcycl(1)*nhelcycl(2).GT.9) THEN
C
C---            Raw asymmetry
C
               DO j=1,2
                  df(j)=dra(j)
                  de(j)=dera(j)
               ENDDO
               denom=df(1)+df(2)
               IF(denom.GT.0.D0) THEN
                  asym=REAL((df(1)-df(2))/denom)
                  err=2.D0/denom**2
     +              *DSQRT(df(1)**2*de(2)+df(2)**2*de(1))
                  RASYM(i)=asym
                  ERASYM(i)=err
               ENDIF
C
C---            Corrected asymmetry
C
               iascal=1
               DO j=1,2
                  ddb=0.                 ! background
                  dnrm=da(12,j)          ! normalization (default: pulser)
                  dds=da(i,j)            ! error; sqrt of contents
C
                  m=massub(i)
                  IF(m.GT.0.AND.NOSUBBG.EQ.0) ddb=da(m,j)
                  IF(i.EQ.12) THEN
                     dnrm=DBLE(nhelcycl(j))
                     dds=dera(j)*DBLE(nhelcycl(j))**2
                  ENDIF
                  IF(INORM.GT.0) THEN
                     m=masnor(i)
                     IF(m.GT.0)  THEN
                        dnrm=da(m,j)
                        IF(m.EQ.12) dds=dera(j)*DBLE(nhelcycl(j))
                     ENDIF
                  ENDIF
C
                  IF(dnrm.GT.0.D0) THEN
                     df(j)=(da(i,j)-ddb)/dnrm
                     de(j)=(dds+ddb)/dnrm**2
                  ELSE
                     iascal=0
                  ENDIF
               ENDDO
C
               IF(iascal.GT.0) THEN
                  denom=df(1)+df(2)
                  IF(denom.GT.0.D0) THEN
                     asym=REAL((df(1)-df(2))/denom)
                     err=2.D0/denom**2
     +                    *DSQRT(df(1)**2*de(2)+df(2)**2*de(1))
                     FASYM(i)=asym
                     EFASYM(i)=err
C       WRITE(6,*) denom,df(1),de(1),df(2),de(2)
                  ENDIF
               ENDIF
C
            ENDIF
         ENDIF
C                  
      END DO 
C
       FASYM(33)= FASYM(3)*fac
      EFASYM(33)=EFASYM(3)*fac
       FASYM(34)= FASYM(1)*fac
      EFASYM(34)=EFASYM(1)*fac
      IF(KTYP.EQ.0) THEN
         FASYM(35)= FASYM(2)*fac
        EFASYM(35)=EFASYM(2)*fac
      ENDIF
C
      CALL HF1(ID,ievv+0.1,FASYM(33))
C
      IF(MOD(nhelcycl(1)+nhelcycl(2),1000).EQ.0.AND.ierr.EQ.0) THEN
         IF(nflip.NE.0) THEN
            WRITE(6,*) ' *** Warning: Helicity sync. wave flip '
         ENDIF
         WRITE(6,2000) ievv,nhelcycl(1)+nhelcycl(2),RASYM(3),ERASYM(3)
     +        ,FASYM(33),EFASYM(33)
C         WRITE(6,*) 'ihelseq',ihelseq,ihelic
 2000    FORMAT(' Cycle=',2I7,'  Raw asymmetry=',F11.7,' +/- ',F11.7
     +          ,5X,'Polarization=',F8.4,' +/-',F8.4)
C         write(6,7777) (da(j,1),j=1,32)
C         write(6,7777) (da(j,2),j=1,32)
C 7777    format(10D12.4)
C         write(6,*) 'dsumh2'
C         write(6,7777) (dsumh2(j,1),j=1,32)
C         write(6,7777) (dsumh2(j,2),j=1,32)
C         write(6,*) (nsumh(j,1),j=1,32)
C         write(6,*) (nsumh(j,2),j=1,32)

      ENDIF
C      ASYMMET11=ievv
C
 999  CONTINUE
      END



