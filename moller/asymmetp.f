      REAL FUNCTION ASYMMETP(ID,SCA3TOT)
C
C===     Asymmetry/polarization calculation 
C===     for the new setup 
C
      IMPLICIT NONE
      INTEGER  ID            !  starting ID
      REAL     SCA3TOT       !  scaler 3 (coin): total amount for this run
C
      INCLUDE ?
C
      INCLUDE 'inc/crunpol.inc'           ! output COMMON
      INCLUDE 'inc/v_asym.inc'            ! auxill. vectors
C
C      INTEGER MXASYMV
C      PARAMETER (MXASYMV=50)
C      REAL CSCAL(32,2),ESCAL(32,2),ASYPAR(10)
C      REAL CASYM(16,4),EASYM(16,4),CPOLA(6,4),EPOLA(6,4)
C      INTEGER NASYM(50),NSYPAR(10)
C
      LOGICAL HEXIST
C
      VECTOR IDD0(1)
      VECTOR ICHIS(10)
      VECTOR NNLIM(2)
      VECTOR BCMLIN(3)
C
C      INTEGER IDD0(1)
C      INTEGER ICHIS(10)
C      INTEGER NNLIM(2)
C      REAL    BCMLIN(3)
C
C ===           The increment of the scaler for each cycles is calculated.
C ===           Some cycles can be rejected.
C ===           Summs of the increments are calculated. 
C
      DOUBLE PRECISION da1(32,2,2)  ! (i,jh,jna) - sum of all increments for selected cycles for helicity jh,
C                                     i=1 - left, 2 - right, 3 - coinc, 4 - accid, 
C                                     i - BCM, 6 - clock, 7 - LED
C                                     jna = 1 - the increment, = 2 - the increment normalized 
     +                ,da2(32,2,2)  ! (i,jh,jna) - sum of the the (increment)**2 
     +                ,df(2)        ! asymmetry calculation: normalized... values
     +                ,de(2)        !                        errors
     +                ,ddb          !                        background
     +                ,ddbe         !                        error of background
     +                ,dnrm         !                        normalization factor
     +                ,dds          !                        auxil
C
      INTEGER          jsca(64)     ! the scaler values from the previous cycle
     +                ,jdif(64)     ! the scaler increments during the last cycle
     +                ,jdifold(64)  ! the scaler increments of the previous cycle
C
      INTEGER id0
      INTEGER ievv,ieva,ihelic,ierr,ierr1,i,nsc,iflip,nflip,k
     +       ,ierr2,itick1,itick2,iticksc,iticksc0
     +       ,jd3(2),jas,jab,idifchk,ihelicold,i1,i2,i3,jh,jan,ich,ian
     +       ,kab,kan
     +       ,nhelcycl(2)    !  number of selected cycles with a given helicity
     +       ,jhel           !  helicity of the current cycle
     +       ,jhelold        !  helicity of the previous cycle, =0 - unknown
     +       ,jheloldo       !  helicity of the second to previous cycle, =0 - unknown
     +       ,jhelcyc        !  =1 for the 1-st 0.5sec of helicity, =2 - 2nd, =0 ?
     +       ,jhelchn        !  =0 if helicity did not change from the prev. readout, =1 if it changed
     +       ,jnorm          !  normalization pointer
     +       ,ihelseq(16)    ! helicities in 16 previous cycles
     +       ,mapsca(32)     ! map the scalers to the da1.. arrays: da1(i,jhel)=jdif(mapsca(i)) ,i=1,NSCA 
     +       ,nscout         ! number of the scalers to be read out to da1 ...
C
C                               jan=1,2   1 - scalers, 2 - normalized scaler
      INTEGER kdanor(16)     ! =0 - no normalization of the counter (j) (da1(j,jhel,jna)) j=1,nscout
C                            ! =k>0 - normalize to  da1(k,jhel)
     +       ,kasda(16)      ! (i) = k, for the asymmetry i, use da1(k,jh,jna)    
     +       ,kasjan(16,4)   ! (i,j)=jna, so use da1(k,jh,jna)   ,j=1,4 - different types of asymmetries
     +       ,kasbgr(16,4)   ! (i,j)=k,    use da1(k,jh,jna) for BG   
     +       ,kasnor(16,4)   ! (i,j)=k,    use da1(k,jh,jan) for normalization   
     +       ,kpolas(6)      ! kpolas(i)=k  use rasy(k.., for polarization value i, i=1,6
     +       ,nasymmet       ! number of asymmetry values
     +       ,npolariz       ! number of polarization values
C
      REAL asym,err,fac,diffav(32,2),qq,asymbcm,angl1
     +       ,rasy(16,4)     ! (i,j)  - asymmetries
     +       ,easy(16,4)     ! (i,j)  - error on the asymmetries
     +       ,rpol(6,4)      ! (i,j)  - polarizations
     +       ,epol(6,4)      ! (i,j)  - polarizations
C
      DATA ievv/0/
      DATA ieva/0/
      DATA ihelic/-1/
      DATA jsca/64*0/
C      DATA nsumh/64*0/
      DATA ierr/0/
      DATA nflip/0/
      DATA jhelold/0/
      DATA jhelcyc/0/
      DATA iticksc0/0/
C      DATA dsum2/32*0.D0/
C      DATA dsumh2/64*0.D0/
      DATA da1/128*0.D0/
      DATA da2/128*0.D0/
      DATA nhelcycl/2*0/
      DATA idifchk/-1/
      DATA ihelseq/16*0/
      DATA ihelicold/-1/
C
C         l,r,coin,accid,BCM,clock,LED,l_1,r_1,coin_1,accid_1         
      DATA mapsca/ 1, 2, 3, 4, 5,12, 6,17,18,19,20, 0, 0, 0, 0, 0
     +           , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
C
      DATA kdanor/ 5, 5, 5, 5, 6, 0, 0, 5, 5, 5, 5, 0, 0, 0, 0, 0/
C
      DATA kasda / 1, 2, 3, 4, 5, 6, 8, 9,10,11, 0, 0, 0, 0, 0, 0/
C
      DATA kasjan/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0
     +           , 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0
     +           , 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0
     +           ,16*0/
      DATA kasbgr/ 0, 0, 4, 0, 0, 0, 0, 0,11, 0, 0, 0, 0, 0, 0, 0
     +           , 0, 0, 4, 0, 0, 0, 0, 0,11, 0, 0, 0, 0, 0, 0, 0
     +           , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
     +           ,16*0/
      DATA kasnor/ 5, 5, 5, 5, 6, 0, 5, 5, 5, 5, 0, 0, 0, 0, 0, 0
     +           , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
     +           , 5, 5, 5, 5, 6, 0, 5, 5, 5, 5, 0, 0, 0, 0, 0, 0
     +           ,16*0/
      DATA kpolas/ 3,1,2,9,7,8 / 
C
C     ------------------------------------------------------------------
C
C===    Copy the parameters from vectors to the CRUNPOL COMMON'
C
      INCLUDE 'inc/v_asym_a.inc'          ! vector ==> output COMMON 
C
C
      angl1=TANGL
      IF(ABS(angl1).GT.90.) angl1=180.-angl1
C
      nsc=MIN(NSCA,32)
      id0=IDD0(1)
      ASYMMETP=1.
C
C      WRITE(6,*) 'NNN',idnevt,nasymmet,ITRIG(1),nsc
      nscout=0
      DO i=1,nsc
         IF(mapsca(i).GT.0) nscout=i
      ENDDO
      nasymmet=0
      DO i=1,16
         IF(kasda(i).GT.0) nasymmet=i
      ENDDO
      npolariz=6
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
            IF(CSCAL(i,1).GT.100.) THEN
               k=1
               diffav(i,1)=CSCAL(i,1)
               diffav(i,2)=CSCAL(i,1)*0.4
            ENDIF
            IF(k.EQ.0) idifchk=0
C            WRITE(6,*) ' diffav ',diffav(i,1),diffav(i,2)
         END DO 
         WRITE(6,*) 'Start asymmetp.f, idifchk=',idifchk
      ENDIF
C
      IF(ITRIG(1).NE.0) THEN
C
C---       ADC trigger
C
         ieva=ieva+1
         IF(ieva.EQ.1) THEN
C
C---         The 1-st ADC trigger in the helicity cycle
C
            itick1=ITICK
            itick2=ITICK
C
C---         Push the helicity stack
C
            DO i=16,2,-1
               ihelseq(i)=ihelseq(i-1)
            ENDDO
            ihelseq(1)=ihelicold     ! store the old helicity
            ihelic=ITRIG(6)
            ihelicold=ihelic         ! define the current helicity
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
C---       Bypass the rest for non-scaler events
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
         IF(jhelcyc.EQ.2) THEN
            jhelcyc=1
         ELSE           ! jhelcyc=1
            jhelcyc=2
            IF(jhel.NE.jheloldo) THEN
               jhelcyc=1        ! this may happen if readout_cycle=helicity_cycle (at 30Hz always)
C               WRITE(6,*) 'Wrong cycle mark',jhelcyc,jhel
C     +                     ,jhelold,jheloldo
            ENDIF
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
C               IF(ABS(qq-diffav(i,1)).GT.diffav(i,2)) k=0
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
C      WRITE(6,*) 'Helicity ',jhel
      IF(jhel.GT.0) THEN
         NASYM(jhel)=NASYM(jhel)+1
      ELSE
C         WRITE(6,*) 'JJJJJ HEL',ievv,jhel
      ENDIF
C
C===      For a selected cycle: update the counters
C
      IF(ierr.EQ.0.AND.ierr1.EQ.0.AND.
     +   ievv.GE.NNLIM(1).AND.ievv.LE.NNLIM(2)) THEN
C
         nhelcycl(jhel)=nhelcycl(jhel)+1
         NASYM(15+jhel)=nhelcycl(jhel)
C
         DO i=1,nsc
            jdif(i)=ISCA(i)-jsca(i) 
         END DO
         IF(NORBCM.NE.0) THEN
            jdif(5)=BCMLIN(1)+jdif(5)*BCMLIN(2)+jdif(5)**2*BCMLIN(3)
         ENDIF
C
         DO i=1,nscout
C            IF(MOD(ievv,2).EQ.0) THEN
            IF(mapsca(i).GT.0) THEN
               i1=jdif(mapsca(i))
               i2=0
               IF(kdanor(i).GT.0) i2=jdif(mapsca(kdanor(i)))
C
               df(1)=DBLE(i1)        ! plain scaler
               df(2)=df(1)           ! normalized
               IF(i2.GT.0) THEN
                  df(2)=df(1)/DBLE(i2)
               ENDIF
               DO jan=1,2
                  da1(i,jhel,jan)=da1(i,jhel,jan)+df(jan)     
                  da2(i,jhel,jan)=da2(i,jhel,jan)+df(jan)**2
               ENDDO
            ENDIF
C            ENDIF
         END DO
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
C===       Fill the vectors for the scalers 
C
      IF(nhelcycl(1).GT.0.AND.nhelcycl(2).GT.0) THEN
         DO i=1,nscout
           DO jan=1,2
             DO jh=1,2   ! dispersion is calculated independently for both helicities
                dds   =da1(i,jh,jan)/DBLE(nhelcycl(jh))
                de(jh)=da2(i,jh,jan)/DBLE(nhelcycl(jh))-dds**2
             ENDDO
             dds=DBLE(nhelcycl(1)+nhelcycl(2))
             CSCAL(i,jan)=REAL(da1(i,1,jan)+da1(i,2,jan))/dds
             err       =REAL(de(1)+de(2))
             IF(err.LT.0.) THEN
                IF(i.NE.6) WRITE(6,2100) ievv,i,CSCAL(i,jan),err
 2100           FORMAT(' *** Error in calculating the error:'
     +               ,2I6,2F12.2)
                err=0.
             ENDIF
             ESCAL(i,jan)=SQRT(err) 
           ENDDO 
         ENDDO
      ENDIF
C
      asym=0.
C
C===       Calculate the asymmetries 
C
      IF(NOCUT.GT.0.OR.
     +  (da1(3,1,1).GT.-1.D0.AND.
     +   da1(3,2,1).GT.-1.D0.AND.
     +   da1(6,1,1).GT. 0.D0.AND.
     +   da1(6,2,1).GT. 0.D0)) THEN
         IF(MOD(nhelcycl(1)+nhelcycl(2),2).EQ.0.AND.
     +          nhelcycl(1)*nhelcycl(2).GT.9) THEN
            fac=1./PTARG/ANPOW/COS(angl1*3.1415/180.) 
C            IF(nhelcycl(1)+nhelcycl(2).GT.66) 
C     +      WRITE(6,*) 'A', da1(3,1,2),da1(3,2,2),da1(3,1,1),da1(3,2,1)
C
            DO jas=1,nasymmet
               ich=kasda(jas)
               IF(ich.GT.0) THEN
C
                  DO ian=1,3
                     jan=kasjan(jas,ian)
                     IF(jan.GT.0) THEN
                        kab=kasbgr(jas,ian)
                        kan=kasnor(jas,ian)
C
                        rasy(jas,ian)=0.
                        easy(jas,ian)=1.
C
                        DO jh=1,2
                           dnrm=1.D0
                           ddb=0.
                           ddbe=0.
                           IF(kab.GT.0.)  THEN
                              ddb =da1(kab,jh,jan) 
                              ddbe=da2(kab,jh,jan) 
                           ENDIF
                           IF(kan.GT.0.) THEN
                              dnrm=da1(kan,jh,jan)
                           ELSE
                              dnrm=nhelcycl(jh)
                           ENDIF
                           IF(REAL(dnrm).LE.0.) dnrm=1.D0
                           df(jh)=(da1(ich,jh,jan)-ddb)/dnrm
C
C---                        The normalization statistical error (BCM) is ignored
C
                           IF(kan.GT.0) THEN
                              de(jh)=(da1(ich,jh,jan)+ddb)/dnrm**2 
                           ELSE
                              de(jh)=(da2(ich,jh,jan)/dnrm
     +                              -(da1(ich,jh,jan)/dnrm)**2 
     +                              + ddbe/dnrm-(ddb/dnrm)**2)/dnrm
                           ENDIF
                        ENDDO
C
                        dds=df(1)+df(2)
                        IF(REAL(dds).GT.0.) THEN
                           asym =REAL((df(1)-df(2))/dds)
                           err  =REAL(2.D0/dds**2
     +                         *DSQRT(df(1)**2*de(2)+df(2)**2*de(1)))
                           rasy(jas,ian)=asym
                           easy(jas,ian)=err
C
                           CASYM(jas,ian)=asym
                           EASYM(jas,ian)=err
C
                           IF(ABS(asym).GT.1.001) THEN
                              WRITE(6,*) 'Error: asym=',asym,jas,ian
                           ENDIF
C                  if(MIN(nhelcycl(1),nhelcycl(2)).GT.65
C     +                      .AND.ian.EQ.1.AND.jas.EQ.3) 
C     +    WRITE(6,*) 'B',REAL(da1(ich,1,jan)),REAL(da2(ich,1,jan))
C     +            ,df,de,asym,err
                        ENDIF
C
                     ENDIF
                  ENDDO
C
               ENDIF
            ENDDO
C
C---             Polarization
C
            DO i=1,6
               rpol(i,ian)=0.
               epol(i,ian)=1.
               jas=kpolas(i)
               IF(jas.GT.0) THEN
                  DO ian=1,3
                     rpol(i,ian)=rasy(jas,ian)*fac
                     epol(i,ian)=easy(jas,ian)*fac
                     CPOLA(i,ian)=rpol(i,ian)
                     EPOLA(i,ian)=epol(i,ian)
                  ENDDO
               ENDIF
            ENDDO
C
         ENDIF
      ELSE
         IF(ievv.GT.20) THEN
            WRITE(6,*) 'Error in asymmetry calculation iev=',ievv
            WRITE(6,1000) (i,jsca(i),ISCA(i),ISCA(i)-jsca(i),i=1,nsc)
         ENDIF
      ENDIF

C
      CALL HF1(ID,ievv+0.1,CPOLA(3,1))
C
      IF(MOD(nhelcycl(1)+nhelcycl(2),INT(60./GATE)).EQ.0.AND.
     +       ierr.EQ.0) THEN
         IF(nflip.NE.0) THEN
            WRITE(6,*) ' *** Warning: Helicity sync. wave flip '
         ENDIF
         WRITE(6,2000) ievv,CASYM(3,1),EASYM(3,1),CPOLA(3,1)
     +                 ,EPOLA(3,1)
C         WRITE(6,*) 'ihelseq',ihelseq,ihelic
 2000    FORMAT(' Cycle=',I6,'  Asymmetry=',F11.7,' +/- ',F11.7
     +          ,5X,'Polarization=',F8.4,' +/-',F8.4)
      ENDIF
C      ASYMMETP=ievv
C
 999  CONTINUE
C      IF(ITRIG(1).EQ.0.OR.ieva.LT.3.OR.ieva.GT.98) 
C     +WRITE(6,*) idnevt,ieva,ITRIG(1),ITRIG(6),ihelic,ihelicold,jhel
C     +          ,jhelold,INT(60./GATE)
      END
