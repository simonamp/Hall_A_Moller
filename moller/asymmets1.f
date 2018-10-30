      REAL FUNCTION ASYMMETS1(ANGL,ANPOW,PTAR,IHELADC,NOCOCUT,LTICK)
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
     +       ,IDELAY        !  >0 - delay (normally 8 cycles)
     +       ,KTYP          !  run type
     +       ,IHELADC       !  >0 - take the helicity from ADC
     +       ,NOCOCUT       !  >0 - no cut on coincidence counting rate
     +       ,LTICK         !  >0 - required number of ticks between SCALER events (+/-1)
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
      VECTOR IDD0(1)
      VECTOR ICHIS(10)
      VECTOR NNLIM(2)
      VECTOR BCMLIN(3)
C
      LOGICAL HEXIST
C
      INTEGER    mxsca,mxsca2
      PARAMETER (mxsca=32,mxsca2=mxsca*2)
C
      VECTOR LIMSCA(2,mxsca)
C
      DOUBLE PRECISION da(mxsca,2),dsum2(mxsca)
     +            ,dsumh2(mxsca,2)
     +            ,de(2),dra(2),dera(2)
     +            ,ddb,dnrm,dds,denom
C
      INTEGER jsca(mxsca),nsumh(mxsca,2)
C
C---           Stack for cycles
C
      INTEGER    mxstack       ! max delay length 
      PARAMETER (mxstack=129)
      INTEGER jsca(mxsca,mxstack)  ! scaler values for the history of cycles 
     +       ,jdif(mxsca,mxstack)  ! scaler increments for the history of cycles 
     +           ,jhelcur(mxstack) ! current helicities (shifted in case of delayed)
     +           ,jhelsca(mxstack) ! current helicity of the scaler event
     +           ,jhelrec(mxstack) ! reconstructed helicities (correct also in case of delayed)
     +           ,jtick(mxstack)   ! tick values (for the scaler readout)
     +           ,jzugsca(mxstack) ! helicity train (4 cycles) starting signal (1-st cycle), this scaler event  
     +           ,jzug(mxstack)    ! helicity train (4 cycles) starting signal (1-st cycle)  
     +           ,jcycer(mxstack)  ! error in this cycle =0 - OK
C
C==========         Event data
      INTEGER itickev              ! tick
     +       ,izugev               ! train start flag (from TRIG)
     +       ,ihelev               ! helicity flag (from TRIG)
     +       ,nscalim              ! number of readout scalers, limited by the array dimension
C
C==========         Cycle data
      INTEGER ipstack            ! pointer of the current cycle in the delay stack
     +       ,iheladc            ! current helicity: ADC    events  (0 or 1)
     +       ,ihel               ! current helicity: cycle (previous ADC events == previous scaler event)
     +       ,itickadcf          ! tick for the 1-st ADC
     +       ,itickadcl          ! tick for the last ADC
     +       ,izugadc            ! train start signal from ADC events
     +       ,izugsca            ! train start signal from SCALER events
     +       ,izug               ! train start 
     +       ,isprev             ! pointer to the previous cycle in the stack (0 if missing)
     +       ,ispdel             ! pointer to the cycle for which the delayed helicity arrives
     +       ,itimerr            ! >0 detected error in timing of the cycle
     +       ,ierr               ! error in the cycle
C
      INTEGER id0
      INTEGER ierr1              ! error/inconsistency in the scaler event  
     +       ,ierr2              ! error flag (max of ierr and ierr1)
     +       ,mxerrpri           ! max number of error messages to print
     +       ,nhelcycl(2),jd3(2),jas,jab,idifchk,ihelicold,idel
     +       ,mtick          !  distance between the current tick and the delayed tick 
     +       ,idelcur        !  the number of the cycle delayed, =1 - for no delay =9 for delayed  
     +       ,jhel           !  helicity of the current cycle
     +       ,jhelold        !  helicity of the previous cycle, =0 - unknown
     +       ,jheloldo       !  helicity of the second to previous cycle, =0 - unknown
     +       ,jhelcyc        !  =1 for the 1-st 0.5sec of helicity, =2 - 2nd, =0 ?
     +       ,mxdiff         !  max difference between 2 scalers
     +       ,mapscal(32)    !  mapping scaler --> CASYM  : mapscal(i)=iscal
     +       ,massub(32)     !  final asymmetry calculation:  subtract this scaler   
     +       ,masnor(32)     !  final asymmetry calculation:  normalize to this scaler   
     +       ,iascal         !  >0 - orrected asymmetry calculated
C============= Counters
      INTEGER ievv               ! counter of the scaler triggers
     +       ,ieva               ! counter of the ADC triggers in the given cycle
     +       ,ncyclusf           ! useful cycles
     +       ,nzugusf            ! useful trains (4-cycles)
     +       ,nerr               ! number of printable errors
C
      INTEGER i,j,k,m,ips
C
      REAL asym,err,fac,pol,epol,diffav(32,2),qq,asymbcm,angl1
C
      DATA ievv/0/
      DATA ieva/0/
      DATA iheladc/-1/
      DATA izugadc/-1/
      DATA istack/0/
      DATA ierr/0/
      DATA nerr/0/
      DATA mxerrpri/100/
      DATA jhelold/0/
      DATA jhelcyc/0/
      DATA itickadcf/0/
      DATA itickadcl/0/
      DATA ncyclusf/0/
      DATA nzugusf/0/
      DATA dsum2/32*0.D0/
      DATA dsumh2/64*0.D0/
      DATA nhelcycl/2*0/
      DATA idifchk/-1/
      DATA ihelicold/-1/
      DATA mapscal/ 1, 2, 3, 4, 5,12, 6, 7, 8, 9
     +            ,17,18,19,20,21,22,23,24,25,26
     +            ,12*0/
      DATA massub/ 6, 0, 4, 13*0
     +           , 6, 0,20, 13*0/
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
C
      idelcur=IDELAY
      mtick=idelcur*120./30.
C
      angl1=ANGL
      IF(ABS(angl1).GT.360.) angl1=MOD(angl1,360.) 
      angl1=MOD(angl1+360.,360.)                  ! angle in 0-360 range 
      IF(ABS(angl1-180.).LT.90.) angl1=angl1-180. ! -90 - 90 range
C      write(6,*) ' Target angle=',angl1
      IF(ABS(angl1).LT.85.) THEN
         fac=1./PTAR/ANPOW/COS(angl1*3.1415/180.)
      ELSE
         fac=0.
C         WRITE(6,*) '=== The target angle is  ',ANGL
C     +     ,':target polarization is ignored' 
      ENDIF
      COASYM(9)=fac
C
      mxdiff=ACUTS(1)
C
      nscalim=MIN(NSCA,mxsca)
      id0=IDD0(1)
      ASYMMETS1=1.
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
         WRITE(6,*) 'Start asymmetr.f, idifchk=',idifchk
      ENDIF
C
      itickev=ITICK
      ihelev=ITRIG(6)
      izugev=ITRIG(8)
      ierr=0
C
C---          ADC events
C
      IF(ITRIG(1).NE.0) THEN
         ieva=ieva+1
         itickadcl=itickev
C---        Find the current helicity signal
         ihel=0
         IF(IADC(12).GT.150) ihel=1 ! Use the ADC signal for helicity
C
         IF(IHELADC.LT.0) THEN
            ihel=ihelev             ! Use only the helicity from ITRIG 
         ELSE IF(IHELADC.EQ.0) THEN
            IF(ihelev.NE.ihel) THEN ! Check the helicity consistency
               ihel=-1
               nerr=nerr+1
               IF(nerr.LT.mxerrpri) THEN
                  WRITE(6,*) ' Helicity inconsistency ADC ev=',idnevt
     +              ,ieva,' ADC12=',ihel,' TRIG(6)=',ihelev
               ENDIF
            ENDIF
         ENDIF
C
         IF(ieva.EQ.1) THEN
            itickadcf=itickev
            iheladc=ihel
            izugadc=izugev
         ELSE
            IF(iheladc.NE.ihel) THEN
               iheladc=-1
               NASYM(3)=NASYM(3)+1
               nerr=nerr+1
               IF(nerr.LT.mxerrpri) THEN
                  WRITE(6,*) ' Helicity flip at ADC ev=',idnevt
     +              ,ieva,ievv,itickadcl-itickadcf,iheladc,ihel
               ENDIF
            ENDIF
            IF(izugadc.NE.izugev) THEN
               izugadc=-1
               NASYM(4)=NASYM(4)+1
               nerr=nerr+1
               IF(nerr.LT.mxerrpri) THEN
                  WRITE(6,*) ' Train start flip at ADC ev=',idnevt
     +              ,ieva,ievv,itickadcl-itickadcf
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
      IF(NSCA.LE.0) GO TO 999
C
C----          Scaler event - reconstruct one cycle
C
      ievv=ievv+1
      NASYM(1)=ievv
C
C---     Store the current cycle
C
      isprev=istack
      istack=istack+1
      IF(istack.GT.mxstack) istack=istack-mxstack
C
      jcycer(istor)=ierr
      jzug(istor)=-1
      jzugsca(istor)=izugev
      jtick(istor)=itickev
      jhelsca(istor)=ihelev
      jhelcur(istor)=-1
      jhelrec(istor)=-1
      DO i=1,nscalim
         jsca(i,istor)=ISCA(i)
         jdif(i,istor)=-1
      ENDDO
C
C---     Check the clock of the cycle
C
      IF(LTICK.GT.0) THEN
         IF(ieva.GT.0) THEN
            IF(itickev-itickadcf.GT.LTICK+1) ierr=1
C            IF(itickev-itickadcl.LT.2) ierr=1
         ENDIF
         IF(isprev.GT.0) THEN
            IF(IABS(itickev-jtick(isprev)-LTICK).GT.1) ierr=1
         ENDIF
         IF(ierr.NE.0) THEN
            jcycer(istack)=1
            NASYM(5)=NASYM(5)+1
            nerr=nerr+1
            IF(nerr.LT.mxerrpri) THEN
               WRITE(6,*) ' Wrong timing in cycle=',ievv
            ENDIF
         ENDIF
      ENDIF
C
C---    Check the number of ADC events for this gate
C
      IF(ierr.EQ.0.AND.NEVADC.GT.0) THEN
         IF(ieva.NE.NEVADC) THEN
            ierr=1
            NASYM(6)=NASYM(6)+1
            nerr=nerr+1
            IF(nerr.LT.mxerrpri) THEN
               WRITE(6,*) ' Wrong number of ADC events in cycle=',ievv
     +               ,ieva,NEVADC
            ENDIF
         ENDIF
      ENDIF
C
C---    Find the helicity (delay ignored) of the current cycle: previous scaler, or the current ADC
C
      IF(ierr.EQ.0) THEN
         ihel=-1
         IF(IHELADC.LE.0) THEN
            IF(isprev.GT.0) THEN
               ihel=jhelsca(isprev)      ! previous scaler event
               IF(ieva.GT.0) THEN
                  IF(ihel.NE.iheladc) ihel=-1
               ENDIF
            ENDIF
         ELSE
            IF(ieva.GT.0) ihel=iheladc   ! only ADC used
         ENDIF
         IF(ihel.LT.0) THEN
            ierr=1
            jcycer(istack)=1
            NASYM(7)=NASYM(7)+1
            nerr=nerr+1
            IF(nerr.LT.mxerrpri) THEN
               WRITE(6,*) ' Helicity flip in cycle=',ievv
            ENDIF
         ELSE
            jhelcur(istor)=ihel
         ENDIF
      ENDIF
C
C---    Find the train status (start of train) - ignore if time/helicity errors occured
C
      IF(ierr.EQ.0) THEN
         izug=-1
         IF(ieva.GT.0) izug=izugadc
         IF(isprev.GT.0) THEN
            IF(izug.GT.-1) THEN
               IF(izug.NE.jzugsca(isprev)) izug=-1
            ELSE
               izug=jzugsca(isprev)
            ENDIF
         ENDIF
         IF(izug.LT.0) THEN
            NASYM(8)=NASYM(8)+1
            nerr=nerr+1
            IF(nerr.LT.mxerrpri) THEN
               WRITE(6,*) ' Train inconsistency in cycle=',ievv
            ENDIF
         ELSE
            jzug(istor)=izug
         ENDIF
      ENDIF               
C
C---    Calculate the scaler increments and check their consistency
C
      IF(ierr.EQ.0.AND.isprev.GT.0) THEN
         DO i=1,nscalim
            jdif(i,istack)=ISCA(i)-jsca(i,isprev)
C
C---       Limits: for histogramming    
C
            IF(jdif(i,istack).GT.0.AND.jdif(i,istack).LT.4000000) THEN
               LIMSCA(1,i)=MIN(LIMSCA(1,i),jdif(i,istack))
               LIMSCA(2,i)=MAX(LIMSCA(2,i),jdif(i,istack))
            ENDIF
         ENDDO
C
C---       Check the consistency of the increments in 2 scalers
C
         k=1
         DO i=3,4
            IF(IABS(jdif(i,istack)-jdif(i+16,istack)).GT.mxdiff) THEN
               k=0
               nerr=nerr+1
               IF(nerr.LT.mxerrpri) THEN
               WRITE(6,*) ' Difference for 2 scalers, ievv=',ievv
     +                 ,i,jdif(i,istack)-jdif(i+16,istack)
            ENDIF
         END DO
C
         IF(k.EQ.0) THEN
            ierr=1
            NASYM(9)=NASYM(9)+1
         ENDIF
C
C---       Print cases with negative increments
C
         IF(ierr.EQ.0) THEN
            DO i=1,5 
               IF(jdif(i,istack).LT.0) THEN
                  nerr=nerr+1
                  IF(nerr.LT.mxerrpri) THEN
                     WRITE(6,*) ' Negative increment, ievv=',ievv
     +                    ,i,jdif(i,istack)
                     NASYM(10)=NASYM(10)+1
                  ENDIF
               ENDIF
C
            ENDDO
         ENDIF
C
C---     Reject cycles with no or little beam
C
         IF(ierr.EQ.0) THEN
            IF(jdif(5,istack).LT.1) THEN
               ierr=1
               NASYM(11)=NASYM(11)+1
               nerr=nerr+1
               IF(nerr.LT.mxerrpri) THEN
                  WRITE(6,*) ' Low beam, ievv=',ievv
     +                 ,jdif(5,istack)
               ENDIF
            ENDIF
         ENDIF
C
C---     Check various conditions in the coincidence rate
C
         IF(NOCOCUT.EQ.0) THEN
C
C---       Reject cycles with a big change in coincidence counts,
C
            IF(ierr.EQ.0) THEN
               IF(IABS(jdif(3,istack)-jdif(3,isprev)).GT.
     +            0.3*(jdif(3,istack)+jdif(3,isprev))) THEN
                  ierr=1
                  NASYM(12)=NASYM(12)+1
                  nerr=nerr+1
                  IF(nerr.LT.mxerrpri) THEN
                     WRITE(6,*) ' Big change in coincidence rate, ievv='
     +                    ,ievv,jdif(3,istack),jdif(3,isprev)
                  ENDIF
               ENDIF
            ENDIF
C
C---       Reject too low rate of coincidence
            IF(ierr.EQ.0) THEN
               IF(jdif(3,istack).LT.10) THEN
                  ierr=1
                  NASYM(13)=NASYM(13)+1
                  nerr=nerr+1
                  IF(nerr.LT.mxerrpri) THEN
                     WRITE(6,*) ' Low coincidence rate, ievv='
     +                    ,ievv,jdif(3,istack)
               ENDIF
            ENDIF
C
C---       Reject too low rate of coincidence
            IF(ierr.EQ.0) THEN
                  ierr=1
                  NASYM(14)=NASYM(14)+1
                  nerr=nerr+1
                  IF(nerr.LT.mxerrpri) THEN
                     WRITE(6,*) ' Low coincidence rate, ievv='
     +                    ,ievv,jdif(3,istack)
               ENDIF
            ENDIF
C
         ENDIF
C
         IF(ierr.NE.0) jcycer(istack)=1
      ENDIF
C
C---     Assign the delayed helicity to the appropriate cycle 
C
      ispdel=0
      IF(jhelcur(istack).GE.0.AND.ievv.GT.idelcur) THEN
C
C---   Check all the cycles up to the right one
         k=0
         ispdel=istack
         DO i=1,idelcur
            ispdel=ispdel-1
            IF(ispdel.LE.0) ispdel=ispdel+mxstack
            IF(jhelcur(ispdel).LT.0) k=1
         ENDDO
         IF(k.NE.0) THEN
            ispdel=0
         ELSE
            IF(LTICK.GT.0) THEN
               IF(IABS(jtick(istack)-jtick(ispdel)-LTICK*idelcur).GT.3)
     +                THEN
                  ispdel=0
                  NASYM(7)=NASYM(7)+1
                  nerr=nerr+1
                  IF(nerr.LT.mxerrpri) THEN
                     WRITE(6,*) ' Wrong tick count, delayed ievv='
     +                    ,ievv,jtick(istack)-jtick(ispdel)
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         IF(ispdel.GT.0) jhelrec(ispdel)=jhelrec(istack)
      ENDIF
            
C
C!!      iticksca=ITICK
      IF(iticksca.NE.0)  CALL HF1(ID+50,itickev-iticksca+.1,1.)
      IF(itickadcl.NE.0) CALL HF1(ID+51,itickev-itickadcl+.1,1.)
      IF(itickadcl.NE.0) CALL HF1(ID+52,itickadcl-itickadcf+.1,1.)
      CALL HF1(ID+53,ievv+.1,REAL(iticksc-iticksc0+.1))
C
      IF(ITRIG(6).GE.0) THEN
         DO i=1,3
            CALL HF1(ID+53+i,
     +           ISCA(i)-jsca(i)-ISCA(i+16)+jsca(i+16)+0.1,1.)
         END DO
      ENDIF
C
C---     Reset the ticks for the ADC triggers
C
      itickadcf=0
      itickadcl=0
C
      CALL HF1(ID+30,ievv+.1,REAL(jhel))
      CALL HF1(ID+99,ievv+.1,REAL(jhelcyc))
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
               jdif(5,idelcur)=BCMLIN(1)+jdif(5,idelcur)*BCMLIN(2)
     +                     +jdif(5,idelcur)**2*BCMLIN(3)
C            ENDIF
         ENDIF
         DO i=1,32
            dsum2(i)=dsum2(i)+(DBLE(jdif(i,idelcur)))**2
            m=jdif(i,idelcur)
            nsumh(i,jhel)=nsumh(i,jhel)+m
            dsumh2(i,jhel)=dsumh2(i,jhel)+(DBLE(m))**2
C            WRITE(6,*) 'i,jdef ',i,jdif(i,idelcur)
         ENDDO
         NASYM(17+jhel)=nsumh(5,jhel)
C         WRITE(6,*) 'jhel..',nhelcycl(1)+nhelcycl(2)
C     +         ,jhelst(1),jhelst(idelcur)
C     +         ,jdif(3,1),jdif(3,idelcur)
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
      DO i=1,nscalim
         jsca(i)=ISCA(i)
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
      DO i=1,nscalim
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
         IF(nhelcycl(1).GT.1.AND.nhelcycl(1).GT.1.AND.
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
                  IF(m.GT.0) ddb=da(m,j)
                  IF(i.EQ.12) THEN
                     dnrm=DBLE(nhelcycl(j))
                     dds=dera(j)*DBLE(nhelcycl(j))**2
                  ENDIF
                  m=masnor(i)
                  IF(m.GT.0)  THEN
                     dnrm=da(m,j)
                     IF(m.EQ.12) dds=dera(j)*DBLE(nhelcycl(j))
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
         WRITE(6,2000) ievv,nhelcycl(1)+nhelcycl(2),RASYM(3),ERASYM(3)
     +        ,FASYM(33),EFASYM(33)
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
C      ASYMMETS1=ievv
C
 999  CONTINUE
      END



