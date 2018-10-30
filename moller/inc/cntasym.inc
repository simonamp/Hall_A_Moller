C=== cntasym.inc 
C=== Data for the asymmetry NTUPLE
C
      INTEGER    MXELEM,MXCNT
      PARAMETER (MXELEM=4,MXCNT=32)
C
      COMMON/CNTASYM/ KRUN               ! run number
     +               ,ANGL               ! target angle, degree
     +               ,HELMH              ! current in Helmholtz coils
     +               ,PTARG              ! target polarization
     +               ,ITARG              ! target number
     +               ,XYTARG(2)          ! relative beam position relative to the target center target
     +               ,KZUG               ! the train (quadruplet) number
     +               ,IFZUG              ! flag (=0 - the zug is OK) 
     +               ,NELEM              ! number of helicity gates in the train
     +               ,NSCAL              ! number of scaler channels
     +               ,JTICK(MXELEM)      ! scaler trigger tick values
     +               ,JDTICKAF(MXELEM)   ! diff tick(scal) - 1-st ADC tick 
     +               ,JDTICKAL(MXELEM)   ! diff tick(scal) - last ADC tick 
     +               ,JFLA(MXELEM)       ! flag, =0 -OK
     +               ,JADC(MXELEM)       ! number of ADC events in DAQ for this window
     +               ,JHEL(MXELEM)       ! helicity values
     +               ,JCNT(MXCNT,MXELEM) ! scaler counts
C
      INTEGER         KRUN
     +               ,ITARG
     +               ,KZUG
     +               ,IFZUG  
     +               ,NELEM 
     +               ,NSCAL
     +               ,JTICK
     +               ,JDTICKAF
     +               ,JDTICKAL
     +               ,JFLA
     +               ,JADC
     +               ,JHEL
     +               ,JCNT
C
      REAL            ANGL
     +               ,HELMH
     +               ,PTARG
     +               ,XYTARG
