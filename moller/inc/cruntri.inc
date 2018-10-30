C
C===      Information from the triggers for the current run
C===      ADC peak positions
C
      INTEGER    MXAMPL        ! max number of peaks (Gauss-fitted)
      PARAMETER (MXAMPL=20)
C
      COMMON/CRUNTRI/ IRUNT,NADCMX,NADCT
     +               ,PEAKADC(3,MXAMPL),EPEAKADC(3,MXAMPL)
C
      INTEGER IRUNT     ! the current run
     +       ,NADCMX    ! =MXAMPL - dimension of the array PEAK... 
     +       ,NADCT     ! number of ADC channels 
C
C                          i=1,NADCT+3 (+1 - Left, +2 - Right, +3 - Sum)
      REAL    PEAKADC   ! (j,i) - Gauss fit parameters j=1 - ampl, 2-mean, 3-sigma 
     +      ,EPEAKADC   ! errors on the fit 
C
      REAL         RADCMX,RADCT
      EQUIVALENCE (RADCMX,NADCMX),(RADCT,NADCT)
C