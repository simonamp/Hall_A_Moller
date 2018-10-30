C
C===      Counting rates/polarizations for a given run
C
      INTEGER MXCNT,MXSCAL,MXASYM,MXPOL,MXADD,MXINTE,MXREAL
      PARAMETER (MXCNT=20,MXSCAL=16,MXASYM=10,MXPOL=10,MXADD=8)
      PARAMETER (MXINTE=10+MXCNT
     +          ,MXREAL=2*MXSCAL*2+2*MXPOL+4*MXASYM*4+5+MXADD)
C
      COMMON/CRUNPOL/ IRUNR
     +               ,NCOUNTR,KCOUNTR(MXCNT)
     +               ,NSCALER,NASYMR,NPOLR
     +               ,NEVADC,NORM,NORBCM,NOCUT,MXDIFF,IACSUB
     +               ,NADDVR
     +               ,SCALER(2,MXSCAL,2)
     +               ,ASYMR(2,MXASYM,4)
C     +               ,ASYMCR(2,MXASYM)
     +               ,POLR(2,MXPOL)
     +               ,TANGL,ANPOW,PTARG,GATE,COILP
     +               ,ADDVR(MXADD)
C
C
      INTEGER IRUNR     ! run number
     +       ,NCOUNTR   ! number of counters (number of hel cycles ...) 
     +       ,KCOUNTR   ! (1) - full number of H+ cycles, (2) - full number of H-,
C                         (3) accepted H+, (4) - accepted H-, (5-16)- cycles
C                          lost due to various reasons  
     +       ,NSCALER   ! number of scalers 
     +       ,NASYMR    ! number of asymmetry values 
     +       ,NPOLR     ! number of polarization values 
     +       ,NEVADC    ! number of ADC triggers in the readout window
     +       ,NORM      ! >0 use BCM for normalization
     +       ,NORBCM    ! >0 correct for BCM linearity
     +       ,NOCUT     ! >0 do not reject cycles
     +       ,MXDIFF    ! max difference on 2 scalers
     +       ,IACSUB    ! >0 - subtract accidentals from coinc
     +       ,NADDVR    ! additional values
C
      REAL    SCALER    ! (1,k) - counters rates / sec, averaged over the run:
C                         (2,k) - errors
C                         1-Left, 2-Right, 3-Coin, 4-Accid, 5-BCM, 6-Clock, 7 - LED
C                         8-11 - like 1-4 but taken from the 2-nd scaler (parallel)
C                         k=1 - scalers,
C                           2 - scalers normalized to the BCM
     +       ,ASYMR     ! raw asymmetry (1,i,j) - asymmetry, (2,i,j) -error
C                         i=1 - Left arm, 2 - Right, 3 - coin, 4 - accid, 5 - BCM(normalized to the clock)
C                           6 - clock (normalized to the number of cycles), 
C                           7-10 - like 1-4 but using the 2-nd scaler (a parallel one)
C                           1-4,7-10 - normalized to the BCM, 5 - norm. to clock, 6 - norm. to cycles
C                           3,9 - accidentals subtracted
C                         j=1 - normalization is done for the whole sample
C                           2 - normalization is done for each cycles
C                           3 - like 1, but no BG (accid) subtraction
     +       ,POLR      ! (1,i) polarization, (2,i) -error, 
C                              i=1 - coin, 2- left, 3 - right, 4-6 - the same using 2-nd scaler
C     +       ,ASYMCR    ! as above, but corrected for the BCM
     +       ,TANGL     ! target angle (from the dial) (set by hand)
     +       ,ANPOW     ! analyzing power     (set by hand)
     +       ,PTARG     ! target polarization (set by hand)
     +       ,GATE      ! readout cycle (sec) - either 1 or 1/2 of the helicity gate
     +       ,COILP     ! coil polarity (set by hand)
     +       ,ADDVR     ! additional parameters
C



