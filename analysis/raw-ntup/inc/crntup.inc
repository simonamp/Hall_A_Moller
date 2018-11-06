C
C
C=== crntup.inc  RAW data converted to a NTUPLE-ready COMMON
C

      INTEGER MXADC,MXTDC,MXTDCW,MXSCAL,MXTRIG
      PARAMETER (MXADC=36,MXTDC=256,MXTDCW=MXTDC*16,MXSCAL=33,MXTRIG=8)
C
      COMMON/CRNTUP/ ITYP,IRETC
     +              ,NADC,IADC(MXADC)
     +              ,NTDC,ITCH(MXTDCW),ITIM(MXTDCW),ITED(MXTDCW)
     +              ,NSCA,ISCA(MXSCAL)
     +              ,ITRIG(MXTRIG)
     +              ,ITIMTICK
C
      INTEGER        ITYP   ! our data type
     +              ,IRETC  ! return code
     +              ,NADC   ! number of ADC hits
     +              ,IADC   ! amplitudes
     +              ,NTDC   ! number of TDC hits it=1,NTDC
     +              ,ITCH   ! (it) - channel number
     +              ,ITIM   ! (it) - time
     +              ,ITED   ! (it) - =1 - leading edge
     +              ,NSCA   ! number of scalers
     +              ,ISCA   ! scalers
     +              ,ITRIG  ! triggers and the other tir_dat values (0 or 1)
     +              ,ITIMTICK ! time tick from the VxWorks(1/120 sec)
C
