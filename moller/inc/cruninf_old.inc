C
      INTEGER MXDEL,MXTHR,MXPLU,MXRUNS
      PARAMETER (MXDEL=16,MXTHR=16,MXPLU=16,MXRUNS=500)
      COMMON/CRUNINF/ IRUNR(MXRUNS),IDATR(5,MXRUNS)
     +               ,NDELAYR(MXRUNS),IDELAYR(MXDEL,MXRUNS)
     +               ,NTHRER(MXRUNS),ITHRER(MXTHR,MXRUNS)
     +               ,NPLUR(MXRUNS),IPLUR(3,MXPLU,MXRUNS)
     +               ,WIDTHR(MXRUNS)
     +               ,ENERR(MXRUNS)
     +               ,BPMR(2,2,MXRUNS),BCMR(5,MXRUNS),SLDR(2,MXRUNS)
     +               ,AMAGNR(4,MXRUNS),POSTAR(2,MXRUNS),ITARGR(MXRUNS)
     +               ,TEMPTR(MXRUNS),HELCOIL(MXRUNS)
     +               ,WIENAN(MXRUNS),TADIAL(MXRUNS)
C
      INTEGER         IRUNR,IDATR
     +               ,NDELAYR,IDELAYR,NTHRER,ITHRER
     +               ,NPLUR,IPLUR
     +               ,ITARGR
      REAL            WIDTHR
     +               ,ENERR,BPMR,BCMR,SLDR
     +               ,AMAGNR,POSTAR
     +               ,TEMPTR,HELCOIL
     +               ,WIENAN,TADIAL
C
