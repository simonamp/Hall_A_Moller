C
      INTEGER MXDEL,MXTHR,MXPLU
      PARAMETER (MXDEL=16,MXTHR=16,MXPLU=16)
      COMMON/CRUNINF/ IRUNI,IDATI(5),NDELAYI,IDELAYI(MXDEL)
     +               ,NTHREI,ITHREI(MXTHR),NPLUI,IPLUI(3,MXPLU)
     +               ,WIDTHI,ENERI,BPMI(2,2),BCMI(5),SLDI(2)
     +               ,AMAGNI(4),POSTAI(2),ITARGI
     +               ,TEMPTI,HELCOIL
     +               ,WIENAN,TADIAL
C
      INTEGER         IRUNI,IDATI
     +               ,NDELAYI,IDELAYI,NTHREI,ITHREI
     +               ,NPLUI,IPLUI
     +               ,ITARGI
      REAL            WIDTHI
     +               ,ENERI,BPMI,BCMI,SLDI
     +               ,AMAGNI,POSTAI
     +               ,TEMPTI,HELCOIL
     +               ,WIENAN,TADIAL
C
