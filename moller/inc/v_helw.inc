C == Vectors for helicities window-wise
      INTEGER MXHELW
      PARAMETER (MXHELW=4411)
      VECTOR KHELWIN(4411)
      VECTOR KTICWIN(4411)
      VECTOR KFLAWIN(4411)
      VECTOR KRECWIN(4411)
      VECTOR KPAIWIN(4411)
      VECTOR KREFWIN(4411)
      VECTOR NCOWIN(8)
C --- NCOWIN(1) - number of windows
C ---       (2) - oversampling (typically =1)
C ---       (3) - =0 - pseudorandom,=1 - toggle
C ---       (4) - nevadc                       
C ---       (5) - ticks/sample 
