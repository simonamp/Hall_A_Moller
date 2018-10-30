      PROGRAM HTST
C
C
      INTEGER    mxlenb,mxeve,mxhbook
      PARAMETER (mxlenb=8192,mxeve=500000,mxhbook=20)
C
      INTEGER mhbook
      COMMON/PAWC/ mhbook(mxhbook)
C
C
C
      CALL HLIMIT(mxhbook)

      END
