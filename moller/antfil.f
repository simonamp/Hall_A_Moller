      SUBROUTINE ANTFIL(ID0)
C
C---     Fill the CW NTUPLE for asymmetry (scaler) data
C
      IMPLICIT NONE
      INTEGER ID0
C
      INTEGER id
C
C     ------------------------------------------------------------------
C
      id=ID0
      CALL HFNT(id)
C
 999  CONTINUE
      END
