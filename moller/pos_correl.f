      REAL FUNCTION POS_CORREL(X)
C
C=== Lead glass position L-R correlation
C
      IMPLICIT REAL(A-H,O-Z),INTEGER(J-N)
C
C
      siz=8.
      ame=271.*SIN(11.*3.1415/180.)
      ash=-siz*0.4
      y=(X-2.5)*siz+ame
      POS_CORREL=(y/(y*2./(ame+ash)-1.)-ame)/siz+2.5
C
      END


