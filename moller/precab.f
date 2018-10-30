      REAL FUNCTION PRECAB(E)
C
C===    Precession difference between halls A abd B
C
      IMPLICIT NONE
      REAL E
      VECTOR VPREC(8)
      REAL SPINPRED
      INTEGER npassa,npassb
      REAL preca,precb,elin,ratinj
C
C     ------------------------------------------------------------------
C
      npassa=5
      npassb=5
      ratinj=0.1125
      VPREC(4)=1.
C
      elin=E/(2*npassa+ratinj)
C      elin=E
      VPREC(1)=elin
C
      VPREC(2)=1.
      VPREC(3)=npassa
      preca=SPINPRED(0.)
C
      VPREC(2)=2.
      VPREC(3)=npassb
      precb=SPINPRED(0.)
C
      PRECAB=(preca-precb)/180.
      END
C
      INCLUDE 'spinpred.f'
