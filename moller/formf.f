      REAL FUNCTION FORMF(Q)
C
C---      Formfactor
C
      IMPLICIT NONE
      REAL Q
      REAL r,a,z,qf,qr
C
      z=26.
      a=56.
C
      r=1.07*a**0.3333
      qf=Q/0.197
      qr=qf*r
C
      FORMF=3.*z/qr**3*(SIN(qr)-qr*COS(qr))
      FORMF=FORMF**2
      WRITE(6,*) r,qf,qr,FORMF
C     
      END
