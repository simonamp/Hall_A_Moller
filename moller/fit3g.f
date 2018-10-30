      REAL FUNCTION FIT3G(X)
C
      IMPLICIT REAL(A-H,O-Z),INTEGER(J-N)
      COMMON/PAWPAR/ PAR(6)
C
      
      FIT3G=PAR(1)*EXP(-((X-PAR(2))/PAR(3))**2/2.)
     +     +PAR(4)*EXP(-((X-PAR(2)*PAR(6))/(2.*PAR(3)*PAR(5)))**2/2.)
     +     +PAR(4)*EXP(-((X-PAR(2))/(PAR(3)*PAR(5)))**2/2.)
C
      END
