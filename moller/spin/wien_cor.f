      REAL FUNCTION WIEN_COR(W)
C
C==      Correction of the Wien filter angles: after Joe Grames
C==      Input -  Wien angle (from the window) degrees
C
      IMPLICIT NONE
      REAL W
C
      REAL par(3),epar(3),val,err
      DATA  par/-0.2725, 1.0223, 0.69128/  ! Grames's parameters
      DATA epar/ 0.0006, 0.0036, 0.16286/
C
      val=par(2)*W+par(3)
      err=SQRT((epar(2)*val)**2
     +        + epar(3)**2)
C
      WIEN_COR=val
C
      WRITE(6,1000) val,err
 1000 FORMAT(' Angle=',F8.2,' +/- ',F6.2)
      END
