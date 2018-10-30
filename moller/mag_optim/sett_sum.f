      REAL FUNCTION SETT_SUM(E)
      IMPLICIT NONE
      REAL E
C
      VECTOR MAGN(1)
C
      REAL par(3,3,3),elim(4),p,x,q
      INTEGER i,j,mag,interv
C
      DATA elim/0.7,2.0,5.9,8.0/
      DATA par /
     +      0.515  ,  1.3046 , -0.26062
     +   ,  0.     ,  0.     ,  0.
     +   , -0.49378,  1.02605, -0.18970
     +   ,  0.515  ,  1.3046 , -0.26062
     +   , -0.73333,  0.36667,  0.
     +   ,  0.76296, -0.13713,  0.07855
     +   , 12.698  , -3.5253 ,  0.20862
     +   , -0.73333,  0.36667,  0.
     +   , -3.80181,  1.89482, -0.13590/
C
C     -----------------------------------------------------------------
C
      SETT_SUM=-99.
C
      interv=0
      DO i=1,4
         IF(E.LT.elim(i)) GO TO 10
         interv=i
      ENDDO
 10   IF(interv.EQ.0.OR.interv.EQ.4) GO TO 999
C
      mag=MAGN(1)
      IF(mag.LE.3) THEN
         p=0
         DO i=1,3
            q=par(1,i,interv)+par(2,i,interv)*E+par(3,i,interv)*E**2
            p=p+q
         ENDDO
      ELSE IF(mag.EQ.4) THEN
         p=1.74*E
      ENDIF
      SETT_SUM=p
C
 999  RETURN
      END

