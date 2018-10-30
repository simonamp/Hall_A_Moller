      REAL FUNCTION SETT_MAG(E)
      IMPLICIT NONE
      REAL E
C
      VECTOR MAGN(1)
      VECTOR NQUADS(1)
C
      REAL par(3,3,4),elim(4),p,x
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
     +   , -3.80181,  1.89482, -0.13590
     +   ,  0.506  ,  1.224  , -0.2167
     +   ,  0.     ,  0.     ,  0.
     +   , -0.125  ,  0.461  ,  0.0285
     +  /
C
C     -----------------------------------------------------------------
C
      SETT_MAG=-99.
C
      interv=0
      DO i=1,4
         IF(E.LT.elim(i)) GO TO 10
         interv=i
      ENDDO
 10   IF(interv.EQ.0.OR.interv.EQ.4) GO TO 999
      IF(NQUADS(1).EQ.2) interv=4
C
      mag=MAGN(1)
      IF(mag.LE.3) THEN
         p=par(1,mag,interv)+par(2,mag,interv)*E+par(3,mag,interv)*E**2
      ELSE IF(mag.EQ.4) THEN
         p=1.74*E
      ENDIF
      SETT_MAG=p
C
 999  RETURN
      END
