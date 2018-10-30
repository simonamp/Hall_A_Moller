      REAL FUNCTION SHOWER_F(Y)
C
C=== Fit
C===                
C
      IMPLICIT REAL(A-H,O-Z),INTEGER(J-N)
C
      COMMON/PAWPAR/ PAR(5)
C      VECTOR PAR(20)
C
C--- Find X
C
      y0=Y
C      WRITE(6,*) 'X,Y=',X,y0
      xx=0.
      step=0.1
      IF(y0.LT.0.) step=-0.1
 10   xx=xx+step
C      WRITE(6,*) 'xx=',xx
      yy=SHOWERF(xx)
      IF(ABS(yy).LT.ABS(y0)) GO TO 10
C
      nstep=0
 20   nstep=nstep+1
      x1=xx+0.01
      y1=SHOWERF(x1)
      der=(y1-yy)/(x1-xx)
      dx=-(yy-y0)/der
      xx=xx+dx
      yy=SHOWERF(xx)
      del=yy-y0
      IF(ABS(del).GT.0.0001) GO TO 20
C      WRITE(6,*) 'nstep,del=',nstep,del,y0,y,x0,xx,X
C
      SHOWER_F=PAR(1)*(1.-0.0*xx)/der
C      SHOWER_F=xx
C
      WRITE(6,*) 'Y,FUN=',Y,SHOWER_F
C
 999  CONTINUE
      END
C
      REAL FUNCTION SHOWERF(X)
C
C=== Shower simulation
C===                
C
      IMPLICIT REAL(A-H,O-Z),INTEGER(J-N)
      COMMON/PAWPAR/ PAR(5)
C
C      VECTOR PAR(20)
C
      siz=1.
      an=PAR(2)
      ap=PAR(3)*8./siz
      bn=0.5-an
      bp=PAR(4)*8./siz
C
C
      a1=an*EXP(-ap*(siz/2.+X))+bn*EXP(-bp*(siz/2.+X))
      a3=an*EXP(-ap*(siz/2.-X))+bn*EXP(-bp*(siz/2.-X))
      a2=1-a2-a3
      SHOWERF=a1*(-siz)+a3*siz
C
      END


