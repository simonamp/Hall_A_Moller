      REAL FUNCTION SHOWERT(X)
C
C=== Shower simulation
C===                
C
      IMPLICIT REAL(A-H,O-Z),INTEGER(J-N)
C
      siz=1.
      an=0.4 
      ap=2.8*8./siz
      bn=0.5-an
      bp=0.44*8./siz
C
      a1=an*EXP(-ap*(siz/2.+X))+bn*EXP(-bp*(siz/2.+X))
      a3=an*EXP(-ap*(siz/2.-X))+bn*EXP(-bp*(siz/2.-X))
      a2=1-a2-a3
      SHOWERT=a1*(-siz)+a3*siz
C      SHOWERT=ALOG10(a1/a3)
      SHOWERT=a3*siz
      GO TO 999
C
      y0=SHOWERT
C      WRITE(6,*) 'X,Y=',X,y0
      x0=X
      y=0.
      xx=0.
      step=0.1
      IF(y0.LT.0.) step=-0.1
 10   xx=xx+step
C      WRITE(6,*) 'xx=',xx
      y=SHOWERF(xx)
      IF(ABS(y).LT.ABS(y0)) GO TO 10
C
      nstep=0
 20   nstep=nstep+1
      x1=xx+0.01
      y1=SHOWERF(x1)
      der=(y1-y)/(x1-xx)
      dx=-(y-y0)/der
      xx=xx+dx
      y=SHOWERF(xx)
      del=y-y0
      IF(ABS(del).GT.0.0001) GO TO 20
C      WRITE(6,*) 'nstep,del=',nstep,del,y0,y,x0,xx,X
C
      SHOWERT=xx
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
C
      siz=1.
      an=0.4 
      ap=2.8*8./siz
      bn=0.5-an
      bp=0.44*8./siz
C
      a1=an*EXP(-ap*(siz/2.+X))+bn*EXP(-bp*(siz/2.+X))
      a3=an*EXP(-ap*(siz/2.-X))+bn*EXP(-bp*(siz/2.-X))
      a2=1-a2-a3
      SHOWERF=a1*(-siz)+a3*siz
C
      END


