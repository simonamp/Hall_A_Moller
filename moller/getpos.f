      REAL FUNCTION GETPOS(ICOL,IFL)
C
C=== Get the hit position (baricenter) of the column ICOL 
C===                             (=1 -Left,=2 - Right)
C=== IFL = 0 - baricenter, =1 - with correction, =2 - LOG weights
C
      IMPLICIT REAL(A-H,O-Z),INTEGER(J-N)
      INCLUDE ?
C
      VECTOR IDD0(1)
      VECTOR PEDES(24)
      VECTOR CALIB(24)
C
      DATA ievv/0/
C
      id0=IDD0(1)
      ievv=ievv+1

      earm=0.
      pos=0.
      i1=1
      IF(ICOL.EQ.2) i1=5
C
C---    Find the max
C
      imx=0
      i2=i1+3
      DO i=i1,i2
        a=IADC(i)-PEDES(i)
        IF(a.GT.earm) THEN
          earm=a
          imx=i
        ENDIF
      END DO
C
C      pch=-2.5
      pch=0.
      IF(imx-i1+1.LE.2) THEN
         i2=i1+2
      ELSE
         i1=i1+1
         pch=pch+1.
      ENDIF
C
      earm=0
C      IF(ievv.LT.20) write(6,*) 'i1,i2=',i1,i2
      DO i=i1,i2
        pch=pch+1.
        a=IADC(i)-PEDES(i)
        e=a*CALIB(i)
        IF(IFL.EQ.2) e=ALOG(e+1.E-8)
        earm=earm+e
        pos=pos+pch*e
      END DO
C
      pos=pos/earm
      GETPOS=pos
C
      IF(IFL.NE.1) GO TO 999
C
C---     Correction (a la Heinrich)
C
      y=pos
      y0=INT(y)+0.5
      dy=y-y0
      dd=dy*2.
      dd2=dd**2
      dy1=dy
      fac=2.6
      IF(ABS(dy).GT.0.01) THEN
        dy1=0.5*SINH(fac*dy*8.)/SINH(fac*0.5*8.)
      ELSE
        dy1=0.5*SINH(fac*0.42*8.)/SINH(fac*0.5*8.)*dy/0.42
      ENDIF
      y1=y0+dy1
      pos=y1
C
      GETPOS=pos
C
 999  CONTINUE
      END









