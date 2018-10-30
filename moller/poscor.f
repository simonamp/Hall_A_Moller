      REAL FUNCTION POSCOR(X)
C
C=== Lead glass position correction.
C
      IMPLICIT REAL(A-H,O-Z),INTEGER(J-N)
C
      VECTOR VPOSCOR(500,4,2)
      VECTOR VPOSTEP(1)
C
      POSCOR=X
C
      k=INT(X+0.5)
      xd=X-REAL(k)
C
      iarm=2
C
      nch=(1./VPOSTEP(1)+0.1)
      dx=(xd+0.5)/VPOSTEP(1)
      i=INT(dx)
      ddx=dx-REAL(i)
C      WRITE(6,*) 'dx,i,ddx ',dx,i,ddx
      i=i+1
      IF(i.LT.1.OR.i.GT.nch) THEN
        WRITE(6,*) 'poscor i,X,k,xd=',i,X,k,xd
        IF(i.LT.0) THEN
           i=1
        ELSE
           i=nch
        ENDIF
      ENDIF
      y1=VPOSCOR(i,k,iarm)
      IF(i.LT.nch) THEN
         y2=VPOSCOR(i+1,k,iarm)
         dy=(y2-y1)*ddx
         y1=y1+dy
      ENDIF
      y=y1
C
      POSCOR=y+REAL(k)-0.5
C      POSCOR=ddx
C
      END


