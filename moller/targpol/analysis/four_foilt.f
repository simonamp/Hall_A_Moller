      PROGRAM FOUR_FOIL
C
C --- Fourier analysis of the foil magnetic spectrum
C
      IMPLICIT NONE
      INTEGER    mxp,mxps,mxf
      PARAMETER (mxp=80000,mxps=80000,mxf=10000)
      REAL vin(mxp),fk(mxf)
      DOUBLE PRECISION dv1(mxps),dv2(mxps),dfourk(0:mxf,2)
      DOUBLE PRECISION dpi,dbin,df,dx,dom,dom0,dfus,dfuc,dres,dnorm,dgap
      INTEGER lun,np,np1,i,j,jf,k,m,k1,k2,m1,m2,mgap
      REAL pi,f,a,x,dt
C
      INTEGER mpow,mfla
      COMPLEX cx(mxps),cy(mxps)
      REAL    rx(mxps),ry(mxps)
      EQUIVALENCE (cx,rx)
C
      lun=21
      np=0
 10   READ(lun,*,END=20) a
      np=np+1
      vin(np)=a
      GO TO 10
C
 20   CONTINUE
C      k1=30001
C
      mpow=15
      k2=np
      k1=k2-2**mpow+1
      np1=k2-k1+1
      WRITE(6,*) ' Number of points =',np1,k1,k2
C
      j=0
      DO i=k1,k2
         j=j+1
         rx(j)=vin(i)
         ry(j)=vin(i)
      ENDDO
C
      mfla=-mpow
      CALL RFSTFT(mfla,cx)
C
      mfla=mpow
      CALL RFSTFT(mfla,cx)
C
C      DO i=1,10
C         WRITE(6,*) rx(i)
C      ENDDO 
C
C      DO i=1,np1/2
C         WRITE(6,*) i,cx(i)
C      ENDDO 
      WRITE(6,FMT='(I9,2F12.8,D16.8)') (i,rx(i),ry(i),rx(i)-ry(i)
     +     ,i=1,np1)
C
C---     Fourier series
C
C
C      WRITE(22,FMT='(I7,E14.5,5X,2E13.5)') 
C     +         (jf,fk(jf),(REAL(dfourk(jf,i)),i=1,2),jf=0,mxf)
C      WRITE(23,FMT='(E13.5)') 
C     +         (REAL(dv2(i)),i=1,np1)
      END
C
C      INCLUDE 'rfstft.F'
C      INCLUDE 'cfstft.F'
