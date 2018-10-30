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
      INTEGER lun,np,np1,i,jf,k,m,k1,k2,m1,m2,mgap
      REAL pi,f,a,x,dt
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
      k1=1
      k2=np
      np1=k2-k1+1
C
      m1=20001
      m2=30000
      mgap=m2-m1+1
C
      DO i=1,np1
         dv1(i)=vin(k1+i-1)
      ENDDO
C
C---     Fourier series
C
      dpi=2.D0*DACOS(0.D0)
      dbin=1.D-4
      dt=DBLE(np1)*dbin
      dgap=DBLE(mgap)*dbin
      dom0=2.D0*dpi/dt
      dnorm=2.D0/(dt-dgap)
C
      DO i=1,np1
         dv2(i)=0.D0
         dx=-dt/2.D0+(i-0.5D0)*dbin
C         dv1(i)=0.0001D0*DCOS(60.D0*2.D0*dpi*dx+0.3)
      ENDDO

      WRITE(6,*) 'Start the Fourier transform for ',mxf
     +          ,' waves and ',np1,' points'
      DO jf=0,mxf
         dom=DBLE(jf)*dom0
         fk(jf)=REAL(dom/2.D0/dpi)
C
         dfus=0.D0
         dfuc=0.D0
         DO i=1,np1
            IF(i.LT.m1.OR.i.GT.m2) THEN
               dx=-dt/2.D0+(i-0.5D0)*dbin
               dfuc=dfuc+dv1(i)*DCOS(dom*dx)
               dfus=dfus+dv1(i)*DSIN(dom*dx)
            ENDIF
         ENDDO
         dfourk(jf,1)=dfuc*dbin*dnorm
         dfourk(jf,2)=dfus*dbin*dnorm
C
C         DO i=1,np1
C            dv2(i)=dv2(i)+
C            dx=-dt/2.D0+(i-0.5D0)*dbin
CC           dv1(i)=0.0001D0*DCOS(60.D0*2.D0*dpi*dx+0.3)
C         ENDDO
      ENDDO
C
      WRITE(6,*) 'Start reconstructing the function'
      DO i=1,np1
         dres=dfourk(0,1)/2.
C
         DO jf=1,mxf
            dom=DBLE(jf)*dom0
C
            dx=-dt/2.D0+(i-0.5D0)*dbin
            dres=dres+dfourk(jf,1)*DCOS(dom*dx)
     +               +dfourk(jf,2)*DSIN(dom*dx)
         ENDDO
         dv2(i)=dres
      ENDDO
C
      WRITE(22,FMT='(I7,E14.5,5X,2E13.5)') 
     +         (jf,fk(jf),(REAL(dfourk(jf,i)),i=1,2),jf=0,mxf)
      WRITE(23,FMT='(E13.5)') 
     +         (REAL(dv2(i)),i=1,np1)
      END
