C      PROGRAM TEMP_TAR
C      SUBROUTINE TEMP_TAR(TIM)
C
C ---  Calculates the target temperature at a given time TIM.
C      2-dim thermal flow equation.
C      Target: 30x3cm**2 (SLAC)
C      Beam: 1mm**2
C      Grid: "temper. transfer" from element 2 to 1 (adjacent): 
C      Delta(T)=(T2-T1)/dx**2*ak/rho/cv*Delta(t)
C
      IMPLICIT NONE
      REAL TIM
C
      INTEGER ix,iy,it,i,j,nt,ntv,ipulse,ib
     +    ,iyp(3),iost,ixd,iyd,idv1,dtav
     +    ,ix1,ix2,iy1,iy2,ixdc,iydc,ipri,npulses,iradiat
      REAL ak,rho,cv,pulse,ang,fac,scal,tim0
     +    ,dp,rb,dtmp,dtmpmx,t1,t0,dtv,tpulse,dtpulse
     +    ,dx,dy,xsiz,ysiz,zsiz,x,y,r2,dx2,dy2,dt,depos
     +    ,xd,yd,qq,dxd,dyd,dxd2,dyd2,xd1,yd1,dener,dflow
     +    ,enpulse,enfull,enfac,fradiat,troom,troom4,facr,fpulse
C
      INTEGER    mxx,mxy,mxt,mxbeam,mxd,mxdc,mxdi
      PARAMETER (mxx=140,mxy=14,mxt=100000,mxbeam=9000,mxd=8,mxdc=6)
C      PARAMETER (mxx=30,mxy=4,mxt=100000,mxbeam=9000,mxd=10,mxdc=2)
      PARAMETER (mxdi=mxd*mxdc)
      REAL tt0(mxy,mxx),tt(mxy,mxx)
      INTEGER*1 icell(mxy,mxx)    ! status of the cell =0 - ignore, 1 - ok, =2 - use the store t0 to add
      INTEGER*1 iadjc(4,mxy,mxx)  ! status of the adjacent cell (i,iy,ix) i=1,2, 3, 4 
C                                                                          x,y,-x,-y
C                                  =0 normal cell, =1 - temperature zero, =2 - no thermal contact
      REAL tdiv(mxdi,mxdi),tdiv0(mxdi,mxdi)   ! area with fine division 
      REAL ttbeam(mxbeam)
      INTEGER ixydbeam(2,mxbeam),nxybeam
C
      REAL ttim(mxt),ttemp(mxt)
C      VECTOR ttim(mxt),ttemp(mxt)
C
      DATA ak/0.75/        ! thermal conductivity W/cm/K
      DATA cv/0.4/         ! specific heat J/g/K
      DATA rho/8.0/        ! density g/cm**3
      DATA pulse/3.6E11/   ! e-/pulse 
      DATA ang/20./        ! beam angle, deg (20 deg)
      DATA depos/2.4E-13/  ! energy deposit by one particle J/(g/cm**2)
      DATA rb/0.1/        ! beam density: dN/ds = pulse/(2*pi*rb**2)*exp(-r**2/2/rb**2)
C                           in order to simulate the beam angle: the spot size AND beam
C                           flux are scaled by 1/sin(ang)
C                          RMS(X)=RMS(y)=1mm (from wire array measurements) E158
      DATA dtv/0.5/      ! time step for writing temperature into ttim 
      DATA fpulse/20./   ! pulse repetition rate
C
      TIM=2000.
      fradiat=0.1    ! radiation power: thematerial factor (0.1 is a guess)
      troom=300.            ! room temperature
      ipri=0
      xsiz=30.       !  x-size of the foil
      ysiz=3.        !  y-size of the foil
      zsiz=0.01      !  thickness
C
C---  Try to read the input data
C
      OPEN(UNIT=9,FILE='temp_tar.input',STATUS='OLD',IOSTAT=iost)
      IF(iost.EQ.0) THEN
         READ(9,*) TIM,pulse,fpulse,rb,zsiz,fradiat,dtv
         CLOSE(9)
         WRITE(6,*) 'Input file read in'
      ENDIF
      WRITE(6,*) ' Input data TIM,pulse,fpulse,rb,zsiz,fradiat,dtv '
      WRITE(6,*) TIM,pulse,fpulse,rb,zsiz,fradiat,dtv 
C
      IF(fpulse.LE.0.) THEN
         WRITE(6,*) ' *** Error: fpulse=0.'
         GO TO 999
      ENDIF
      pulse=pulse*1.E11
      tpulse=1./fpulse
      iradiat=0
      IF(fradiat.GT.0.0001) iradiat=1  ! flag to turn on radiation
      fradiat=5.67E-12*fradiat  ! radiation power in W/cm**2/K**4 (0.1 is a material factor - a guess)
      enfull=0.
      npulses=0
      troom4=troom**4
C
      IF(mxdc.GT.mxx-2.OR.mxdc.GT.mxy-2) THEN
         WRITE(6,*) ' *** Error: internal fine plug should be'
     +          ,' inside the foil ',mxdc,mxx,mxy
         GO TO 999
      ENDIF
C
      dx=xsiz/mxx
      dy=ysiz/mxy
      dx2=dx**2
      dy2=dy**2
      dxd=dx/mxd
      dyd=dy/mxd
      dxd2=dxd**2
      dyd2=dyd**2
C
      tim0=0.
C
C---    Define the fine division of the central area
C
      ix1=mxx/2-mxdc/2+1
      ix2=ix1+mxdc-1
      iy1=mxy/2-mxdc/2+1
      iy2=iy1+mxdc-1
      xd1=(ix1-1)*dx-xsiz/2.
      yd1=(iy1-1)*dy-ysiz/2.
C
      DO ix=1,mxx
         DO iy=1,mxy
            icell(iy,ix)=1
            DO i=1,4
               iadjc(i,iy,ix)=0
            ENDDO
         ENDDO
         iadjc(4,1  ,ix)=2
         iadjc(2,mxy,ix)=2
      ENDDO
      DO iy=1,mxy
         iadjc(3,iy,1  )=1
         iadjc(1,iy,mxx)=1
      ENDDO
      DO ix=ix1,ix1+mxdc-1
         DO iy=iy1,iy1+mxdc-1
            icell(iy,ix)=0
         ENDDO
         icell(  iy1-1   ,ix)=2
         icell(  iy1+mxdc,ix)=2
         iadjc(2,iy1-1   ,ix)=2
         iadjc(4,iy1+mxdc,ix)=2
      ENDDO
      DO iy=iy1,iy1+mxdc-1
         icell(  iy,ix1-1   )=2
         icell(  iy,ix1+mxdc)=2
         iadjc(1,iy,ix1-1   )=2
         iadjc(3,iy,ix1+mxdc)=2
      ENDDO
      IF(ipri.NE.0) THEN
         WRITE(6,FMT='(A20/(I4,3X,30I3))') 
     +   ' icell ',(ix,(icell(iy,ix),iy=1,mxy),ix=1,mxx)
         WRITE(6,FMT='(A20/(I4,3X,30I3))') 
     +   ' adj 1 ',(ix,(iadjc(1,iy,ix),iy=1,mxy),ix=1,mxx)
         WRITE(6,FMT='(A20/(I4,3X,30I3))') 
     +   ' adj 2 ',(ix,(iadjc(2,iy,ix),iy=1,mxy),ix=1,mxx)
         WRITE(6,FMT='(A20/(I4,3X,30I3))') 
     +   ' adj 3 ',(ix,(iadjc(3,iy,ix),iy=1,mxy),ix=1,mxx)
         WRITE(6,FMT='(A20/(I4,3X,30I3))') 
     +   ' adj 4 ',(ix,(iadjc(4,iy,ix),iy=1,mxy),ix=1,mxx)
      ENDIF
C
C---    Beam deposit per pulse (cut at 1% of the peak value)
C
      write(6,*) ' xd,yd ',ix1,ix2,iy1,iy2,xd1,yd1,dxd,dyd
      dtmpmx=0.
      nxybeam=0
      scal=1./SIN(ang*3.1415/180.)
      fac=pulse*depos/(2*3.14*rb**2)/cv
      DO ixd=1,mxdi
         xd=(ixd-1)*dxd+xd1
         DO iyd=1,mxdi
            yd=(iyd-1)*dyd+yd1
C
            tdiv(iyd,ixd)=0.
C
C --- Integrate the beam on 20x20 inside one cell 
            dtmp=0.
C      write(6,*) ' xd,yd ',xd1,yd1,dxd,dyd,xd,yd
            DO i=1,20
               x=(i-0.5)*dxd/20.+xd
               DO j=1,20
                  y=(j-0.5)*dyd/20.+yd
                  r2=(x/(rb*scal))**2+(y/rb)**2
                  dtmp=dtmp+exp(-0.5*r2)
               ENDDO
            ENDDO
            dtmp=dtmp/400.*fac
            tdiv(iyd,ixd)=dtmp
C            write(6,*) ixd,iyd,fac,scal,dtmp
            IF(dtmp.GT.dtmpmx) THEN
               ixdc=ixd
               iydc=iyd
               dtmpmx=dtmp
            ENDIF
         ENDDO
      ENDDO
      dener=0.
      DO ixd=1,mxdi
         DO iyd=1,mxdi
            dtmp=tdiv(iyd,ixd)
            tdiv(iyd,ixd)=0.   !   reset the temperatures
            IF(dtmp.GT.dtmpmx*0.01) THEN
               dener=dener+dtmp*cv*dxd*dyd*zsiz*rho
               IF(nxybeam.LT.mxbeam) THEN
                  nxybeam=nxybeam+1
                  ixydbeam(1,nxybeam)=ixd
                  ixydbeam(2,nxybeam)=iyd
                  ttbeam(nxybeam)=dtmp
C                  WRITE(6,*) 'beam',x,y,ix,iy,nxybeam,dtmp,dtmpmx
               ELSE
                  WRITE(6,*) ' *** Error: not enough space for beam'
     +                  ,ix,iy,dtmp
               ENDIF
            ENDIF
         ENDDO
      ENDDO
      enpulse=dener
C
      DO ix=1,mxx
         DO iy=1,mxy
            tt(iy,ix)=0.
         ENDDO
      ENDDO
C
      WRITE(6,*) ' Full energy (Joul) released per pulse =',dener
      WRITE(6,*) ' Beam center ',ixdc,iydc
     +          ,(ixdc-0.5)*dxd+xd1,(iydc-0.5)*dyd+yd1,dtmpmx
     +          ,tdiv(iydc,ixdc)
C
      dtpulse=0.
      ipulse=1
C
      nt=0
      ntv=0
 20   nt=nt+1
C
C---   Add a new pulse?
C
      IF(ipulse.NE.0) THEN
C         write(6,*) 'Pulse:',tim0
         dtpulse=0.
         DO ib=1,nxybeam
            ixd=ixydbeam(1,ib)
            iyd=ixydbeam(2,ib)
            tdiv(iyd,ixd)=tdiv(iyd,ixd)+ttbeam(ib) !   beam
C            write(6,*) ' beam ',ib,ixd,iyd,tdiv(iyd,ixd)
         ENDDO
         ipulse=0
         npulses=npulses+1
C---  Record the point: skip some points
         IF(MOD(npulses,MAX(INT(dtv/tpulse),1)).EQ.0) THEN
            IF(ntv.LT.mxt) THEN
               ntv=ntv+1
               ttim(ntv)=tim0
               ttemp(ntv)=tdiv(iydc,ixdc)
            ENDIF
         ENDIF
      ENDIF
C
C----  Iterration: estimate the temp rise for 1 sec, find the time iterval
C----   to keep the max rise at 0.2K
C
C -- Reset the rim of the coarse matrix (corners are rest twice)
C
      DO ix=ix1-1,ix1+mxdc
         tt0(iy1-1,ix)=0.
         tt0(iy1+mxdc,ix)=0.
      ENDDO
      DO iy=iy1-1,iy1+mxdc
         tt0(iy,ix1-1)=0.
         tt0(iy,ix1+mxdc)=0.
      ENDDO
C
C----  Start with the fine plug 
C
      fac=ak/rho/cv
      facr=2.*fradiat/zsiz/rho/cv
      dtmpmx=0.
C
      dener=0
      enfac=rho*dxd2*zsiz*cv
      qq=0
      DO ixd=1,mxdi
         DO iyd=1,mxdi
            qq=qq+tdiv(iyd,ixd)
         ENDDO
      ENDDO
      dener=dener+qq*enfac
      enfac=rho*dx*dy*zsiz*cv
      qq=0
      DO ix=1,mxx
         DO iy=1,mxy
            qq=qq+tt(iy,ix)
         ENDDO
      ENDDO
      dener=dener+qq*enfac
C      write(6,*) ' iter,dener=',nt,dener,tt(13,148),tt(14,148)
C
C---         Internal plug with fine granularity
C
      DO ixd=1,mxdi
         DO iyd=1,mxdi
            dp=0.
            t0=tdiv(iyd,ixd)
C
C---          Is the cell close to the edge?
C
            ix=0
            iy=0
            IF(ixd.EQ.1.OR.ixd.EQ.mxdi.OR.
     +         iyd.EQ.1.OR.iyd.EQ.mxdi) THEN
               ix=ix1+(ixd-1)/mxd ! the column number on the coarse cell
               iy=iy1+(iyd-1)/mxd 
            ENDIF
C
            IF(iyd.GT.1) THEN
               dp=dp+(tdiv(iyd-1,ixd)-t0)/dyd2
            ELSE                   ! the fine cell contacts a coarse cell    
               qq=(tt(iy-1,ix)-t0)/(dy+dyd)*2.*dxd
C               qq=0
               dp=dp+qq/dxd/dyd
               tt0(iy-1,ix)=tt0(iy-1,ix)-qq/dx/dy*fac
            ENDIF
C               
            IF(iyd.LT.mxdi) THEN
               dp=dp+(tdiv(iyd+1,ixd)-t0)/dyd2
            ELSE                   ! the fine cell contacts a coarse cell    
               qq=(tt(iy+1,ix)-t0)/(dy+dyd)*2.*dxd
C               qq=0
               dp=dp+qq/dxd/dyd
               tt0(iy+1,ix)=tt0(iy+1,ix)-qq/dx/dy*fac
            ENDIF
C
            IF(ixd.GT.1) THEN
               dp=dp+(tdiv(iyd,ixd-1)-t0)/dxd2
            ELSE                   ! the fine cell contacts a coarse cell    
               qq=(tt(iy,ix-1)-t0)/(dx+dxd)*2.*dyd
C               qq=0
               dp=dp+qq/dxd/dyd
               tt0(iy,ix-1)=tt0(iy,ix-1)-qq/dx/dy*fac
            ENDIF
C
            IF(ixd.LT.mxdi) THEN
               dp=dp+(tdiv(iyd,ixd+1)-t0)/dxd2
            ELSE                   ! the fine cell contacts a coarse cell    
               qq=(tt(iy,ix+1)-t0)/(dx+dxd)*2.*dyd
C               qq=0
               dp=dp+qq/dxd/dyd
               tt0(iy,ix+1)=tt0(iy,ix+1)-qq/dx/dy*fac
            ENDIF
C
            dtmp=dp*fac
            IF(iradiat.NE.0) dtmp=dtmp-facr*((t0+troom)**4-troom4)  ! radiation
            dtmpmx=MAX(dtmpmx,dtmp)
            tdiv0(iyd,ixd)=dtmp
C
C         write(6,*) 'ir,t0,dp0,dp,dtmp',ir,t0,dp0,dp,dtmp
         ENDDO
      ENDDO
C
C---  Continue with the coarse part   
C
      dflow=0.
      DO ix=1,mxx
         DO iy=1,mxy
C
            IF(icell(iy,ix).GT.0) THEN
               dp=0
               t0=tt(iy,ix)
C
               t1=t0
               IF(iadjc(1,iy,ix).EQ.0) THEN
                  t1=tt(iy,ix+1)
               ELSEIF(iadjc(1,iy,ix).EQ.1) THEN
                  t1=0.
               ENDIF
               dp=dp+(t1-t0)/dx2
C
               t1=t0
               IF(iadjc(3,iy,ix).EQ.0) THEN
                  t1=tt(iy,ix-1)
               ELSEIF(iadjc(3,iy,ix).EQ.1) THEN
                  t1=0.
               ENDIF
               dp=dp+(t1-t0)/dx2
C
               t1=t0
               IF(iadjc(2,iy,ix).EQ.0) THEN
                  t1=tt(iy+1,ix)
               ELSEIF(iadjc(2,iy,ix).EQ.1) THEN
                  t1=0.
               ENDIF
               dp=dp+(t1-t0)/dy2
C
               t1=t0
               IF(iadjc(4,iy,ix).EQ.0) THEN
                  t1=tt(iy-1,ix)
               ELSEIF(iadjc(4,iy,ix).EQ.1) THEN
                  t1=0.
               ENDIF
               dp=dp+(t1-t0)/dy2
C
               dtmp=dp*fac
               IF(iadjc(1,iy,ix).EQ.1.OR.iadjc(2,iy,ix).EQ.1.OR.
     +            iadjc(3,iy,ix).EQ.1.OR.iadjc(4,iy,ix).EQ.1) THEN
                  dflow=dflow+t0*fac*rho*zsiz*cv  ! heat flow to the sink
C                  write(6,*) ix,iy,t0,t0*fac*rho*zsiz*cv
               ENDIF
               IF(icell(iy,ix).EQ.2) THEN
C                  write(6,*) ' from plug ',ix,iy,tt0(iy,ix),dtmp
                  dtmp=dtmp+tt0(iy,ix)  !  Use the heat transfer from the fine plug
               ENDIF
               IF(iradiat.NE.0) dtmp=dtmp-facr*((t0+troom)**4-troom4) ! radiation
               dtmpmx=MAX(dtmpmx,dtmp)
               tt0(iy,ix)=dtmp
            ENDIF
C         write(6,*) 'ir,t0,dp0,dp,dtmp',ir,t0,dp0,dp,dtmp
         ENDDO
      ENDDO
C
      dt=1.
      IF(dtmpmx.GT.0) THEN
         dt=0.5/dtmpmx
         dt=MIN(dt,0.1)
      ENDIF
C
      IF(dtpulse+dt.GT.tpulse) THEN
         dt=tpulse-dtpulse
         ipulse=1
      ENDIF
C
      IF(tim0+dt.GT.TIM) THEN
         dt=TIM-tim0
      ENDIF
C
      DO ixd=1,mxdi
         DO iyd=1,mxdi
            tdiv(iyd,ixd)=tdiv(iyd,ixd)+tdiv0(iyd,ixd)*dt
         ENDDO
      ENDDO
C
      DO ix=1,mxx
         DO iy=1,mxy
C
            IF(ix.LE.ix1-1.OR.ix.GE.ix1+mxdc.OR.
     +         iy.LE.iy1-1.OR.iy.GE.iy1+mxdc) THEN
C
               tt(iy,ix)=tt(iy,ix)+tt0(iy,ix)*dt
            ENDIF
         ENDDO
      ENDDO
C      WRITE(6,FMT='(A30,I6,F9.5,F9.5,7F9.3,2X,F8.4)') 
C     +  ' iter,dt,tim0,tt,tt1,ener ',nt,dt,tim0
C     +         ,(tdiv(iydc+i,ixdc),i=-3,3),dener
C
      tim0=tim0+dt
      dtpulse=dtpulse+dt
C
      IF(tim0.GT.ntv*dtv) THEN
         IF(ntv.LT.mxt) THEN
            ntv=ntv+1
            ttim(ntv)=tim0
            ttemp(ntv)=tdiv(iydc,ixdc)
         ENDIF
      ENDIF
C
      IF(MOD(nt,1000).EQ.0) 
     +    WRITE(6,FMT='(A16,I8,F10.4,F9.3,2F9.4)') 
     +    ' iter, time',nt,tim0,tdiv(iydc,ixdc),dflow,dflow+dener
      IF(tim0.LT.TIM) GO TO 20
C
      WRITE(6,*) ' iter, time',nt,tim0,tdiv(iydc,ixdc),dener,dflow

C
C      WRITE(6,*) 'Try to open file'
C      OPEN(UNIT=9,FILE='temper_1.dat',STATUS='UNKNOWN')
C     +           ,FORM='UNFORMATTED')
C      WRITE(6,*) 'Open file, iostat=',iost
C      WRITE(9) mxx,mxy,xsiz,ysiz,ntv
C      DO ix=1,mxx
C         WRITE(9) (tt(iy,ix),iy=1,mxy)
C      ENDDO
C      WRITE(9) ((tt(iy,ix),iy=1,mxy),ix=1,mxx)
C      WRITE(9) (ttim(i),ttemp(i),i=1,ntv)
C
      OPEN(UNIT=9,FILE='temper_1.dat',STATUS='UNKNOWN')
      WRITE(9,FMT='(F10.4,5X,F10.4)') (ttim(i),ttemp(i),i=1,ntv)
      CLOSE(9)
C
      iyp(1)=1
      iyp(2)=mxy/4
      iyp(3)=mxy/2
      OPEN(UNIT=9,FILE='temper_2.dat',STATUS='UNKNOWN')
      WRITE(9,FMT='(4F10.4)') 
     +      (-xsiz/2.+(ix-0.5)*dx,(tt(iyp(i),ix),i=1,3),ix=1,mxx)
      CLOSE(9)
C
      iyp(1)=1
      iyp(2)=mxdi/4
      iyp(3)=mxdi/2
      OPEN(UNIT=9,FILE='temper_3.dat',STATUS='UNKNOWN')
      WRITE(9,FMT='(4F10.4)') 
     +      (xd1+(ixd-0.5)*dxd,(tdiv(iyp(i),ixd),i=1,3),ixd=1,mxdi)
      CLOSE(9)
C
 999  CONTINUE
      END





