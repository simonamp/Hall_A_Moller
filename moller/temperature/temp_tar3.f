C      PROGRAM TEMP_TAR
C      SUBROUTINE TEMP_TAR(TIM)
C ---        Test - linear task
C ---   Adapted for CEBAF (the original was written for SLAC E158)
C ---  Calculates the target temperature at a given time TIM.
C      2-dim thermal flow equation.
C      Target: 15x3cm**2 (JLAB)
C      Beam: 0.06mm**2
C      Grid: "temper. transfer" from element 2 to 1 (adjacent): 
C      Delta(T)=(T2-T1)/dx**2*ak/rho/cv*Delta(t)
C
      IMPLICIT NONE
      DOUBLE PRECISION TIM
C
      INTEGER ix,iy,it,i,j,nt,ntv,ipulse,ib
     +    ,iyp(3),iost,ixd,iyd,idv1,dtav
     +    ,ix1,ix2,iy1,iy2,ixdc,iydc,ipri,npulses,iradiat
     +    ,nbspl  ! - for beam deposit calculation - split the cell into nbspl
     +    ,ixss,iyss
      DOUBLE PRECISION ak,rho,cv,pulse,ang,fac,scal,tim0
     +    ,dp,rb,dtmp,dtmpmx,t1,t0,dtv,tpulse,dtpulse
     +    ,dx,dy,xsiz,ysiz,zsiz,x,y,r2,dx2,dy2,dt,depos
     +    ,xd,yd,qq,dxd,dyd,dxd2,dyd2,xd1,yd1,dener,dflow
     +    ,enfac,fradiat,troom,troom4,facr,fpulse
     +    ,bcurr,pulses
     +    ,dtmax  ! maximum time interval, calculated from the grid size: dtmax=0.5dx**2*cv*rho/ak
C                                                     (Wikipedia http://en.wikipedia.org/wiki/Finite_differences)
     +    ,dtstep  ! time interval of the first try - calculated from the beam energy release (temp increase<1 deg)
     +    ,dxy2(4,3) ! (i,iplug) (see ixyadj(j,i)) - dx**2 or dy**2 iplug=1 - plug, =2 - coarse, =3 - betweed fine and coarse
     +    ,tempstepmx ! maximum temperature step for the beam
     +    ,tcenprev
C
C
      INTEGER    mxx,mxy,mxt,mxbeam,mxdx,mxdcx,mxdix,mxdy,mxdcy,mxdiy
      PARAMETER (mxx=20,mxy=10,mxt=100000,mxbeam=400000)
      PARAMETER (mxdx=10,mxdcx=15,mxdy=10,mxdcy=10)
C      PARAMETER (mxx=30,mxy=4,mxt=100000,mxbeam=9000,mxd=10,mxdc=2)
      PARAMETER (mxdix=mxdx*mxdcx,mxdiy=mxdy*mxdcy)
      DOUBLE PRECISION                  ! coarse grid
     +                 tt(mxy,mxx)      !  - temperature of the coarse grid, at the end of an iterration
     +                ,ttadd(mxy,mxx)   !  - temparature increments, at the end of an iterration
     +                ,ttadd0(mxy,mxx)  !  - temparature increments, from the previous step
      INTEGER*1 icell(mxy,mxx)    ! status of the cell =0 - ignore, 1 - ok, =2 - use the store t0 to add
      INTEGER*1 iadjc(4,mxy,mxx)  ! status of the adjacent cell (i,iy,ix) i=1,2, 3, 4 
C                                                                          x,y,-x,-y
C                                  =0 normal cell, =1 - temperature zero, =2 - no thermal contact
      INTEGER*1 ixyadj(2,4)          ! (1/2,i)=ishx/y (shift in x/y) for i=1-4 (see iadcjc(i,ix,iy))
      INTEGER*1 iadjd(4,mxdiy,mxdix) ! status of adjacent cells for the plug =0 - fine cell, =1 - zero temp, =2 - no termal contact, 
C                                       =3 - coarse cell
      DOUBLE PRECISION                     ! area with fine division 
     +                 tdiv(mxdiy,mxdix)     ! temperature of the fine grid, at the end of an iterration
     +                ,tdivadd(mxdiy,mxdix)  ! temperature increments of the fine grid, at the end of an iterration
     +                ,tdivadd0(mxdiy,mxdix) ! temperature increments of the fine grid, from the previous step
      DOUBLE PRECISION ttbeam(mxbeam)
      INTEGER ixydbeam(2,mxbeam),nxybeam
C
      DOUBLE PRECISION ttim(mxt),ttemp(mxt)
C      VECTOR ttim(mxt),ttemp(mxt)
C
      DATA ak/0.75D0/        ! thermal conductivity W/cm/K
      DATA cv/0.4D0/         ! specific heat J/g/K
      DATA rho/8.0D0/        ! density g/cm**3
      DATA bcurr/0.5D-6/   ! beam current, uA 
C      DATA pulse/3.6D11/   ! e-/pulse 
      DATA ang/90.D0/        ! beam angle, deg (20 deg)
      DATA depos/2.4D-13/  ! energy deposit by one particle J/(g/cm**2)
      DATA rb/0.020D0/        ! beam density: dN/ds = pulse/(2*pi*rb**2)*exp(-r**2/2/rb**2)
C                           in order to simulate the beam angle: the spot size AND beam
C                           flux are scaled by 1/sin(ang)
C                          RMS(X)=RMS(y)=1mm (from wire array measurements) E158
      DATA dtv/1000.D-6/     ! time step for writing temperature into ttim 
      DATA fpulse/500.D6/   ! pulse repetition rate
      DATA ixyadj/1,0, 0,1, -1,0, 0,-1/
C
      TIM=10000.E-3
      fradiat=0.      ! radiation power: the material factor (0.1 is a guess)
      troom=300.            ! room temperature
      ipri=0
      xsiz=2.0         !  x-size of the foil
      ysiz=1.0         !  y-size of the foil
      zsiz=0.001      !  thickness
      tempstepmx=0.05 ! max temperature step, by  the beam
C
C---  Try to read the input data
C
      iost=0
C      OPEN(UNIT=9,FILE='temp_tar.input',STATUS='OLD',IOSTAT=iost)
      IF(iost.EQ.0) THEN
C         READ(9,*) TIM,bcurr,rb,ang,zsiz,fradiat,dtv
         CLOSE(9)
C         WRITE(6,*) 'Input file read in'
      ENDIF
      WRITE(6,*) ' TIM,bcurr,rb,ang,zsiz,fradiat,dtv'
      WRITE(6,*) TIM,bcurr,rb,ang,zsiz,fradiat,dtv
C
      IF(fpulse.LE.0.) THEN
         WRITE(6,*) ' *** Error: fpulse=0.'
         GO TO 999
      ENDIF
      pulse=bcurr/1.6E-19/fpulse
      tpulse=1./fpulse
      iradiat=0
      IF(fradiat.GT.0.0001) iradiat=1  ! flag to turn on radiation
      fradiat=5.67E-12*fradiat  ! radiation power in W/cm**2/K**4 (0.1 is a material factor - a guess)
      npulses=0
      troom4=troom**4
C
C      IF(mxdc.GT.mxx-2.OR.mxdc.GT.mxy-2) THEN
      IF(mxdcx.GT.mxx.OR.mxdcy.GT.mxy) THEN
         WRITE(6,*) ' *** Error: internal fine plug should be'
     +          ,' inside the foil ',mxdcx,mxdcy,mxx,mxy
         GO TO 999
      ENDIF
C
      dx=xsiz/mxx
      dy=ysiz/mxy
      dx2=dx**2
      dy2=dy**2
      dxd=dx/mxdx
      dyd=dy/mxdy
      dxd2=dxd**2
      dyd2=dyd**2
C
      dxy2(1,1)=dxd2
      dxy2(2,1)=dyd2
      dxy2(3,1)=dxd2
      dxy2(4,1)=dyd2
      dxy2(1,2)=dx2
      dxy2(2,2)=dy2
      dxy2(3,2)=dx2
      dxy2(4,2)=dy2
      dxy2(1,3)=(dx+dxd)/2.*dxd
      dxy2(2,3)=(dy+dyd)/2.*dyd
      dxy2(3,3)=dxy2(1,3)
      dxy2(4,3)=dxy2(2,3)
C
      dtmax=0.5*dxd2*cv*rho/ak*0.1  ! max step for stable calculations (Wikipedia) *0.1
C
      tim0=0.
C
C---    Define the fine division of the central area
C
      ix1=mxx/2-mxdcx/2+1
      ix2=ix1+mxdcx-1
      iy1=mxy/2-mxdcy/2+1
      iy2=iy1+mxdcy-1
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
         iadjc(4,1  ,ix)=2 ! no contact
         iadjc(2,mxy,ix)=2 ! no contact
      ENDDO
      DO iy=1,mxy
         iadjc(3,iy,1  )=1 ! 0 deg
         iadjc(1,iy,mxx)=1 ! 0 deg
      ENDDO
      DO ix=ix1,ix2
         DO iy=iy1,iy2
            icell(iy,ix)=0
         ENDDO
         IF(iy1.GT.1) THEN
            icell(  iy1-1,ix)=2
            iadjc(2,iy1-1,ix)=2
         ENDIF
         IF(iy2.LT.mxy) THEN
            icell(  iy2+1,ix)=2
            iadjc(4,iy2+1,ix)=2
         ENDIF
      ENDDO
      DO iy=iy1,iy2
         IF(ix1.GT.1) THEN
            icell(  iy,ix1-1)=2
            iadjc(1,iy,ix1-1)=2
         ENDIF
         IF(ix2.LT.mxx) THEN
            icell(  iy,ix2+1)=2
            iadjc(3,iy,ix2+1)=2
         ENDIF
      ENDDO
      DO ixd=1,mxdix
         DO iyd=1,mxdiy
            DO i=1,4
               iadjd(i,iyd,ixd)=0
            ENDDO
         ENDDO
         iadjd(2,mxdiy,ixd)=2 ! 3 
         iadjd(4,1    ,ixd)=2 ! 3 
      ENDDO
      DO iyd=1,mxdiy
         iadjd(1,iyd,mxdix)=3 
         iadjd(3,iyd,1    )=3 
      ENDDO
      IF(ipri.NE.0) THEN
         WRITE(6,FMT='(A20/(I4,3X,10I3))') 
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
      write(6,*) ' xd,yd ',ix1,ix2,iy1,iy2,xd1,yd1,dx,dy,dxd,dyd
C      GO TO 999

      dtmpmx=0.
      nxybeam=0
      scal=1./SIN(ang*3.1415/180.)
      fac=pulse*depos*fpulse/cv/(dxd*dyd*mxdiy)  ! heating per second
      DO ixd=1,mxdix
         xd=(ixd-1+0.5)*dxd+xd1
         DO iyd=1,mxdiy
            yd=(iyd-1+0.5)*dyd+yd1
C
C --- Integrate the beam on nbspl x nbspl inside one cell 
            dtmp=0.
C      write(6,*) ' xd,yd ',xd1,yd1,dxd,dyd,xd,yd
            nbspl=1
C            DO i=1,nbspl
C               x=(i-0.5)*dxd/nbspl+xd
C               DO j=1,nbspl
C                  y=(j-0.5)*dyd/nbspl+yd
C                  r2=(x/(rb*scal))**2+(y/rb)**2
C                  dtmp=dtmp+exp(-0.5*r2)
C               ENDDO
C            ENDDO
            IF(ABS(xd).LT.dxd*0.999D0) dtmp=1./2.
            dtmp=dtmp/nbspl**2*fac
            tdivadd(iyd,ixd)=dtmp
C            IF(dtmp.GT.0.) write(6,*) ixd,iyd,xd,yd,fac,scal,dtmp
            IF(dtmp.GT.dtmpmx) THEN
               ixdc=ixd
               iydc=iyd
               dtmpmx=dtmp
            ENDIF
         ENDDO
      ENDDO
      dener=0.
C
      DO ixd=1,mxdix
         DO iyd=1,mxdiy
            dtmp=tdivadd(iyd,ixd)
            tdiv(iyd,ixd)=0.   !   reset the temperatures
            tdivadd0(iyd,ixd)=0.   !   reset the temperatures
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
C
      DO ix=1,mxx
         DO iy=1,mxy
            tt(iy,ix)=0.
            ttadd0(iy,ix)=0.
         ENDDO
      ENDDO
C
      WRITE(6,*) ' Full energy (Joul) released per pulse =',dener
      WRITE(6,*) ' Beam center ',nxybeam,ixdc,iydc
     +          ,(ixdc-0.5)*dxd+xd1,(iydc-0.5)*dyd+yd1,dtmpmx
     +          ,tdiv(iydc,ixdc),dtstep
      WRITE(6,*) ' Maximum time step (from the grid) =',dtmax
C
C      GO TO 999
      dtpulse=0.
      ipulse=1
C
      nt=0
      ntv=0
 20   nt=nt+1
C
C
      dtstep=dtmax
      IF(dtmpmx.GT.0.D0) dtstep=tempstepmx/dtmpmx   ! time interval - maximum tempstepmx deg temperature rise
      dtstep=MIN(dtstep,dtmax)   ! time interval - combination of the beam and grid conditions
C
      tcenprev=tdiv(iydc,ixdc)
      dtpulse=0.
      DO ib=1,nxybeam
         ixd=ixydbeam(1,ib)
         iyd=ixydbeam(2,ib)
         tdiv(iyd,ixd)=tdiv(iyd,ixd)+ttbeam(ib)*dtstep !   beam
C            write(6,*) ' beam ',ib,ixd,iyd,tdiv(iyd,ixd)
      ENDDO
      npulses=npulses+1
C
C----  Iterration: estimate the temp rise for 1 sec, find the time iterval
C----   to keep the max rise at 0.2K
C
C -- Reset the rim of the coarse matrix (corners are reset twice)
C
      DO ix=ix1-1,ix2+1
         ttadd(iy1-1,ix)=0.
         ttadd(iy2+1,ix)=0.
      ENDDO
      DO iy=iy1-1,iy2+1
         ttadd(iy,ix1-1)=0.
         ttadd(iy,ix2+1)=0.
      ENDDO
C
C----  Start with the fine plug 
C
      fac=ak/rho/cv*dtstep
      facr=2.*fradiat/zsiz/rho/cv*dtstep
      dtmpmx=0.
C
      dener=0
      enfac=rho*dxd2*zsiz*cv
      qq=0
      DO ixd=1,mxdix
         DO iyd=1,mxdiy
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
      DO ixd=1,mxdix
         DO iyd=1,mxdiy
            dp=0.
            t0=tdiv(iyd,ixd)
            t0=t0+tdivadd0(iyd,ixd)/2.  ! add the prediction (from the previous step) for the average temperature rise 
C
C---          Is the cell close to the edge?
C
            ix=0
            iy=0
            IF(ixd.EQ.1.OR.ixd.EQ.mxdix.OR.
     +         iyd.EQ.1.OR.iyd.EQ.mxdiy) THEN

               ix=ix1+(ixd-1)/mxdx ! the column number on the coarse cell
               iy=iy1+(iyd-1)/mxdy
C               write(6,*) (iadjd(i,iyd,ixd),i=1,4)
            ENDIF
C
            DO i=1,4
               t1=t0
               IF(iadjd(i,iyd,ixd).EQ.0) THEN
                  ixss=ixd+ixyadj(1,i)
                  iyss=iyd+ixyadj(2,i)
                  t1=tdiv(iyss,ixss)+tdivadd0(iyss,ixss)/2. ! temperature of the adjacent cell, extrapolated from the previous step
                  dp=dp+(t1-t0)/dxy2(i,1)
               ELSEIF(iadjd(i,iyd,ixd).EQ.1) THEN
                  t1=0.
                  dp=dp+(t1-t0)/dxy2(i,1)
               ELSEIF(iadjd(i,iyd,ixd).EQ.3) THEN
                  ixss=ix+ixyadj(1,i)
                  iyss=iy+ixyadj(2,i)
                  t1=tt(iyss,ixss)+ttadd0(iyss,ixss)/2.
                  qq=(t1-t0)/dxy2(i,3)
C     qq=0
                  dp=dp+qq
                  ttadd(iyss,ixss)=ttadd(iyss,ixss)-qq*dxd*dyd/dx/dy*fac
C                  IF(nt.EQ.10000) write(6,FMT='(A15,7I3,7D12.4)') 
C     +                    ' fine->coarse '
C     +                   ,ixd,iyd,ix,iy,i,ixss,iyss
C     +                   ,qq,dp,ttadd(iyss,ixss),t0,t1,dp*fac
               ENDIF
            ENDDO
C
            dtmp=dp*fac
            IF(iradiat.NE.0) dtmp=dtmp-facr*((t0+troom)**4-troom4)  ! radiation
            dtmpmx=MAX(dtmpmx,ABS(dtmp))
            tdivadd(iyd,ixd)=dtmp
C
C            IF(dtmp.LT.0) THEN
C               WRITE(6,*) 'nt,ix,iy,dtmp',nt,ixd,iyd,dtmp,t0
C     +                    ,tdiv(iyd,ixd)
C            ENDIF
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
               dp=0.D0
               t0=tt(iy,ix)+ttadd0(iy,ix)/2.
C
               DO i=1,4
                  t1=t0
                  IF(iadjc(i,iy,ix).EQ.0) THEN
                     ixss=ix+ixyadj(1,i)
                     iyss=iy+ixyadj(2,i)
                     t1=tt(iyss,ixss)+ttadd0(iyss,ixss)/2.
                  ELSEIF(iadjc(i,iy,ix).EQ.1) THEN
                     t1=0.
                  ENDIF
                  dp=dp+(t1-t0)/dxy2(i,2)
               ENDDO
C
               dtmp=dp*fac
C
               IF(icell(iy,ix).EQ.2) THEN
C                  write(6,*) ' from plug ',ix,iy,ttadd(iy,ix),dtmp
                  dtmp=dtmp+ttadd(iy,ix)  !  Use the heat transfer from the fine plug
               ENDIF
C
               IF(iradiat.NE.0) dtmp=dtmp-facr*((t0+troom)**4-troom4) ! radiation
C
               dtmpmx=MAX(dtmpmx,ABS(dtmp))
               ttadd(iy,ix)=dtmp
            ENDIF
C         write(6,*) 'ir,t0,dp0,dp,dtmp',ir,t0,dp0,dp,dtmp
         ENDDO
      ENDDO
C
C---   This was a calculation for temperature change per second. 
C---   Adjust the time interval to have a reasonable small change
C
      dt=dtstep
C      qq=ABS(tdiv(iydc,ixdc)+tdivadd(iydc,ixdc)-tcenprev)
C      IF(qq.GT.0.) dt=dtstep*tempstepmx/ABS(qq)
C      IF(dtmpmx.GT.0.) THEN
C         dt=0.2/dtmpmx*dt
C         dt=MIN(dt,0.1)
C      ENDIF
C      WRITE(6,'(A4,I8,5E13.4)') ' dt=',nt,dt,qq,tcenprev,tdiv(iydc,ixdc)
C     +       ,tdivadd(iydc,ixdc)
C
C      IF(dtpulse+dt.GT.tpulse) THEN
C         dt=tpulse-dtpulse
C         ipulse=1
C      ENDIF
C
C      IF(tim0+dt.GT.TIM) THEN
C         dt=TIM-tim0
C      ENDIF
C      WRITE(6,*) ' dt=',nt,dt
C
C      dt=dtv
C
      DO ixd=1,mxdix
         DO iyd=1,mxdiy
            tdiv(iyd,ixd)=tdiv(iyd,ixd)+tdivadd(iyd,ixd)
            tdivadd0(iyd,ixd)=tdivadd(iyd,ixd)
         ENDDO
      ENDDO
C
      DO ix=1,mxx
         DO iy=1,mxy
C
            IF(ix.LT.ix1.OR.ix.GT.ix2.OR.
     +         iy.LT.iy1.OR.iy.GT.iy2) THEN
               tt(iy,ix)=tt(iy,ix)+ttadd(iy,ix)
               ttadd0(iy,ix)=ttadd(iy,ix)
            ENDIF
         ENDDO
      ENDDO
C      WRITE(6,FMT='(A30,I6,E12.3,F13.7,7F12.3,2X,F8.4)') 
C     +  ' iter,dt,tim0,tt,tt1,ener ',nt,dt,tim0
C     +         ,(tdiv(iydc+i,ixdc),i=-3,3),dener

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
      IF(MOD(nt,500).EQ.0) 
     +    WRITE(6,FMT='(A16,I8,E11.3,F13.7,3F14.5)') 
     +    ' iter, time',nt,dt,tim0,tdiv(iydc,ixdc),tdivadd(iydc,ixdc),qq
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
      WRITE(9,FMT='(I7,2X,F13.6,5X,F13.7)') (i,ttim(i),ttemp(i),i=1,ntv)
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
      iyp(2)=mxdiy/4
      iyp(3)=mxdiy/2
      OPEN(UNIT=9,FILE='temper_3.dat',STATUS='UNKNOWN')
      WRITE(9,FMT='(4F10.4)') 
     +      (xd1+(ixd-0.5)*dxd,(tdiv(iyp(i),ixd),i=1,3),ixd=1,mxdix)
      CLOSE(9)
C
 999  CONTINUE
      END





