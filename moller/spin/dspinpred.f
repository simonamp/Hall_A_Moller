      DOUBLE PRECISION FUNCTION DSPINPRED(THETA)
C
C===    Spin predictions for the wien angle=THETA (degrees)
C===    vprec(1) - E linac (GeV)
C===         (2) - =1 - hall A, =2, hall B...
C===         (3) - number of passes
C===         (4) - =0 - return the COS(angle), =1. - return the spin angle in degrees
C===         (5) - asymmetry between 2 linacs A=(E1-E2)/(E1+E2)
C===         (6) - shift of the injector energy Einj/Elin
C===         (7) - shift in the arc angle, rad
C===         (8) - Einjector/Elinac
C
      IMPLICIT NONE
      REAL THETA
C
      VECTOR VPREC(8)
      DOUBLE PRECISION delin,dang,dpi,da,danga,ddeg,dalph1
      INTEGER ihall,ifla
C
      DOUBLE PRECISION dpass          !  number of passes
      DOUBLE PRECISION dalpha         !  E inject/E linac
      DOUBLE PRECISION darc(3)        !  arc influence
      DOUBLE PRECISION dg2            ! (g-2)/2
      DOUBLE PRECISION dame           ! electron mass
      DOUBLE PRECISION dasyml         ! linac asymmetry
      DOUBLE PRECISION deinj          ! injector energy shift
      DOUBLE PRECISION devarc         ! shift in the angle in the arc, rad
C
C     ------------------------------------------------------------------
C
      dpi=ACOS(0.)*2.D0
      ddeg=180.D0/dpi
C
      dalpha=0.1125D0
C      dalpha=0.1238D0
      IF(VPREC(8).GT.0) dalpha=VPREC(8)
      dg2=0.00115965D0
      dame=0.000511D0
C
      delin= VPREC(1)
      ihall= VPREC(2)+0.1
      dpass= VPREC(3)
      ifla=  VPREC(4)
      dasyml=VPREC(5)
      deinj= VPREC(6)
      devarc=VPREC(7)
C
      darc(1)= 1.D0/2.4+devarc/dpi
      darc(2)=0.
      darc(3)=-1.D0/2.4-devarc/dpi
      dalph1=dalpha*(1.D0+deinj)
C
      da=delin/dame*dg2
      dang=da*(2.D0*dpass**2-dpass*(1.D0-2.D0*dalph1-darc(ihall))
     +   -dalph1*(1.D0-darc(ihall)/2.D0)
     +   +dasyml*dpass)*dpi
      danga=dang+DBLE(THETA)/ddeg
C      WRITE(6,*) ihall,INT(dpass),delin,dasyml,danga*ddeg
      IF(ifla.EQ.0) THEN
C         DSPINPRED=ACOS(DCOS(danga))*ddeg
         DSPINPRED=DCOS(danga)
      ELSE
         DSPINPRED=danga*ddeg
      ENDIF
C      WRITE(6,*) ihall,darc(ihall),THETA,dalph1,DSPINPRED
C      WRITE(6,*) da,dpass,dalph1,darc(ihall),dasyml,dpi
C      WRITE(6,*) 'dspin theta=',THETA,DSPINPRED
C
      END








