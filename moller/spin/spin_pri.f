      SUBROUTINE SPIN_PRI(NA,NB,NC,WIEN)
C
C===    Prints "spin dance" values
C       NA,NB,NC - number of passes
C       WIEN     - wien filter angle
C===    vprec(1) - E linac (GeV)
C===         (2) - =1 - hall A, =2, hall B...
C===         (3) - number of passes
C===         (4) - =0 - return the COS(angle), =1. - return the spin angle in degrees
C===         (5) - asymmetry between 2 linacs A=(E1-E2)/(E1+E2)
C===         (6) - shift of the injector energy Einj/Elin
C===         (7) - shift in the arc angle, rad
C===         (8) - E injector/linac
C
      IMPLICIT NONE
      INTEGER  NA,NB,NC
      REAL WIEN
      DOUBLE PRECISION DSPINPRED
C
      VECTOR VPREC(8)
      INTEGER i,j,itry,ihall,npass(3),ncycl
C
      DOUBLE PRECISION dshift(4),dang(6,5),dif,d0
      REAL   e0,shift(4),rdif(5,6),rzero(3)
      CHARACTER chall(6)*4,chzer*8
      DATA      chall/'A   ','B   ','C   ','A-B ','A-C ','B-C '/
C
C     ------------------------------------------------------------------
C
      shift(1)=0.001    ! Linac energy shift (relative)
      shift(2)=0.001    ! Linac asymmetry
      shift(3)=0.001    ! Injector energy shift (relative)
      shift(4)=0.003    ! Arc angle shift (rad)
      DO i=1,4
         dshift(i)=DBLE(shift(i))
      ENDDO
C
      npass(1)=NA
      npass(2)=NB
      npass(3)=NC
C
      e0=VPREC(1)
      VPREC(4)=1.
C
      DO itry=1,5
         VPREC(1)=e0
         DO i=5,7
            VPREC(i)=0.
         ENDDO
         IF(itry.GT.1) THEN
            IF(itry.EQ.2) THEN
               VPREC(1)=e0*(1.+shift(1))
            ELSE
               VPREC(itry+2)=shift(itry-1)
            ENDIF
         ENDIF
C     
         DO ihall=1,3
            VPREC(2)=ihall
            VPREC(3)=npass(ihall)
            dang(ihall,itry)=DSPINPRED(WIEN)
            ncycl=INT(dang(ihall,itry)/180.D0)
            IF(itry.EQ.1) THEN
               rzero(ihall)=
     +           REAL(dang(ihall,itry)-DBLE(ncycl)*180.D0)
               rzero(ihall)=90.-rzero(ihall)
            ENDIF
C            write(6,*) itry,ihall,VPREC,dang(ihall,itry)
         ENDDO
C         write(6,*) itry,(dang(ihall,itry),ihall=1,3)
         dang(4,itry)=dang(1,itry)-dang(2,itry)
         dang(5,itry)=dang(1,itry)-dang(3,itry)
         dang(6,itry)=dang(2,itry)-dang(3,itry)
C
      ENDDO
C      write(6,*) dang
C
      WRITE(6,1000) e0,(chall(i)(1:1),npass(i),i=1,3),WIEN
 1000 FORMAT(//' Spin precession for E_linac=',F9.6,'  Passes:'
     +      ,3(A1,'=',I1,1X),2X,'Wien angle=',F7.2)
      WRITE(6,1100) shift
 1100 FORMAT(' Hall   zero(deg) prec.angle(deg)',20X
     +      ,'D(angle) (degrees)'
     +      /30X,'D(E)/E linac '
     +      , 2X,'Linac asymm. '
     +      , 2X,'D(E)/E inject'
     +      , 2X,'D(arc) rad   '
     +      /25X,4(6X,F7.4,2X))
      WRITE(6,1150) 
 1150 FORMAT(2X,80('-'))
C
      DO i=1,6
         d0=dang(i,1)
         rdif(1,i)=REAL(d0)
         chzer=' '
         IF(i.LT.4) WRITE(chzer,FMT='(F8.2)') rzero(i)
         DO itry=1,4
            dif=dang(i,itry+1)-d0
            rdif(itry+1,i)=REAL(dif)
         ENDDO
         WRITE(6,1200) chall(i),chzer,(rdif(j,i),j=1,5)
 1200    FORMAT(2X,A4,2X,A8,2X,F9.2,4(2X,F8.2,5X))
      ENDDO
      WRITE(6,1150) 
C
      END
C
      INCLUDE 'dspinpred.f'





