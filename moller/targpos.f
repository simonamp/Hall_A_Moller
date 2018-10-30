      SUBROUTINE TARGPOS(IRUN)
C
C===     New target holder: find the beam / target coordinates and the target number
C===      using the run_sett vector. Put the results in the same vector.
C
      IMPLICIT NONE
      INTEGER IRUN
C
      VECTOR RUN_SETT(120)
C
      INTEGER i,mxtar,mxset,iset,itar,jrun
      PARAMETER (mxtar=7,mxset=2)

      REAL xcv(mxtar,mxset),ycv(mxtar,mxset),xsca(mxset),ysca(mxset)
      REAL xv,yv,xcm,ycm,xdf,xdcm,ydcm,xtdis,xccm(mxtar),yccm(mxtar)
C      ---      mset - number of different settings of the encoder voltages and distances
C
C-- target       0      1      2      3      4      5      6
C               Al     old                                park
      DATA xcv/4.150, 3.565, 2.977, 2.303, 1.678, 1.030, 0.650
     +        ,7*0./
      DATA ycv/7*1.400
     +        ,7*0./
C
C      xsca(1)=8.89*(5.-1.)/(3.565-1.030)
C      ysca(1)=5.5/(2.56-0.4)
      xtdis=8.89  ! distance between two adjacent target centers defined by the target frame
      DO iset=1,mxset
         xsca(iset)=xtdis*(5.-1.)/(xcv(1,iset)-xcv(5,iset))
         ysca(iset)=5.5/(2.56-0.4)  ! 5.5cm motion
      ENDDO
C
      xdcm=0.
      ydcm=0.
      iset=0
      itar=-1
      IF(IRUN.LT.11790) THEN
        itar=1
        iset=0
      ELSE IF (IRUN.LT.999999) THEN
         jrun=INT(RUN_SETT(100)+0.1)
         IF(IRUN.NE.jrun) THEN
            WRITE(6,*) ' TARGPOS error - runs do not match ',IRUN,jrun
         ELSE
            iset=1
         ENDIF
      ENDIF
C
C
      IF(iset.NE.0) THEN
         DO i=1,mxtar
            xccm(i)=xcv(i,iset)*xsca(iset)
            yccm(i)=ycv(i,iset)*ysca(iset)
         ENDDO

      IF(IRUN.LT.15500) THEN
         xv=RUN_SETT(35)
         yv=RUN_SETT(30)
       ELSE
         xv=RUN_SETT(31)
         yv=RUN_SETT(26)
       ENDIF

         xcm=xv*xsca(iset)
         ycm=yv*ysca(iset)
C         write(6,*) xcm,ycm
         DO i=1,mxtar
            xdf=xcm-xccm(i)
C             write(6,*) i,xv,xcv(i,iset),xcm,xccm(i),xdf
            IF(ABS(xdf).LT.3.0) THEN
               itar=i-1
C                write(6,*) i,itar
               GO TO 20
            ENDIF
         ENDDO
 20      CONTINUE
         IF(itar.GE.0) THEN
            xdcm=-xcm+xccm(itar+1)
            ydcm= ycm-yccm(itar+1)
         ENDIF
C         write(6,*) itar,xdcm,ydcm
C 
      ENDIF
C
      RUN_SETT(101)=itar
      RUN_SETT(102)=xdcm
      RUN_SETT(103)=ydcm
C      write(6,*) 'TARG',itar,xdcm,ydcm
C      write(6,*) 'TARG',xcm,xccm,ycm,yccm
C
      RETURN
      END
