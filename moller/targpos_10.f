      SUBROUTINE TARGPOS_10(IRUN)
C
C===     "Brute force" target holder: find the beam / target coordinates and the target number
C===      using the run_sett vector. Put the results in the same vector.
C
      IMPLICIT NONE
      INTEGER IRUN
C
      VECTOR RUN_SETT(120)
C
      INTEGER i,mxtar,mxset,iset,itar,jrun
      PARAMETER (mxtar=6,mxset=2)

CC      REAL xcv(mxtar,mxset),ycv(mxtar,mxset),xsca(mxset),ysca(mxset)
CC      REAL xv,yv,xcm,ycm,xdf,xdcm,ydcm,xtdis,xccm(mxtar),yccm(mxtar)
      REAL xcv(mxtar,mxset),xsca(mxset)
      REAL xv,xcm,xdf,xdcm,xtdis,xccm(mxtar)
C      ---      mset - number of different settings of the encoder voltages and distances
CC
CC-- target       0      1      2      3      4      5      6
CC               Al     old                                park
CC      DATA xcv/4.150, 3.565, 2.977, 2.303, 1.678, 1.030, 0.650
CC     +        ,7*0./
CC      DATA ycv/7*1.400
CC     +        ,7*0./
C-- target      0      1      2      3      4      5      
C             park                              extended                              
      DATA xcv/0.298, 1.731, 2.011, 2.281, 2.554, 2.725
     +        ,6*0./
C
C      xsca(1)=8.89*(5.-1.)/(3.565-1.030)
C      ysca(1)=5.5/(2.56-0.4)
      xtdis=8.89  ! distance between two adjacent target centers defined by the target frame
      DO iset=1,mxset
         xsca(iset)=xtdis*(5.-1.)/(xcv(1,iset)-xcv(5,iset))
C         ysca(iset)=5.5/(2.56-0.4)  ! 5.5cm motion
      ENDDO
C
      xdcm=0.
CC      ydcm=0.
      iset=0
      itar=-1
CC      IF(IRUN.LT.11790) THEN
CC        itar=1
CC        iset=0
CC      ELSE IF (IRUN.LT.999999) THEN
      IF (IRUN.LT.999999) THEN
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
CC            yccm(i)=ycv(i,iset)*ysca(iset)
         ENDDO
         xv=RUN_SETT(35)
CC         yv=RUN_SETT(30)
         xcm=xv*xsca(iset)
CC         ycm=yv*ysca(iset)
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
CC            ydcm= ycm-yccm(itar+1)
         ENDIF
C         write(6,*) itar,xdcm,ydcm
C 
      ENDIF
C
      RUN_SETT(101)=itar
      RUN_SETT(102)=xdcm
CC      RUN_SETT(103)=ydcm
      RUN_SETT(103)=0.0
C      write(6,*) 'TARG',itar,xdcm,ydcm
C      write(6,*) 'TARG',xcm,xccm,ycm,yccm
C
      RETURN
      END
