C      SUBROUTINE TEMP_GET
C
C ---  Read temperature maps: result of temp_tar.f
C
      IMPLICIT NONE
      REAL TIM
C
      INTEGER ix,iy,ntv,i,nx,ny,iost
C
      INTEGER    mxp,mxt
      PARAMETER (mxp=100000,mxt=10000)
      REAL xsiz,ysiz
      REAL tt(mxp)
C
      REAL ttim(mxt),ttemp(mxt)
C      VECTOR ttim(mxt),ttemp(mxt)
C
      DO i=1,30
         ttim(i)=i
      ENDDO
      OPEN(UNIT=9,FILE='temper.dat',STATUS='OLD'
     +       ,FORM='UNFORMATTED')
      REWIND 9
      READ(9) nx,ny,xsiz,ysiz,ntv
      WRITE(6,*) 'nx,ny=',nx,ny,xsiz,ysiz,ntv
C      DO ix=1,nx
C         READ(9,END=991,ERR=992) (tt(iy),iy=1,ny)
C         write(6,*) 'Try ',ix,(tt(i),i=1,5)
C      ENDDO
C      GO TO 100
C 991  WRITE(6,*) '--EOF'
C      GO TO 100
C 992  WRITE(6,*) '--ERROR'
C      GO TO 100
C 100  CONTINUE
C
C      READ(9) (tt(i),i=1,nx*ny)
C      WRITE(6,*) 'nx,ny=',nx,ny,xsiz,ysiz,ntv
      READ(9) (ttim(i),ttemp(i),i=1,MIN(ntv,mxt))
      CLOSE(9)
      WRITE(6,*) 'nx,ny=',nx,ny,xsiz,ysiz,ntv
      WRITE(6,FMT='(I5/(4(F8.4,2X,F8.3,4X)))') 
     +       ntv,(ttim(i),ttemp(i),i=1,ntv)
C
      END






