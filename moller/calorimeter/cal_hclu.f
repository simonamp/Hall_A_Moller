      REAL FUNCTION CAL_HCLU(ITIM1,ITIM2,IWID,NCLU,ICLU,ID)
C
C---          Hodoscope clusters
C---   Input: ITIM1,ITIM2 - time window
C             IWID        - max cluster width
C             NCLU        - select events with NCLU clusters
C                         = 0 - fill the histogram ID with all the cluster 
C                               coordinates (ICLU is ignored)
C             ICLU        - the seral cluster number
C               ID        - hist ID (if ICLU=0)
C---   Output:   the coordinate of the center of gravity
C                of the cluster ICLU
C              -1. - number of clusters .NE. NCLU
C               number of clusters - if NCLU=0
C
      IMPLICIT NONE
      INTEGER  ITIM1,ITIM2,IWID,NCLU,ICLU,ID
      INCLUDE  ?
      LOGICAL HEXIST
C
      INTEGER    mxh,mxh1
      PARAMETER (mxh=32)
      PARAMETER (mxh1=mxh+1)
      INTEGER i,ich,nh(mxh1),jch,icl,ncl,iclwid,kh,ibeg,iend
      REAL    xcl(mxh)
      INTEGER ifirst
      DATA    ifirst/1/
C
      IF(ifirst.EQ.1) THEN
         IF(HEXIST(ID)) THEN
            CALL HRESET(ID,'    ')
            ifirst=0
         ELSE
            WRITE(6,*) ' Error: no histogram ID=',ID
            ifirst=-1
         ENDIF
      ENDIF
C
      CAL_HCLU=-1.
      IF(NTDC.EQ.0) GO TO 999
C      
      kh=0
      DO i=1,mxh1
         nh(i)=0
      ENDDO
C
      DO i=1,NTDC
         ich=ITCH(i)
         jch=ich-16
         IF(jch.GE.1.AND.jch.LE.16) THEN
            IF(ITIM(i).GE.ITIM1.AND.ITIM(i).LE.ITIM2) THEN
               kh=kh+1
               nh(jch)=nh(jch)+1
            ENDIF
         ENDIF
      ENDDO
      icl=0
      ncl=0
      IF(kh.GT.0) THEN
         DO i=1,mxh1
            ibeg=0
            iend=0
            IF(nh(i).GT.0) THEN
               IF(icl.EQ.0) THEN
                  ibeg=1
               ELSE
                  IF(i-icl.GE.IWID) THEN
                     iend=1
                     ibeg=1
                  ENDIF
               ENDIF
            ELSE
               IF(icl.GT.0) THEN
                  iend=1
               ENDIF
            ENDIF
C
            IF(iend.GT.0) THEN
               iclwid=i-icl
               xcl(ncl)=icl+(iclwid-1.)/2.
               icl=0
            ENDIF
C
            IF(ibeg.GT.0) THEN
               ncl=ncl+1
               icl=i
            ENDIF
         ENDDO
      ENDIF
C
      IF(NCLU.GT.0) THEN
         IF(ncl.EQ.NCLU.AND.ICLU.GT.0.AND.ICLU.LE.NCLU) THEN
            CAL_HCLU=xcl(ICLU)
         ENDIF
      ELSE
         CAL_HCLU=ncl
         DO i=1,ncl
            IF(ifirst.EQ.0) CALL HF1(ID,xcl(i),1.)
         ENDDO
      ENDIF
C
 999  CONTINUE
      END

