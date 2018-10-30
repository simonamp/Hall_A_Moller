      SUBROUTINE PNTEVINI
C
C---     Initialize an event in the CW NTUPLE for run-wise data
C
      IMPLICIT NONE
C
      INCLUDE 'inc/crunpol.inc'
      INCLUDE 'inc/cruninf.inc'
C
      INTEGER id,i
C
C     ------------------------------------------------------------------
C
      IRUN(1)=0
      IRUNR(1)=0
C
      NCOUNTR(1)=0
      NPOLR(1)=0
      DO i=1,2
        ASYMR(i,1)=0.
        BASYMR(2,1)=0.
      ENDDO
      NPOLPAR(1)=0
      NADDVR(1)=0
C
      DO i=1,5
        IDATR(i,1)=0
      ENDDO
      NDELAYR(1)=0
      NTHRER(1)=0
      NPLUR(1)=0
      WIDTHR(1)=0.
      ENERR(1)=0.
      DO i=1,2
         BPMR(1,i,1)=0.
         BPMR(2,i,1)=0.
         BCMR(i,1)=0.
         SLDR(i,1)=0.
      ENDDO
      DO i=1,4
         AMAGNR(i,1)=0.
      ENDDO
      DO i=1,2
         POSTAR(i,1)=0.
      ENDDO
      ITARGR(1)=0
      TEMPTR(1)=0.
      HELCOIL(1)=0.
      WIENAN(1)=0.
      TADIAL(1)=0.
C
 999  CONTINUE
      END











