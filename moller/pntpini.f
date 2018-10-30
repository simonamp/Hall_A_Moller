      SUBROUTINE PNTPINI(ID0)
C
C---     Initialize an CW NTUPLE for run-wise polarization data 
C
      IMPLICIT NONE
      INTEGER  ID0
C
      INCLUDE 'inc/crunpol.inc'
C
      INTEGER id
C
      CHARACTER lenc*2,clin*20
C
C     ------------------------------------------------------------------
C
      id=ID0
      CALL HBNAME(id,'pol',IRUN  ,'irun')
C
      WRITE(lenc,FMT='(I2)') MXCNT
      clin='ncount[0,'//lenc//']'
      CALL HBNAME(id,'pol', NCOUNTR(1)   ,clin)
      CALL HBNAME(id,'pol',  COUNTR(1,1) , 'count(ncount)')
C
      WRITE(lenc,FMT='(I2)') MXPOL
      clin='npolr[0,'//lenc//']'
      CALL HBNAME(id,'pol', NPOLR(1   ,clin)
      CALL HBNAME(id,'pol', POLR(1,1) , 'polr(npolr)')
      CALL HBNAME(id,'pol',EPOLR(1,1) , 'epolr(npolr)')
C
      CALL HBNAME(id,'pol', ASYMR(1,1)   ,  'asymr(2)')
      CALL HBNAME(id,'pol',BASYMR(1,1)   , 'basymr(2)')
C
      WRITE(lenc,FMT='(I2)') MXPPAR
      clin='npolpar[0,'//lenc//']'
      CALL HBNAME(id,'pol', NPOLPAR(1)   , clin)
      CALL HBNAME(id,'pol', POLPAR(1,1)  , 'polpar(npolpar)')
C
      WRITE(lenc,FMT='(I2)') MXADD
      clin='naddvr[0,'//lenc//']'
      CALL HBNAME(id,'pol', NADDVR(1)    ,clin)
      CALL HBNAME(id,'pol', ADDVR(1,1)   , 'addvr(naddvr)')
C
 999  CONTINUE
      END











