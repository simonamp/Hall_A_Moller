      SUBROUTINE ANTPINI(ID0)
C
C---     Initialize an CW NTUPLE for asymmetry data 
C
      IMPLICIT NONE
      INTEGER ID0
C
      INCLUDE 'inc/cntasym.inc'
C
      INTEGER id
C
      CHARACTER lenc*2,clin*20,lens*3
C
C     ------------------------------------------------------------------
C
      id=ID0
      CALL HBNAME(id,'run',KRUN  ,'izrun')
      CALL HBNAME(id,'run',ANGL  ,'angl')
      CALL HBNAME(id,'run',HELMH ,'helmh')
      CALL HBNAME(id,'run',PTARG ,'ptarg')
      CALL HBNAME(id,'run',ITARG ,'itarg[-10,200]')
      CALL HBNAME(id,'run',XYTARG(1),'xytarg(2)')
C
      CALL HBNAME(id,'zug', KZUG  ,'izug')
      CALL HBNAME(id,'zug', IFZUG ,'ifzug')
      WRITE(lenc,FMT='(I2)') MXELEM
      WRITE(lens,FMT='(I3)') MXCNT
      clin='nelem[0,'//lenc//']'
      CALL HBNAME(id,'zug', NELEM , clin)
C      CALL HBNAME(id,'zug', NELEM ,'nelem[0,16]')
      clin='nscal[0,'//lens//']'
      CALL HBNAME(id,'zug', NSCAL , clin)
C
      CALL HBNAME(id,'zug', JFLA(1)     ,'jfla(nelem)[-1,14]')
      CALL HBNAME(id,'zug', JHEL(1)     ,'jhel(nelem)[-1,1]')
C
      clin='jcnt('//lens//',nelem)'
      CALL HBNAME(id,'zug', JCNT(1,1)    ,clin)
C
      CALL HBNAME(id,'zug', JADC(1)    ,'jadc(nelem)[0,255]')
      CALL HBNAME(id,'zug', JTICK(1)   ,'jtick(nelem)')
      CALL HBNAME(id,'zug', JDTICKAF(1),'jdtickaf(nelem)[0,63]')
      CALL HBNAME(id,'zug', JDTICKAL(1),'jdtickal(nelem)[0,63]')
C
 999  CONTINUE
      END
