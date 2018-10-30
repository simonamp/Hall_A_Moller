      SUBROUTINE PNTIINI(ID0)
C
C---     Initialize an CW NTUPLE for run-wise Info data 
C
      IMPLICIT NONE
      INTEGER  ID0
C
      INCLUDE 'inc/cruninf.inc'
C
      INTEGER id
C
      CHARACTER lenc*2,clin*20
C
C     ------------------------------------------------------------------
C
      id=ID0
      CALL HBNAME(id,'inf', IRUNR(1) ,'irunr')
C
      clin='idatr(5)[0,2047]'
      CALL HBNAME(id,'inf', IDATR(1,1)   ,  clin)
C
      WRITE(lenc,FMT='(I2)') MXDEL
      clin='ndelayr[0,'//lenc//']'
      CALL HBNAME(id,'inf', NDELAYR(1)   ,  clin)
      CALL HBNAME(id,'inf', IDELAYR(1,1) , 'idelayr(ndelayr)')
C
      WRITE(lenc,FMT='(I2)') MXTHR
      clin='nthrer[0,'//lenc//']'
      CALL HBNAME(id,'inf', NTHRER(1)    ,  clin)
      CALL HBNAME(id,'inf', ITHRER(1,1)  , 'ithrer(nthrer)')
C
      WRITE(lenc,FMT='(I2)') MXPLU
      clin='nplur[0,'//lenc//']'
      CALL HBNAME(id,'inf', NPLUR(1)     ,  clin)
      CALL HBNAME(id,'inf', IPLUR(1,1,1) , 'iplur(3,nplur)')
C
      CALL HBNAME(id,'inf', WIDTHR(1)    , 'widthr')
      CALL HBNAME(id,'inf', ENERR(1)     , 'enerr')
      CALL HBNAME(id,'inf', BPMR(1,1,1)  , 'bpmr(2,2)')
      CALL HBNAME(id,'inf', BCMR(1,1)    , 'bcmr(2)')
      CALL HBNAME(id,'inf', SLDR(1,1)    , 'sldr(2)')
      CALL HBNAME(id,'inf', AMAGNR(1,1)  , 'amagnr(4)')
      CALL HBNAME(id,'inf', POSTAR(1,1)  , 'postar(2)')
      CALL HBNAME(id,'inf', ITARGR(1)    , 'itargr')
C
      CALL HBNAME(id,'inf', TEMPTR(1)    , 'temptr')
      CALL HBNAME(id,'inf', HELCOIL(1)   , 'helcoil')
      CALL HBNAME(id,'inf', WIENAN(1)    , 'wienan')
      CALL HBNAME(id,'inf', TADIAL(1)    , 'tadial')
C
 999  CONTINUE
      END











