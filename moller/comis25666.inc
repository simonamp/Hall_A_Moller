*********************************************************
*                                                       *
* This file was generated by HUWFUN.                    *
*                                                       *
*********************************************************
*
*     Ntuple Id:      1    
*     Ntuple Title:   Moller data
*     Creation:       29/05/2018 09.51.00
*
*********************************************************
*
      LOGICAL         CHAIN
      CHARACTER*128   CFILE
      INTEGER         IDNEVT,NCHEVT,ICHEVT
      REAL            OBS(13)
*
      COMMON /PAWIDN/ IDNEVT,OBS
      COMMON /PAWCHN/ CHAIN, NCHEVT, ICHEVT
      COMMON /PAWCHC/ CFILE
*
*--   Ntuple Variable Declarations
*
      INTEGER irun,idtyp,ievtyp,iret,itrig(8),itick,nadc,iadc(36),ntdc
     + ,itch(4096),itim(4096),ited(4096),nhcha,ihit(10),nsca,isca(33)
*
      COMMON /PAWCR4/ irun,idtyp,ievtyp,iret,itrig,itick,nadc,iadc,ntdc
     + ,itch,itim,ited,nhcha,ihit,nsca,isca
