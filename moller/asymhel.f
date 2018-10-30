      REAL FUNCTION ASYMHEL(IGATE)
C
C===     Helicity analysis
C
      IMPLICIT NONE
      INTEGER  IGATE
C
      INCLUDE ?
C
      INCLUDE 'inc/v_helw.inc'
C
      INTEGER i,ievv,ieva
     +       ,itick0         !  tick mark of the previous cycle
     +       ,missw          !  number of missing windows
     +       ,ihelw          !  helicity window
     +       ,jhelold        !  helicity of the previous scaler event
     +       ,ierr           !  error for the helicity window
     +       ,ievvac         !  number of accepted scaler events
     +       ,ievaac         !  number of accepted adc  events
C
      DATA ierr/0/
      DATA ievv/0/
      DATA ieva/0/
      DATA ievvac/0/
      DATA ievaac/0/
      DATA ihelw/0/
      DATA itick0/0/
      DATA jhelold/0/
C
C     ------------------------------------------------------------------
C
      ASYMHEL=1
C
      IF(ITRIG(1).NE.0) THEN
C         IF(IDNEVT.GT.2000.AND.IDNEVT.LT.2030) GO TO 999
C         IF(IDNEVT.GT.11000.AND.IDNEVT.LT.11050) GO TO 999
         ieva=ieva+1
         IF(ihelw.GT.0.AND.jhelold.GT.0.AND.ITRIG(6)+1.NE.jhelold) THEN   ! compare this ADC event with the previous scaler event
            WRITE(6,*) ' Helicity flip at ev=',idnevt,' , adc ev=',ieva
     +              ,' , scal ev=',ievv,jhelold,ITRIG(6)+1
            ierr=3
         ENDIF
C
      ELSE IF(NSCA.GT.0) THEN
         ievv=ievv+1
         IF(ievv.GT.1) THEN
            IF(ieva.GT.0) THEN
               ievvac=ievvac+1
               ievaac=ievaac+ieva
               NCOWIN(4)=REAL(ievaac)/REAL(ievvac)+0.5
            ENDIF
         ENDIF
         ieva=0
C
C         IF(IDNEVT.GT.2000.AND.IDNEVT.LT.2030) GO TO 999
C         IF(IDNEVT.GT.11000.AND.IDNEVT.LT.11050) GO TO 999
C
         IF(ihelw.GE.MXHELW) THEN
            WRITE(6,*) ' *** Error: ihelw.GT.MXHELW ',ihelw,MXHELW
            GO TO 999
         ENDIF
C
C---     Is there a missing window?
C
C         IF(ievv.GT.50.and.ievv.LT.53) GO TO 999 
         IF(itick0.GT.0) THEN
            IF(ITICK-itick0-IGATE.GT.MAX(IGATE/4,2)) THEN
               missw=(ITICK-itick0+IGATE/2)/IGATE-1
               WRITE(6,*) ' *** Add ',missw,' windows at ievv=',ievv 
               DO i=1,missw
                  ihelw=ihelw+1
                  KHELWIN(ihelw)=0
                  KTICWIN(ihelw)=0
                  KFLAWIN(ihelw)=3
               ENDDO
               ierr=3
            ENDIF
         ENDIF
         ihelw=ihelw+1
         KHELWIN(ihelw)=ITRIG(6)+1
         KTICWIN(ihelw)=ITICK
         KFLAWIN(ihelw)=ierr
C
         KREFWIN(ievv)=ihelw
C
         itick0=ITICK
         jhelold=ITRIG(6)+1
         ierr=0
C
         NCOWIN(1)=ihelw
C
      ENDIF
C
 999  RETURN
      END



