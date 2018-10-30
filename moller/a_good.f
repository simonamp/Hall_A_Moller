      REAL FUNCTION A_GOOD(IFL,IPAIR,TOLER)
C
C ===  Check the consistency of the scalers and return: -1. - bad, 1. - good
C ===  IFL   > 0 scaler IFL
C            = 0 scalers 1,2,3,4,5
C      IPAIR = 1/2 - the pair number, =0 - all pairs (2)
C      TOLER > 0. - reject the pair if scaler 1 - scaler 2 gives a difference more than (scaler 1 + scaler 2)*TOLER
C
      IMPLICIT NONE
      INTEGER IFL,IPAIR
      REAL TOLER
      INCLUDE ?
C
      INTEGER iq,n,nel,i,iok,isc,isc1,isc2,jsc1,jsc2
      INTEGER mxsca
      PARAMETER (mxsca=5)
      INTEGER msc1(mxsca),msc2(mxsca)
C
      DATA msc1/ 1, 2, 3, 4, 5/
      DATA msc2/17,18,19,20, 0/
C
      A_GOOD=-1.
C
      IF(IFZUG.NE.0) GO TO 999  ! remove faulty trains (quads)
      IF(NELEM.LT.2) GO TO 999  ! at least
C
      IF(IFL.LT.0.OR.IFL.GT.NSCAL) THEN
         WRITE(6,*) ' A_GOOD: Wrong parameter IFL=',IFL
         GO TO 999
      ENDIF
C
      nel=NELEM
      IF(IFL.GT.0) THEN
         isc1=IFL
         isc2=IFL
      ELSE
         isc1=1
         isc2=mxsca
      ENDIF
C
      iok=1
      DO iq=1,nel
         IF(IPAIR.EQ.0.OR.INT((iq+1)/2).EQ.IPAIR) THEN
            DO isc=isc1,isc2
               jsc1=msc1(isc)
               IF(jsc1.GT.0) THEN
                  IF(JCNT(jsc1,iq).LT.-190) iok=0
               ENDIF
               jsc2=msc2(isc)
               IF(jsc2.GT.0) THEN
                  IF(JCNT(jsc2,iq).LT.-190) iok=0
                  IF(TOLER.GT.1.E-20) THEN
                     IF(REAL(IABS(JCNT(jsc2,iq)-JCNT(jsc1,iq))).GT.
     +                  REAL(JCNT(jsc2,iq)+JCNT(jsc1,iq))*TOLER) iok=0
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDDO
C
      IF(iok.EQ.1) A_GOOD=1.
C
 999  RETURN
      END

