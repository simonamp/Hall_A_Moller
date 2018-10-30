      REAL FUNCTION A_SIGLR(IV1,IV2,NORM,IHEL,IPAIR)
C
C ===  Analyze the SCALER ntuple (one quad per entry)
C ==   Returns signals/asymmetries for left/right/coinc.:
C ==       IV1 > 0 
C ==       IV2 > 0 - result= summ (sig(IV1)+sig(IV2))/2.
C ==           = -IV2a < 0 result= (sig(IV1)-sig(IV2))/(sig(IV1)+sig(IV2))
C       NORM = 0 - not normalized
C            = 1 - normalized to BCM
C            = 2 - normalized to scaler 
C       IHEL > 0 - select only certain helicity
C      IPAIR = 1/2 - the pair number, =0 - all pairs (2)
C
      IMPLICIT NONE
      INTEGER IV1,IV2,NORM,IPAIR,IHEL
      INCLUDE ?
C
      LOGICAL  HEXIST
      EXTERNAL HEXIST
C
      INTEGER iq,n,nel,i,iv2a
      REAL aa,a1,a2,an,res
C
      A_SIGLR=-2.
      IF(IFZUG.NE.0) GO TO 999  ! remove faulty trains (quads)
      IF(NELEM.LT.2) GO TO 999  ! at least
      IF(IV1.LT.1.OR.IV1.GT.NSCAL) THEN
         WRITE(6,*) ' *** Error: zug=',IZUG,' IV1=',IV1
     +              ,' is out of range ',1,NSCAL
         GO TO 999
      ENDIF
      iv2a=ABS(IV2)
      IF(iv2a.LT.1.OR.iv2a.GT.NSCAL) THEN
         WRITE(6,*) ' *** Error: zug=',IZUG,' IV2=',IV2
     +              ,' is out of range ',1,NSCAL
         GO TO 999
      ENDIF
C
      nel=NELEM
      n=0
      res=0.
C
      DO iq=1,nel
         IF(IHEL.EQ.0.OR.jhel(iq).EQ.IHEL-1) THEN
            IF(IPAIR.EQ.0.OR.INT((iq+1)/2).EQ.IPAIR) THEN
               a1=JCNT(IV1,iq)
               a2=JCNT(iv2a,iq)
               an=1.
               IF(NORM.EQ.1) THEN
                  an=JCNT(5,iq)
               ELSE IF(NORM.EQ.2) THEN
                  an=JCNT(12,iq)
               ENDIF
               IF(IV2.GT.0) THEN
                  aa=(a1+a2)/2./an
               ELSE
                  aa=0.
                  IF(a1+a2.GT.0.) THEN
                     aa=(a1-a2)/(a1+a2)
                  ENDIF
               ENDIF
               res=res+aa
               n=n+1
            ENDIF
         ENDIF
      ENDDO
C
      IF(n.GT.0) THEN
         res=res/n
         A_SIGLR=res
      ENDIF
C
C
 999  RETURN
      END

