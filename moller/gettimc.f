      REAL FUNCTION GETTIMC(NCH1,NCH2,ITOL,ISH,ID)
C
C=== Fills a histogram ID for the time coincidence between NCH1,NCH2 in ITOL tolerance 
C=== ISH - shift to check accidentals
C
      IMPLICIT NONE
      INTEGER NCH1,NCH2,ITOL,ISH,ID
      INCLUDE ?
C
      INTEGER i1,i2,ich1,ich2,it1,it2
C
      GETTIMC=1.
C
      DO i1=1,NTDC
         ich1=ITCH(i1)
         IF(ich1.EQ.NCH1) THEN
            it1=ITIM(i1)
            DO i2=1,NTDC
               ich2=ITCH(i2)
               IF(ich2.EQ.NCH2) THEN
                  it2=ITIM(i2)
                  IF(IABS(it1-it2+ISH).LE.ITOL) THEN
                     CALL HF1(ID,it1+0.1,1.)
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      END DO
C
 999  CONTINUE
      END

