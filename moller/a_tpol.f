      REAL FUNCTION A_TPOL(IDUMMY)
C
C ===  Returns the targert polarization for the given scaler event
C
      IMPLICIT NONE
      INTEGER IDUMMY
      INCLUDE ?
C
      INCLUDE 'inc/a_tpol.inc' ! target polarization
C
      REAL A_TPOS,TPOL_Z
      EXTERNAL A_TPOS,TPOL_Z
C
      INTEGER  itarvec,itarev,izstor,i1,i2,j1,j2
      REAL xyt(2),helmhvec,helmev
C
      CHARACTER cline*132,ctarg*2,chelm*8
C
      A_TPOL=1.E-6
C
      IF(IZRUN.LT.14000) THEN
C
C---      Old target (4 foils at 20 deg vert)
C
         itarev=INT(A_TPOS(1,2)+0.1)
         xyt(2)=A_TPOS(2,0)
C
         IF(itarev.NE.itarg) THEN
            WRITE(6,1000) izrun,itarg,itarev
 1000       FORMAT(' Error in run=',I5,' target numbers mismatch ',2I4)
            GO TO 999
         ENDIF
         helmev=ABS(helmh)
         helmhvec=ABS(TPOLCUR(3))
         itarvec=INT(TPOLCUR(2)+0.1)
         izstor=INT(TPOLCUR(4)+0.1)
C     
C---       Has the target polarization for this target/helmholtz coils loaded already?
C
C      write(6,*) itarg,itarvec,helmev,helmhvec
         IF(izstor.EQ.0.OR.
     +       itarg.NE.itarvec.OR.ABS(helmev-helmhvec).GT.0.01) THEN
C
C---       Load the vector
C
            WRITE(6,*) ' Initialize the target polarization, run=',
     +               izrun,' target=',itarg,' Helmh=',helmh
            WRITE(ctarg,FMT='(I2)') itarg
            CALL STR_TRIM(ctarg,i1,i2)
            WRITE(chelm,FMT='(F8.3)') ABS(helmh)
            CALL STR_TRIM(chelm,j1,j2)
C         write(6,*) i1,i2,j1,j2
            cline='exec get_targpol hcoil='//chelm(j1:j2)//' target='
     +                //ctarg(i1:i2)//' zcm=0. init=1'
            WRITE(6,*) cline
            CALL KUEXEL(cline)
         ENDIF
C      
         A_TPOL=TPOL_Z(xyt(2))
C
      ELSE IF(IZRUN.LT.99999) THEN
C
C---    High field target: the data are taken from the NTUPLE
C
         A_TPOL=PTARG
C
      ENDIF
C
 999  RETURN
      END
C
      INCLUDE 'a_tpos.f'
      INCLUDE 'tpol_z.f'
      INCLUDE 'str_trim.f'

