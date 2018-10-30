      SUBROUTINE ASYM_REND10(IRUN,ICOP)
C
C===     At the end on the run analysis: print the results and store the data
C
      IMPLICIT NONE
      INTEGER IRUN          !  run number
     +       ,ICOP          ! >0 - store the data
C
      INCLUDE 'inc/v_asym.inc'
      INCLUDE 'inc/v_run.inc'
C
      INTEGER i,j,m
     +       ,kp                    ! pointer to the run table
     +       ,mxres                 ! number of words / resrun array
C
      REAL    fac                   ! factor for asymm --> polariz
     +       ,gate                  ! gate length in sec
     +       ,dtim                  ! dead time (LED)
C
      PARAMETER (mxres=MXRESW/4)
      INTEGER
     +        mapres(mxres)         ! mapping CASYM --> RESRUN
C
      CHARACTER*32 cnam(mxres)      ! names for the variables
      DATA mapres/ 1, 2, 3, 4, 5, 6,12,15,17,18
     +           ,19,20,21,22,23,24,25,0,0,0/ 
      DATA cnam/'Left  arm                       '
     +         ,'Right arm                       '
     +         ,'Coincidence                     '
     +         ,'Coincidence accidentals         '
     +         ,'BCM                             '
     +         ,'Left arm accidentals / LED puls '
     +         ,'Timer                           '
     +         ,'Timer no helicity window in PLU '
     +         ,'Left  arm 2-nd                  '
     +         ,'Right arm 2-nd                  '
     +         ,'Coincidence  2-nd               '
     +         ,'Coincidence accidentals  2-nd   '
     +         ,'Left  LG delayed                '
     +         ,'Right LG                        '
     +         ,'Sum   LG                        '
     +         ,'Left  App                       '
     +         ,'Right App                       '
     +         ,'                                '
     +         ,'                                '
     +         ,'                                '/
C
C     ------------------------------------------------------------------
C
      IF(IRUN.LT.2.OR.IRUN.GT.MXKRUN) THEN
         WRITE(6,*) ' *** Error in asym_rend10.f: IRUN out of range ',IRUN
         GO TO 999
      ENDIF
C
      kp=KRUNPNT(IRUN)
      IF(kp.LE.0.OR.kp.GT.MXRRUN) THEN
         WRITE(6,*) ' *** Error in asym_rend10.f: IRUN slot is missing'
     +     ,' IRUN,kp=',IRUN,kp
         GO TO 999
      ENDIF
C
      IF(ICOP.GT.0) THEN
C
C---     Copy the constants
C
         gate=COASYM(7)
         IF(gate.LT.0.00001) THEN
            WRITE(6,*) ' *** Error in asym_rend10.f: IRUN=',IRUN
     +           ,' wrong gate=',gate
            GO TO 999
         ENDIF
         fac=COASYM(9)
C
         DO i=1,MXRCONS
            CONSRUN(i,kp)=COASYM(i)
         ENDDO
C
         NRSRUN(1,kp)= NASYM(1)
         NRSRUN(2,kp)= NASYM(16)
         NRSRUN(3,kp)= NASYM(17)
C
C---     Copy the scalers/asymmetries
C
         DO i=1,mxres
            m=mapres(i)
            IF(m.GT.0) THEN
                RESRUN(i+mxres*0,kp)= CASYM(m)/gate
               ERESRUN(i+mxres*0,kp)=ECASYM(m)/gate
                RESRUN(i+mxres*1,kp)= RASYM(m)
               ERESRUN(i+mxres*1,kp)=ERASYM(m)
                RESRUN(i+mxres*2,kp)= FASYM(m)
               ERESRUN(i+mxres*2,kp)=EFASYM(m)
            ENDIF
         ENDDO
C
         DO i=1,3
            m=mapres(i)
            IF(m.GT.0) THEN
                RESRUN(i+mxres*3,kp)= FASYM(m)*fac
               ERESRUN(i+mxres*3,kp)=EFASYM(m)*fac
            ENDIF
         ENDDO
C
         DO i=9,11
            m=mapres(i)
            IF(m.GT.0) THEN
                RESRUN(i+mxres*3,kp)= FASYM(m)*fac
               ERESRUN(i+mxres*3,kp)=EFASYM(m)*fac
            ENDIF
         ENDDO
C
      ENDIF
C
C---        Print the run
C
      gate=CONSRUN(7,kp)
      WRITE(6,1000) IRUN,(NRSRUN(i,kp),i=1,3),(CONSRUN(i,kp),i=1,9)
 1000 FORMAT(//5X,'Summary for run=',I6,'  Cycles: all=',I5
     +      ,'   H+=',I5,'   H-=',I5,5X,
     +      /2X,'T.angle=',F7.1,2X,'An.power=',F7.4,2X,'T.pol=',F7.4
     +      ,2X,'Norm=',F3.0,2X,'NormBCM=',F3.0,2X,'Coils=',F5.1,' A'
     +      ,2X,'gate=',F7.4,2X,'RunType=',F3.0,2X,'Factor=',F8.4,
     +      /' no  Meaning ',28X,'rate/cycle',10X,'rate/sec',10X
     +      ,'raw asymmetry'
     +      ,4X,'corrected asymmetry',3X,'Polarization')
      DO i=1,mxres
         IF(cnam(i)(1:6).NE.'      ') THEN
            WRITE(6,1200) i,cnam(i)
     +           ,RESRUN(i+mxres*0,kp)*gate
     +           ,(RESRUN(i+mxres*j,kp),ERESRUN(i+mxres*j,kp),j=0,3)
 1200       FORMAT(I4,2X,A32,1X,F9.1,3X,F11.1,' +/-',F8.1
     +            ,3(1X,F8.4,' +/-',F7.4))
         ENDIF
      ENDDO
C
      IF(RESRUN(6,kp).GT.0..AND.COASYM(8).GT.0.) THEN
         dtim=(RESRUN(6,kp)-RESRUN(3,kp))/RESRUN(6,kp)
         WRITE(6,1500) dtim
 1500    FORMAT(/2X,'LED dead time (if relevant)=',F8.4)
      ENDIF
C
 999  CONTINUE
      END







