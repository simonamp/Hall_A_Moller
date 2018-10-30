      SUBROUTINE POL_PRI_1
C
C===     Prints a summary for several measurements 
C
      IMPLICIT NONE
      INCLUDE 'inc/v_run.inc'
      INCLUDE 'inc/v_runset.inc'
      INCLUDE 'inc/v_runmea.inc'
C
      REAL ATT_LASER,ATT_SLIT
C
      INTEGER i,j,ir,im,nmea,kp
      REAL    att,beam,atten(3)
C
      nmea=MXRMEA
      WRITE(6,1500)
 1500 FORMAT(3X,'   m   d   h',3X,'1-st run   GeV     Polarization'
     +      ,3X,'COS(spin)    uA   Las:power atten slit'
     +      ,4X,'loss  1/2wave B   C')
      DO im=1,nmea
         ir=KRUNM(im) 
         IF(ir.GT.0) THEN
            kp=KRUNPNT(ir)
            IF(kp.GT.0) THEN
               atten(1)=RUNSET(15,kp)
               atten(2)=RUNSET(14,kp)
               atten(3)=RUNSET(13,kp)
               beam=RUNSET(11,kp)
               att=0.
               IF(atten(2).GT.0.AND.atten(3).GT.0) THEN
                  att=beam/0.44
     +               *ATT_LASER(500.)/ATT_LASER(atten(2))
     +               *ATT_SLIT(15.4)/ATT_SLIT(atten(3))
               ENDIF
               WRITE(6,2000) im,(INT(RUNSET(j,kp)),j=3,5),ir
     +                 ,RUNSET(8,kp)
     +                 ,polm(im,3)*100.,epolm(im,3)*100.
     +                 ,RUNSET(29,kp)
     +                 ,beam
     +                 ,(atten(j),j=1,3),att
     +                 ,(INT(RUNSET(j,kp)),j=18,18)
     +                 ,(INT(RUNSET(j,kp)),j=16,17)
 2000          FORMAT(I3,3(2X,I2),3X,I5,2X,F8.4
     +          ,1(2X,F6.2,' +/-',F5.2),2X,F7.3,2X
     +          ,F7.3,4X,F6.1,F6.0,F6.2,1X,F7.2,3X,I1,3X,2I4)
            ENDIF
         ENDIF 
      ENDDO
C
 999  CONTINUE
      END
C
      INCLUDE 'att_laser.f'
      INCLUDE 'att_slit.f'








