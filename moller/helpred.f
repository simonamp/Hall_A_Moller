      SUBROUTINE HELPRED(IHEL,ITICK,IHELPRED,IPAIR,MODEHEL)
C
C---      Simulated the pseudorandom helicity generator
C---      Returns the predicted helicity and shifts the register
C
      IMPLICIT NONE
      INTEGER IHEL           ! INPUT : the current helicity
     +       ,ITICK          ! INPUT : the tick (time mark, 120Hz) of the entry
     +       ,IHELPRED       ! OUTPUT: the predicted helicity
     +       ,IPAIR          ! OUTPUT: =1 - 1st wave in the cycle, =2 - 2nd wave, =0 -?
     +       ,MODEHEL        ! OUTPUT: helicity mode: 0 - unknown, =1 - pseudorandom, 2 - toggle
C                                        +10 - double (oversampling: 2 gates for 1 helicity window) 
      INTEGER    mxar,mxoversmp
      PARAMETER (mxar=1000,mxoversmp=4)
      INTEGER i,j,k,i1mx,i1tot,i1,ipri
     +       ,i1win          ! start of the 1-st helicity window in the register (can be >1 if oversampling>1) 
     +       ,ibeg           ! start of the 1-st helicity cycle  in the register 
     +       ,mbit(4)        ! the bist used for exclusive OR
     +       ,kbit(4)
     +       ,ireg(mxar)     !  the shift register for pseudorandom generator
     +       ,itim(mxar)     !  the shift register for tick marks
     +       ,ioversmp       !  oversampling ( =1 - normal - 1 readout per helicity window, = 2 - double, <5 )
     +       ,itoggle        !  >0 - toggle mode
     +       ,lenmx          !  useful length of the shift register 
C
      DATA    mbit/17,22,23,24/
      DATA    ireg/mxar*0/
      DATA    itim/mxar*0/
      DATA    ipri/0/
C
C     ------------------------------------------------------------------
C
      IPAIR=0
      MODEHEL=0
C
C---      Find the helicity mode and the beginning of a cycle
C
C---       Is it the double (oversampling) mode?
C
      i1=1
      DO i=1,46
         IF(IREG(i).NE.IREG(i+1)) THEN
            i1=i+1
            GO TO 10
         ENDIF
      ENDDO
 10   CONTINUE
      ioversmp=mxoversmp
      j=IREG(i1)
      k=0
      i1mx=0
      DO i=i1+1,46
         k=k+1
         IF(IREG(i).EQ.1) i1mx=i1mx+1
         IF(j.NE.IREG(i)) THEN
            ioversmp=MIN(ioversmp,k)
            j=IREG(i)
            k=0
         ENDIF
      ENDDO
      IF(i1mx.LT.36) THEN
         ioversmp=1
      ENDIF
      lenmx=ioversmp*50
C
C---       Is it the toggle mode?
C
      itoggle=1
      DO i=1,lenmx-10,ioversmp
         IF(IREG(i).EQ.IREG(i+ioversmp)) THEN
            itoggle=0
            GO TO 20
         ENDIF
      ENDDO
 20   CONTINUE
      
      i1mx=1
      i1tot=0
      DO i=1,lenmx-1,ioversmp
         IF(IREG(i).EQ.1) THEN
            i1mx=i
            i1tot=i1tot+1
         ENDIF
         IF(IREG(i).EQ.IREG(i+ioversmp).AND.IREG(i).EQ.1) THEN
            j=(i-1)/(ioversmp)+1
            IF(MOD(j,2).EQ.0) THEN
               ibeg=1        !   1-st wave in the cycle  
            ELSE
               ibeg=2        !   2-nd wave in the cycle
            ENDIF
            IPAIR=ibeg
            GO TO 100
         ENDIF
      ENDDO
C
C---       No double 1 is found: either <40 (or so) cycle, or the toggle mode
C
      ibeg=-1
      IF(i1mx.GT.40.AND.i1tot.GE.(lenmx-1)/2-1) THEN
         ibeg=2                 ! in the toggle mode the helicity is always inverted
         IPAIR=1
         IF(IHEL.EQ.1) IPAIR=2
      ENDIF

 100  CONTINUE
C
      IHELPRED=-1
C
      IF(ibeg.EQ.2) THEN
C
C---       2-nd wave: invert the previous helicity
C
         IHELPRED=0
         IF(IREG(1).EQ.0) IHELPRED=1
C
      ELSEIF(ibeg.EQ.1) THEN
C
C---       1-st wave: predict the pseudorandom helicity
C
         DO i=1,4
            kbit(i)=IREG(3-ibeg+(mbit(i)-1)*2)
         ENDDO
C
C---        XOR of 4 bits
C
         j=kbit(1)
         DO i=2,4
            IF(j.EQ.kbit(i)) THEN
               j=0
            ELSE
               j=1
            ENDIF
         ENDDO
         IHELPRED=j
C         WRITE(6,FMT='('' IHEL,kbit ='',I3,2X,4I2,I4)') IHEL,kbit
C     +          ,IHELPRED
C     
      ENDIF
C
C---      Shift the register
C
      DO i=lenmx,2,-1
         IREG(i)=IREG(i-1)
         ITIM(i)=ITIM(i-1)
      ENDDO
      IF(IHEL.GE.0) THEN
         IREG(1)=IHEL
      ELSE
         IREG(1)=IHELPRED
      ENDIF
      ITIM(1)=ITICK
C
      IF(ipri.LT.120) THEN
         ipri=ipri+1
C         WRITE(6,*) 'lenmx=',lenmx,ioversmp,itoggle
         WRITE(6,FMT='(I5,2I3,4X,52I2)') ipri,IHEL,IHELPRED
     +               ,(IREG(i),i=1,52)
      ENDIF
C
C      WRITE(6,FMT='('' end'',I3,3X,50I2)') IHEL,IREG
C
      RETURN
      END









