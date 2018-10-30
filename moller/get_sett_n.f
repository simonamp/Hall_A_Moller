      SUBROUTINE GET_SETT_N
C
C===    Get the run settings from 
C===     fort.1 - variable definitions
C===     fort.2 - the settings file 
C===     vector run_sett - output
C
      IMPLICIT NONE
      INTEGER mxkey
      PARAMETER (mxkey=120)
C
      VECTOR RUN_SETT(mxkey)
      INTEGER LNBLNK,KNBLNK
C
      INTEGER lunt,luns,k1,lenvar,lenlin,nvar
      INTEGER i,j,k,n,nkey,ikey,jkey(mxkey),nent(mxkey),lenkey(mxkey)
      INTEGER kd1(6)
      REAL var(10)
      INTEGER    mxlin
      PARAMETER (mxlin=132)
      CHARACTER ckeynam(mxkey)*80,ckeytyp(mxkey)*1
      CHARACTER cline*132,cvar*32,cvar1*32,cline1*132
C
      DO ikey=1,mxkey
         nent(ikey)=0
      ENDDO
C
      lunt=1
      luns=2
      k1=36
      write(6,*) 'Start get_Sett_n'
C
      nkey=0
      OPEN(UNIT=lunt,FILE='fort.1',STATUS='OLD')
      REWIND lunt
 10   READ(lunt,FMT='(A132)',END=20) cline
C      write(6,*) cline
      IF(nkey.LT.mxkey) THEN
         lenlin=LNBLNK(cline)
         IF(lenlin.GT.8) THEN
            k=KNBLNK(cline(7:lenlin))+6
            IF(k.GT.0) THEN
               nkey=nkey+1
               jkey(nkey)=0
               READ(cline,FMT='(I3)') jkey(nkey)
               ckeytyp(nkey)=cline(5:5)
               lenvar=INDEX(cline(k:lenlin),' ')-1
*               write(6,*) nkey,k,lenlin,lenvar
               ckeynam(nkey)=cline(k:k+lenvar-1)
               lenkey(nkey)=lenvar
C               write(6,*) nkey,' ',ckeytyp(nkey),jkey(nkey),lenkey(nkey)
C     +        ,' ',ckeynam(nkey)
C     +        ,lnblnk(ckeynam(nkey))
            ENDIF
         ENDIF
      ELSE
         WRITE(6,*) ' ### get_sett.f: Too many keys in the table file '
     +             ,nkey
      ENDIF
      GO TO 10
C
 20   CONTINUE
      WRITE(6,*) ' get_sett.f: defined keys=  ',nkey
C
C      RETURN
C
C---     Read the settings file
C
      OPEN(UNIT=luns,FILE='fort.2',STATUS='OLD')
      REWIND luns
 60   READ(luns,FMT='(A132)',END=120) cline
C      write(6,*) cline
      n=0
      DO ikey=1,nkey
         i=INDEX(cline(k1:mxlin),ckeynam(ikey)(1:lenkey(ikey)))
         IF(i.GT.0.AND.i.LT.5) THEN
            n=n+1
C            write(6,*) ikey,i,n,cline(k1:mxlin)
            IF(n.EQ.1) THEN
               cline1=cline(i+lenkey(ikey):mxlin)
C               write(6,*) cline1
               i=INDEX(cline1,':')
               IF(i.GT.0) THEN
                  j=INDEX(cline1(i+1:mxlin),':')
C                  write(6,*) i,j
                  IF(j.GT.0) i=i+j
                  k=MIN(i+32,132)
                  cvar=cline1(i+1:k)
                  nent(ikey)=nent(ikey)+1
                  lenvar=LNBLNK(cvar)
                  nvar=1
C
C----                Read in the value
C
                  IF(ckeytyp(ikey).EQ.'f') THEN
C---    real
                     i=INDEX(cvar,'Pass')
                     IF(i.GT.0) THEN
                        cvar1=cvar(i+4:32)
                        cvar=cvar1
                        lenvar=LNBLNK(cvar)
                     ENDIF
                     i=INDEX(cvar,',')
                     IF(i.GT.0) THEN
                        cvar1=cvar
                        cvar(i:lenvar-1)=cvar1(i+1:lenvar)
                        lenvar=lenvar-1
                     ENDIF
                     i=INDEX(cvar,'.')
                     IF(i.EQ.0) THEN
                        lenvar=lenvar+1
                        cvar(lenvar:lenvar)='.'
                     ENDIF
C                     write(6,*) cvar
                     READ(cvar,*) var(1)
C                     write(6,*) jkey(ikey),' ',ckeynam(ikey),' ',var
C
                  ELSE IF(ckeytyp(ikey).EQ.'c') THEN
C---    character
                     IF(INDEX(cvar,'null').EQ.1) THEN
                        var(1)=0.
                     ELSE IF(INDEX(cvar,'OFF').EQ.1) THEN
                        var(1)=0.
                     ELSE IF(INDEX(cvar,'OUT').EQ.1) THEN
                        var(1)=0.
                     ELSE IF(INDEX(cvar,'IN').EQ.1) THEN
                        var(1)=1.
                     ELSE IF(INDEX(cvar,'CW MODE').EQ.1) THEN
                        var(1)=1.
                     ENDIF
C
                  ELSE IF(ckeytyp(ikey).EQ.'D') THEN
C---    Date
                     nvar=6
C                     write(6,*) 'cvar=',cvar
                     READ(cvar,*) kd1
                     DO i=1,nvar
                        var(i)=REAL(kd1(i))
                     ENDDO
                  ENDIF
C
                  DO i=1,nvar
                     RUN_SETT(jkey(ikey)+i-1)=RUN_SETT(jkey(ikey)+i-1)
     +                                       +var(i)
                  ENDDO

               ENDIF
            ENDIF
C            write(6,*) '=== ',ikey,ckeynam(ikey)(1:lenkey(ikey))
C---          Skip the other keys
            GO TO 100
         ENDIF
      ENDDO
 100  CONTINUE
      GO TO 60
C
 120  CONTINUE
C
      CLOSE(lunt)
      CLOSE(luns)
C 
      n=0
      DO ikey=1,mxkey
         IF(nent(ikey).GT.1) THEN
            n=n+1
            RUN_SETT(ikey)=RUN_SETT(ikey)/REAL(nent(ikey))
         ENDIF
      ENDDO
      WRITE(6,*) ' get_sett.f: read variables=',nkey
C
      RETURN
      END
C
      INTEGER FUNCTION LNBLNK(STR)
C
C---     Returns the position of the last non-blank character in string STR
C
      IMPLICIT NONE
      CHARACTER STR*(*)
      INTEGER i,j,n,nnb
C
      n=LEN(STR)
      nnb=n
 10   CONTINUE
      IF(STR(nnb:nnb).NE.' ') GO TO 20
      nnb=nnb-1
      IF(nnb.GT.0) GO TO 10
C
 20   CONTINUE
      LNBLNK=nnb
C
      RETURN
      END

C
      INTEGER FUNCTION KNBLNK(STR)
C
C---     Returns the position of the first non-blank character in string STR
C
      IMPLICIT NONE
      CHARACTER STR*(*)
      INTEGER i,j,n,nnb
C
      n=LEN(STR)
      nnb=1
 10   CONTINUE
      IF(STR(nnb:nnb).NE.' ') GO TO 20
      nnb=nnb+1
      IF(nnb.LT.n) GO TO 10
      nnb=0
C
 20   CONTINUE
      KNBLNK=nnb
C
      RETURN
      END

C
      
