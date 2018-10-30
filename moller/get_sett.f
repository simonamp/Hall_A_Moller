      SUBROUTINE GET_SETT
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
      INTEGER LNBLNK
C
      INTEGER lunt,luns,k1,k2,lenvar,lenlin,nvar
      INTEGER i,j,k,n,nkey,ikey,jkey(mxkey),nent(mxkey),lenkey(mxkey)
      INTEGER kd1(6)
      REAL var(10)
      CHARACTER ckeynam(mxkey)*80,ckeytyp(mxkey)*1
      CHARACTER cline*132,cvar*32,cvar1*32,cline1*132
C
      DO ikey=1,mxkey
         nent(ikey)=0
      ENDDO
C
      lunt=1
      luns=2
      k1=9
      k2=40
C
      nkey=0
      OPEN(UNIT=lunt,FILE='fort.1',STATUS='OLD')
      REWIND lunt
 10   READ(lunt,FMT='(A132)',END=20) cline
C      write(6,*) cline
      IF(nkey.LT.mxkey) THEN
         nkey=nkey+1
         ckeytyp(nkey)=cline(1:1)
         jkey(nkey)=0
         READ(cline,FMT='(4X,I3)') jkey(nkey)
         ckeynam(nkey)=cline(k1:k2)
         n=k2-k1+1
         lenkey(nkey)=LNBLNK(ckeynam(nkey)(1:n))
C         write(6,*) nkey,' ',ckeytyp(nkey),jkey(nkey),lenkey(nkey)
C     +        ,lnblnk(ckeynam(nkey))
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
         i=INDEX(cline,ckeynam(ikey)(1:lenkey(ikey)))
         IF(i.GT.0.AND.i.LT.5) THEN
            n=n+1
            IF(n.EQ.1) THEN
               cline1=cline(i+lenkey(ikey):132)
C               write(6,*) cline1
               i=INDEX(cline1,':')
               IF(i.GT.0) THEN
                  j=INDEX(cline1(i+1:132),':')
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
                  IF(ckeytyp(ikey).EQ.'r') THEN
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
C                     write(6,*) cvar
                     READ(cvar,*) kd1                     
                     DO i=1,nvar
                        var(i)=REAL(kd1(i))
                     ENDDO
                  ENDIF
C
                  DO i=1,nvar
                     RUN_SETT(jkey(ikey)+i-1)=RUN_SETT(jkey(ikey)+i-1)
     +                                       +var(i)
C                     write(6,*) i,jkey(ikey)+i-1,var(i)
                  ENDDO

               ENDIF
            ENDIF
C            write(6,*) '=== ',ikey,ckeynam(ikey)(1:lenkey(ikey))
         ENDIF
      ENDDO
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

      
