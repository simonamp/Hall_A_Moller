C
C === cevcoda.inc
C === CODA event information
C
      INTEGER    MXFRAG
      PARAMETER (MXFRAG=10)
C
C
      COMMON/CEVCODA/ IRUNCUR,ITIMST,ITIMCUR
     +               ,LENEVE,IEVDTYP,IEVTYP
     +               ,IEVNUM,IEVCLAS,IEVSTAT
     +               ,NFRAG,IFRAG,LENFRAG(MXFRAG),IROCFRAG(MXFRAG)
C
      INTEGER         IRUNCUR          ! the current run number
     +               ,ITIMST           ! starting time for the run
     +               ,ITIMCUR          ! current time (of the previous special event)
     +               ,LENEVE           ! event length
     +               ,IEVDTYP          ! event data type
     +               ,IEVTYP           ! event type
     +               ,IEVNUM           ! event number
     +               ,IEVCLAS          !       classification
     +               ,IEVSTAT          !       status
     +               ,NFRAG            ! total number of the fragments
     +               ,IFRAG            ! the current fragment
     +               ,LENFRAG          ! lengths of the fragments 
     +               ,IROCFRAG         ! names of the ROCs for each fragment
C
