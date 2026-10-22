      SUBROUTINE INTRAN
C	TRANSPORT BLOCK
C     CALLED BY TRANS NEAR LINE 53
C=======================================================================
C     University of Florida Transport Model.
C     This large subroutine performs input of Transport data.
C     Updated May 1992 to use Subroutine .
C     Updated June 1993 by RED to read natural channel data correctly.
C     Modified by WCH (CDM - Chuck Moore), 8/93 for new Type 26 flow
C       divider based on a tabular flow split.
C     Fix metric conversion and eliminate one JSTOP, 9/23/93. WCH (RED).
C     Add additional parameter for WASP linkage, James L. Martin,
C       AScI Corp., 10/93.
C     Fix check for next ID after reading natural channel data,
C       WCH, 5/10/94.
C     Add additional error checks for F1 particle size data, WCH,
C       WCH, 1/13/95.
C     Eliminate CMET5 since don't use, WCH, 6/5/95.
C     Add error check for zero E1 elements read in, WCH, 8/15/96.
C     Fix metric conversons for weirs, WCH, 8/10/00.
C     Error check for needed extra E1 line with constant pollutant
C       inflows, for missing pollutant data, for missing
C       KGEOM value, Better printout of KGEOM. Tiny adjustment
C       of metric conversions (CMET1, CMET3).  Fix erroneous
C       printout of DIST etc. for manholes.  WCH, 8/29/00.
C     Add optional inputs for detailed irregular sections.  CIM 9/8/00
C     Add optional inputs for writing depth and velocity to interface.
C       CIM 9/8/00
C     Move water quality input routines to Sub. INTRQUAL and modify 
C       quality routines for optional BOD-DO simulation.  WCH 7/6/01. 
C     Provide for no headers in middle of long tables, WCH, 10/3/01.
C     Fix for metric input in GETCUR, WCH, 3/14/02. 
C     New error message for E1 lines, WCH, 4/15/02.
C	Error message for incompatible $ANUM. WCH, 4/18/02.
C     Allow override of interface file starting date/time. WCH, 4/18/02.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'DRWF.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'NAMES.INC'
      INCLUDE 'TST.INC'
      INCLUDE 'NEW81.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'FLODAT.INC'
      INCLUDE 'NEWTR.INC'
      INCLUDE 'FSPLIT.INC'
CIM  CHANGE FOR VARIABLE BASE FLOW IN TRANSPORT   11/97
      INCLUDE 'MOBFF.INC'
Cwch, 7/6/01
      INCLUDE 'TRANWQ.INC'
C=======================================================================
CIMT  CHANGE DIMENSIONS FROM 4 TO MQUAL  (NDUM, PNDUM, PUDUM)
      DIMENSION QI(NET),QO(NET),QO1(NET),QO2(NET),NDUM(MQUAL)
      CHARACTER PNONE*8,PNDUM(MQUAL)*8,PUDUM(MQUAL)*8,BLANK*8,KNEED*10,
     +          BMJ*10,KSURF(NTOA)*8
      EQUIVALENCE (QO(1),Q(1,2,2)),(QI(1),Q(1,1,2))
      EQUIVALENCE (QO1(1),QMAX(1)),(QO2(1),QFULL(1))
      DATA BLANK/'        '/
      DATA PNONE/'  NONE  '/
C=======================================================================
C     Initialization.
C=======================================================================
      INCNT  = INCNT  + 1
      IOUTCT = IOUTCT + 1
      LAST   = JIN(INCNT)
      NEXT   = JOUT(IOUTCT)
      SOURCE = 'TRANSPORT BLOCK'
      KPASS  = 0
      KDAY   = 1
C#### WCH (JLM), 10/93
      ISUMRY = 0
      IF(NOQ.EQ.0) THEN
                   WRITE(N6,10)
                   WRITE(*,10)
                   ELSE
                   WRITE(N6,11)
                   WRITE(*,11)
                   ENDIF
C=======================================================================
C     Open all scratch files for the Transport Block.
C=======================================================================
      IF(JIN(INCNT).GT.0.AND.(FFNAME(INCNT).EQ.'JOT.UF'.OR.
     +      FFNAME(INCNT).EQ.'JIN.UF'))
     +      OPEN(JIN(INCNT),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JIN(INCNT).GT.0.AND.FFNAME(INCNT).NE.'JOT.UF'.AND.
     +      FFNAME(INCNT).NE.'JIN.UF')
     +      OPEN(JIN(INCNT),FILE=FFNAME(INCNT),FORM='UNFORMATTED',
     +      STATUS='UNKNOWN')
      IF(JOUT(IOUTCT).GT.0.AND.(FFNAME(25+IOUTCT).EQ.'JOT.UF'.OR.
     +      FFNAME(25+IOUTCT).EQ.'JIN.UF'))
     +      OPEN(JOUT(IOUTCT),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JOUT(IOUTCT).GT.0.AND.FFNAME(25+IOUTCT).NE.'JOT.UF'.AND.
     +      FFNAME(25+IOUTCT).NE.'JIN.UF')
     +      OPEN(JOUT(IOUTCT),FILE=FFNAME(25+IOUTCT),FORM='UNFORMATTED',
     +      STATUS='UNKNOWN')
      IF(JKP(51).NE.2.AND.NSCRAT(1).GT.0.AND.FFNAME(51).NE.'SCRT1.UF') O
     +PEN(NSCRAT(1),FILE=FFNAME(51),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(52).NE.2.AND.NSCRAT(2).GT.0.AND.FFNAME(52).NE.'SCRT2.UF') O
     +PEN(NSCRAT(2),FILE=FFNAME(52),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(53).NE.2.AND.NSCRAT(3).GT.0.AND.FFNAME(53).NE.'SCRT3.UF') O
     +PEN(NSCRAT(3),FILE=FFNAME(53),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(54).NE.2.AND.NSCRAT(4).GT.0.AND.FFNAME(54).NE.'SCRT4.UF') O
     +PEN(NSCRAT(4),FILE=FFNAME(54),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(55).NE.2.AND.NSCRAT(5).GT.0.AND.FFNAME(55).NE.'SCRT5.UF') O
     +PEN(NSCRAT(5),FILE=FFNAME(55),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(56).NE.2.AND.NSCRAT(6).GT.0.AND.FFNAME(56).NE.'SCRT6.UF') O
     +PEN(NSCRAT(6),FILE=FFNAME(56),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(57).NE.2.AND.NSCRAT(7).GT.0.AND.FFNAME(57).NE.'SCRT7.UF') O
     +PEN(NSCRAT(7),FILE=FFNAME(57),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(51).NE.2.AND.NSCRAT(1).GT.0.AND.FFNAME(51).EQ.'SCRT1.UF')
     +             OPEN(NSCRAT(1),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JKP(52).NE.2.AND.NSCRAT(2).GT.0.AND.FFNAME(52).EQ.'SCRT2.UF')
     +             OPEN(NSCRAT(2),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JKP(53).NE.2.AND.NSCRAT(3).GT.0.AND.FFNAME(53).EQ.'SCRT3.UF')
     +             OPEN(NSCRAT(3),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JKP(54).NE.2.AND.NSCRAT(4).GT.0.AND.FFNAME(54).EQ.'SCRT4.UF')
     +             OPEN(NSCRAT(4),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JKP(55).NE.2.AND.NSCRAT(5).GT.0.AND.FFNAME(55).EQ.'SCRT5.UF')
     +             OPEN(NSCRAT(5),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JKP(56).NE.2.AND.NSCRAT(6).GT.0.AND.FFNAME(56).EQ.'SCRT6.UF')
     +             OPEN(NSCRAT(6),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JKP(57).NE.2.AND.NSCRAT(7).GT.0.AND.FFNAME(57).EQ.'SCRT7.UF')
     +             OPEN(NSCRAT(7),FORM='UNFORMATTED',STATUS='SCRATCH')
C#######################################################################
C     WCH, 10/93.  CAUTION!  NSCRAT(8) WILL BE OPENED IN SUBROUTINE
C       LINK IF WASP LINKAGE IS USED.
C=======================================================================
C     READ IN DATA FOR EACH SEWER SYSTEM
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP A1 <<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,TITLE(1)
      READ(N5,*,ERR=888) CC,TITLE(2)
      WRITE(N6,907)         TITLE(1),TITLE(2)
C=======================================================================
C>>>>>>>>>>>> READ OPTIONAL PARAMETERS ON DATA GROUP B0 <<<<<<<<<<<<
C=======================================================================
      ISLOPE = 0
      ITRAP  = 0
      IFLIP  = 0
      INFLEW = 0
      IDETAIL = 0
Cwch, 7/6/01
      NWQ    = 0
Cwch, 10/3/01  Store NOHEAD in HUGO.INC. 
      NOHEAD = 0
      READ(N5,*,ERR=888) CC
      IF(CC.EQ.'B0') THEN
                     BACKSPACE N5
CIM### 9/8/00 Read optional parameter IDETAIL
C  Note, data line has been padded with zeros in STRIP.  
                     READ(N5,*,ERR=888) CC,ISLOPE,ITRAP,IFLIP,INFLEW,
     a                                  IDETAIL,NOHEAD
                     IF(ISLOPE.LT.0) ISLOPE = 0
                     IF(ITRAP.LT.0)  ITRAP  = 0
                     IF(IFLIP.LT.0)  IFLIP  = 0
                     IF(INFLEW.LT.0) INFLEW = 0
CIM### CHECK IDETAIL
                     IF(IDETAIL.LT.0) IDETAIL = 0
                     IF(ISLOPE.GT.1) ISLOPE = 0
                     IF(ITRAP.GT.1)  ITRAP  = 0
                     IF(IFLIP.GT.1)  IFLIP  = 0
                     IF(INFLEW.GT.1) INFLEW = 0
CIM### CHECK IDETAIL
                     IF(IDETAIL.GT.1) IDETAIL = 0
                     ELSE
                     BACKSPACE N5
                     ENDIF
CIM ### Write IDETAIL
Cwch, 10/3/01  Write NOHEAD
      WRITE(N6,908)  ISLOPE,ITRAP,IFLIP,INFLEW,IDETAIL,NOHEAD
C=======================================================================
C>>>>>>>>>>>> READ EXECUTION DATA ON DATA GROUP B1 <<<<<<<<<<<<
C=======================================================================
Cwch, 7/6/01.  Add new parameter NWQ. Add zeroes in STRIP.
      READ(N5,*,ERR=888) CC,NDT,NINPUT,NNYN,NNPE,NOUTS,NPRINT,NPOLL,
     1                   NITER,JDATEZ,METRIC,INTPRT,NWQ
Cwch, 4/18/02. Provide option for using B1 starting date/time.
	               IISIGN  = ISIGN(1,JDATEZ)
	               JDATEZ  = IABS(JDATEZ)
                     NYEAR = JDATEZ/10000
                     IF (NYEAR.LT.100) THEN
                     JDATEZ = JDATEZ - NYEAR*10000
                     NYEAR = NYEAR + 1900
                     JDATEZ = JDATEZ + NYEAR*10000
                     ENDIF
C=======================================================================
C             NDT    = NUMBER OF TIME STEPS
C             NINPUT = NUMBER OF INPUT ELEMENTS
C             NNYN   = NUMBER OF ELEMENTS FOR WHICH INFLOW
C                      PRINTOUT IS DESIRED
C             NNPE   = NUMBER OF ELEMENTS FOR WHICH OUTFLOW
C                      PRINTOUT IS DESIRED
C             INTPRT = NUMBER OF TIME STEPS BETWEEN INFLOW AND
C                       OUTFLOW PRINTS.
C             NOUTS  = NUMBER OF ELEMENTS FOR WHICH OUTFLOW
C                      HYDROGRAPHS AND POLLUTOGRAPHS ARE TO BE
C                      PROVIDED ON TAPE FOR INTERFACING
C             NPRINT = 0 SOME INTERNAL ERROR MESSAGES SUPPRESSED
C                      1 PRINT ALL INTERNAL ERROR MESSAGES
C             NPOLL  = NUMBER OF POLLUTANTS TO BE ROUTED
C             NITER  = NUMBER OF ITERATIONS IN ROUTING SCHEME
C             JDATEZ = INITIAL DATE, YEAR-MO-DAY
C             METRIC = 0 USE U.S. CUSTOMARY UNITS FOR I/O
C                      1 USE METRIC UNITS FOR I/O
C                      NOTE: METRIC = METRIC+1 LATER IN CODE.
C             NWQ    = 0 NO SPECIAL DO-BOD-NOD SIMULATION.
C                      1 SPECIAL LINKED DO SIMULATION WITH SPECIFIED
C                        FIRST FOUR CONSTITUENTS. 
CIM ### 9/8/00 changes for writing depth and velocity
CIMhv    npoll .lt.0 the write depth and velocity in addition to
cimhv    -npoll pollutants
cimhv    npool .eq. -999 then write only depth and velocity
cimhv
      IF (NPOLL.lt.0) then
           NPOLL = - NPOLL
           IF (NPOLL.EQ.999) NPOLL = 0
           NPOLL2 = NPOLL + 2
           ELSE
           NPOLL2 = NPOLL
           ENDIF
CIMHV
CIMT     NPOLL2 must be less than or equal to MQUAL
      IF (NPOLL2.GT.MQUAL) CALL ERROR(201)
CIM### 9/8/00
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP B2 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,DT,EPSIL,DWDAYS,TZERO,GNU,TRIBA
C=======================================================================
C
C             DT     = LENGTH OF TIME STEP IN SECONDS
C             EPSIL  = CONVERGENCE CRITERION IN ROUTING
C             DWDAYS = NUMBER OF DRY WEATHER DAYS PRIOR TO STORM
C             TZERO  = STARTING TIME OF DAY OF STORM, DECIMAL HOUR
C             GNU    = KINEMATIC VISCOCITY OF WATER.
C             TRIBA  = CATCHMENT AREA
C=======================================================================
C     ASSIGN DEFAULT VALUES
C=======================================================================
      IF(NITER.LE.0)    NITER = 4
      IF(EPSIL.LE.0.0)  EPSIL = 0.0001
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP B3 <<<<<<<<<<<<
C=======================================================================
C
      READ (N5,*,ERR=888) CC,NCNTRL,NINFIL,NFILTH,NDESN
C=======================================================================
C             NCNTRL = 0  INTERFACE FILE ACCESSED (CARD INPUT OPTIONAL)
C                      1 INPUT FROM CARDS ONLY
C             NINFIL = 1 IF INFILTRATION ROUTINE IS TO BE CALLED
C                      0 IF INFITRATION ROUTINE IS NOT TO BE CALLED
C             NFILTH = 1 IF DRY WEATHER FLOW ROUTINE IS TO BE CALLED
C                      0 IF DRY WEATHER FLOW ROUTINE IS NOT TO BE CALLED
C             NDESN  = 1 IF DESIGN ROUTINE IS TO BE CALLED
C                      0 IF DESIGN ROUTINE NOT CALLED
C=======================================================================
C     PRINT INPUT VARIABLES
C=======================================================================
      WRITE(N6,950) NDT,NINPUT,NNYN,NNPE,INTPRT,NOUTS,NPOLL,NITER
Cwch, 7/6/01. 
      IF(NWQ.EQ.0) WRITE(N6,955) 
	IF(NWQ.EQ.1) WRITE(N6,956)
cim  check that array limits aren't exceeded
      IF (NNPE.GT.NTOA) THEN
           WRITE(N6,7001) NNPE, NTOA
           STOP 'NNPE EXCEEDS NTOA. STOP'
           ENDIF
      IF (NNYN.GT.NTOA) THEN
           WRITE(N6,7002) NNYN,NTOA
           STOP 'NNYN EXCEEDS NTOA. STOP'
           ENDIF
      IF (NOUTS.GT.NTHO) THEN
           WRITE(N6,7003) NOUTS,NTHO
           STOP 'NOUTS EXCEEDS NTHO. STOP'
           ENDIF
      IF((NOUTS.GT.0.AND.NEXT.LE.0).OR.(NOUTS.LE.0.AND.NEXT.GT.0))
     1                    WRITE (N6,3005) NOUTS,NEXT
      WRITE (N6,951) NPRINT,NCNTRL,NINFIL,NFILTH,NDESN
      IF(METRIC.EQ.0) WRITE (N6,3010)
      IF(METRIC.EQ.1) WRITE (N6,3015)
C=======================================================================
C     ESTABLISH VARIOUS METRIC CONVERSION FACTORS.
C     MULTIPLY FIRST UNITS TO OBTAIN SECOND UNITS.
C=======================================================================
      METRIC = METRIC + 1
      IF(METRIC.EQ.1) THEN
                      CMET1 = 1.0
                      CMET2 = 1.0
                      CMET3 = 1.0
                      CMET4 = 1.0
C#### WCH, 6/5/95.  DON'T USE SO ELIMINATE SO DON'T ANNOY LAHEY.
C####                      CMET5 = 1.0
                      ELSE
C             M TO FT.
                      CMET1 = 3.2808
C             SQ M TO SQ FT.
                      CMET2 = 10.764
C             CU M TO CU FT.
                      CMET3 = 35.314667
C             SQ CM TO SQ FT.
                      CMET4 = 0.001076
C             HA TO AC.
C#### WCH, 6/5/95.  DON'T USE SO ELIMINATE SO DON'T ANNOY LAHEY.
C####                      CMET5 = 2.471
                      ENDIF
      IF(GNU.LE.0.0.AND.METRIC.EQ.1) GNU = 0.00001
      IF(GNU.LE.0.0.AND.METRIC.EQ.2) GNU = 0.01
      IF(METRIC.EQ.1) WRITE (N6,952) DT,EPSIL,DWDAYS,TZERO,GNU,TRIBA
      IF(METRIC.EQ.2) WRITE (N6,953) DT,EPSIL,DWDAYS,TZERO,GNU,TRIBA
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP B4 <<<<<<<<<<<<
C    CIM   6 94  REVISED 2/96
C=======================================================================
C  INITIALIZE STUFF
      NUMSETS = 0
      MONTHOLD = 0
      DO 947 I=1,MAXSETS
      NUMBFF(I) = 0
      INBFF(I) = 0
      IBFF(I) = 0
      DO 947 J=1,MAXBFF
  947 BFFMO(I,J) = 1.0
  948 READ (N5,*,ERR=888) CC
      IF (CC.EQ.'B4') THEN
           NUMSETS = NUMSETS+1
           IF (NUMSETS.GT.MAXSETS) THEN
         WRITE(N6,*) ' NUMBER OF BASE FLOWS SETS SPECIFIED ON B4 LINES',
     .'EXCEEDS MAXIMUM NUMBER ALLOWED = ',MAXSETS
              STOP 'TOO MANY B4 LINES'
              ENDIF
           BACKSPACE N5
           READ (N5,*,ERR=888) CC, IBFF(NUMSETS), NUMBFF(NUMSETS)
           IF (NUMBFF(NUMSETS).GT.MAXBFF) THEN
              WRITE(N6,*) ' NUMBER OF BASE FLOWS SPECIFIED ON B4 LINE',
     .'EXCEEDS MAXIMUM NUMBER ALLOWED = ',MAXBFF,' FOR SET ',
     .'NUMBER ',NUMSETS
              STOP 'TOO MANY BASE BASE FLOW VALUES SPECIFED ON B4 LINE'
              ENDIF
           BACKSPACE N5
           READ (N5,*,ERR=888) CC,IBFF(NUMSETS),NUMBFF(NUMSETS),
     .              (BFFMO(NUMSETS,I),I=1,NUMBFF(NUMSETS))
           WRITE(N6,*)
           WRITE(N6,*) ' MONTHLY BASE FLOW FACTOR SET NUMBER ',NUMSETS
           SELECT CASE (IBFF(NUMSETS))
                CASE (1)
                WRITE(N6,5000)
                CASE (2)
                WRITE(N6,5001)
                CASE (3)
                WRITE(N6,5002)
                CASE DEFAULT
                WRITE(N6,*) '   IBFF ENTERED ON B4 LINE IS NOT VALID'
                STOP        '   IBFF ENTERED ON B4 LINE IS NOT VALID'
                END SELECT
           WRITE(N6,949) (BFFMO(NUMSETS,I),I=1,NUMBFF(NUMSETS))
C GO BACK TO TRY TO READ NEXT B4 LINE
           GOTO 948
          ELSE
C NO B4 LINES READ, THEN BACKSPACE AND CONTININUE
           BACKSPACE N5
           END IF
      IF (NUMSETS.LE.0) NUMSETS = 1
C=======================================================================
C      MONTHLY BASE FLOW FACTORS,  FLOWS INPUT ON E1 CARDS FOR TYPE 19
C      ARE MULTIPLIED BY THESE FACTORS
C=======================================================================
C
CIM&
C=======================================================================
C     READ DATA DESCRIBING USER SUPPLIED CONDUITS, IF ANY.
C=======================================================================
C     READ DATA GROUP C1
C=======================================================================
      READ (N5,*,ERR=888) CC,NKLASS,KPRINT
      NKLAS  = NKLASS + 16
      IF(NKLASS.GT.2) CALL ERROR(118)
      IF(NKLASS.LE.0)  GO TO 41
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP D1 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,(NAME(I),I=17,NKLAS)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP D2 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,(NN(I),I=17,NKLAS)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP D3 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,(MM(I),I=17,NKLAS)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP D4 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,(ALFMAX(I),I=17,NKLAS)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP D5 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,(PSIMAX(I),I=17,NKLAS)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP D6 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,(AFACT(I),I=17,NKLAS)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP D7 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,(RFACT(I),I=17,NKLAS)
C=======================================================================
C     READ PARAMETERS FOR DEPTH-AREA RELATIONSHIPS.
C=======================================================================
      DO 18 I   = 17,NKLAS
      KDEPTH(I) = 2
      NNN       = NN(I)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP D8 <<<<<<<<<<<<
C=======================================================================
      DO 19 K = 1,NNN,8
      NN1     = K + 7
      IF(NN1.GT.NNN) NN1 = NNN
      READ(N5,*,ERR=888) CC,(DNORM(I,J),J=K,NN1)
   19 CONTINUE
      NNN = NNN+1
      IF(NNN.LE.50) THEN
                    DO 17 J    = NNN,51
   17               DNORM(I,J) = 0.0
                    ENDIF
   18 CONTINUE
C=======================================================================
C     READ PARAMETERS AND PERFORM INITIALIZING CALCULATIONS ON Q-A CURVE
C=======================================================================
      DO 40 I  = 17,NKLAS
      KLASS(I) = 2
C=======================================================================
C     CONDUIT WITH TABULAR Q-A RELATIONSHIP
C=======================================================================
      MMM = MM(I)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP D9 <<<<<<<<<<<<
C=======================================================================
      DO 21 K = 1,MMM,8
      MM1     = K + 7
      IF(MM1.GT.MMM) MM1 = MMM
      READ(N5,*,ERR=888) CC,(QNORM(I,J),J=K,MM1)
   21 CONTINUE
      DALPHA     = 1.0/FLOAT(MMM-1)
      ANORM(1,1) = 0.0
      DO 33 J    = 2,MMM
   33 ANORM(I,J) = ANORM(I,J-1)+DALPHA
      MMM        = MMM+1
      IF (MMM.LE.50) THEN
                     DO 39 J = MMM,51
                     ANORM(I,J) = 0.0
   39                QNORM(I,J)=0.0
                     ENDIF
   40 CONTINUE
   41 CONTINUE
C=======================================================================
C     WRITE DATA DESCRIBING DIFFERENT TYPES OF SEWER ELEMENTS(IF DESIRED)
C=======================================================================
      IF(KPRINT.EQ.1) THEN
                WRITE(N6,970)
                DO 43 I = 1,18
                MMM     = MM(I)
                IF(MM(I).LT.NN(I)) MMM = NN(I)
   43           WRITE(N6,971) I,NAME(I),ALFMAX(I),PSIMAX(I),AFACT(I),
     1                        RFACT(I),KDEPTH(I),KLASS(I),
     2                 (J,ANORM(I,J),QNORM(I,J),DNORM(I,J),J=1,MMM)
                WRITE(N6,972)
                WRITE(N6,973)  (I,KDEPTH(I),KLASS(I),NAME(I),I=19,25)
                ENDIF
C=======================================================================
C     SET DEFAULT POLLUTANT NAMES AND UNITS.
C=======================================================================
      GNU        = GNU*CMET4
CIMT  CHANGE UPPER RANGE FROM 4 TO MQUAL
      DO 3090 K  = 1,MQUAL
      CPINF(K)   = 0.0
      PNAME(K) = PNONE
 3090 PUNIT(K) = BLANK
C=======================================================================
C     BYPASS TAPE OPERATIONS IF DESIRED
C=======================================================================
      IF(NCNTRL.GT.0.AND.LAST.GT.0) WRITE(N6,5080)
	TZEROX = TZERO
      IF(NCNTRL.GT.0) GO TO 57
      IF(LAST.LE.0) WRITE (N6,3095) LAST
C=======================================================================
C     INTERFACING MECHANISM FOR QUANTITY AND QUALITY OF RUNOFF
C     READ HEADING INFORMATION ON INPUT FILE.
C=======================================================================
      KPASS = 1
      CALL INFACE(1,LAST)
C=======================================================================
Cwch, 4/18/02.  Add error check for incompatible JCE.
C	Allow program to continue but warn of impending error. 
C=======================================================================
      IF(NJCE.NE.JCE) THEN
		WRITE(N6,4005) 
	    WRITE(*,4005)
	    ENDIF
C=======================================================================
C     TEST FOR ADEQUATE ARRAY SPACE FOR FILE INPUT.
C=======================================================================
      IF(LOCATS.GT.NTHO) THEN
                        WRITE (N6,4007) locats
                        STOP 'TOO MANY INPUT LOCATIONS'
                        ENDIF
C=======================================================================
Cwch, 4/18/02. Option for insisting upon IDATEZ/TZERO from B1,B2 lines.
C=======================================================================
  57  IF(KPASS.EQ.0.OR.IISIGN.LT.0) THEN
                     NDAY  = JDATEZ - NYEAR*10000
                     MONTH = NDAY/100
                     NDAY  = NDAY - MONTH*100
                     IF(NDAY.LE.0) NDAY   = 2
                     IF(MONTH.LE.0) MONTH = 8
                     IF(NYEAR.LE.1900) NYEAR = 1941
CIM NOTE TO MYSELF. FOR SOME REASON I REPLACED ABOVE WITH
CIM THE FOLLOWING CODE
C	IF(NDAY+MONTH+NYEAR.EQ.0) THEN
C	    NDAY = 2
C	    MONTH = 8
C	    NYEAR = 1941
C	ENDIF
                     JULDAY  = 1000*NYEAR + JDATE(NDAY,MONTH,NYEAR)
                     IDATEZ  = JULDAY
Cwch, 4/18/02. Use saved TZEROX
                     TZERO   = TZEROX*3600.0
                     ELSE
C=======================================================================
C     Here, use value of IDATEZ and TZERO from interface file.
C=======================================================================
                     JULDAY  = IDATEZ
                     ENDIF
      TIMDAY = TZERO
      CALL DATED
Cwch, 4/18/02.
	IF(KPASS.EQ.1.AND.IISIGN.LT.0) WRITE(N6,965)
      WRITE(N6,966) NYEAR,MONTH,NDAY,JHR,MINUTE,JSEC
C=======================================================================
C     Read in element data and initialize variables.
C=======================================================================
      I = 1
      KSTOR = 0
C#### WCH (CDM), 8/93.
      KINOU = 0
      NSTOR = NTSE
C=======================================================================
C     SET INITIAL DEFAULT VALUES AND RATIOS.
C     USER MUST SUPPLY ANY DEFAULT VALUES FOR PHYSICAL PARAMETERS.
C=======================================================================
      K         = 0
      NTD       = 0
      DO 4010 I = 1,6
      DEF(I)    = 0.0
 4010 RATIO(I)  = 1.0
      JSTOP     = 0
      WRITE(*,5050)
      DO 4070 I = 1,2*NET
      K         = K + 1
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP E1 <<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.EQ.'E1') THEN
                     IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,NOE(K),
     +                      (NUE(K,J),J=1,3),NTPE,(VAR(J),J=1,5),
     +                                           BARREL(K),VAR(6)
                     IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,KOE(K),
     +                      (KUE(K,J),J=1,3),NTPE,(VAR(J),J=1,5),
     +                                       BARREL(K),VAR(6),KGEOM(K)
Cwch, 4/15/02. Try adding error message re. KGEOM
                     IF(KGEOM(K).EQ.'E1') THEN
                          WRITE(*,94010)
                          WRITE(N6,94010)
                          GOTO 888
	                    ENDIF
                     ELSE
                     GO TO 4080
                     ENDIF
CIMT=======================================================================
C>>>>>>>>>>>> READ DATA GROUP E1b <<<<<<<<<<<<
CIMT   Constant constituent inflow concentrations of manhole inflows
CIMT   REQUIRED ONLY IF FLOW IS NOT ZERO.
CIMT=======================================================================
         IF ((NTPE.EQ.19).AND.(NPOLL.GT.0).AND.(VAR(1).NE.0.0)) THEN
Cwch, 8/29/00.  Add error check for omitted extra lines.
            READ (N5,*,ERR=888) CC
		  IF(CC.EQ.'E1') THEN
		       WRITE (N6,4085) K
	           STOP ' ERROR! RUN STOPPED WHILE READING E1 LINES. SEE
     1OUTPUT FILE.'
	           ELSE
	           BACKSPACE N5
			   READ(N5,*,ERR=888) (PMANN(K,J),J=1,NPOLL)
	           ENDIF
            ELSE
            DO 4012 J=1,NPOLL
 4012       PMANN(K,J)=0.0
            END IF
CIMQP=======================================================================
C>>>>>>>>>>>> READ DATA GROUP E1b <<<<<<<<<<<<
CIMQP   TYPE 27 CONSTITUENT DELIVERY RATIO
CIMQP   REQUIRED IF NTPE IS 27 AND NPOLL > 0.
CIMQP=======================================================================
      IF ((NTPE.EQ.27).AND.(NPOLL.GT.0)) THEN
	READ(N5,*,ERR=888) (DRATIO(K,J),J=1,NPOLL)
	ELSE
	DO J=1,NPOLL
	DRATIO(K,J) = 1.0
	ENDDO
	ENDIF
C=======================================================================
C     SET NEW DEFAULT VALUES(-2) AND NEW RATIOS(-1)
C=======================================================================
      IF(JCE.EQ.0)       THEN
      IF(NOE(K).EQ.-2)   THEN
                         IF(NTPE.GT.0) NTD = NTPE
                         DO 4015 L = 1,6
                         IF(VAR(L).GT.0.0) DEF(L) = VAR(L)
 4015                    CONTINUE
                         K = K-1
                         GO TO 4070
                         ENDIF
      IF(NOE(K).EQ.-1)   THEN
                         DO 4025 L = 1,6
                         IF(VAR(L).GT.0.0) RATIO(L) = VAR(L)
 4025                    CONTINUE
                         K = K-1
                         GO TO 4070
                         ENDIF
                         ENDIF
      IF(JCE.EQ.1)       THEN
      IF(KOE(K).EQ.'-2') THEN
                         IF(NTPE.GT.0) NTD = NTPE
                         DO 8015 L = 1,6
                         IF(VAR(L).GT.0.0) DEF(L) = VAR(L)
 8015                    CONTINUE
                         K = K-1
                         GO TO 4070
                         ENDIF
      IF(KOE(K).EQ.'-1') THEN
                         DO 8025 L = 1,6
                         IF(VAR(L).GT.0.0) RATIO(L) = VAR(L)
 8025                    CONTINUE
                         K = K-1
                         GO TO 4070
                         ENDIF
                         ENDIF
C=======================================================================
C     ASSIGN DEFAULT VALUES AND MULTIPLY BY RATIOS.
C=======================================================================
      IF(NTPE.EQ.0) NTPE     = NTD
      IF(NTPE.LT.0) THEN
                     NTPE     = IABS(NTPE)
                     NODSGN(K) = 1
                     ENDIF
CIM CHECK FOR VALID PIPE TYPE
      IF(NTPE.LE.0.OR.NTPE.GT.27) WRITE(N6,7011) NOE(K),NTPE
      NTYPE(K) = NTPE
      IF(NTYPE(K).GT.16.AND.BARREL(K).LT.1.0) BARREL(K) = 1.0
      IF(NTYPE(K).LT.14.AND.BARREL(K).LE.0.0) BARREL(K) = 1.0
      SCF(K)   = GNO
      IF(VAR(1).LT.0.0) THEN
                        VAR(1)  = ABS(VAR(1))
                        SCF(K)  = YES
                        ENDIF
      DO 4045 L = 1,6
      IF(VAR(L).LE.0.0) VAR(L) = DEF(L)
                        VAR(L) = VAR(L)*RATIO(L)
 4045 CONTINUE
C=======================================================================
C     CONVERT TO FT-SEC UNITS. CONVERT BACK LATER FOR PRINTING ONLY.
C=======================================================================
CIM   NT    = NTYPE(K)
      CMETT = 1.0
Cwch 8/10/00 Conversions are screwy.  Try to fix.
Cwch 8/10/00      IF(METRIC.EQ.2.AND.NTYPE(K).LE.18) CMETT = CMET1
C#### WCH (RED), 9/23/93.
Cwch 8/10/00      IF(METRIC.EQ.2.AND.NTYPE(K).EQ.23) CMETT = CMET3
      IF(METRIC.EQ.2) CMETT = CMET1
      IF(NTYPE(K).LE.18) THEN
           DIST(K)  = VAR(1)*CMETT
           GEOM1(K) = VAR(2)*CMETT
           SLOPE(K) = VAR(3)
           ROUGH(K) = VAR(4)
           GEOM2(K) = VAR(5)*CMETT
           IF(NTYPE(K).NE.13) GEOM3(K) = VAR(6)*CMETT
           IF(NTYPE(K).EQ.13.AND.ITRAP.EQ.0) GEOM3(K) = VAR(6)
           IF(NTYPE(K).EQ.13.AND.ITRAP.EQ.1) GEOM3(K) = 1.0/VAR(6)
	     ENDIF
      IF(NTYPE(K).GE.19) GEOM3(K) = VAR(6)
      IF(NTYPE(K).EQ.19.OR.NTYPE(K).EQ.20.OR.NTYPE(K).EQ.23)
     1                             DIST(K) = VAR(1)*CMET3
      IF(NTYPE(K).EQ.20.OR.NTYPE(K).EQ.21)
     1                             GEOM1(K) = VAR(2)*CMET3
C*** Type 23, Weir
Cwch 8/10/00
      IF(NTYPE(K).EQ.23) THEN
                   GEOM1(K) = VAR(2)*CMET1
                   GEOM2(K) = VAR(5)*CMET1
                   SLOPE(K) = VAR(3)*CMET3
Cwch                   ROUGH(K) = ROUGH(K)*CMET1
Cwch 8/10/00 Weir coef. x weir length has units of length^1.5/time
                   ROUGH(K) = VAR(4)*CMET1**1.5
                   ENDIF
C*** Type 22, Storage Unit
      KSTORE(K) = 0
      IF(NTYPE(K).EQ.22) THEN
                         KSTOR     = KSTOR + 1
                         KSTORE(K) = KSTOR
                         ENDIF
C#### WCH (CDM), 8/93.  NEW VARIABLES FOR TYPE 26 FLOW DIVIDER.
      KINOUT(K) = 0
      IF(NTYPE(K).EQ.26) THEN
                         KINOU      = KINOU + 1
                         KINOUT(K) = KINOU
                         ENDIF
      QFULL(K) = 0.0
      QMAX(K)  = 0.0
      AFULL(K) = 0.0
      IF(NTYPE(K).LE.18.AND.SLOPE(K).LE.0.0) THEN
                        JSTOP = 1
                        IF(JCE.EQ.0) WRITE(N6,9500) NOE(K)
                        IF(JCE.EQ.1) WRITE(N6,9501) KOE(K)
                        ENDIF
      IF(NTYPE(K).LE.15.AND.ROUGH(K).LE.0.0) THEN
                        JSTOP = 1
                        IF(JCE.EQ.0) WRITE(N6,9510) NOE(K)
                        IF(JCE.EQ.1) WRITE(N6,9511) KOE(K)
                        ENDIF
      IF(JCE.EQ.0.AND.NTYPE(K).EQ.21.AND.GEOM3(K).LE.0.0) THEN
                        JSTOP = 1
                        WRITE(N6,9520) NOE(K)
                        ENDIF
      IF(JCE.EQ.1.AND.NTYPE(K).EQ.21.AND.KGEOM(K).EQ.' ') THEN
                        JSTOP = 1
                        WRITE(N6,9521) KOE(K)
                        ENDIF
      IF(JCE.EQ.0.AND.NTYPE(K).EQ.23.AND.GEOM3(K).LE.0.0) THEN
                        JSTOP = 1
                        WRITE(N6,9520) NOE(K)
                        ENDIF
      IF(JCE.EQ.1.AND.NTYPE(K).EQ.23.AND.KGEOM(K).EQ.' ') THEN
                        JSTOP = 1
                        WRITE(N6,9521) KOE(K)
                        ENDIF
      IF(JCE.EQ.0.AND.NTYPE(K).EQ.24.AND.GEOM3(K).LE.0.0) THEN
                        JSTOP = 1
                        WRITE(N6,9520) NOE(K)
                        ENDIF
      IF(JCE.EQ.1.AND.NTYPE(K).EQ.24.AND.KGEOM(K).EQ.' ') THEN
                        JSTOP = 1
                        WRITE(N6,9521) KOE(K)
                        ENDIF
      IF(JCE.EQ.0.AND.NTYPE(K).EQ.25.AND.GEOM3(K).LE.0.0) THEN
                        JSTOP = 1
                        WRITE(N6,9520) NOE(K)
                        ENDIF
      IF(JCE.EQ.1.AND.NTYPE(K).EQ.25.AND.KGEOM(K).EQ.' ') THEN
                        JSTOP = 1
                        WRITE(N6,9521) KOE(K)
                        ENDIF
      IF(JCE.EQ.0.AND.NTYPE(K).EQ.26.AND.GEOM3(K).LE.0.0) THEN
                        JSTOP = 1
                        WRITE(N6,9520) NOE(K)
                        ENDIF
      IF(JCE.EQ.1.AND.NTYPE(K).EQ.26.AND.KGEOM(K).EQ.' ') THEN
                        JSTOP = 1
                        WRITE(N6,9521) KOE(K)
                        ENDIF
C=======================================================================
C     CHECK FOR VALID ELEMENT NUMBER.
C=======================================================================
      IF(K.NE.1) THEN
                 KM1        = K-1
                 DO 4065 KL = 1,KM1
                 IF(JCE.EQ.0.AND.NOE(K).NE.NOE(KL)) GO TO 4065
                 IF(JCE.EQ.1.AND.KOE(K).NE.KOE(KL)) GO TO 4065
                 IF(JCE.EQ.0) WRITE(N6,4064) NOE(K),K,NOE(KL),KL
                 IF(JCE.EQ.1) WRITE(N6,8064) KOE(K),K,KOE(KL),KL
                 JSTOP = 1
                 GO TO 4066
 4065            CONTINUE
                 ENDIF
 4066 IF(JCE.EQ.0) THEN
                   NNEED     = NOE(K)
                   NINNUM(K) = NNEED
                   ELSE
                   KNEED     = KOE(K)
                   KINNUM(K) = KNEED
                   ENDIF
 4070 CONTINUE
 4080 NE = K - 1
      WRITE (N6,967) NE
C#### WCH, 8/15/96.  ADD MESSAGE TO STOPS.
      IF(JSTOP.GT.0) THEN
                     WRITE(N6,4082)
                     STOP 'Error.  Program stopped from Sub. INTRAN.'
                     ENDIF
      IF(NE.GT.NET)  THEN
                     WRITE(N6,968) NE
                     STOP 'Error.  Program stopped from Sub. INTRAN.'
                     ENDIF
C#### WCH, 8/15/96.  ADD ERROR MESSAGE FOR ZERO ELEMENTS READ IN.
      IF(NE.EQ.0)    THEN
                     WRITE(N6,4083)
                     STOP 'Error.  Program stopped from Sub. INTRAN.'
                     ENDIF
C=======================================================================
C     NOW GET DATA FOR IRREGULAR CHANNELS, IF ANY.
C=======================================================================
      NATUR    = 0
      DO 285 N = 1,NE
      IF(NTYPE(N).EQ.16) THEN
            NATUR = NATUR + 1
            KCOND = 0
            IF(ISLOPE.EQ.0) SLAP = SLOPE(N)*0.01
            IF(ISLOPE.EQ.1) SLAP = SLOPE(N)
            KSTOP = 0
            IF(BARREL(N).LT.0.0) THEN
                                 KSTOP     = 1
                                 BARREL(N) = ABS(BARREL(N))
                                 ENDIF
CC$$$$5/3/92
            CALL GETCUR(N,BARREL(N),SLAP,METRIC,1,KCOND,AFULL(N),
     +                  GEOM1(N),GEOM2(N),DIST(N),ROUGH(N),
     +                  RFULL,JSTOP,NOE(N),KOE(N),KSTOP)
C=======================================================================
C     Data for natural channels are still in metric units if METRIC = 2.
C     Hence, must convert to U.S. customary units for internal use
C     in Transport.  Note, DIST(N) already in U.S. units.  Top width is
C     QCURVE(NATUR,3,26).
C=======================================================================
Cwch, 3/14/02.
            IF(METRIC.EQ.2) THEN
	           AFULL(N) = AFULL(N)*CMET(13,METRIC)
	           GEOM1(N) = GEOM1(N)*CMET(1,METRIC)
	           RFULL    = RFULL*CMET(1,METRIC)
	           QCURVE(NATUR,3,26) = GEOM2(N)*CMET(1,METRIC)
	           ENDIF
            QCURVE(NATUR,1,26) = RFULL
            BARREL(N)          = 1.0
            ENDIF
C=======================================================================
C     NOW PROCESS DATA FOR POWER-FUNCTION CHANNELS, IF ANY.
C     Note that no metric conversions are necessary since metric
C     conversions were already made for power-function channels. 
C=======================================================================
      IF(NTYPE(N).EQ.15.OR.NTYPE(N).EQ.14) THEN
            KCOND = 1
            NATUR = NATUR + 1
            IF(ISLOPE.EQ.0) SLAP = SLOPE(N)*0.01
            IF(ISLOPE.EQ.1) SLAP = SLOPE(N)
            KSTOP = 0
            IF(BARREL(N).LT.0.0) THEN
                                 KSTOP     = 1
                                 BARREL(N) = ABS(BARREL(N))
                                 ENDIF
            IF(NTYPE(N).EQ.14) GEOM3(N) = 2.0
CC$$$$5/3/92
            CALL GETCUR(N,GEOM3(N),SLAP,METRIC,1,KCOND,AFULL(N),
     +                  GEOM1(N),GEOM2(N),DIST(N),ROUGH(N),
     +                  RFULL,JSTOP,NOE(N),KOE(N),KSTOP)
            QCURVE(NATUR,1,26) = RFULL
Cwch, 7/6/01. Need top width for new quality options. 
	      QCURVE(NATUR,3,26) = GEOM2(N)
            ENDIF
  285 CONTINUE
C#######################################################################
C RED, 6/2/93. FIX TO READ REVISED NATURAL CHANNEL LINES IN GETCUR.
C     Find special conduit data to read from groups F1, G1 or H1.
C     The following sequence is required if natural channel information
C       was entered in the data input stream.
C#######################################################################
      DO 9700 J = 1,10000
      READ(N5,*,ERR=888) CC
C#### WCH (CDM), 8/93.  CHECK FOR G6 LINE ALSO.
C#### WCH, 5/10/94.  ADD ALL OTHER POSSIBLE IDs: H2,I2,J2,K1,L1.
Cwch, 7/6/01.  ADD F2,F3,F4,F5
      IF(CC.EQ.'F1'.OR.CC.EQ.'F2'.OR.CC.EQ.'F3'.OR.CC.EQ.'F4'.OR.
     +   CC.EQ.'F5'.OR.CC.EQ.'G1'.OR.CC.EQ.'H1'.OR.
     +   CC.EQ.'I1'.OR.CC.EQ.'J1'.OR.CC.EQ.'$'.OR.CC.EQ.'G6'.OR.
     +   CC.EQ.'H2'.OR.CC.EQ.'I2'.OR.CC.EQ.'J2'.OR.
     +   CC.EQ.'K1'.OR.CC.EQ.'L1'             ) THEN
                                                BACKSPACE N5
                                                GO TO 9750
                                                ENDIF
 9700 CONTINUE
 9750 CONTINUE
C#######################################################################
Cwch, 7/6/01 Shift quality input code to new Sub. INTRQUAL.
C     Do this in order to input additional parameters for DO simulation.
C     Also, input quality parameters *after* processing and printout
C     of element information.
C     LOOK FOR OLD CODE IN SUB. INTRQUAL.
C=======================================================================
C=======================================================================
C     Sequence element data in model.
C=======================================================================
      IF(NPOLL.GT.0) WRITE (N6,915)
      CALL SLOP
C=======================================================================
C     Calculate constants and flow parameters for each element.
C=======================================================================
      CALL FIRST(MI,0)
C=======================================================================
C     Write description of run and sewer system.
C=======================================================================
      KARIGE = 1
	WRITE(N6,914) KARIGE
Cwch, 7/6/01. OUTPUT POLLUTANT INFO IN NEW SUB. INTRQUAL.
CC	IF (NPOLL.EQ.0) THEN
      IF(METRIC.EQ.1) WRITE(N6,916)
      IF(METRIC.EQ.2) WRITE(N6,919)
	WRITE(N6,913)
C	       ELSE
C             IF(METRIC.EQ.1) WRITE(N6,1916) (PNAME(K),K=1,NPOLL)
C             IF(METRIC.EQ.2) WRITE(N6,1919) (PNAME(K),K=1,NPOLL)
C	       WRITE(N6,1913) (' ---------',K=1,NPOLL)
C	       ENDIF
      DO 4250 I = 1,NE
CIM   NT        = NTYPE(I)
      SSL       = SLOPE(I)
      RGH       = ROUGH(I)
Cwch, 8/29/00.  Initialize VAR(J) before each printout.
      DO J = 1,6
	  VAR(J) = 0.0
	END DO
C
      IF(NTYPE(I).LE.18) THEN
          VAR(1) = DIST(I)/CMET1
          VAR(2) = GEOM1(I)/CMET1
          VAR(3) = GEOM2(I)/CMET1
          IF(NTYPE(I).NE.13) VAR(4) = GEOM3(I)/CMET1
          IF(NTYPE(I).EQ.13.AND.ITRAP.EQ.0) VAR(4) = GEOM3(I)
          IF(NTYPE(I).EQ.13.AND.ITRAP.EQ.1) VAR(4) = 1.0/GEOM3(I)
         ELSE
          CMETT = 1.0
Cwch 8/10/00 Fix conversions for print-out.
          IF(NTYPE(I).EQ.19.OR.NTYPE(I).EQ.20.OR.NTYPE(I).EQ.23)
     1         VAR(1) = DIST(I)/CMET3
Cwch     1                            CMETT = CMET3
          VAR(4) = GEOM3(I)
Cwch          CMETT = 1.0
Cwch          IF(NTYPE(I).EQ.20.OR.NTYPE(I).EQ.21) CMETT = CMET3
Cwch          VAR(2) = GEOM1(I)/CMETT
          IF(NTYPE(I).NE.23) THEN
                      VAR(3) = GEOM2(I)
                    ELSE
                      VAR(2) = GEOM1(I)/CMET1
                      SSL    = SLOPE(I)/CMET3
Cwch
					RGH    = ROUGH(I)/CMET1**1.5
                      VAR(3) = GEOM2(I)/CMET1
                    ENDIF
          ENDIF
      VAR(5) = AFULL(I)/CMET2
      VAR(6) = QFULL(I)/CMET3
      QMM    = QMAX(I)/CMET3
      IF(NTYPE(I).LT.19.AND.JCE.EQ.0)
     1                   WRITE(N6,920) NOE(I),NTYPE(I),NAME(NTYPE(I)),
     +                   SSL,VAR(1),RGH,(VAR(J),J=2,4),BARREL(I),
     +                                  (VAR(J),J=5,6),QMM,SCF(I)
      IF(NTYPE(I).LT.19.AND.JCE.EQ.1)
     1                   WRITE(N6,940) KOE(I),NTYPE(I),NAME(NTYPE(I)),
     +                   SSL,VAR(1),RGH,(VAR(J),J=2,4),BARREL(I),
     +                                  (VAR(J),J=5,6),QMM,SCF(I)
c
CIMT  MODIFY PRINT STATEMENT FOR TYPE 19 (MANHOLES)
Cwch, 7/6/01.  MOVE CONST CONC PRINTOUTS TO NEW SUB. INTRQUAL.
      IF(NTYPE(I).EQ.19.AND.JCE.EQ.0) THEN
                         WRITE(N6,921) NOE(I),NTYPE(I),NAME(NTYPE(I)),
     +                   SSL,VAR(1),RGH,(VAR(J),J=2,4),BARREL(I)
C                    IF((NPOLL.GT.0).AND.(VAR(1).NE.0.0))
C     1                   WRITE(N6,922) (PMANN(I,J),J=1,NPOLL)
                    ENDIF
Cwch, 8/19/00. Change to Format 942. Don't print VAR5 & KGEOM for manholes.
      IF(NTYPE(I).EQ.19.AND.JCE.EQ.1) THEN
                         WRITE(N6,942) KOE(I),NTYPE(I),NAME(NTYPE(I)),
     +                   SSL,VAR(1),RGH,(VAR(J),J=2,4),BARREL(I)
Cwch    +                   VAR(5),KGEOM(I)
C                    IF((NPOLL.GT.0).AND.(VAR(1).NE.0.0))
C     1                   WRITE(N6,922) (PMANN(I,J),J=1,NPOLL)
      ENDIF
CIMT CHANGE NTYPE(I).GE.19 to NTYPE(I).GT.19
      IF(NTYPE(I).GT.19.AND.JCE.EQ.0) THEN
           WRITE(N6,921) NOE(I),NTYPE(I),NAME(NTYPE(I)),
     +                   SSL,VAR(1),RGH,(VAR(J),J=2,4),BARREL(I)
CIMQP    WRITE DELIVERY RATIOS
Cwch, 7/6/01.  MOVE WRITE STATEMENTS FOR DELIVERY RATIOS 
C     TO NEW SUB. INTRQUAL.
C       IF(NTYPE(I).EQ.27.AND.NPOLL.GT.0)
C     1     WRITE(N6,922)  (DRATIO(I,J),J=1,NPOLL)	
	     ENDIF
      IF(NTYPE(I).GT.19.AND.JCE.EQ.1) THEN
Cwch, 8/29/00.  Don't print VAR(5) for flow dividers.
           WRITE(N6,941) KOE(I),NTYPE(I),NAME(NTYPE(I)),
     +                   SSL,VAR(1),RGH,(VAR(J),J=2,4),BARREL(I),
     +                   KGEOM(I)
C       IF(NTYPE(I).EQ.27.AND.NPOLL.GT.0)
C     1     WRITE(N6,922)  (DRATIO(I,J),J=1,NPOLL)	
	ENDIF
C=======================================================================
C     CONVERT CONST. INFLOW AND CONCENTRATIONS TO LOADS,
C     IN UNITS OF CONCENTRATION * CFS.
C=======================================================================
      IF(NTYPE(I).EQ.19) THEN
Cwch, 7/6/01. Move to Sub. INTRQUAL
c      DO 4249 J=1,NPOLL
c 4249 PMANN(I,J)=PMANN(I,J)*DIST(I)
CIMT               GEOM1(I) = GEOM1(I)*DIST(I)
CIMT               SLOPE(I) = SLOPE(I)*DIST(I)
CIMT               ROUGH(I) = ROUGH(I)*DIST(I)
CIMT               GEOM2(I) = GEOM2(I)*DIST(I)
CIM   CHECK THAT GEOM3 IS SET OK RELATIVE TO DATA INPUTS ON B4 CARDS
           IF (NINT(GEOM3(I)).EQ.0) GEOM3(I)=1.0
           IF (GEOM3(I).GT.NUMSETS) THEN
       WRITE(N6,*) 'ERROR - BASE FLOW FACTOR SET FOR ELEMENT (GEOM3)',
     .' IS GREATER THAN NUMBER OF SETS READ ON B4 LINES'
                IF (JCE.EQ.0) WRITE(N6,*) ' CONDUIT = ',NOE(I)
                IF (JCE.EQ.1) WRITE(N6,*) ' CONDUIT = ',KOE(I)
                STOP 'INCORRECT BASE FLOW FACTOR'
                END IF
           ENDIF
      IF(NTYPE(I).EQ.14.OR.NTYPE(I).EQ.15) NTYPE(I) = 16
 4250 CONTINUE
C=======================================================================
Cwch, 7/6/01.  New location for input of Transport quality parameters.
C>>>>>>>>>>>> READ DATA GROUPS F1, F2, F3, F4, F5 <<<<<<<<<
C=======================================================================
      IF(NPOLL.GT.0) CALL INTRQUAL
C=======================================================================
C     Input data for storage elements.
C=======================================================================
      IF(KSTOR.GT.NSTOR) THEN
                         WRITE(N6,4252) KSTOR,NSTOR
                         STOP
                         ENDIF
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUPS G1,G2,G3,G4,G5 <<<<<<<<<<<<
C=======================================================================
      IF(KSTOR.GT.0) CALL TSTRDT
C#######################################################################
C     WCH, 8/93.  NEW FLOW DIVIDER, TYPE 26.  CHUCK MOORE, CDM
C
C>>>>>>>>>>>> READ DATA GROUPS G6,G7  TABULAR FLOW SPLITER <<<<<<<<<<<<
C=======================================================================
      IF(KINOU.GT.0) CALL RSPLIT
C=======================================================================
C     INITIALIZATION
C=======================================================================
CIMT  change upper range of loop from 4 to MQUAL
      DO 4255 I  = 1,MQUAL
 4255 CPPP(I)    = 0.0
      DO 50 K    = 1,NE+1
      IOLD(K)    = 1
      QINFIL(K)  = 0.0
      QDWF(K)    = 0.0
      RNOFF(K)   = 0.0
      DO 5006 I  = 1,2
      DO 5006 II = 1,2
      A(K,II,I)  = 0.0
      Q(K,II,I)  = 0.0
 5006 CONTINUE
      IF(NPOLL.GT.0) THEN
                     DO 5010 L = 1,NPOLL
                     WDWF(K,L) = 0.0
                     SCOUR(K,L)= 0.0
                     DS(K,L)   = PSIZE(5,L)
                     DB(K,L)   = PSIZE(1,L)
                     PLUTO(L,K)= 0.0
                     DO 5008 I = 1,2
                     CPOL1(K,I,L) = 0.0
                     CPOL2(K,I,L) = 0.0
 5008                CONTINUE
 5010                CONTINUE
                     ENDIF
   50 CONTINUE
      DO K=1,NTHR
         QE1(K)=0.0
         QE2(K)=0.0
      DO L = 1,NPOLL
         PE1(L,K)=0.0
         PE2(L,K)=0.0
      ENDDO
      ENDDO
      DO K=1,NTHI
         QF1(K)=0.0
         QF2(K)=0.0
      DO L=1,NPOLL
         PF1(L,K)=0.0
         PF2(L,K)=0.0
      ENDDO
      ENDDO
      QO(NE+1)    = 0.0
      QI(NE+1)    = 0.0
      NTYPE(NE+1) = 19
      RNOFF(NINPUT+1) = 0.0
      IF(NPOLL.GT.0) THEN
                     DO 51 I = 1,NPOLL
   51                CPOL2(NE+1,2,I) = 0.0
                     ENDIF
      DO 52 I  = 1,7
      DVDWF(I) = 1.0
      DVBOD(I) = 1.0
   52 DVSS(I)  = 1.0
      DO 53 I  = 1,24
      HVDWF(I) = 1.0
      HVBOD(I) = 1.0
   53 HVSS(I)  = 1.0
C=======================================================================
C     CHECK FOR COMPATIBILITY OF INTERFACE FILE LOCATIONS WITH
C     TRANSPORT ELEMENT NUMBERS.
C=======================================================================
      IF(NCNTRL.EQ.0) THEN
                      DO 4270 J = 1,LOCATS
                      DO 4265 I = 1,NE
                      IF(JCE.EQ.0.AND.NLOC(J).EQ.NOE(I)) GO TO 4270
                      IF(JCE.EQ.1.AND.KAN(J).EQ.KOE(I))  GO TO 4270
 4265                 CONTINUE
C#### WCH (RED), 9/93.  RED SUGGESTS ELIMINATING THIS JSTOP.
CCC                      JSTOP = 1
                      IF(JCE.EQ.0) WRITE(N6,4268) NLOC(J)
                      IF(JCE.EQ.1) WRITE(N6,4368) KAN(J)
C=======================================================================
C     CHANGE TO NEGATIVE NO. AND IGNORE INPUT FROM FILE.
C=======================================================================
                      NLOC(J) = -NLOC(J)
                      KAN(J)  = ' '
 4270                 CONTINUE
                      ENDIF
C=======================================================================
C     READ ELEMENT NUMBERS OF LOCATIONS TO BE PLACED ON INTERFACE FILE.
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP H1 <<<<<<<<<<<<
C=======================================================================
      IF(NOUTS.GT.0) THEN
                     IF(JCE.EQ.0) THEN
                          READ(N5,*,ERR=888) CC,(JN(I),I=1,NOUTS)
                          WRITE(N6,917)         (JN(I),I=1,NOUTS)
                          ELSE
                          READ(N5,*,ERR=888) CC,(KJN(I),I=1,NOUTS)
                          WRITE(N6,918)         (KJN(I),I=1,NOUTS)
                          ENDIF
                     ENDIF
C#######################################################################
C     READ INPUT DATA FOR WASP LINKAGE, IF PRESENT.
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUPS H2 AND H3 (OPTIONAL) <<<<<<<<<<<
C=======================================================================
      CALL LINK(0)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP I1 <<<<<<<<<<<<
C=======================================================================
      IF(NINPUT.GT.0) THEN
                      IF(JCE.EQ.0) THEN
                           READ(N5,*,ERR=888) CC,(KORDER(I),I=1,NINPUT)
                           WRITE(N6,4290)        (KORDER(I),I=1,NINPUT)
                           ELSE
                           READ(N5,*,ERR=888) CC,(BORDER(I),I=1,NINPUT)
                           WRITE(N6,4291)        (BORDER(I),I=1,NINPUT)
                           ENDIF
                      ENDIF
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP I2 (IF PRESENT) <<<<<<<<<<<<
C=======================================================================
       READ(N5,*,ERR=888) CC
       IF(CC.EQ.'I2') THEN
                      BACKSPACE N5
                      IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,NSURF,
     +                                     (JSURF(N),N=1,NSURF)
                      IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,NSURF,
     +                                     (KSURF(N),N=1,NSURF)
                      IF(JCE.EQ.0) WRITE(N6,1975) (JSURF(K),K=1,NSURF)
                      IF(JCE.EQ.1) WRITE(N6,1976) (KSURF(K),K=1,NSURF)
                      IF(NSCRAT(7).EQ.0) CALL ERROR(112)
                      ELSE
                      NSURF    = 0
                      BACKSPACE N5
                      ENDIF
C=======================================================================
C    READ IN ELEMENT NUMBERS FOR WHICH INPUT POLLUTOGRAPHS AND
C     HYDROGRAPHS TO BE STORED AT ALL TIME STEPS.
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP J1 <<<<<<<<<<<<
C=======================================================================
      IF(NNYN.GT.0) THEN
                    IF(JCE.EQ.0) THEN
                         READ(N5,*,ERR=888) CC,(NYN(I),I=1,NNYN)
                         WRITE(N6,4370)        (NYN(I),I=1,NNYN)
                         ELSE
                         READ(N5,*,ERR=888) CC,(KYN(I),I=1,NNYN)
                         WRITE(N6,4371)        (KYN(I),I=1,NNYN)
                         ENDIF
                    ENDIF
C=======================================================================
C     READ IN ELEMENT NUMBERS FOR WHICH OUTPUT POLLUTOGRAPHS AND
C     HYDROGRAPHS TO BE STORED AT ALL TIME STEPS.
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP J2 <<<<<<<<<<<<
C=======================================================================
      IF(NNPE.GT.0) THEN
                    IF(JCE.EQ.0) THEN
                         READ(N5,*,ERR=888) CC,(NPE(I),I=1,NNPE)
                         WRITE(N6,4380)        (NPE(I),I=1,NNPE)
                         ELSE
                         READ(N5,*,ERR=888) CC,(KPE(I),I=1,NNPE)
                         WRITE(N6,4381)        (KPE(I),I=1,NNPE)
                         ENDIF
                    ENDIF
C=======================================================================
C     PERFORM SCRATCH TAPE OPERATIONS
C     STORE PRINT INFORMATION ON SCRATCH TAPES
C=======================================================================
      JPRINT = 0
      IF(NNYN.GT.0.AND.NNPE.LE.0) JPRINT = 1
      IF(NNYN.LE.0.AND.NNPE.GT.0) JPRINT = 2
      IF(NNYN.GT.0.AND.NNPE.GT.0) JPRINT = 3
      IF(JPRINT.EQ.0) GO TO 4400
C
      DO 62 III = 1,2
      IF(III.EQ.1.AND.NNYN.EQ.0) GO TO 62
      IF(III.EQ.2.AND.NNPE.EQ.0) GO TO 62
      NTX       = NSCRAT(III)
      IF(NTX.LE.0) WRITE (N6,4390) III,NTX,NTX
      REWIND NTX
      IF(III.GT.1) GO TO 61
C=======================================================================
C     Must resequence NYN array to match computation order.
C=======================================================================
      N         = 0
      DO 4394 I = 1,NE
      M         = JR(I)
      DO 4392 J = 1,NNYN
      IF(JCE.EQ.0.AND.NOE(M).NE.NYN(J)) GO TO 4392
      IF(JCE.EQ.1.AND.KOE(M).NE.KYN(J)) GO TO 4392
      N      = N+1
      IF(JCE.EQ.0) THEN
                   LYDIA  = NYN(N)
                   NYN(N) = NYN(J)
                   NYN(J) = LYDIA
                   ELSE
                   BMJ    = KYN(N)
                   KYN(N) = KYN(J)
                   KYN(J) = BMJ
                   ENDIF
      GO TO 4393
 4392 CONTINUE
 4393 IF(N.GE.NNYN) GO TO 62
 4394 CONTINUE
C=======================================================================
C     Must resequence NPE array to match computation order.
C=======================================================================
   61 N = 0
      DO 4398 I = 1,NE
      M         = JR(I)
      DO 4396 J = 1,NNPE
      IF(JCE.EQ.0.AND.NOE(M).NE.NPE(J)) GO TO 4396
      IF(JCE.EQ.1.AND.KOE(M).NE.KPE(J)) GO TO 4396
      N      = N+1
      IF(JCE.EQ.0) THEN
                   LYDIA  = NPE(N)
                   NPE(N) = NPE(J)
                   NPE(J) = LYDIA
                   ELSE
                   BMJ    = KPE(N)
                   KPE(N) = KPE(J)
                   KPE(J) = BMJ
                   ENDIF
      GO TO 4397
 4396 CONTINUE
 4397 IF(N.GE.NNPE) GO TO 62
 4398 CONTINUE
   62 CONTINUE
 4400 CONTINUE
C=======================================================================
C     Must resequence JSURF array to match computation order.
C=======================================================================
      IF(NSURF.GT.0) THEN
      N         = 0
      DO 5398 I = 1,NE
      M         = JR(I)
      DO 5396 J = 1,NSURF
      IF(JCE.EQ.0.AND.NOE(M).NE.JSURF(J)) GO TO 5396
      IF(JCE.EQ.1.AND.KOE(M).NE.KSURF(J)) GO TO 5396
      N      = N+1
      IF(JCE.EQ.0) THEN
                   LYDIA    = JSURF(N)
                   JSURF(N) = JSURF(J)
                   JSURF(J) = LYDIA
                   ELSE
                   JSURF(J) = M
                   ENDIF
      GO TO 5397
 5396 CONTINUE
 5397 IF(N.GE.NSURF) GO TO 5562
 5398 CONTINUE
 5562 CONTINUE
      ENDIF
C=======================================================================
C     Check for input errors in NYN, NPE or JSURF arrays.
C=======================================================================
      IF(NNYN.GT.0) THEN
              DO 5400 J = 1,NNYN
              DO 5402 I = 1,NE
              M         = JR(I)
              IF(JCE.EQ.0.AND.NOE(M).NE.NYN(J)) GO TO 5402
              IF(JCE.EQ.1.AND.KOE(M).NE.KYN(J)) GO TO 5402
              GO TO 5400
 5402         CONTINUE
              JSTOP = 1
              IF(JCE.EQ.0) WRITE(N6,9560) NYN(J)
              IF(JCE.EQ.1) WRITE(N6,9561) KYN(J)
 5400         CONTINUE
              ENDIF
      IF(NNPE.GT.0) THEN
              DO 5405 J = 1,NNPE
              DO 5407 I = 1,NE
              M         = JR(I)
              IF(JCE.EQ.0.AND.NOE(M).NE.NPE(J)) GO TO 5407
              IF(JCE.EQ.1.AND.KOE(M).NE.KPE(J)) GO TO 5407
              GO TO 5405
 5407         CONTINUE
              JSTOP = 1
              IF(JCE.EQ.0) WRITE(N6,9570) NPE(J)
              IF(JCE.EQ.1) WRITE(N6,9571) KPE(J)
 5405         CONTINUE
              ENDIF
      IF(NSURF.GT.0) THEN
              DO 5410 J = 1,NSURF
              DO 5412 I = 1,NE
              M         = JR(I)
              IF(JCE.EQ.0.AND.NOE(M).NE.JSURF(J)) GO TO 5412
              IF(JCE.EQ.1.AND.KOE(M).NE.KSURF(J)) GO TO 5412
              GO TO 5410
 5412         CONTINUE
              JSTOP = 1
              IF(JCE.EQ.0) WRITE(N6,9580) JSURF(J)
              IF(JCE.EQ.1) WRITE(N6,9581) KSURF(J)
 5410         CONTINUE
              ENDIF
C=======================================================================
C     Stop if input errors have been found.
C=======================================================================
      IF(JSTOP.GT.0)  THEN
                      WRITE(N6,4082)
                      STOP
                      ENDIF
C=======================================================================
C     DETERMINE AVERAGE DAILY DWF AND INFILTRATION
C     DATA ARE READ IN FROM BOTH OF THESE SUBROUTINES.
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUPS K1 AND K2 <<<<<<<<<<<<
C=======================================================================
      IF(NINFIL.EQ.1) THEN
                      WRITE(*,5055)
                      CALL INFIL
                      ENDIF
C=======================================================================
C>>>>>>>>>>> READ DATA GROUPS L1,L2,L3,M1,M2,M3,M4,N1,O1,O2,P1,Q1 <<<<<<
C=======================================================================
      IF(NFILTH.EQ.1) THEN
                      WRITE(*,5060)
                      CALL FILTH
                      ENDIF
C=======================================================================
C     Initialize dry-weather flow (base flow) deposition.
C=======================================================================
      N = 0
      IF(KSPG.GT.0.AND.DWDAYS.GT.0.0) CALL DWLOAD
C=======================================================================
C     INITIALIZE DWF (BASE FLOW) FLOWS, AREAS AND CONCENTRATIONS.
C=======================================================================
      WRITE(*,5070)
      CALL INITAL
C=======================================================================
C     Generate output interface file.
C=======================================================================
      IF(NEXT.LE.0.OR.NOUTS.LE.0) RETURN
C=======================================================================
C     Here, Transport is first block run.
C=======================================================================
      IF(LAST.EQ.0.AND.NCNTRL.GT.0) THEN
                                     TITLE(3) = TITLE(1)
                                     TITLE(4) = TITLE(2)
                                     ENDIF
CIM### 9/8/00
CIMHV    SET UP STUFF FOR WRITING DEPTH AND VELOCITY TO INTERFACE
       IF (NPOLL2.GT.NPOLL) THEN
       WRITE(N6,8000)
       PNAME(NPOLL+1) = ' DEPTH '
       PNAME(NPOLL+2) = 'VELOCITY'
       NDIM(NPOLL+1) = 2
       NDIM(NPOLL+2) = 2
       IF (METRIC.EQ.1) THEN
       PUNIT(NPOLL+1) = ' FEET '
       PUNIT(NPOLL+2) = ' FT/SEC'
       ENDIF
       IF (METRIC.EQ.2) THEN
       PUNIT(NPOLL+1) = ' METERS '
       PUNIT(NPOLL+2) = ' M/SEC'
       ENDIF
        ENDIF
CIMHV  change npoll to npoll2 below
       REWIND NEXT
       WRITE(NEXT) NOUTS,NPOLL2
       IF(JCE.EQ.0) WRITE(NEXT) (JN(I),I=1,NOUTS)
       IF(JCE.EQ.1) WRITE(NEXT) (KJN(I),I=1,NOUTS)
       QQCONV = QCONV
       QCONV  = CMET(8,METRIC)
       SOURCE = 'TRANSPORT BLOCK'
       CALL INFACE(2,NEXT)
       QCONV  = QQCONV
       RETURN
  888  CALL IERROR
C=======================================================================
   10 FORMAT(/,1X,
     1'#####################################################',/,1X,
     2'# Entry made to the Transport Block, last updated   #',/,1X,
     3'# by Oregon State University, April 2002.           #',/,1X,
     4'#####################################################',/,1X,
     5'# "The sewer is the conscience of the city."        #',/,1X,
     6'#                                 Victor Hugo (1862)#',/,1X,
     7'#####################################################',/)
   11 FORMAT(/,1X,
     1'#####################################################',/,1X,
     2'# Entry made to the Transport Block, last updated   #',/,1X,
     3'# by Oregon State University, April 2002.           #',/,1X,
     4'#####################################################',/)
  907 FORMAT (' ',10X,A80,/,11X,A80)
CIM### 9/8/00 Add Idetail.
Cwch, 10/3/01 Add NOHEAD
  908 FORMAT(/,
     1' ************************************************',/,
     2' *  OPTIONAL INPUT PARAMETERS FROM DATA LINE B0 *',/,
     2' *  ------------------------------------------  *',/,
     2' *    The default values are used if line B0    *',/,
     2' *    is not part of the TRANSPORT input data.  *',/,
     2' *    Defaults:  ISLOPE=0  ITRAP=0   IFLIP=0    *',/,
     2' *               IDETAIL=0                      *',/,
     3' ************************************************',//,
     4' Use ft/100 ft or m/100 m for conduit slope...(ISLOPE=0)..',I5,/,
     4' Use ft/ft or m/m for input of conduit slope..(ISLOPE=1)..',/,
     6' Vertical/horizontal trapezoid side slope.....(ITRAP=0)...',I5,/,
     6' Horizontal/vertical trapezoid side slope.....(ITRAP=1)...',/,
     7' Default input of flow/pollutants on line R1..(IFLIP=0)...',I5,/,
     7' Input flow/pollutants on one R1 line.........(IFLIP=1)...',/,
     8' Include only hydrograph input(s) on printout.(INFLEW=0)..',I5,/,
     8' Include all input(s) on input printout.......(INFLEW=1)..',/,
cimdetail
     9' Normal division of irregular section data ...(IDETAIL=0).',I5,/,
     9' Subdivide lowermost equal area segment.......(IDETAIL=1).',/,
     9' Print intermediate headers in tables.........(NOHEAD=0)..',I5,/,
     9' Do not print intermediate headers in table...(NOHEAD=1)..',/)
  915 FORMAT(1H1)
  914 FORMAT(I1,/,
     1' *****************************************************',/,
     1' *           TRANSPORT ELEMENT PARAMETERS            *',/,
     1' *                                                   *',/,
     *' * CAUTION: COLUMN HEADINGS ARE FOR CONDUITS.  REFER *',/,
     2' * TO USERS MANUAL FOR MEANING FOR NON-CONDUITS.     *',/,
     1' *****************************************************',//)
  916 FORMAT(4X,
     3'   EXT.                     SLOPE DISTANCE   MANNING   GEOM1',
     3'  GEOM2  GEOM3  NUMBER   AFULL     QFULL     QMAX',
     3'   SUPER-CRITICAL',/,4X,
     4'   ELE. TY                 (FT/FT)   (FT)   ROUGHNESS   (FT)',
     4'   (FT)   (FT)    OF    (SQ.FT)    (CFS)    (CFS)',
     4'   FLOW WHEN LESS',/,4X,
     5'   NUM. PE TYPE NAME',55X,'BARRELS',30X,'THAN 95% FULL?')
  919 FORMAT(4X,
     3'   EXT.                     SLOPE DISTANCE   MANNING   GEOM1',
     2'  GEOM2  GEOM3  NUMBER   AFULL     QFULL     QMAX',
     2'   SUPER-CRITICAL',/,4X,
     4'   ELE. TY                   (M/M)    (M)   ROUGHNESS    (M)',
     4'    (M)    (M)    OF     (SQ.M)    (CMS)    (CMS)',
     4'   FLOW WHEN LESS',/,4X,
     5'   NUM. PE TYPE NAME',55X,'BARRELS',30X,'THAN 95% FULL?')
  913 FORMAT(4X,'   ---- -- -----------     -------  ------  --------',
     6'  ------  -----  ----- -------  -------   ------  -------',
     6'  --------------')
c	
C 1916 FORMAT(4X,
C     3'   EXT.                     SLOPE DISTANCE   MANNING   GEOM1',
C     3'  GEOM2  GEOM3  NUMBER   AFULL     QFULL     QMAX',
C     3'   SUPER-CRITICAL     CONSTANT INFLOW CONCENTRATIONS',/,4X,
C     4'   ELE. TY                 (FT/FT)   (FT)   ROUGHNESS   (FT)',
C     4'   (FT)   (FT)    OF    (SQ.FT)    (CFS)    (CFS)',
C     4'   FLOW WHEN LESS     FOR TYPE 19 ELEMENTS WITH CONSTANT',
C     4' INFLOW (DISTANCE <> 0)',/,4X,
C     5'   NUM. PE TYPE NAME',55X,'BARRELS',30X,'THAN 95% FULL?',4X,
C	599(1X,A8,1X))
C 1919 FORMAT(4X,
C     3'   EXT.                     SLOPE DISTANCE   MANNING   GEOM1',
C     2'  GEOM2  GEOM3  NUMBER   AFULL     QFULL     QMAX',
C     2'   SUPER-CRITICAL     CONTANT INFLOW CONCENTRATIONS',/,4X,
C     4'   ELE. TY                   (M/M)    (M)   ROUGHNESS    (M)',
C     4'    (M)    (M)    OF     (SQ.M)    (CMS)    (CMS)',
C     4'   FLOW WHEN LESS     FOR TYPE 19 ELEMENTS WITH CONSTANT',
C     4' INFLOW (DISTANCE <> 0)',/,4X,
C     5'   NUM. PE TYPE NAME',55X,'BARRELS',30X,'THAN 95% FULL?',4X,
C	599(1X,A8,1X))
 1913 FORMAT(4X,'   ---- -- -----------     -------  ------  --------',
     6'  ------  -----  ----- -------  -------   ------  -------',
     6'  --------------',4X,99(A10))
  917 FORMAT(//,
     1' ********************************************************',/,
     2' * HYDROGRAPHS AND POLLUTOGRAPHS PROVIDED TO SUBSEQUENT *',/,
     3' * BLOCKS FOR THE FOLLOWING ELEMENTS ON DATA GROUP H1:  *',/,
     4' ********************************************************',//,
     5  (10I9))
  918 FORMAT(//,
     1' ********************************************************',/,
     2' * HYDROGRAPHS AND POLLUTOGRAPHS PROVIDED TO SUBSEQUENT *',/,
     3' * BLOCKS FOR THE FOLLOWING ELEMENTS ON DATA GROUP H1:  *',/,
     4' ********************************************************',//,
     5  (10(1X,A10)))
  920 FORMAT(1X,I10,I3,1X,A16,1PG7.1,0PF8.2,F10.4,F8.2,2F7.2,F8.1,
     +                              F9.2,1PG9.2,1PG9.2,5X,A4)
  921 FORMAT(1X,I10,I3,1X,A16,1PG7.1,0PF8.2,F10.4,F8.2,2F7.2,F8.1,
     +                              F9.2,1PG9.2,1PG9.2,5X,A4)
  922 FORMAT(133X,99F10.4)
  940 FORMAT(1X,A10,I3,1X,A16,1PG7.1,0PF8.2,F10.4,F8.2,2F7.2,F8.1,
     +                              F9.2,1PG9.2,1PG9.2,5X,A4)
Cwch, 8/29/00. Enhance printout for KGEOM.
  941 FORMAT(1X,A10,I3,1X,A16,1PG7.1,0PF8.2,F10.4,F8.2,2F7.2,F8.1,
     +  1X,'KGEOM = ',A10)
  942 FORMAT(1X,A10,I3,1X,A16,1PG7.1,0PF8.2,F10.4,F8.2,2F7.2,F8.1)
  968 FORMAT(//,' ====> TOO MANY ELEMENTS READ IN IN TRANSPORT BLOCK',
     1          '(',I4,').  PROGRAM STOPPED IN SUB TRANS.')
  949 FORMAT(12F10.3)
CWCH, 11/11/99. INCREASE FIELD WITH FOR NDT.
  950 FORMAT(//,' NUMBER OF TIME STEPS (NDT)........................',
     1 T48,I9,/,' NO. OF ELEMENTS FOR CARD HYDROGRAPH INPUT (NINPUT)',
     2 T52,I5,/,' NO. OF ELEMENTS FOR INPUT HYDROGRAPH PRINT (NNYN).',
     3 T52,I5,/,' NO. OF ELEMENTS FOR OUTPUT HYDROGRAPH PRINT (NNPE)',
     4 T52,I5,/,' NO. TIME STEPS BETWEEN I/O PRINTS (INTPRT)........',
     5 T52,I5,/,' NO. OF ELEMENTS FOR INTERFACE TRANSFER (NOUTS)....',
     6 T52,I5,/,' NO. OF POLLUTANTS SIMULATED (NPOLL)...............',
     7 T52,I5,/,' NO. OF ITERATIONS FOR FLOW ROUTING (NITER)........',
     8 T52,I5)
  951 FORMAT(//,' FOR THE FOLLOWING PARAMETERS, 1=YES, 0=NO:',
     1/,'    PRINT INTERNAL ERROR MESSAGES (NPRINT).........',I3,
     2/,' HYDROGRAPH INPUT FROM CARDS ONLY (NCNTRL).........',I3,
     3/,' CALL INFILTRATION ROUTINE (NINFIL)................',I3,
     4/,' CALL DRY-WEATHER FLOW ROUTINE (NFILTH)............',I3,
     5/,' USE HYDRAULIC DESIGN ROUTINE (NDESN)..............',I3)
  952 FORMAT(//,' SIZE OF TIME STEP (DT, SECONDS)...................',
     1 F8.1,/,' ALLOWABLE ROUTING CONVERGENCE ERROR (EPSIL).......',
     2 F8.6,/,' NO. DRY DAYS PRIOR TO SIMULATION (DWDAYS).........',
     2 F8.2,/,' STARTING TIME OF DAY IN HOURS.....................',
C#### WCH, 1/4/94.  FIX TYPO IN VISCOSITY
     3 F8.2,/,' KINEMATIC VISCOSITY (GNU, SQ FT/SEC)..............',
     4 1PE8.2,/,' TOTAL CATCHMENT AREA (TRIBA, ACRES)...............',
     5 0PF8.2)
  953 FORMAT(//,' SIZE OF TIME STEP (DT, SECONDS)...................',
     1 F8.1,/,' ALLOWABLE ROUTING CONVERGENCE ERROR (EPSIL).......',
     2 F8.6,/,' NO. DRY DAYS PRIOR TO SIMULATION (DWDAYS).........',
     2 F8.2,/,' STARTING TIME OF DAY IN HOURS.....................',
     3 F8.2,/,' KINEMATIC VISCOSITY (GNU, SQ CM/SEC)..............',
     4 1PE8.2,/,' TOTAL CATCHMENT AREA (TRIBA, HECTARES)............',
     5 0PF8.2)
Cwch, 7/6/01
  955 FORMAT(/,' NO SIMULATION OF LINKED DO (NWQ = 0).')
  956 FORMAT(/,' SIMULATE LINKED DO-BOD-NOD (NWQ = 1).',/,
     1' REQUIRE FIRST FOUR CONSTITUENTS TO BE: CBOD,NOD,NO3-N,DO.')
Cwch, 4/18/02.
  965 FORMAT(//,' NOTE: STARTING DATE/TIME FROM B1/B2 LINES WILL BE USED
     1,',/,' NOT VALUES FROM INTERFACE FILE.')
  966 FORMAT(//,' STARTING DATE OF SIMULATION (YR/MO/DAY) = ',
     1            I5,'/',I2,'/',I2,/,
     2          ' STARTING TIME OF DAY HR:MN:SEC          = ',
     3            I5,'/',I2,'/',I2)
  967 FORMAT (//,1H1,'THERE WERE ',I4,' ELEMENTS (NE) INPUT',
     1 ' INTO TRANSPORT BLOCK FROM DATA GROUP E1.')
  970 FORMAT(/,'NTYPE  DESCRIPTION         ALFMAX  PSIMAX  AFACT   RFACT
     +  KDEPTH  KLASS  INDEX  ANORM   QNORM    DNORM',/,
     +'-----  -----------         ------  ------  -----   -----  ------
     +  -----  -----  -----   -----    -----')
  971 FORMAT(I5,2X,A20,F7.4,3F8.4,I5,I7,I8,F8.3,2F9.5,/,(70X,I8,F8.3,
     1 2F9.5))
  972 FORMAT('  NON-CONDUITS',//,T5,'NTYPE  KDEPTH  KLASS  DESCRIPTION')
  973 FORMAT(1X,I6,I7,I8,4X,A20)
 1975 FORMAT(/,'     ***********************',/,
     +         '     *    DATA GROUP I2    *',/,
     +         '     ***********************',//,
     1          ' CHANNEL/DEPTH PRINT DATA GROUP.......',10I7,
     2          19(/,31X,10I7))
 1976 FORMAT(/,'     ***********************',/,
     +         '     *    DATA GROUP I2    *',/,
     +         '     ***********************',//,
     1         ' CHANNEL/DEPTH PRINT DATA GROUP.......',10(A6,1X),
     2          19(/,31X,10(A6,1X)))
 3005 FORMAT(/,' ===> WARNING !! FROM SUB TRANS. NOUTS=',I4,' AND JOUT =
     1',I4,'.  BOTH MUST BE GT 0 TO GENERATE INTERFACE FILE.',/,
     2 ' SIMULATION WILL CONTINUE, BUT NO NEW INTERFACE FILE WILL BE GEN
     3ERATED.')
 3010 FORMAT(/,' USE U.S. CUSTOMARY UNITS FOR INPUT/OUTPUT(METRIC=0).')
 3015 FORMAT(/,' USE METRIC UNITS FOR MOST INPUT/OUTPUT(METRIC=1).',/,
     1' UNITS OF FEET AND SECONDS STILL USED FOR MOST INTERNAL COMPUTATI
     2ONS.')
 3095 FORMAT(' ===> WARNING  !! FROM SUB INTRAN.  INPUT FILE UNIT NUMBER
     1 (JIN) = ',I9,/,1X,'PROGRAM WILL SOON TRY TO READ FROM THIS FILE',
     2' AND AN ERROR MESSAGE MAY APPEAR.')
Cwch, 4/18/02.
 4005 FORMAT(/,' ERROR! SAME ALPHANUMERIC OPTION ($ANUM) WAS NOT USED IN
     1 TRANS',/,' AS IN BLOCK THAT CREATED INTERFACE FILE.',/,
	2' PROGRAM CONTINUES BUT UNPREDICTABLE ERROR WILL SOON RESULT.')
 4007 FORMAT(/,' ===>  ERROR IN SUB INTRAN.  NO. OF INPUT LOCATIONS FROM
     1 INTERFACE FILE =',I9,' IS GT NTHI ALLOWABLE FOR TRANSPORT INPUT.'
     2,/,' PROGRAM STOPPED.')
 4064 FORMAT (' ====> ERROR IN SUB INTRAN.  ELE. NO.',I10,'(INPUT SEQUEN
     1CE',I4,') IS SAME AS ELE. NO.',I10,' (INPUT SEQUENCE',I4,').',/,
     2 ' THIS IS NOT ALLOWED.  PROGRAM WILL STOP BELOW.')
 8064 FORMAT (' ====> ERROR IN SUB INTRAN.  ELE. NO.',A10,'(INPUT SEQUEN
     1CE',I4,') IS SAME AS ELE. NO.',A10,' (INPUT SEQUENCE',I4,').',/,
     2 ' THIS IS NOT ALLOWED.  PROGRAM WILL STOP BELOW.')
 4082 FORMAT (///,' <> <> <> <> <> PROGRAM STOPPED IN SUBROUTINE INTRAN
     1 DUE TO ELEMENT NUMBER ERROR(S) GIVEN ABOVE. <> <> <> <> <>')
C#### WCH, 8/15/96.
 4083 FORMAT(//,' ==> ERROR IN SUB INTRAN.  ZERO ELEMENTS ENTERED ON E1
     1 LINES.',/,' LIKELY CAUSE IS INSUFFICIENT PARAMETER ENTRIES ON PRI
     2OR DATA LINES OR',/,' MISSING ASTERISK ON A COMMENT LINE.',/,
     3' TRANSPORT BLOCK WILL STOP HERE.')
Cwch, 8/29/00
 4085 FORMAT(//,' ==> ERROR IN SUB INTRAN, READING E1 LINES.',/,
     1' FOR MANHOLE (NTYPE 19) WHEN NPOLL.NE.0 AND DIST (CONSTANT INFLOW
     2).NE.0',/,
	3' REQUIRE ADDITIONAL INPUT LINE WITH POLLUTANT CONCENTRATIONS, FOL
	4LOWING EACH SUCH E1 LINE.',/,
	5' SEE TRANS.DOC FOR DETAILS.  RUN STOPPED AT E1 LINE ',I5)
Cwch, 7/6/01. REMOVE FORMATS 4087 - 4157 TO NEW SUB. INTRQUAL
 4252 FORMAT (//,' >>>>> ERROR IN SUB. TRANS.  NUMBER OF STORAGE UNITS='
     1,I3,' WHICH EXCEEDS ALLOWABLE TOTAL = ',I3,' <<<<<',/,
     2 ' >>>>> PROGRAM STOPPED <<<<<')
 4268 FORMAT (/,' ===> ERROR !!! FROM SUB. TRANS.  NO ELEMENT NUMBER MAT
     1CH FOUND FOR INPUT LOCATION ',I7,' ON INTERFACE FILE.  SIMULATION
     2CONTINUES.')
 4368 FORMAT (/,' ===> WARNING !!! FROM SUB. TRANS.  NO ELEMENT NUMBER M
     1ATCH FOUND FOR INPUT LOCATION ',A10,' ON INTERFACE FILE.  SIMULATI
     2ON CONTINUES.')
 4290 FORMAT(//,
     1' *********************************************************',/,
     2' * INPUT OF HYDROGRAPHS (AND POLLUTOGRAPHS) WILL BE READ *',/,
     3' * FOR THE FOLLOWING ELEMENT NUMBERS ON DATA GROUP I1:   *',/,
     4' *********************************************************',//,
     5  (10I9))
 4291 FORMAT(//,
     1' *********************************************************',/,
     2' * INPUT OF HYDROGRAPHS (AND POLLUTOGRAPHS) WILL BE READ *',/,
     3' * FOR THE FOLLOWING ELEMENT NUMBERS ON DATA GROUP I1:   *',/,
     4' *********************************************************',//,
     5  (10(1X,A10)))
 4370 FORMAT(//,
     1' *********************************************************',/,
     2' * INPUT HYDROGRAPHS (AND POLLUTOGRAPHS) WILL BE PRINTED *',/,
     3' * FOR THE FOLLOWING ELEMENT NUMBERS ON DATA GROUP J1:   *',/,
     4' *********************************************************',//,
     5  (10I9))
 4371 FORMAT(//,
     1' *********************************************************',/,
     2' * INPUT HYDROGRAPHS (AND POLLUTOGRAPHS) WILL BE PRINTED *',/,
     3' * FOR THE FOLLOWING ELEMENT NUMBERS ON DATA GROUP J1:   *',/,
     4' *********************************************************',//,
     5  (10(1X,A10)))
 4380 FORMAT(//,
     1' **********************************************************',/,
     2' * OUTPUT HYDROGRAPHS (AND POLLUTOGRAPHS) WILL BE PRINTED *',/,
     3' * FOR THE FOLLOWING ELEMENT NUMBERS ON DATA GROUP J2:    *',/,
     4' **********************************************************',//,
     5  (10I9))
 4381 FORMAT(//,
     1' **********************************************************',/,
     2' * OUTPUT HYDROGRAPHS (AND POLLUTOGRAPHS) WILL BE PRINTED *',/,
     3' * FOR THE FOLLOWING ELEMENT NUMBERS ON DATA GROUP J2:    *',/,
     4' **********************************************************',//,
     5  (10(1X,A10)))
 4390 FORMAT(/,' ===> ERROR !!! IN SUB TRANS. NSCRAT(',I1,') = ',
     1 I4,'.  MUST BE GT 0 FOR PRINT OF INPUT AND OUTPUT HYDROGRAPHS.',
     2 /,' A SYSTEM ERROR MAY SOON BE EXPECTED AS PROGRAM TRIES TO WRITE
     3 ON UNIT',I4)
 5000 FORMAT('   IBFF = 1, FACTORS ARE APPLIED ONLY TO CONSTANT',
     a' MANHOLE INFLOWS ENTERED ON E1 LINES')
 5001 FORMAT('   IBFF = 2, FACTORS ARE APPLIED ONLY TO DWF',
     a' CALCULATED USING INPUTS ON N1 THROUGH Q1 LINES.')
 5002 FORMAT('   IBFF = 3, FACTORS ARE APPLIED TO BOTH CONSTANT',
     a' MANHOLE INFLOWS ON E1 LINES AND',/,
     b'             DWF CALCULATED USING INPUTS ON N1',
     c' THROUGH Q1 lines.')
 5050 FORMAT(/,' Reading element data.')
 5055 FORMAT(/,' Reading infiltration data.')
 5060 FORMAT(/,' Reading dry-weather flow data.')
 5070 FORMAT(/,' Calculating initial conditions.')
 5080 FORMAT(/,' ===> WARNING !! NCNTRL GT 0 BUT INPUT INTERFACE FILE IS
     + DEFINED.',/)
 7001 FORMAT(' ERROR - NO. OF ELEMENTS FOR OUTPUT HYDROGRAPH PRINT',
     A' (NNPE) EQUALS ',I10,' WHICH EXCEEDS MAXIMUM DEFINED BY',
     A' NTOA OF ',I10)
 7002 FORMAT(' ERROR - NO. OF ELEMENTS FOR WHICH INFLOW PRINT (NNYN)',
     A' EQUALS ',I10,' WHICH EXCEEDS MAXIMUM DEFINED BY NTOA OF ',I10)
 7003 FORMAT(' ERROR - NO. OF ELEMENTS FOR WHICH WHICH OUTFLOW',
     A' HYDROGRAPHS AND POLLUTOGRAPHS ARE WRITTEN TO TAPE ',
     A'(NOUTS) EQUALS ',I10,' WHICH EXCEEDS MAXIMUM ALLOWED',
     A' (NTHO) OF ',I10)
 7011 FORMAT(' ERROR - INVALID ELEMENT TYPE FOR ELEMENT ',I10,
     A' ELEMENT TYPE = ',I10)
cim### depth and velocity 9/8/00
 8000 FORMAT(/,
     110X,' *****************************************************',/,
     210X,' *  DEPTH AND VELOCITY DATA ARE WRITTEN TO TRANSPORT  *',/,
     310X,' *  INTERFACE FILE.                                   *',/,
     410X,' *****************************************************',//)
 9500 FORMAT(/,' ===> ERROR !! ZERO SLOPE FOR ELEMENT # ',I10)
 9501 FORMAT(/,' ===> ERROR !! ZERO SLOPE FOR ELEMENT # ',A10)
 9510 FORMAT(/,' ===> ERROR !! ZERO ROUGHNESS FOR ELEMENT # ',I10)
 9511 FORMAT(/,' ===> ERROR !! ZERO ROUGHNESS FOR ELEMENT # ',A10)
 9520 FORMAT(/,' ===> ERROR !! NO GEOM3 ELEMENT IS SPECIFIED FOR ELEMENT
     + # ',I10)
Cwch, 8/29/00.  Add to this message.
 9521 FORMAT(/,' ===> ERROR !! NO KGEOM VALUE IS SPECIFIED FOR ELEMENT N
     +O. ',A10,/,
     +' ALPHANUMERIC ID (KGEOM) IS NEEDED FOR FLOW DIVIDER.',/,
     +' GEOM3 MAY NOT BE USED FOR THIS DATA ENTRY.')
 9560 FORMAT(/,' ===> ERROR !! NO MATCH FOUND IN E1 DATA GROUP FOR ELEME
     +NT # ',I10,' ON DATA GROUP J1.')
 9561 FORMAT(/,' ===> ERROR !! NO MATCH FOUND IN E1 DATA GROUP FOR ELEME
     +NT # ',A10,' ON DATA GROUP J1.')
 9570 FORMAT(/,' ===> ERROR !! NO MATCH FOUND IN E1 DATA GROUP FOR ELEME
     +NT # ',I10,' ON DATA GROUP J2.')
 9571 FORMAT(/,' ===> ERROR !! NO MATCH FOUND IN E1 DATA GROUP FOR ELEME
     +NT # ',A10,' ON DATA GROUP J2.')
 9580 FORMAT(/,' ===> ERROR !! NO MATCH FOUND IN E1 DATA GROUP FOR ELEME
     +NT # ',I10,' ON DATA GROUP I2.')
 9581 FORMAT(/,' ===> ERROR !! NO MATCH FOUND IN E1 DATA GROUP FOR ELEME
     +NT # ',A10,' ON DATA GROUP I2.')
Cwch, 4/15/02.
94010 FORMAT(/,' >>Error message is about to appear regarding E1 lines.'
     1,/,      ' Probable cause is missing value of KGEOM at end of line
	2.',/,    ' This is required with $ANUM. See documentation for E1 l
     3ines.')
C=======================================================================
      END
