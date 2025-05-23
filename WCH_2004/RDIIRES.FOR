      SUBROUTINE RDIIRES
C     RUNOFF BLOCK
C     CALLED BY HYDRO
C ======================================================================
C     COMPUTES RDII RESPONSE CURVES FROM RAINFALL
C     WRITES TO SCRATCH FILE(8) - NEW FOR RUNOFF
C
C     WRITTEN BY CHUCK MOORE, CDM, 8/93.
C     EDITED FOR STYLE BY WCH, 8/93
C     WCH, 1/4/94.  CORRECTIONS TO RAINFALL TOTAL AND PLACEMENT OF
C       UHs ON FILE FOR CASE WHEN RAIN ENDS BEFORE SIMULATION ENDS.
C     WCH, 8/4/95.  ALTER IOSTAT FOR LAHEY.
C     CIM 9/8/00 Change for metric version.  In this version rainfall
C       remains in mm and area is hectares.  Conversions are made to
C       compute flows in cfs.
C       Also changed JSTA to character*8 to match other programs.
C       Change time step comparison to not be absolutely equal.
C       Increase array size.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'RDII.INC'
      PARAMETER(MAXARRAY=1000)
      DIMENSION RMAX(NTK,3,12),QPEAK(NTK,3,12),TTKMAX(NTK,3,12),
     1 ISHIFT(NTK,3,12)
      DIMENSION ARRAY(NTK,3,MAXARRAY)
CIM INCREASE HYETOGRAPHS  ~~~~~~~~~
CIM CHANGE JSTA TO CHARACTER*8 TO MATCH CHANGES IN RAINBLOCK AND ELSEWHERE  8/10/00
C      DIMENSION REIN(MAXRG),JSTA(MAXRG),BAIN(MAXRG)
      DIMENSION REIN(MAXRG),BAIN(MAXRG)
      CHARACTER*8 JSTA(MAXRG)
cim      DIMENSION REIN(10),JSTA(10),BAIN(10)
cim ~~~~~~~~~~
      DIMENSION EXCESS(NTK,3),SUMEX(NTK,3)
      DOUBLE PRECISION SUMEX
      LOGICAL  INZERO
      CHARACTER*10 PRT(4,2)
      DATA PRT /'cfs/ac-in ','in','in/day','ac',
     1          'cms/ha-mm ','mm','mm/day','ha'/
C=======================================================================
C  FIND MAXIMUM R VALUES
C=======================================================================
      INZERO = .TRUE.
      IULDAY  = JULDAY
      STIMDAY = TIMDAY
      TIME    = TZERO
      NNZERO  = 0
      SUMOFRS = 0.0
      CNEXCESS = 0.0
      ICOUNT=0
C#### WCH, 1/4/94.  NEW VARIABLE IFEND.
      IFEND   = 0
      DO 10 JJ = 1,12
      DO 10 J = 1,NTK
      DO 10 I = 1,3
      SUMEX(J,I)   = 0.0
      STORAGE(J,I,JJ) = AMIN1(DSTORE(J,I,JJ),STORAGE(J,I,JJ))
      STORAGE(J,I,JJ) = AMAX1(0.0,STORAGE(J,I,JJ))
      DREC(J,I,JJ)    = DREC(J,I,JJ) / (24.0*60.0*60.0)
      TTKMAX(J,I,JJ)  = 0.0
      RMAX(J,I,JJ)    = 0.0
      DO 10 K = 1,2
   10 IZERO(J,I,K) = .FALSE.
      RRMAX        = 0.0
      RDIIAREA     = 0.0
      DO 21 J=1,NOW
      IC = ICURVE(J)
      IF(IC.GT.0) THEN
           RDIIAREA = RDIIAREA + SEWAREA(J)
           DO 20 I=1,3
           DO 20 JJ=1,12
           RRMAX = AMAX1(RDIIR(J,I,JJ),RRMAX)
   20      RMAX(IC,I,JJ) = AMAX1(RDIIR(J,I,JJ),RMAX(IC,I,JJ))
           ENDIF
   21 CONTINUE
C=======================================================================
C  RRMAX IS INDICATOR FOR WHETHER I/I CALCULATIONS ARE PERFORMED.
C=======================================================================
      IF(RRMAX.EQ.0.0) RETURN
C=======================================================================
      WRITE(*,*)
      WRITE(*,*)  ' Computing RDII response curve from rainfall data.'
      WRITE(*,*)
C=======================================================================
C  CHECK FOR EXISTANCE OF SCRATCH FILE 8
C=======================================================================
      NRDII = NSCRAT(8)
      IF(NRDII.EQ.0) THEN
           WRITE(N6,*) ' SCRATCH UNIT NUMBER 8 (NSCRAT(8))',
     1 ' MUST BE NON-ZERO TO PERFORM RDII CALCULATIONS'
           WRITE(*,*) ' SCRATCH UNIT NUMBER 8 (NSCRAT(8))',
     1 ' MUST BE NON-ZERO TO PERFORM RDII CALCULATIONS'
           STOP
           ENDIF
C=======================================================================
      TKMAX = 0.0
      TMIN  = 999.0
      DO 25 J=1,NNTK
      DO 25 I=1,3
      DO 25 JJ=1,12
      IF(RDIIT(J,I,JJ).GT.0.0) TMIN = AMIN1(RDIIT(J,I,JJ),TMIN)
      TTKMAX(J,I,JJ) = AMAX1(RDIIT(J,I,JJ)*(1.0+RDIIK(J,I,JJ)),
     2 TTKMAX(J,I,JJ))
   25 TKMAX = AMAX1(RDIIT(J,I,JJ)*(1.0+RDIIK(J,I,JJ)),TKMAX)
C
      TSTEPS  = TSTEP*60.0*60.0
      TSTEP2S = TSTEP2*60.0*60.0
      WRITE(N6,6010) TSTEP,TSTEP2
C=======================================================================
C  CHECK MAXARRAY
C=======================================================================
      NARRAY = INT(TKMAX/TSTEP2)+IRATIOS+1
      DO 32 J=1,NNTK
      DO 32 I=1,3
      DO 32 JJ=1,12
      ISHIFT(J,I,JJ) = INT(TTKMAX(J,I,JJ)/TSTEP2)+IRATIOS+1
   32 CONTINUE
      IF(NARRAY.GT.MAXARRAY) THEN
           WRITE(N6,6000) NARRAY
           STOP
           ENDIF
      DO 35 K = 1,NNTK
      DO 35 I = 1,3
      DO 35 M = 1,MAXARRAY
   35 ARRAY(K,I,M) = 0.0
C=======================================================================
C  COMPUTE QPEAK/(AIR)  THIS IS UNIT QPEAK TO BE MULTIPLIED LATER BY
C    AREA, RAINFALL, AND R FACTOR TO COMPUTE PEAK FLOW
C=======================================================================
      DO 40 J = 1,NNTK
      DO 40 I = 1,3
      DO 40 JJ= 1,12
C=======================================================================
C  RAIN (INCHES)  AREA (ACRES)  T (HOURS)  Q (CFS)
C  AREA OF TRIANGULAR UH IS 1 AC-IN. = 1/12 AC-FT.
C=======================================================================
      IF(RDIIT(J,I,JJ).GT.0.0) THEN
           QPEAK(J,I,JJ) = (2.0*43560.0/12.0)/
     +       (RDIIT(J,I,JJ)*(1.0+RDIIK(J,I,JJ))*60.0*60.0)
           ELSE
           IF(RMAX(J,I,JJ).NE.0.0) THEN
                WRITE(N6,*) ' T CANNOT EQUAL ZERO WHEN R IS NON-ZERO'
                WRITE(N6,*) ' RESPONSE CURVE = ',J,' TRIANGLE = ',I
                WRITE(N6,*) ' MONTH = ',JJ
                STOP
                ENDIF
           ENDIF
   40 CONTINUE
C=======================================================================
C  PRINT DATA FROM F4 LINES.
C=======================================================================
      WRITE(N6,6016) PRT(1,METRIC),PRT(2,METRIC),PRT(2,METRIC),
     1               PRT(3,METRIC)
      WRITE(N6,6017) PRT(1,METRIC),PRT(2,METRIC),PRT(2,METRIC),
     1               PRT(3,METRIC)
      DO 50 J = 1,NNTK
      DO 50 JJ= 1,12
      DO 50 I = 1,3
C=======================================================================
C  CFS/AC-IN / 362.9956 = CMS/HA-MM
C=======================================================================
cim### 9/8/00
      IF(METRIC.EQ.2) THEN
      QPP = QPEAK(J,I,JJ)/362.9956
      else
      QPP = QPEAK(J,I,JJ)
      endif
      IF(I.EQ.1) WRITE(N6,6019) J,NRDHYET(J),JJ,I,RDIIT(J,I,JJ),
     1 RDIIK(J,I,JJ),TTKMAX(J,I,JJ),QPP,DSTORE(J,I,JJ),STORAGE(J,I,JJ),
     2 DREC(J,I,JJ)*60.0*60.0*24.0
      IF(I.GT.1) WRITE(N6,6020) I,RDIIT(J,I,JJ),RDIIK(J,I,JJ),
     1 TTKMAX(J,I,JJ),QPP,DSTORE(J,I,JJ),STORAGE(J,I,JJ),
     2 DREC(J,I,JJ)*60.0*60.0*24.0
C=======================================================================
C  CONVERT DEPTH PARAMETERS FROM METRIC (MM) TO U.S. (IN.)
C=======================================================================
      IF(METRIC.EQ.2) THEN
CIM### 9/8/00
CIM these remain in metric
c           DSTORE(J,I,JJ)  = DSTORE(J,I,JJ)/25.4
c           STORAGE(J,I,JJ) = STORAGE(J,I,JJ)/25.4
c           DREC(J,I,JJ)    = DREC(J,I,JJ)/25.4
C  conversion from cfs/acre-in to cfs/ha-mm for subsequent calculations
           QPEAK(J,I,JJ) = QPEAK(J,I,JJ)*2.471/25.4
           ENDIF
   50 CONTINUE
C=======================================================================
C  PRINT SUBCATCHMENT DATA FROM H5 LINES.
C=======================================================================
      WRITE(N6,7000) PRT(4,METRIC)
      DO 60 J = 1,NOW
      IC = ICURVE(J)
      IF(IC.GT.0) THEN
cim### 9/8/00 leave as hectares
           SEWW = SEWAREA(J)
c           IF(METRIC.EQ.2) SEWW = SEWW/2.471
           IF(JCE.EQ.0) WRITE(N6,7010) J,NAMEW(J),IC,SEWW,
     1                             (JJ,(RDIIR(J,I,JJ),I=1,3),JJ=1,12)
           IF(JCE.EQ.1) WRITE(N6,7011) J,KAMEW(J),IC,SEWW,
     1                             (JJ,(RDIIR(J,I,JJ),I=1,3),JJ=1,12)
           ENDIF
   60 CONTINUE
      SEWW = RDIIAREA
cim### 9/8/00 leave as hectares
c      IF(METRIC.EQ.2) SEWW = SEWW/2.471
      WRITE(N6,7015) SEWW,PRT(4,METRIC)
C=======================================================================
C  IF IIRDII = 1, USE PREVIOUSLY CALCULATED RESPONSE CURVE.
C  SKIP UNIT HYDROGRAPH CALCULATIONS.
C=======================================================================
      IF(IIRDII.EQ.1) THEN
           WRITE(N6,7050) NRDII,FFNAME(58)
           GO TO 210
           ENDIF
C=======================================================================
C  READ RAINFALL TAPE
C=======================================================================
      NREIN = NSCRAT(1)
      REWIND NREIN
      READ(NREIN,END=777,IOSTAT=IOSS,ERR=777) NSTA,MRAIN,
     1              (JSTA(I),I=1,NSTA)
      WRITE(N6,2115)  NSTA
      WRITE(N6,2120) (I,JSTA(I),I=1,NSTA)
      READ(NREIN,END=210,IOSTAT=IOSS,ERR=777) JDAY,TMDAY,THISTO,
     1                                    (BAIN(J),J=1,NSTA)
      JYEAR = JDAY/1000
      IF (JYEAR.LT.100) THEN
      JDAY = JDAY-JYEAR*1000
      JYEAR = JYEAR + 1900
      JDAY = JDAY + JYEAR*1000
      ENDIF
cim ### 9/8/00  change to have a tolerance of .5 minutes
c      IF(THISTO.NE.TSTEPS) THEN
      IF(ABS(THISTO-TSTEPS).GT.30.0) THEN
           WRITE(N6,*) ' ERROR - RAINFALL TIME STEP IN RAINFALL FILE ',
     1       'DOES NOT EQUAL RAINFALL TIME STEP SET ON F3 CARD'
           WRITE(N6,*)  'THISTO = ',THISTO,' Seconds'
           WRITE(N6,*)  'TSTEPS = ',TSTEPS,' Seconds'
           STOP
           ENDIF
      JOLDAY  = JDAY
      TOMDAY  = TMDAY
      JODAY   = JDAY
      TODAY   = TMDAY
      TOHISTO = THISTO
      DO  98 J = 1,NSTA
   98 REIN(J)  = BAIN(J)
C#### WCH, 1/3/94.  DON'T BACKSPACE OR GET EXTRA RAIN AFTER FIRST
C                   TIME STEP.
C####      BACKSPACE(NREIN)
      JULDAY = JDAY
      TIMDAY = TMDAY
      CALL DATED
      WRITE(N6,272) MONTH,NDAY,NYEAR,JULDAY,TIMDAY,TIMDAY/3600.0
CIM  MONTHLY VALUES
cim here initialize storage(j,i,1) to initial value for starting month
cim of rainfall record
      DO 99 J = 1,NTK
      DO 99 I = 1,3
   99 STORAGE(J,I,1) = STORAGE(J,I,MONTH)
      REWIND NRDII
C=======================================================================
C  BEGIN LOOPING OVER RAINFALL USING TIME STEP EQUAL TO RAINFALL STEP
C=======================================================================
      CNRAIN = 0.0
  100 CONTINUE
      IF(JULDAY.GT.JDAY.OR.(JULDAY.EQ.JDAY.AND.TIMDAY.GE.TMDAY)) THEN
           JOLDAY  = JDAY
           TOMDAY  = TMDAY
           TOHISTO = THISTO
           DO 101 J = 1,NSTA
  101      REIN(J)  = BAIN(J)
C#### WCH, 1/4/94.  NEW END= LOCATION (151, NOT 210).
           READ(NREIN,END=151,IOSTAT=IOSS,ERR=777) JDAY,TMDAY,THISTO,
     +                                    (BAIN(J),J=1,NSTA)
      JYEAR = JDAY/1000
      IF (JYEAR.LT.100) THEN
      JDAY = JDAY-JYEAR*1000
      JYEAR = JYEAR + 1900
      JDAY = JDAY + JYEAR*1000
      ENDIF
cim### 9/8/00  change to have a tolerance of .5 minutes
      IF(ABS(THISTO-TSTEPS).GT.30.0) THEN
           WRITE(N6,*) ' ERROR - RAINFALL TIME STEP IN RAINFALL FILE ',
     1       'DOES NOT EQUAL RAINFALL TIME STEP SET ON F3 CARD'
           WRITE(N6,*)  'THISTO = ',THISTO,' Seconds'
           WRITE(N6,*)  'TSTEPS = ',TSTEPS,' Seconds'
           STOP
           ENDIF
c  write to 6 to get carriage control
           WRITE(6,104) JDAY
           ELSE
           CALL NTIME(JOLDAY,TOMDAY,DELTA)
           IF(ABS(DELTA).GT.(1.0-1.0/FLOAT(IRATIOS))*TOHISTO) THEN
                DO 105 J = 1,NSTA
  105           REIN(J) = 0.0
                ENDIF
           ENDIF
C=======================================================================
C COMPUTE RAINFALL EXCESS AMOUNTS AND ADJUST INITIAL ABSTRACTION STORAGE
C=======================================================================
      DO 550 J = 1,NNTK
      IF(NRDHYET(J).GT.0) THEN
           IF(REIN(NRDHYET(J)).GT.0.0) THEN
                RAINFALL = REIN(NRDHYET(J))*TSTEP
                CNRAIN   = CNRAIN + RAINFALL
                DO 540 I = 1,3
                EXCESS(J,I) = RAINFALL-STORAGE(J,I,1)
                EXCESS(J,I) = AMAX1(EXCESS(J,I),0.0)
                STORAGE(J,I,1) = STORAGE(J,I,1)-(RAINFALL-EXCESS(J,I))
                SUMEX(J,I) = SUMEX(J,I) + EXCESS(J,I)
  540           CONTINUE
                ELSE
C=======================================================================
C  STORAGE RECOVERS IF NOT RAINING
C=======================================================================
                DO 545 I = 1,3
                EXCESS(J,I) = 0.0
                STORAGE(J,I,1) = STORAGE(J,I,1)+DREC(J,I,MONTH)*TSTEPS
                STORAGE(J,I,1) = AMIN1(STORAGE(J,I,1),DSTORE(J,I,MONTH))
  545           CONTINUE
                ENDIF
           ENDIF
  550 CONTINUE
      ICOUNT=ICOUNT+1
      DO 360 JJ = 1,NOW
      JT = ICURVE(JJ)
      IF(JT.GT.0) THEN
      DO 350 I=1,3
      SUMOFRS = SUMOFRS+SEWAREA(JJ)*RDIIR(JJ,I,month)
      CNEXCESS = CNEXCESS + EXCESS(JT,I)*SEWAREA(JJ)*RDIIR(JJ,I,MONTH)
  350 CONTINUE
      ENDIF
  360 CONTINUE
C=======================================================================
C  LOOP OF TSTEP2
C  LOOP OVER UNIT HYDROGRAPHS TO COMPUTE ARRAY VALUES
C=======================================================================
      DO 150 J=1,NNTK
C=======================================================================
C  LOOP OVER TRIANGLES
C=======================================================================
      DO 140 I = 1,3
      IF(EXCESS(J,I).GT.0.0) THEN
           IF(RMAX(J,I,MONTH).GT.0.0) THEN
C======================================================================
C  LOOP OVER TIMESTEPS TO FILL ARRAY
C=======================================================================
                TRDII = 0.0
                DO 139 IT = 2,ISHIFT(J,I,MONTH)
                TRDII = TRDII + TSTEP2
                IF(TRDII.LE.RDIIT(J,I,MONTH)) THEN
                     FLOW = QPEAK(J,I,MONTH)*TRDII/RDIIT(J,I,MONTH)
     +               *EXCESS(J,I)
                     ELSE
                     RRATIO = 1.0 - (TRDII-RDIIT(J,I,MONTH))/
     +                              (RDIIT(J,I,MONTH)*RDIIK(J,I,MONTH))
                     RRATIO = AMAX1(RRATIO,0.0)
                     FLOW = QPEAK(J,I,MONTH)*RRATIO*EXCESS(J,I)
                     ENDIF
                ARRAY(J,I,IT) = ARRAY(J,I,IT)+FLOW
                IF(FLOW.LE.0.0) GO TO 140
  139           CONTINUE
                ENDIF
           ENDIF
  140 CONTINUE
  150 CONTINUE
C#######################################################################
C#### WCH, 1/4/93.
C  PROVIDE NEW CODE FOR SITUATION IN WHICH END OF RAINFALL IS REACHED
C  WELL BEFORE END OF SIMULATION.
C=======================================================================
      GO TO 153
  151 IFEND = 1
      IF(TIME.GE.(LONG+TZERO)) GO TO 210
  153 DO 200 JJJ = 1,IRATIOS
C=======================================================================
C  WRITE THIS FLOW TO FILE  (MOSTLY, WRITE JUST NON-ZERO VALUES).
C  USE ARRAY IZERO TO INDICATE PRESENCE OF NON-ZERO I/I EXCESS VALUE.
C  IZERO = .TRUE. ==> NON-ZERO EXCESS VALUE TO WRITE.
C  IZERO = .FALSE. => DON'T WRITE ZERO VALUE OF ARRAY().
C=======================================================================
      IF(NNZERO.GT.0) THEN
           IF(INZERO)
     +        WRITE(NRDII) JODAY,TODAY,((IZERO(J,I,2),I=1,3),J=1,NTK)
           INZERO = .FALSE.
           WRITE(NRDII) JULDAY,TIMDAY,((IZERO(J,I,1),I=1,3),J=1,NTK)
           ELSE
C=======================================================================
C  WRITE FIRST ZERO VALUE
C=======================================================================
           IF(.NOT.INZERO) THEN
             WRITE(NRDII) JULDAY,TIMDAY,((IZERO(J,I,1),I=1,3),J=1,NTK)
             INZERO = .TRUE.
             ENDIF
           ENDIF
C=======================================================================
C  SHIFT ARRAYS
C=======================================================================
C#### WCH, 1/4/93.  MOVE TIME CHANGES TO AFTER 160 LOOP FOR CONVENIENCE
C                   ONLY.
C
      NNZERO = 0
      DO 160 J = 1,NNTK
      DO 160 I = 1,3
C=======================================================================
C  ONLY STORE NON-ZERO I/I EXCESS VALUES (IZERO = .TRUE.).
C=======================================================================
      IF(IZERO(J,I,1)) WRITE(NRDII) ARRAY(J,I,1)
      DO 155 IT = 1,ISHIFT(J,I,MONTH)-1
  155 ARRAY(J,I,IT) = ARRAY(J,I,IT+1)
      IZERO(J,I,2)  = IZERO(J,I,1)
      IZERO(J,I,1)  = (ARRAY(J,I,1).NE.0.0)
      IF(IZERO(J,I,1)) NNZERO = NNZERO+1
  160 ARRAY(J,I,ISHIFT(J,I,MONTH)) = 0.0
C#### WCH, 1/4/94.  TIME CHANGES MOVED TO HERE.
      JODAY = JULDAY
      TODAY = TIMDAY
      CALL STIME(TSTEP2S)
      TIME   = TIME+TSTEP2S
  200 CONTINUE
C#######################################################################
C#### WCH, 1/4/94
C  CHECK TO BE SURE ARRAY() IS "FLUSHED" OF ALL NON-ZERO VALUES  AND
C  A LAST ZERO STORED ON FILE BEFORE QUITTING.
C=======================================================================
      IF(IFEND.EQ.1.AND.NNZERO.GT.0) GO TO 151
      IF(IFEND.EQ.1.AND.NNZERO.LE.0) THEN
           IFEND = 2
           GO TO 153
           ENDIF
      IF(IFEND.EQ.2) GO TO 210
C=======================================================================
C  READ NEXT RAINFALL
C=======================================================================
      IF(TIME.LT.(LONG+TZERO)) GO TO 100
  210 CONTINUE
C=======================================================================
C  APPROXIMATE AVERAGE RAIN BY DIVIDING TOTAL BY NUMBER OF UH SETS.
C=======================================================================
      CNRAIN = CNRAIN/DBLE(NNTK)
      JULDAY = IULDAY
      TIMDAY = STIMDAY
      CALL DATED
      REWIND NRDII
C=======================================================================
C  READ FIRST TWO VALUES SETTING UP INPUT FILE FOR RDIISHED
C=======================================================================
      CALL READNEXT
      CALL READNEXT
      DO 300 I = 1,3
 300  CNTRDII(I) = 0.0
      SUMOFRS=SUMOFRS/ICOUNT
      CNEXCESS = CNEXCESS/SUMOFRS
      RETURN
C=======================================================================
C#### WCH, 8/4/95.  ALTER IOSTAT FOR LAHEY.
 777  WRITE(N6,9777) MOD(IOSS,256)
      STOP
C=======================================================================
  104 FORMAT(1H+,' Reading rainfall Julian day',I8)
  272 FORMAT(/,
     1' ************************************************',/,
     2' *      INITIAL RAINFALL RECORD                 *',/,
     3' ************************************************',//,
     4' Date (Mo/Day/Year)   = ',T50,I2,'/',I2,'/',I4,/,
     6' Julian Date  =',T45,5X,I8,/,
     7' Time of day  =',T45,F13.0,' seconds.',/,
     8' Time of day  =',T45,F13.2,'   hours.',/)
 2115 FORMAT(//,
     +' ***************************************************',/,
     +' *  Precipitation Interface File Summary           *',/,
     +' *  Number of precipitation station....',I9,'   *',/,
     +' ***************************************************',/)
 2120 FORMAT(' Location Station Number',/,
     +       ' -------- --------------',/,
cim### 9/8/00 change rain id to character*8
c     +       10(I9,'. ',I13,/))
     +       10(I9,'. ',5X,A8,/))
 6000 FORMAT(' ERROR - ARRAY DIMENSIONS IN RDIIRES NOT SUFFICIENT TO',
     1' SIMULATE COMPLETE HYDROGRAPH.',/,' EITHER DECREASE TP(1+K) OR ',
     2'CHANGE PARAMETER STATEMENT IN RDIIRES.',/,
     3' DIMENSION REQUIRED FOR THIS SIMULATION = ',I5)
 6010 FORMAT('1 RAINFALL DEPENDENT INFILTRATION/INFLOW (RDII) CALCULATIO
     1NS',/,' DATA FROM F3 AND F4 LINES FOR COMPUTATION OF UNIT HYDROGRA
     2PH RESPONSE:',//
     3 '  TSTEP  = RAINFALL INCREMENT (MUST = HYETOGRAPH TIME INTERVAL,'
     4,/,12X,'THISTO)   =',F10.3,' HOURS',/,
     5 '  TSTEP2 = TIME STEP FOR UH RESPONSE (MUST BE INTEGER FRACTION'
     6,/,12X,'OF TSTEP) =',F10.3,' HOURS')
 6015 FORMAT(//,' LINE F4 DATA INPUTS FOR TRIANGULAR UNIT HYDROGRAPH NUM
     1BER ',I2)
 6016 FORMAT(/,
     1'  Qpeak   = Peak RDII flow produced by 1 unit of rainfall excess
     2',/,13X,'over 1 area unit, ',A10,'.',/,
     3'  DSTORE  = Maximum depression storage to be satified before I/I
     4begins, ',A2,'.',/,
     5'  STORAGE = Initial depression storage, ',A2,'.',/,
     6'  DREC    = DSTORE recovery rate when not raining, ',A6,'.',/,
     7'  TP      = Time to peak, hr.',/,
     8'  K       = Ratio, recession time to TP.',/,
     9'  TB      = Time base = TP*(1+K), hr.')
 6017 FORMAT(/,
     1'   UH   HYET MONTH TRIANG. TP     K      TB    Qpeak  DSTORE',
     2'  STORAGE   DREC',/,
C                                              cfs/ac-in  in       in    in/day
     3' SET NO. NO.         NO.   hr            hr  ',A10,1X,A2,7X,A2,
     4 4X,A6,/,
     5' ------ ---- ----- ----- ------ ------ ------ -------- -------',
     6' ------- -------')
C          1     1      2   3.00   2.00   9.00  33.86877  0.275   0.376   0.084
 6019 FORMAT(/,3I5,I7,3F7.2,F10.5,F7.3,2F8.3)
 6020 FORMAT(I22,3F7.2,F10.5,F7.3,2F8.3)
C
 7000 FORMAT(//,' SUBCATCHMENT PARAMETERS FROM H5 DATA LINES',//,
     1' SUBCATCHMENT  USE  SEWERED MONTH RATIOS FOR TRIANGLE NOS.',/,
     2' SEQ.     ID    UH    AREA             1       2       3',/,
     3'  NO.     NO.   NO.    ',A2,/,
     4'----- ------- ----- -------- ----- ------- ------- -------')
C         4     977     3   4568.93   0.3333  0.3333  0.3333
 7010 FORMAT(I4,1X,I10,1X,I6,F10.2,I5,F9.4,2F8.4,
     1       11(/,32X,I5,F9.4,2F8.4))
 7011 FORMAT(I4,1X,A10,1X,I6,F10.2,I5,F9.4,2F8.4,
     1       11(/,32X,I5,F9.4,2F8.4))
 7015 FORMAT(19X,'---------',/,' TOT. SEWERED AREA =',F8.2,1X,A2)
 7050 FORMAT(/,' PARAMETER IIRDII = 1.  SKIP UH RESPONSE CURVE CALCULATI
     1ONS.',/,' USE RESPONSE HYDROGRAPH STORED ON UNIT',I3,', DOS FILE '
     2,A10,/,' DATES ON FILE MUST OVERLAP DATES REQUIRED FOR THIS SIMULA
     3TION')
 9777 FORMAT(/,' ===> Error !!  Reading precipitation time history',
     1' on the NSCRAT(1) file.',/,' IOSTAT =',I5,' Run stopped from Subr
     2outine RDIIRES')
      END
