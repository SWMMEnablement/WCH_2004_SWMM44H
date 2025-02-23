```fortran 
      SUBROUTINE WSHED(REIN,KWIK)
C     RUNOFF BLOCK
C     CALLED BY HYDRO NEAR LINE 349
C=======================================================================
C     WSHED last updated December, 1990 by Bob Dickinson
C     Green-Ampt infiltration option added Feb 79 by Russell G. Mein.
C
C     This subroutine computes the instantaneous water depth
C          and flow rate for the watershed areas.
C=======================================================================
C      UPDATED 11/92 BY WCH TO READ EVAP DATA FROM NSCRAT(3) WHEN IVAP=4
C      UPDATED 3/93 BY RED TO CORRECT FOR RETURNING INFILTRATION FROM
C        SUBROUTINE GROUND.
C      UPDATED 8/93 BY CHUCK MOORE, CDM, TO ADD SUBCATCHMENT STATISTICS.
C      METRIC FIX AND NEW IF-STMT, 9/23/93.  WCH (RED).
C      OPTION FOR NO EVAPORATION DURING RAINY TIME STEP, WCH (CDM,
C        CHUCK MOORE), 10/5/93.
C      CORRECTION FOR MORE THAN ONE AIR TEMPERATURE PER DAY, RED (WCH),
C        1/31/94.
C      INITIALIZE VARIABLE  KWIKSN  TO INDICATE PRESENCE OF SNOW IN
C        ORDER TO USE  WET  TIME STEP, WCH, 4/7/94.
C      CHANGE LOCATION OF CHECK FOR GROUNDWATER FLOW, WCH, 4/7/94.
C      CHECK TO SEE IF K=1 CALCS HAVE BEEN MADE FOR USE BY K=3 SUBAREA,
C        WCH, 5/25/94.
C      SEVERAL CHANGES TO ALLOW FOR REROUTING OF OVERLAND FLOW 
C        INTERNALLY WITHIN A SUBCATCHMENT AND FROM SUBCATCHMENT TO
C        SUBCATCHMENT, WCH, 12/20/00.
C      SAVE RAINFALL + MELT (RFLOW) FOR EACH SUBCATCHMENT FOR SUBCAT TO 
C        SUBCAT FLOWS AND FOR EROSION CALCULATIONS, WCH, 5/8/01. 
C      CHECK FOR ZERO PERV AND IMPERV AREAS DURING OVERLAND FLOW
C        REROUTING.  WCH, 6/7/01.
C      FIX METRIC CONVERSION FOR GROUNDWATER, 3/1/02. WCH FROM SANDY
C        ELLIOT AND BOB DICKINSON. 
C      ADD SUBCAT CONTINUITY SUM FOR RLOSS FOR PERVIOUS AREAS TO ACCOUNT  
C        FOR SUBCAT TO SUBCAT ROUTING AND OTHER FIX. 3/28/02. WCH
C      STREAMLINE LOSS CALC WHEN SUBCAT GOES DRY. 4/11/02. WCH
C      ADD FLOW2SUB(J) TO DRY CHECK FOR ALL SUBAREAS. 4/11/02. WCH
C      ADD ADDITIONAL GROUNDWATER PRINTS PLUS CORRECTIONS. 7/1/03. WCH
C      DELETE IF STMT THAT SKIPS SOME CONTINUITY SUMMATIONS, 8/8/03, WCH
C      DOUBLE PRECISION FIX, 7/22/04, WCH. 
C#######################################################################
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'QUALTY.INC'
      INCLUDE 'GRWTR.INC'
C#### C. MOORE, CDM, 8/93
      INCLUDE 'RUNSTAT.INC'
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CIM   MAXINF  C.Moore B. Cunningham CDM
      INCLUDE 'MAXINF.INC'
Cwch, 12/20/00
      INCLUDE 'OVERLAND.INC'
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CIM INCREASE HYETOGRAPHS   ~~~~~
      DIMENSION IDXSNO(4),TP(NW),REIN(MAXRG)
CIM      DIMENSION IDXSNO(4),TP(NW),REIN(10)
CIM  ~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DIMENSION GWFLWB(NGW),STPOLL(2,NGW)
      CHARACTER*3 CTYPE
      DATA IDXSNO/3,2,3,1/
C=======================================================================
C#######################################################################
C WCH, 11/92  ADD OPTION FOR IVAP=4 TO READ TEMP BLOCK OUTPUT
C               FOR EVAPORATION DATA
C#######################################################################
      IF((ISNOW.EQ.2.OR.IVAP.EQ.4).AND.MTIME.EQ.1) THEN
               IF(NSCRAT(3).EQ.0) CALL ERROR(20)
C### WCH 11/92
               REWIND NSCRAT(3)
C###
               READ(NSCRAT(3)) ISTA
               WRITE(N6,9200) ISTA
 5             READ(NSCRAT(3)) JTDAY,TMAX,TMIN,EVAPOR,WINDER
               JYEAR = JTDAY / 1000
               IF (JYEAR.LT.100) THEN
                    JTDAY = JTDAY - JYEAR*1000
                    JYEAR = JYEAR + 1900
                    JTDAY = JTDAY + JYEAR*1000
                    ENDIF
C#### WCH (RED), 9/93.  METRIC FIX.
               EVAPOR = EVAPOR/CMET(7,METRIC)
               TMAX1 = TMAX
               IF(JTDAY.LT.JULDAY) GO TO 5
               ENDIF
C=======================================================================
      IF(MTIME.EQ.1) THEN
                     JDREF   = JULDAY - 1
                     RINE    = 0.0
                     XSINFL  = 0.0
                     WINDER  = 0.0
                     DO 10 N = 1,NW
   10                TP(N)   = 0.0
                                    KKK = 3
                     IF(ISNOW.GE.1) KKK = 4
                     ENDIF
C=======================================================================
      IF(ISNOW.EQ.1) THEN
                     INDT = (TIME-TZERO)/(DTAIR*3600.0)
                     IF(INDT.LT.1) INDT = 1
Cwch, 1/23/04.  Need "end of array" error message here.
                     IF(INDT.GT.NAIRT) INDT = NAIRT
                     TA = TAIR(INDT)
                     ENDIF
C=======================================================================
C     Here, compute melt factors for different days.
C     Assume sinusoidal variation between min on Dec 21 &
C     Max on June 21.  Compute only when starting a new day.
C     The number 0.0172615 = PI/182.
C=======================================================================
C####################
C WCH 11/92  CHANGE TO READ NSCRAT3 FOR EVAP ALONE
C####################
      IF((ISNOW.EQ.2.OR.IVAP.EQ.4).AND.JTDAY.LT.JULDAY) THEN
               TMAX1 = TMAX
Cwch, 1/23/04.  Need end of file error message here!
               READ(NSCRAT(3)) JTDAY,TMAX,TMIN,EVAPOR,WINDER
               JYEAR = JTDAY / 1000
               IF (JYEAR.LT.100) THEN
                    JTDAY = JTDAY - JYEAR*1000
                    JYEAR = JYEAR + 1900
                    JTDAY = JTDAY + JYEAR*1000
                    ENDIF
C#### WCH (RED), 9/93.  METRIC FIX.
               EVAPOR = EVAPOR/CMET(7,METRIC)
               ENDIF
C=======================================================================
      IF(ISNOW.GT.1.AND.JULDAY.GT.JDREF) THEN
                     JDREF   = JULDAY
                     MDAY    = JULDAY - 1000*(JULDAY/1000)
                     DAYNO   = FLOAT(MDAY)
                     SEASON  = SIN(0.0172615*(DAYNO-81.0))
                     DO 80 N = 1,NOW
                     DO 80 I = 1,3
   80                DH(I,N) = DHMAX(I,N)+DHMIN(I,N)*SEASON
C=======================================================================
C******** COMPUTE HOUR OF DAY OF SUNRISE AND SUNSET.
C******** THEN MIN. TEMP. IS ASSUMED TO OCCUR AT SUNRISE.
C******** MAX. TEMP. IS ASSUMED TO OCCUR AT SUNSET MINUS THREE HOURS.
C
C******** COMPUTE EARTH'S DECLINATION.
C******** THE NUMBER 0.40928 = 23.45 (DEGREES) * PI / 180.
C******** THE NUMBER 0.017202 = 2 PI / 365.
C
C******** COMPUTE THE HOUR ANGLE OF SUNRISE/SUNSET.
C******** THE NUMBER 3.8197 = 12 / PI.
C
C******** COMPUTE HOURLY TEMPERATURE USING SINSUOIDAL INTERPOLATION.
C=======================================================================
                     DECL  = 0.40928*COS(0.017202*(172.0-DAYNO))
                     HRANG = 3.8197*ACOS(-TAN(DECL)*ANGLAT)
                     HRSR  = 12.0 - HRANG + DTLONG + 0.5
                     HRSS  = 12.0 + HRANG + DTLONG - 2.5
                     DHRDY =  HRSR - HRSS
                     DYDIF =  24.0 + HRSR - HRSS
                     HRDAY = (HRSR + HRSS)/2.0
                     TAVE  = (TMAX + TMIN) / 2.0
                     TRNG  = (TMAX - TMIN) / 2.0
                     TRNG1 = (TMAX1 - TMIN)
                     ENDIF
C#### RED (WCH), 1/31/94.  SHOULD HAVE ENDIF HERE, OTHERWISE
C####   ONLY ONE AIR TEMPERATURE PER DAY.  BUT ALSO NEED ADDITIONAL
C####   IF-STMT HERE TO PERFORM FOLLOWING ONLY FOR ISNOW > 1.
      IF(ISNOW.GT.1) THEN
                     HR    = TIMDAY/3600.0
                     IF(HR.LT.HRSR) TA = TMIN +
     +                  TRNG1 * SIN(3.14159 / DYDIF * (HRSR - HR))
                     IF(HR.GE.HRSR.AND.HR.LE.HRSS) TA = TAVE
     +                 + TRNG * SIN(3.14159 / DHRDY * (HRDAY - HR))
                     IF(HR.GT.HRSS) TA  = TMAX -
     +                  TRNG * SIN(3.14159 / DYDIF *(HR - HRSS))
                     ENDIF
C=======================================================================
C     Do prior to main loop for WSHED.
C=======================================================================
      RI       = 0.0
      RIN      = 0.0
      IF(IVAP.LE.2) EVAP = VAP(MONTH)
      IF(IVAP.EQ.3) THEN
                    INDX = 12*(NYEAR - NVAP(1)) + MONTH
                    IF(INDX.LE.0.OR.INDX.GT.NVAP(2)) CALL ERROR(153)
                    EVAP = VAP(INDX)
                    ENDIF
      IF(IVAP.EQ.4) EVAP = EVAPOR
                        WINDY  = WINDER
      IF(WINDER.EQ.0.0) WINDY  = WIND(MONTH)
      KWIK     = 0
      KOMPUT   = 0
C#### WCH, 4/7/94.  USE ONLY VARIABLE KWIKSN FOR SNOW.
C#### WCH (RED), 9/93.  ADD IF-STMT.
C####      IF(ISNOW.EQ.1) KWIK = 1
C#### WCH, 4/7/94.  INITIALIZE SNOW INDICATOR VARIABLE.
      KWIKSN   = 0
C=======================================================================
Cwch, 12/20/00.  Must compute flows from other subcatchments *before*
C    main loop since WFLOW() set back to zero inside loop.
C=======================================================================
C	Check for inflow from another subcatchment(s).  Distribute
C     uniformly over all subcatchment subareas.
C=======================================================================
	IF(ISC2SC.EQ.1) THEN
		DO 150 J = 1,NOW
		FLOW2SUB(J) = 0.0
		DO JJ = 1,NCP
			IF(NWTOW(JJ,J).EQ.0) GO TO 145
			JJJ = NWTOW(JJ,J)
			FLOW2SUB(J) = FLOW2SUB(J) + WFLOW(JJJ)
			ENDDO
C  Convert from flow to flow per unit area (ft/sec).
  145		IF(FLOW2SUB(J).GT.0.0.AND.WAREA(J).GT.0.0) 
     +       FLOW2SUB(J) = FLOW2SUB(J)/WAREA(J)
  150		CONTINUE
		ENDIF
Cwch, 7/1/03. Set up GW output index.
      LU = 0
C=======================================================================
C    Main loop on subcatchments for WSHED calculations.  
C=======================================================================
      DO 390 J = 1,NOW
      NGAG     = NHYET(J)
      RI       = REIN(NGAG)/CMET(4,METRIC)
      IF(ISNOW.EQ.2.AND.TA.LE.SNOTMP)  RI = -RI * SCF
      IF(ISNOW.EQ.1.AND.NGAG.GT.1) KOMPUT = 0
C#######################################################################
C     WCH (CDM, Chuck Moore), 10/5/93.  If IIVAP > 0 then use zero
C     evaporation during time steps with precipitation.
C     IF ABS(IIVAP) = 1 evap is allowed from channels.
C     IF ABS(IIVAP) = 2 evap is not allowed from channels.
C     SVAP = evaporation from this subcatchment that will be set to
C     zero if it is raining or snowing.
C     GVAP = evaporation from channel surfaces that will be set to
C     zero if there is rain or snow over any subcatchment.
C===> NOTE! EVAP changed to SVAP from here to end of subroutine (5
C     locations) and to SVAP or GVAP in subroutines GAMP, HORTON,
C     GROUND, and GUTTER.
C#######################################################################
C#### WCH (CDM), 10/5/93.  NEW PARAMS FOR NO EVAP DURING RAIN.
      IF(IIVAP.GT.0) THEN
           IF(RI.LE.0.0) THEN
                SVAP = EVAP
                GVAP = EVAP
                IF (IIVAP.EQ.2) GVAP = 0.0
                ELSE
                SVAP = 0.0
                GVAP = 0.0
                ENDIF
            ELSE
            SVAP = EVAP
            GVAP = EVAP
            IF (IIVAP.EQ.-2) GVAP = 0.0
           ENDIF
C
      RIN      = RI
      RINE     = 0.0
      IF(RIN.GT.0.0) RINE = RIN
C=======================================================================
Cwch, 12/20/00  Initialize for subcat to subcat flows. 
C    Initialize for every time step.
C=======================================================================
      FL2PV       = FLOW2PV(J)
	FLOW2PV(J)  = 0.0
	FL2IMP      = FLOW2IMP(J)
	FLOW2IMP(J) = 0.0
C     RFLOW will be rainfall over each subcat, as a flow rate (cfs). 
	RFLOW(J)     = 0.0
C
      WFLOW(J) = 0.0
      IF(WAREA(J).EQ.0.0) GO TO 390
C=======================================================================
C     Begin loop on 3 or 4 subareas within each subcatchment.
C
C     Subscripts for different areas ---
C
C     K=1 -- IMPERVIOUS AREA WITH DEPRESSION STORAGE.  NORMALLY BARE,
C            BUT MAY HAVE SNOW FOR ISNOW=2.
C     K=2 -- PERVIOUS AREA WITH DEPRESSION STORAGE.  SNOW COVER VARIES
C            ACCORDING TO AREAL DEPLETION CURVE FOR ISNOW=2 AND IS CONSTANT
C            FOR ISNOW=1.
C     K=3 -- IMPERVIOUS AREA WITH ZERO DEPRESSION STORAGE.  SNOW COVER
C            IS SAME AS FOR K=1.
C     K=4 -- IMPERVIOUS AREA WITH DEPRESSION STORAGE.  USED ONLY WHEN
C            SNOW CALCS ARE MADE.  SNOW COVER VARIES ACCORDING TO AREAL
C            DEPLETION CURVE FOR ISNOW=2 AND IS 100% FOR ISNOW=1.
C
C     SUBSCRIPTS FOR SNOW VARIABLES ---
C        FOR K =  1 II = 3
C            K =  2 II = 2
C            K =  3 II = 3 (SAME SNOW PACK AS K=1)
C            K =  4 II = 1
C=======================================================================
C#### WCH, 5/25/94.  INDICATOR VARIABLE TO SEE IF K=1 IS USED.
      K3CALC = 0
      DO 380 K = 1,KKK
      IF(WAR(K,J).LT.0.01) GO TO 380
Cwch, 12/20/00
      FINF   = 0.0
      WFLO   = 0.0
      SMIMED = 0.0
      ASC    = 0.0
      II     = IDXSNO(K)
                 RLOSS  = 0.0
      IF(K.NE.2) RLOSS  = SVAP
C=======================================================================
C     Treat melt in same manner as rain on area.
C     SMIMED = immediate melt.
C     Start out with:
C     RI = RIN = precip, positive for rain, negative for snow.
C     RINE   = rain, if raining, else zero if snow.
C     (Note, RI is in COMMON "Detail.inc")
C     End with:
C     RI     = melt, after routing melt through snowpack. 
C     RINE   = melted precip on bare area or = rain, if not snowing.
C=======================================================================
      IF(K.EQ.1) THEN
C#### WCH, 5/25/94. NEED K=1 CALCS FOR K=3.  CHECK IF CALCULATED.
                 K3CALC = 1
                 IF(ISNOW.GT.0) CALL SNOW(TA,SMIMED,J,K,II,RIN,RINE,
     +                                    ASC,WINDY,KOMPUT)
C=======================================================================
C     This weighted average to get mix of contribution from 
                 RI     = RI*ASC + (1.0-ASC)*RINE + SMIMED
                 RISAVE = RI
                 DOI    = WDEPTH(K,J) - WSTORE(1,J)
                 WSTOR  = WSTORE(1,J)
Cwch, 4/11/02.  Add FLOW2SUB
                 IF(WDEPTH(K,J).LE.0.0.AND.RI.LE.0.0.AND.
     1              FL2IMP.LE.0.0.AND.FLOW2SUB(J).LE.0.0) GO TO 350
                 ENDIF
C=======================================================================
      IF(K.EQ.3) THEN
C#### WCH, 5/25/94. BE SURE THAT K=1 CALCS HAVE BEEN MADE!
                 IF(K3CALC.EQ.0) THEN
                      IF(ISNOW.GT.0) CALL SNOW(TA,SMIMED,J,K,II,RIN,
     +                                    RINE,ASC,WINDY,KOMPUT)
                      RI     = RI*ASC + (1.0-ASC)*RINE + SMIMED
                      RISAVE = RI
                      ENDIF
                 RI     = RISAVE
                 DOI    = WDEPTH(K,J)
                 WSTOR  = 0.0
Cwch, 4/11/02.  Add FLOW2SUB
                 IF(WDEPTH(K,J).LE.0.0.AND.RI.LE.0.0.AND.
     1              FLOW2SUB(J).LE.0.0) GO TO 350
                 ENDIF
C=======================================================================
      IF(K.EQ.4) THEN
                 IF(ISNOW.GT.0) CALL SNOW(TA,SMIMED,J,K,II,RIN,RINE,
     +                                    ASC,WINDY,KOMPUT)
                 RI     = RI*ASC + (1.0-ASC)*RINE + SMIMED
                 DOI    = WDEPTH(K,J) - WSTORE(1,J)
                 WSTOR  = WSTORE(1,J)
Cwch, 4/11/02.  Add FLOW2SUB
                 IF(WDEPTH(K,J).LE.0.0.AND.RI.LE.0.0.AND.
     1              FLOW2SUB(J).LE.0.0) GO TO 350
                 ENDIF
C=======================================================================
C     K = 2 Calls infiltration routines for pervious areas.
C                 Treat melt in same manner as rain on area.
C     Note, when return from infiltration subroutines, RLOSS = 
C       actual evap + infiltration loss.  Must subtract
C       evaporation to get infiltration alone. 
C=======================================================================
      IF(K.EQ.2) THEN
                 IF(ISNOW.GT.0) CALL SNOW(TA,SMIMED,J,K,II,RIN,RINE,
     +                                    ASC,WINDY,KOMPUT)
                 RI     = RI*ASC + (1.0-ASC)*RINE + SMIMED
                 DOI   = WDEPTH(K,J) - WSTORE(2,J)
                 WSTOR = WSTORE(2,J)
C   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CIM FIX FOR INFILM =2 or 3
Cwch, 12/20/00. Add new argument in HORTON call.
C	Add rerouted flow to water available for infiltration.
C   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	           FFL2 = FL2PV/WAR(2,J) + FLOW2SUB(J)
                 IF(INFILM.EQ.0.OR.INFILM.EQ.2) 
     +              CALL HORTON(TP(J),K,J,FFL2)
                 IF(INFILM.EQ.1.OR.INFILM.EQ.3)
C   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Cwch, 12/20/00.  Add another argument at end of GAMP call.
C   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     +           CALL GAMP(WLMAX(J),SMD(J),DECAY(J),
     1                  IFLAG(J),FU(J),FTOT(J),WLMIN(J),WDEPTH(K,J),
     2                  TP(J),DELT,TIME,RI,RLOSS,UL(J),SVAP,
     3                  KAMEW(J),NAMEW(J),FFL2)
C=======================================================================
C     Call groundwater subroutine.
C=======================================================================
cim   the following line causes the number of subcatchments with
cim   groundwater to have to exceed or equal the number of subcatchments
cim   I changed array of NMSUB to NW in GRWTR.INC.
                 IF(NMSUB(J).GT.0) THEN
					       ENFIL = RLOSS - SVAP
						   IF(ENFIL.LT.0.0) ENFIL = 0.0
                             CALL GROUND(J,XSINFL)
C#### WCH, 4/7/94.  CHECK HERE FOR GROUNDWATER FLOW IN ORDER TO
C     USE WETDRY TIME STEP.
                             IF(GWFLOW(J).GT.0.0) KWIK = 1
C#### CORRECTION BY RED, 3/93. DIVIDE BY DELT.
                             ENFIL   = ENFIL - XSINFL/DELT
                             IF(ENFIL.LT.0.0) ENFIL = 0.0
C#### CORRECTION BY RED, 3/93. DIVIDE BY DELT.
                             RLOSS   = RLOSS - XSINFL/DELT
                             IF(RLOSS.LT.0.0) RLOSS = SVAP
                             CNT(11) = CNT(11) + ETU*WAR(2,J)*DELT
Cwch, 7/1/03 Deep ET also occurs only over pervious area.
C                             CNT(12) = CNT(12) + ETD*WAREA(J)*DELT
                             CNT(12) = CNT(12) + ETD*WAR(2,J)*DELT
                             CNT(14) = CNT(14) + DEPPRC*WAREA(J)*DELT
                             CNT(17) = CNT(17) + ENFIL*WAR(K,J)*DELT
Cwch, 7/1/03. Save more groundwater variables for printing, units of cfs.
	                       IF(NGWGW(J).EQ.1) THEN
                                LU         = LU + 1
                                ETUPR(LU) = ETU*WAR(2,J)
	                          ETDPR(LU) = ETD*WAR(2,J)
Cwch  Save combined ET for total ET save later. 
                                ETGR2     = (ETU+ETD)*WAR(2,J)
	                          DEPPR(LU) = DEPPRC*WAREA(J)
	                          ENFPR(LU) = ENFIL*WAR(2,J)
                                ENDIF                         
                             ENDIF

C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C CHANGED IF BECAUSE TOTAL INFILTRATION CALCULATIONS SHOULD NOT BE
C BYPASSED DURING A DRY TIME STEP
C                IF(WDEPTH(K,J).LE.0.0.AND.RI.LE.0.0) GO TO 350
Cwch, 12/20/00.  Include rerouted flow in check.
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Cwch 3/28/02.  Add flow2sub
Cwch 8/8/03. It appears this IF-statement is not needed and in fact
C     avoids inclusion of important continuity sums for groundwater.
C     The main purpose is to avoid unnecessary calcuations, but there
C     are several IF-statements imbedded between here and Stmt 350 
C     that serve the same purpose.  So delete and hope for the best!
C     Reommended by Carlos Diaz of GeoSyntec.  
C        IF(WDEPTH(K,J).LE.0.0.AND.RI.LE.0.0.AND.FL2PV.LE.0.0.
C     +     AND.FLOW2SUB(J).LE.0.0.AND.INFILM.LT.2) GO TO 350
CIM  Fix to limit maximum infiltration volume and track infiltration
C%%% here
                 IF (INFILM.GE.2) THEN
                      FINF = DMAX1((RLOSS - SVAP),DBLE(0.0))
                      TEMPIN=TOTINF(J) + FINF*DELT
                      FAVAIL = AMAX1((RMAXINF(J)-TEMPIN)/DELT,0.0)
                      IF (FAVAIL.LE.FINF) THEN
CIM  REDUCE FINF BECAUSE INFILTRATION VOLUME IS USED UP
                          FINF=FAVAIL
                          TEMPIN=TOTINF(J) + FINF*DELT
                          RLOSS=FINF+SVAP
                          ENDIF
                      TOTINF(J)=TEMPIN
                      ENDIF
C=======================================================================
Cwch, 12/20/00. Need to save infiltration value for possible 
C     subcatchment quality routing.  
C=======================================================================
			   ENFILW(J) = RLOSS - SVAP
			   IF(ENFILW(J).LT.0.0) ENFILW(J) = 0.0
			   ENFILW(J) = ENFILW(J)*WAR(2,J)
C endif for k=2
			   ENDIF
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Cwch, 12/20/00.  Add rerouted overland flow.
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      RSTAR = RI - RLOSS + FLOW2SUB(J)
	IF(K.EQ.1) RSTAR = RSTAR + FL2IMP/WAR(1,J)
	IF(K.EQ.2) RSTAR = RSTAR + FL2PV/WAR(2,J)
C
                                        CTYPE = 'DR2'
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C CHANGED STATEMENT BELOW BECAUSE ROUNDOFF ERROR WAS CAUSING DR1s TO BE
C DR2s
C      IF(RSTAR+WDEPTH(K,J)/DELT.LE.0.0) CTYPE = 'DR1'
      IF(RSTAR*DELT+WDEPTH(K,J).LE.0.000001) CTYPE = 'DR1'
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      IF(RSTAR*DELT+DOI.GT.0.0)         CTYPE = 'WET'
C=======================================================================
C     DR1 - No water is available to run off.
C     Evaporation (first) and infiltration (second) take away all water.
Cwch 12/20/00. Add rerouted overland flow to infiltration.
Cwch, 4/11/02.
C     Loss is all available water.  RSTAR includes all necessary 
C       vertical inflows.  Add back RLOSS to RSTAR and add WDEPTH
C       to get this total.  But for K = 2, RLOSS already includes 
C       WDEPTH. 
C=======================================================================
      IF(CTYPE.EQ.'DR1') THEN
           RLOSS       = RSTAR + RLOSS + WDEPTH(K,J)/DELT
C                         IF(K.EQ.2) RLOSS = RLOSS + FFL2
           WDEPTH(K,J) = 0.0
           WFLO        = 0.0
           ENDIF
C=======================================================================
C     DR2 - Depth less than depression storage but > 0.
C     Here, depth is filling (RSTAR > 0) or draining (RSTAR < 0) while
C       staying below depression storage depth. 
C=======================================================================
      IF(CTYPE.EQ.'DR2') THEN
                         WFLO  = 0.0
                         DCORR = WDEPTH(K,J) + RSTAR*DELT
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C HOW CAN DCORR EVER BE LESS THAN 0.0?  IF DCORR IS LESS THAN 0.0, CTYPE
C IS 'DR1' BY DEFINITION.  GET RID OF IF?
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C                         IF(DCORR.LT.0.0) THEN
C                                          RLOSS = WDEPTH(K,J)/DELT + RI
C                                          DCORR = 0.0
C                                          ENDIF
                         WDEPTH(K,J) = DCORR
                         ENDIF
C=======================================================================
C     Storage greater than depression storage - Call flow routing.
C=======================================================================
      IF(CTYPE.EQ.'WET') THEN
                          WCC = WCON(1,J)
               IF(K.EQ.2) WCC = WCON(2,J)
C=======================================================================
C              Test for the condition in which old DOI is negative but
C              the new DOI will be positive based on RI - RLOSS.
C=======================================================================
               IF(DOI.LT.0.0) THEN
                              RSTAR       = RSTAR + DOI/DELT
                              DOI         = 0.0
                              WDEPTH(K,J) = WSTOR
                              ENDIF
               CALL OVERLND(J,WCC,RSTAR,DOI,DEL,DELT)
               DCORR = WDEPTH(K,J) + DEL
               IF(DCORR.GT.WSTOR) THEN
                        WFLO = -WCC*WAR(K,J)*(DCORR-WSTOR)**1.666667
                        ELSE
                        WFLO  = 0.0
                        DCORR = WDEPTH(K,J) + RSTAR*DELT
                        IF(DCORR.LT.0.0) THEN
                                         RLOSS = WDEPTH(K,J)/DELT + RI
                                         DCORR = 0.0
                                         ENDIF
                        ENDIF
               WDEPTH(K,J) = DCORR
               KWIK        = 1
               ENDIF
C=======================================================================
C     Sum for continuity check.
C     CNT(4) = infiltration
C     CNT(6) = evaporation
C=======================================================================
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C FINF HAS ALREADY BEEN CALCULATED IF INFILM=2, SO ADD IF
      IF(K.EQ.2) THEN
C%%%%%
                 IF(INFILM.LT.2) THEN
                                 FINF= RLOSS - SVAP
                      IF(FINF.LT.0.0) FINF = 0.0
                      ENDIF
C%%%%%
                 RLOSS                  = RLOSS - FINF
                 IF(RLOSS.LT.0.) RLOSS  = 0.0
                 CNT(4)      = CNT(4) + FINF*WAR(K,J)*DELT
                 CNT(6)      = CNT(6) + RLOSS*WAR(K,J)*DELT
Cwch, 7/1/03
                 IF(NGWGW(J).EQ.1) EVGWPR(LU) = ETGR2 + RLOSS*WAR(K,J)
Cwch 3/28/02
                 SUBQLOSS(J) = SUBQLOSS(J) + (RLOSS+FINF)*WAR(K,J)*DELT
                 ELSE
                 CNT(6)      = CNT(6) + RLOSS*WAR(K,J)*DELT
                 ENDIF
C#######################################################################
C  C. MOORE, CDM, 8/93.  STATISTICS FOR SUBCATCHMENTS.
C#######################################################################
      IF (SUBQPEAK(K,J).LT.WFLO)  SUBQPEAK(K,J) = WFLO
      SUBDEP(K,J) = SUBDEP(K,J) + WFLO*DMEAN
C###
C  CNT(21) = surface runoff from subcatchments.
C  Only sum if water not being rerouted over further overland flow planes.
C  Move to end of 380 loop.
C      CNT(21)  = CNT(21)  + WFLO*DMEAN
C#######################################################################
Cwch, 12/20/00 Here, provide for rerouting of overland flow.
C#######################################################################
      IF((IFLOWP(J).EQ.0.OR.IFLOWP(J).EQ.3).OR.
     +    ((IFLOWP(J).EQ.1.OR.IFLOWP(J).EQ.4).AND.K.EQ.2).OR.
     +    ((IFLOWP(J).EQ.2.OR.IFLOWP(J).EQ.5).AND.K.NE.2))
     +    WFLOW(J) = WFLOW(J) + WFLO
Cwch, 6/7/01. Add check for zero pervious and impervious areas. 
	IF((IFLOWP(J).EQ.1.OR.IFLOWP(J).EQ.4).AND.K.NE.2) THEN
		IF(WAR(2,J).GT.0.01) THEN
            	FLOW2PV(J) = FLOW2PV(J) + WFLO
	        ELSE
	        WFLOW(J) = WFLOW(J) + WFLO
	        ENDIF
	    ENDIF
      IF((IFLOWP(J).EQ.2.OR.IFLOWP(J).EQ.5).AND.K.EQ.2) THEN
	     IF(WAR(1,J).GT.0.01) THEN
	        FLOW2IMP(J) = WFLO
	        ELSE
	        WFLOW(J) = WFLOW(J) + WFLO
	        ENDIF
	     ENDIF
C
 350  CNT(1)   = CNT(1)   + ABS(RIN)*WAR(K,J)*DELT
      CNT(2)   = CNT(2)   + RINE*WAR(K,J)*DELT
C     Save RFLOW for both subcat to subcat flows and for erosion calcs. 
	RFLOW(J) = RFLOW(J) + RINE*WAR(K,J)
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C   CIM     track total infiltration volume
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C  REGENERATE TOTINF
      IF(K.EQ.2.AND.INFILM.EQ.2) THEN
        IF(TOTINF(J).GE.RMAXINF(J)) TOTINF(J)=RMAXINF(J)
        IF(WDEPTH(K,J).LE.0.0.AND.RI.LE.0.0) THEN
          IF(TOTINF(J).LE.RMAXINF(J)*.01) THEN
            TOTINF(J)=0.0
          ELSE
            TOTINF(J)=RMAXINF(J)*EXP(-DECAY(J)*REGEN*(DELT-ALOG(TOTINF(J
     *)/RMAXINF(J))/(DECAY(J)*REGEN)))
          ENDIF
        ENDIF
      ENDIF
c  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  380 CONTINUE
C#######################################################################
Cwch, 12/20/00. Sum surface runoff continuity variable here, to
C  allow for rerouting of surface flows.
C#######################################################################
      IF(IFLOWP(J).LE.2) CNT(21) = CNT(21) + WFLOW(J)*DMEAN
C#######################################################################
C  C. MOORE, CDM, 8/93.  STATISTICS FOR SUBCATCMENTS.
C  PEAK TOTAL IMPERVIOUS AREA RUNOFF
Cwch, 1/17/01. Subarea type 4 is also impervious.  Add here, remove
C     below. 
C=======================================================================
      TEMP = SUBQPEAK(1,J) + SUBQPEAK(3,J)+ SUBQPEAK(4,J)
      IF (SUBQPEAK(5,J).LT.TEMP) SUBQPEAK(5,J) = TEMP
C=======================================================================
C  PEAK TOTAL RUNOFF RATE
Cwch, 12/20/00. If overland flow is rerouted internally, can't add to
C     get peak from subcat.  Use SUBQPEAK(2) for IFLOWP = 1 or 4 and 
C     use TEMP for IFLOWP = 2 or 5. 
C=======================================================================
C      TEMP = TEMP + SUBQPEAK(2,J) 
      IF (SUBQPEAK(6,J).LT.TEMP+SUBQPEAK(2,J).AND.
     1  (IFLOWP(J).EQ.0.OR.IFLOWP(J).EQ.3)) 
     2            SUBQPEAK(6,J) = TEMP + SUBQPEAK(2,J)
	IF (SUBQPEAK(6,J).LT.TEMP.AND.(IFLOWP(J).EQ.2.OR.IFLOWP(J).EQ.5))
     1            SUBQPEAK(6,J) = TEMP
	IF (SUBQPEAK(6,J).LT.SUBQPEAK(2,J).AND.
     1 (IFLOWP(J).EQ.1.OR.IFLOWP(J).EQ.4)) SUBQPEAK(6,J) = SUBQPEAK(2,J)
C#######################################################################
  390 CONTINUE
C=======================================================================
C     Save Subsurface plot information.
C=======================================================================
      IF(NSVGW.GT.0) THEN
                     MCOUN     = 0
Cwch, 7/1/03. Loop through all NOW subcats!
C                     DO 395 JH = 1,NOGWSC
                     DO 395 JH = 1,NOW
C#### WCH, 4/7/94.  MAKE THIS CHECK EARLIER.
C####                     IF(GWFLOW(JH).GT.0.0) KWIK = 1
C                     IF(NSCSFG(JH).EQ.0) GO TO 395
                     IF(NSCSFG(JH).EQ.1) THEN
				      MCOUN           = MCOUN+1
Cwch 3/1/02 Metric conversion error found by Sandy Elliot and RED.  
C     Reverse the conversions, to CMET8 for flow and CMET1 for stage.
                        GWFLWB(MCOUN)   = GWFLOW(JH)/CMET(8,METRIC)
                        STPOLL(1,MCOUN) = STG(JH)/CMET(1,METRIC)
                        STPOLL(2,MCOUN) = TH1(JH)
	                  ENDIF
  395                CONTINUE
                     WRITE(NSCRAT(6)) JULDAY,TIMDAY,TIME,(GWFLWB(JI),
     .                               (STPOLL(JV,JI),JV=1,2),JI=1,NSVGW)
                     ENDIF
C=======================================================================
Cwch, 12/20/00. Save desired subcatch outflow for printing.
C     Need to do this only for subcats that flow to other subcats but
C     array will allow for any to be saved. 
C     Save quality loads in Sub. QSHED. 
C=======================================================================
      IF(MSUBC.GT.0) THEN
           IF(JSTART(1).GT.0) THEN
               DO 600 L = 1,NDET
               IF(JULDAY.GE.JSTART(L).AND.JULDAY.LE.JSTOP(L)) GO TO 605
  600          CONTINUE
               GO TO 610
  605          CONTINUE
               ENDIF
           WRITE(NSCRAT(9),ERR=779) JULDAY,TIMDAY,DELT,
     +                           (WFLOW(ISUBC(N)),N=1,MSUBC)
  610      CONTINUE
           ENDIF
C=======================================================================
      RETURN
  779 WRITE(N6,9779)
      STOP ' Error in WSHED writing subcat flows to file NSCRAT(9).'
C=======================================================================
 9200 FORMAT(/,' Reading station number ',I10,' from NSCRAT(3) for',
     +' Temperature, Wind Speed, or Evaporation data.')
 9779 FORMAT(/,' ===> ERROR !!  WRITING SUBCATCHMENT FLOWS ON THE',
     +         ' NSCRAT(9) FILE IN SUBROUTINE WSHED.')
C=======================================================================
      END
``` 
