```fortran 
      SUBROUTINE INITAL
C	TRANSPORT BLOCK
C	CALLED BY INTRAN NEAR LINE 1209
C=======================================================================
C     ROUTINE INITIALIZES FLOWS, AREAS, AND CONCENTRATIONS TO VALUES
C     CORRESPONDING TO DRY WEATHER FLOW PLUS INFILTRATION.
C     WHEN THERE ARE NO INITIAL VALUES OF DWF OR INFIL, VALUES REMAIN 0.0
C
C     LAST UPDATED AUGUST 1989 AND MAY 1993 BY R.E.D.
C     BE SURE TO INITIALIZE STORAGE CONCENTRATIONS, WCH, 10/6/93.
C     For linked DO-BOD, print out initial reaeration, etc.  WCH, 7/6/01.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'DRWF.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      INCLUDE 'TST.INC'
# Subroutine INITAL: Comprehensive Summary

This file implements the Fortran subroutine `INITAL`, which initializes flows, areas, and water quality concentrations for a hydraulic simulation model—typically representing dry weather flow plus infiltration. Below is an extensive summary of its structure and functionality.

## Overview

- **Purpose:**  
      Initializes water flows, conduit areas, and pollutant concentrations to simulate conditions such as dry weather flow and infiltration. It also ensures that storage concentrations are set to correct values even if there is no inflow.

- **Key Tasks:**  
      - Sets up conversion factors based on metric or non-metric units.
      - Sums up upstream flows and computes corresponding pollutant inflows.
      - Routes flows and adjusts quality components for various element types (e.g., type 27 for quality splitting, type 19 for variable base flow).
      - Configures storage, backwater, and lift diversion elements.
      - Determines initial flow areas using a utility call (`FINDA`), and computes velocity and depth.
      - Outputs initialization results using custom Fortran format specifications.

## Included Files and Common Blocks

The subroutine includes several external files and common blocks to import necessary parameters and constants:

- `TAPES.INC`, `INTER.INC`, `STIMER.INC`, `TABLES.INC`, `DRWF.INC`, `HUGO.INC`, `NEW81.INC`, `TST.INC`, `NEWTR.INC`, `TRANWQ.INC`
- **Note:**  
      The marker `CIMT  INCLUDE COMMON THAT INCLUDES PMANN` indicates that an important common block, which brings in the variable `PMANN` (used for monthly base flow factors), is included within the routine.

## Detailed Functionality

1. **Initialization and Header Output**  
       - Begins with writing a header to the output device indicating that element flows, areas, and concentrations have been initialized.
       - Uses several formatted output statements (labels like 900, 905, etc.) to ensure consistent display.

2. **Conversion Factor Setup**  
       - Based on the `METRIC` flag, conversion factors (`CMET1`, `CMET2`, `CMET3`) are set to convert between metric and imperial units.
       - Adjusts flow (e.g., converting cubic feet per second to cubic meters per second) and area units accordingly.

3. **Flow and Pollutant Aggregation**  
       - Loops through elements to accumulate upstream flow (`SUM1`) and pollutant inflow arrays (`SUM2`).
       - Accounts for special element types (e.g., type 27) by applying quality splits using scaling factors like `DRATIO`.

4. **Handling Special Element Types**

       - **Flow Divider and Split Elements (Types 21, 23, and 26):**
             - Special handling to ensure proper routing of flows where diversion occurs.
      
       - **Lift Stations (Type 20):**
             - Adjusts parameters for lift stations by ensuring the stored flow does not exceed a preset distance.
      
       - **Storage Units (Type 22):**
             - Invokes an additional routing call if the element is a storage unit. Storage concentrations are safely initialized using values from the imported common block which includes `PMANN`.

       - **Backwater Elements (Type 25):**
             - Sets up outflow conditions where initial flows pass through intermediate conduits.

5. **Area, Velocity, and Depth Computations**  
       - Utilizes the routine `FINDA` to determine the initial conduit area.
       - Calculates average velocity (`VELINT`) and determines the flow depth (`DD`), which are later converted and output using the appropriate conversion factors.

6. **Quality Parameters and Aeration**  
       - For selected configurations (`NWQ` flag), the subroutine calls quality parameter routines (`QUALPARM` and `REAERATE`) to adjust reaeration and other water quality aspects.
       - Writes quality data using different formats depending on the simulation mode (`JCE` flag).

7. **Format Specifications**  
       - A series of format statements (e.g., formats 900, 905, 906, etc.) are provided to structure and align the output data for easy reading.
       - These ensure that results like flow, area, velocity, and quality concentrations are printed in a consistent manner.

8. **Important Note on PMANN Inclusion**  
       - The commented line `CIMT  INCLUDE COMMON THAT INCLUDES PMANN` highlights that the subroutine depends on external common variables (including `PMANN`) for calculating monthly base flow factors when adjusting storage and quality parameters.

## Overall Structure

- **Initial Definitions:**  
      Dimension arrays, variable equivalences, and declarations prepare the routine for mathematical operations.
      
- **Conditional Processing:**  
      Based on element types and metric flags, various conditionals adjust how flows and quality parameters are computed.
      
- **Loop Structures:**  
      Multiple loop constructs process elements and pollutants sequentially, ensuring that the contributions of each are summed and output properly.
      
- **Subroutine Calls:**  
      The integration of other subroutines (`FINDA`, `QUALPARM`, `REAERATE`, etc.) allows complex calculations to be modularized within the simulation framework.

This comprehensive markdown summary provides an extensive overview of the purpose, structure, and detailed operation of the `INITAL` subroutine, including its role in initializing hydraulic and quality simulation parameters.
      INCLUDE 'NEWTR.INC'
CIMT
Cwch, 7/6/01
      INCLUDE 'TRANWQ.INC'
C=======================================================================
CIMT  SUM2 dimensioned to MQUAL
      DIMENSION SUM2(MQUAL),QO(NET),QI(NET),QO1(NET),QO2(NET)
      DIMENSION WELL1(NET),WELL2(NET)
CIMT changed format for writting results
CIMT  DIMENSION FIRMAT(13),KIRMAT(13),EFMT(2)
CIMT  CHARACTER FIRMAT*4,KIRMAT*4,EFMT*4
      CHARACTER BMJ*10
      EQUIVALENCE (QO1(1),QMAX(1)),(QO2(1),QFULL(1))
      EQUIVALENCE (QO(1),Q(1,2,2)),(QI(1),Q(1,1,2))
      EQUIVALENCE (WELL1(1),SLOPE(1)),(WELL2(1),ROUGH(1))
CIMT      DATA FIRMAT/'(I6,','I7,3','F10.','3,2X',',F9.','3,1X',',F9.',
CIMT     1            '3,1X',',F9.','3,1X',',F9.','3,1X',')   '/
CIMT      DATA KIRMAT/'(A9,','I4,3','F10.','3,2X',',F9.','3,1X',',F9.',
CIMT     1            '3,1X',',F9.','3,1X',',F9.','3,1X',')   '/
CIMT      DATA EFMT/',1X,','E9.3'/
C=======================================================================
      WRITE (N6,900)
      IF(NPOLL.LE.0) THEN
           WRITE (N6,905)
           IF(METRIC.EQ.1) WRITE (N6,906)
           IF(METRIC.EQ.2) WRITE (N6,907)
	     WRITE(N6,908)
Cwch, 7/6/01
           ELSEIF(NWQ.EQ.0) THEN
                WRITE (N6,905) (PNAME(K),K=1,NPOLL)
                IF(METRIC.EQ.1) WRITE (N6,906) (PUNIT(K),K=1,NPOLL)
                IF(METRIC.EQ.2) WRITE (N6,907) (PUNIT(K),K=1,NPOLL)
	          WRITE(N6,908) ('--------',K=1,NPOLL)
           ELSEIF(NWQ.EQ.1) THEN
                WRITE (N6,9015) (PNAME(K),K=1,NPOLL)
                IF(METRIC.EQ.1) WRITE (N6,9016) (PUNIT(K),K=1,NPOLL)
                IF(METRIC.EQ.2) WRITE (N6,9017) (PUNIT(K),K=1,NPOLL)
	          WRITE(N6,9018) ('--------',K=1,NPOLL)
           ENDIF
CIMT      IF(NPOLL.GT.0) THEN
CIMT      DO 200 K = 1,NPOLL
CIMT      IF(NDIM(K).NE.1) GO TO 200
CIMT      NNX         = 2 + 2*K
CIMT      DO 190 I    = 1,2
CIMT      MMX         = NNX + I
CIMT      KIRMAT(MMX) = EFMT(I)
CIMT  190 FIRMAT(MMX) = EFMT(I)
CIMT  200 CONTINUE
CIMT      ENDIF
C=======================================================================
C     SET UP METRIC CONVERSION FACTORS.
C     MULTIPLY FIRST UNITS TO OBTAIN SECOND UNITS.
C=======================================================================
      IF(METRIC.EQ.1) THEN
                      CMET1 = 1.0
                      CMET2 = 1.0
                      CMET3 = 1.0
                      ELSE
                      CMET1 = 3.281
C=======================================================================
C                     SQ M TO SQ FT.
C=======================================================================
                      CMET2 = 10.764
                      CMET3 = 35.31
                      ENDIF
      N        = 0
      DO 100 I = 1,NE
      M        = JR(I)
      NTPE       = NTYPE(M)
      IF(NTPE.EQ.22) KSTOR = KSTORE(M)
C=======================================================================
C     SUM UPSTREAM FLOWS AND POLLUTANT INFLOWS.
C=======================================================================
      SUM1    = 0.0
      IF(NPOLL.GT.0) THEN
                     DO 10 K = 1,NPOLL
   10                SUM2(K) = 0.0
                     ENDIF
      DO 20 J = 1,3
      L       = INUE(M,J)
      NTPEU     = NTYPE(L)
      IF(L.GT.NE) GO TO 20
CIM THIS LINE NEEDS TO BE MODIFIED TO ROUTE FLOWS FOR 
CIM TYPE 27
      IF(NTPEU.LE.20.OR.NTPEU.EQ.27) THEN
                    QQ   = Q(L,2,1)*BARREL(L)
                    ELSE
                    KK   = GEOM3(L)
                    BMJ  = KGEOM(L)
                    QQ   = QO2(L)
                    IF(JCE.EQ.0.AND.NOE(M).EQ.KK)  QQ = QO1(L)
                    IF(JCE.EQ.1.AND.KOE(M).EQ.BMJ) QQ = QO1(L)
                    ENDIF
      IF(NPOLL.GT.0) THEN
                     DO 25 K = 1,NPOLL
   25                SUM2(K) = SUM2(K) + CPOL2(L,1,K) * QQ
                     ENDIF
      SUM1 = SUM1 + QQ
   20 CONTINUE
      KHR     = JHR + 1
      IF(KHR.GT.24) KHR = 24
                         QMAN    =  0.0
CIM  CHANGE FOR VARIABLE BASE FLOW IN TRANSPORT
CIM  CHANGE MADE TO INCLUDE MONTHLY FLOW FACTOR TO DETERMINE
CIM  BASE FLOWS IN INITIAL CONDITIONS
CIM
      IF(NTPE.EQ.19) QMAN    = DIST(M)*BFFMONTH(1,NINT(GEOM3(M)))
CIM                                        ------------------------
CIM
CIM      IF(NTYPE(M).EQ.19) QMAN    = DIST(M)
cim  monthly variation in DWF
      IF (NTPE.EQ.19) THEN
	    BFF = BFFMONTH(2,NINT(GEOM3(M)))
	ELSE
	    BFF = 1.0
	ENDIF
      QQQ      = QDWF(M)*DVDWF(KDAY)*HVDWF(KHR)
cim  monthly variation
     a           * BFF
cims
      SUM1     = SUM1 + QINFIL(M) + QMAN + QQQ
      Q(M,1,1) = SUM1/BARREL(M)
      Q(M,2,1) = Q(M,1,1)
      IF(Q(M,1,1).LT.0.0) Q(M,1,1) = 0.0
      IF(Q(M,2,1).LT.0.0) Q(M,2,1) = 0.0
      IF(NPOLL.GT.0.AND.SUM1.GT.0.0) THEN
           DO 35 K = 1,NPOLL
           IF(K.EQ.1) SUM2(K) = SUM2(K)+WDWF(M,K)*DVBOD(KDAY)*
     1                  HVBOD(KHR)*DVDWF(KDAY)*HVDWF(KHR)
cim monthly variation
     2                  * BFF
cim
           IF(K.EQ.2) SUM2(K) = SUM2(K)+WDWF(M,K)*DVSS(KDAY)*
     1                  HVSS(KHR)*DVDWF(KDAY)*HVDWF(KHR)
cim monthly variation
     2                  * BFF
cim
           IF(K.EQ.3) SUM2(K) = SUM2(K)+WDWF(M,K)*1.0*HVCOLI(KHR)*
     1                     DVDWF(KDAY)*HVDWF(KHR)
cim monthly variation
     2                  * BFF
cim
           PMAN = 0.0
CIMT           IF(NTYPE(M).EQ.19) THEN
CIMT                         IF(K.EQ.1) PMAN = GEOM1(M)
CIMT                         IF(K.EQ.2) PMAN = SLOPE(M)
CIMT                         IF(K.EQ.3) PMAN = ROUGH(M)
CIMT                         IF(K.EQ.4) PMAN = GEOM2(M)
CIMT                         ENDIF
CIM CHANGE FOR MONTHLY BASE FLOW FACTORS
cim note that geom1 ...  are now mass not concentration
CIM   
CIMT Change for additional constituents
      IF(NTPE.EQ.19)  PMAN = PMANN(M,K)*BFFMONTH(1,NINT(GEOM3(M)))
CIMT
           SUM2(K)      = SUM2(K) + PMAN + CPINF(K)*QINFIL(M)
           CPOL1(M,1,K) = SUM2(K)/SUM1
CIMQP   INITIAL INITIAL OUTFLOW FOR TYPE 27 Quality Splitting Elements
      IF(NTPE.EQ.27) CPOL1(M,1,K) = CPOL1(M,1,K) * DRATIO(M,K)
CIMQP
           CPOL2(M,1,K) = CPOL1(M,1,K)
           IF(NTPE.EQ.22.AND.PTC0(KSTOR,K).GT.0.0)
     1                               CPOL2(M,1,K) = PTC0(KSTOR,K)
   35      CONTINUE
           ELSE
C#######################################################################
C     WCH, 10/6/93.  DON'T MISS OUT ON INITIALIZING STORAGE VOLUME
C       CONCENTRATIONS JUST BECAUSE THERE'S NO INFLOW.
C=======================================================================
           IF(NPOLL.GT.0.AND.SUM1.LE.0.0.AND.NTPE.EQ.22) THEN
                DO 38 K = 1,NPOLL
   38           CPOL2(M,1,K) = PTC0(KSTOR,K)
                ENDIF
           ENDIF
C=======================================================================
C     ASSUME ALL DWF IS NON-DIVERTED IN A FLOW DIVIDER TYPE ELEMENT.
C=======================================================================
C   MODIFIY TO INCLUDE TYPE 26 FLOW SPLIT
      IF(NTPE.EQ.21.OR.NTPE.EQ.23.OR.NTPE.EQ.26) THEN
                               QO1(M) = SUM1
                               QO2(M) = 0.0
                               ENDIF
C=======================================================================
C     LIFT STATION.
C=======================================================================
      IF(NTPE.EQ.20) THEN
                   WELL1(M) = WELL2(M)
                   IF(Q(M,2,1).GT.DIST(M)) THEN
                                           Q(M,2,1) = DIST(M)
                                           WELL1(M) = GEOM1(M)
                                           WELL2(M) = WELL1(M)
                                           ENDIF
                   ENDIF
C=======================================================================
C     STORAGE UNIT.
C=======================================================================
      IF(NTPE.EQ.22) THEN
C#### RED, 5/25/93.  REMOVE THIS LINE:    NITER = 1
                   CALL ROUTE(D1,Q1)
                   ENDIF
C=======================================================================
C     BACKWATER ELEMENT. ALL INITIAL FLOW IS
C                        THROUGH THE INTERMEDIATE CONDUITS.
C=======================================================================
      IF(NTPE.EQ.25) THEN
                   QO1(M)   = 0.0
                   QO2(M)   = SUM1
                   ENDIF
      VELINT   = 0.0
      A(M,1,1) = 0.0
	DD       = 0.0
C=======================================================================
C     DETERMINE INITIAL FLOW AREA FOR CONDUITS.
C=======================================================================
      IF(KLASS(NTPE).LE.2) THEN
           PS       = Q(M,1,1)/QFULL(M)
           CALL FINDA(PS,A(M,1,1))
           A(M,2,1) = A(M,1,1)
	     A(M,1,2) = A(M,1,1)
	     A(M,2,2) = A(M,1,1)
	     Q(M,2,1) = Q(M,1,1)
	     Q(M,2,2) = Q(M,1,1)
           VELINT   = VEL(Q(M,1,1),A(M,1,1))
Cwch, 7/6/01.  Add depth.
           DD   = DEPTH(A(M,1,1)/AFULL(M))
	     IF(NTPE.NE.10.AND.NTPE.NE.12) THEN
	          DD = DD*GEOM1(M)
	          ELSEIF(NTPE.EQ.10) THEN
C     Modified basket handle
                     DD = DD*(GEOM1(M)+GEOM2(M)/2.0)
	          ELSEIF(NTPE.EQ.12) THEN
C     Rectangular, round bottom
                     DD = DD*P2(M)*DIST(M)
                ENDIF
	     ENDIF
      W1              = SUM1/CMET3
      IF(NTPE.EQ.22) W1 = QO(M)
      W2              = A(M,1,1)/CMET2
      W3              = VELINT/CMET1
	W4              = DD/CMET1
C=======================================================================
Cwch, 7/6/01.  For linked DO-BOD, print out inital Ka values.
C=======================================================================
      IF(NWQ.EQ.1) THEN
	   IF(KLASS(NTPE).LE.2.OR.NTPE.EQ.22) THEN
	     CALL QUALPARM(ASURF,ASSETL,DBAR,DBARSETL,VELL,CS,WAVG)
           CALL REAERATE(DECAY2,DBAR,VELL,WIND(M),TAIR(M),TWATER(M),REA,
     1THETA(3),KWIND,KOVAR,GNUW,GNUA,RHOW,RHOA,DMOLEC,XLAMBDA,ZEE,CAPPA,
     2 GAMMAZ,UTSTAR,PRESURE,UCSTAR,CAPPA3,CDRAG,IYZ,DELG1,ZZERO,USTAR,
     3 CAYEL,DECAY2W,SALINITY,G1,ZAA,N6,DECAY2F)
           IF(JCE.EQ.0.AND.NPOLL.GT.0) WRITE(N6,7011) NOE(M),NTPE,
     1        W1,W2,W3,W4,DBAR/CMET1,WAVG/CMET1,CS,
     2        DECAY2F*86400.,DECAY2W*86400.,(CPOL2(M,1,K),K=1,NPOLL)
           IF(JCE.EQ.1.AND.NPOLL.GT.0) WRITE(N6,7012) KOE(M),NTPE,
     1        W1,W2,W3,W4,DBAR/CMET1,WAVG/CMET1,CS,
     2        DECAY2F*86400.,DECAY2W*86400.,(CPOL2(M,1,K),K=1,NPOLL)
	     ELSE
           IF(JCE.EQ.0) WRITE(N6,7021) NOE(M),NTPE,W1,
     +                            (CPOL2(M,1,K),K=1,NPOLL)
           IF(JCE.EQ.1) WRITE(N6,7022) KOE(M),NTPE,W1,
     +                            (CPOL2(M,1,K),K=1,NPOLL)
           ENDIF
         ELSE
CIMT  CHANGE FORMAT ON OUTPUTS  7001 was FIRMAT and 7002 was KIRMAT    
      IF(JCE.EQ.0.AND.NPOLL.LT.1) WRITE(N6,7001) NOE(M),NTPE,W1,W2,W3,W4
      IF(JCE.EQ.1.AND.NPOLL.LT.1) WRITE(N6,7002) KOE(M),NTPE,W1,W2,W3,W4
      IF(JCE.EQ.0.AND.NPOLL.GT.0) WRITE(N6,7001) NOE(M),NTPE,W1,W2,W3,W4
     +                            ,(CPOL2(M,1,K),K=1,NPOLL)
      IF(JCE.EQ.1.AND.NPOLL.GT.0) WRITE(N6,7002) KOE(M),NTPE,W1,W2,W3,W4
     +                            ,(CPOL2(M,1,K),K=1,NPOLL)
	   ENDIF     
  100 CONTINUE
      RETURN
C=======================================================================
  900 FORMAT(1H1,/,
     1' ************************************************************',/,
     2' * ELEMENT FLOWS, AREAS, AND CONCENTRATIONS ARE INITIALIZED *',/,
     3' *         TO DRY WEATHER FLOW AND INFILTRATION VALUES.     *',/,
     4' ************************************************************',//
     5)
CIM SET MAXIMUM NUMBER TO 99
CWCH, 9/27/99.  USE WIDTH-12 FIELDS, IN CASE OF LARGE BACTERIA NOS. 
  905 FORMAT (3X,' ELEMENT',9X,'FLOW',6X,'AREA',5X,'VELOCITY',
     + '    DEPTH ',99(4X,A8))
  906 FORMAT (5X, 'NUMBER  TYPE   (CFS)    (SQ FT)   (FT/SEC)', 
     + '     (FT) ', 99(4X, A8))
  907 FORMAT (5X, 'NUMBER  TYPE   (CMS)    (SQ M)     (M/SEC)', 
     + '     (M)  ', 99(4X, A8))
  908 FORMAT (5X,'------  ----   -----    ------    -------- ',
     + '  ------- ',99(4x,A8))
 7001 FORMAT(I10,I6,4F10.3,1X,99F12.3)
 7002 FORMAT(A10,I6,4F10.3,1X,99F12.3)
Cwch, 7/6/01
 7011 FORMAT(I10,I6,9F10.3,1X,99F12.3)
 7012 FORMAT(A10,I6,9F10.3,1X,99F12.3)
 7021 FORMAT(I10,I6,F10.3,81X,99F12.3)
 7022 FORMAT(A10,I6,F10.3,81X,99F12.3)
 9015 FORMAT (3X,' ELEMENT',9X,'FLOW',6X,'AREA',5X,'VELOCITY',
     + '    DEPTH ',' AVG DEPTH',' TOP WIDTH','  SAT. DO ',
     2 '  FLOW Ka ','  WIND Ka ',99(4X,A8))
 9016 FORMAT (5X, 'NUMBER  TYPE   (CFS)    (SQ FT)   (FT/SEC)', 
     1 '     (FT) ','     (FT) ','     (FT) ','   (MG/L) ',
     2 '  (1/DAY) ','  (1/DAY) ',99(4X, A8))
 9017 FORMAT (5X, 'NUMBER  TYPE   (CMS)    (SQ M)     (M/SEC)', 
     1 '     (M)  ','     (M)  ','     (M)  ','   (MG/L) ',
     2 '  (1/DAY) ','  (1/DAY) ',99(4X, A8))
 9018 FORMAT (5X,'------  ----   -----    ------    -------- ',
     1 '  -------', '  --------','  --------','  --------',
     2 '  --------','  --------',99(4x,A8))
C=======================================================================
      END
``` 
