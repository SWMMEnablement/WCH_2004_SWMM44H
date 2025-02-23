```fortran 
      SUBROUTINE GETCUR(JNO,XFROM,SLOPE,METRIC,KNORM,KCOND,
cim 3/99 use length on C1 line.
c     +      AFULL,DEEP,TWFULL,XLEN,RN0,RFULL,NSTOP,IDNUM,KDNUM,KSTOP)
     +      AFULL,DEEP,TWFULL,XLENIN,RN0,RFULL,NSTOP,IDNUM,KDNUM,KSTOP)
C     TRANSPORT AND EXTRAN BLOCK
C=======================================================================
C     THIS ROUTINE READS THE INPUT FILE AND FINDS
C     FLOW/COEF CURVES FOR SECTIONS SPECIFIED IN EXTRAN OR TRANSPORT.
C     A.C. ROWNEY JUNE 1986, UPDATED J. E. SWANSON MARCH 1987
C     MODIFIED BY R. E. DICKINSON JANUARY, 1988, MAY, 1989.
C     MODIFIED BY WAYNE C. HUBER, HALLOWEEN, 1991 and August 1992.
C     MODIFIED BY WCH, 2/24/93 TO CORRECT HYDRAULIC RADIUS CALCULATION
C      FOR VERTICAL CROSS SECTIONS, AS PER HINTS FROM CHUCK MOORE AT CDM.
C     MODIFIED BY RED, 6/2/93 FOR USE WITH TRANSPORT BLOCK.
C     MODIFIED BY WCH (RED), 9/23/93 TO MULTIPLY STCHL ETC. BY PXSECR.
C     MODIFIED BY WCH, 11/30/93.  IF ROUGHNESS TRANSITION OCCURS EXACTLY
C      AT VERTICAL SECTION, ASSUME VERTICAL WALL SHOULD HAVE MAIN
C      CHANNEL ROUGHNESS.  CHANGE IF STATEMENT TO STRICTLY LT AND GT.
C     MODIFIED BY WCH, 8/25/94.  ANOTHER CORRECTION FOR CASE WHEN
C      VERTICAL WALL OCCURS AT ROUGHNESS TRANSITION.
C     RED, 12/16/94.  CORRECTION FOR POWER FUNCTION CHANNELS FOR
# GETCUR Fortran Subroutine – Complete Markdown Summary

This document provides a comprehensive summary of a legacy Fortran subroutine called `GETCUR`, which is used for processing channel cross-sectional data in hydraulic modeling. The code handles both natural channels and power function channels, reading input data, performing computations, error checking, and printing formatted output.

---

## Overview

- **Purpose:**  
      The `GETCUR` subroutine reads cross-section data (from Extran or Transport blocks), calculates hydraulic parameters, establishes normalized flow/area curves, and prints both channel and conduit information.

- **History & Modifications:**  
      - Originally developed by A.C. Rowney in 1986.
      - Updated by various authors (Swanson, Dickinson, Huber, WCH, RED, CIM) over years.
      - Historical comments note adjustments (e.g., Manning’s n, vertical cross-section corrections, power function channel enhancements).

---

## Code Structure & Main Components

### 1. File Inclusions
The subroutine includes several common blocks and configuration files:
- `TAPES.INC`
- `FLODAT.INC`
- `BALINES.INC`
- `NEWTR.INC`

These files provide shared variables and configuration constants used throughout the subroutine.

---

### 2. Data Input and Variable Initialization

- **Parameters and Dimensions:**  
      A parameter (`NXSPTS`) limits the maximum number of cross-section points (set to 100). The array `ELSTA(2, NXSPTS)` stores station data.

- **Variables:**  
      Variables such as `JNO`, `XFROM`, `SLOPE`, `METRIC`, `KNORM`, and `KCOND` control the behavior:
      - `KNORM`: Determines if conversion is for Extran (0) or Transport (1).
      - `KCOND`: Represents if natural cross section data is used (`0`) or if the channel is a power function channel (`1`).

- **Default Values and Adjustments:**  
      - For error prevention, if `SLOPE` is less than or equal to zero, it defaults to 0.01.
      - Other variables such as `NUMST`, `PXSECR`, and arrays like `QCURVE` are initialized.

---

### 3. Reading Cross-Section Data

- **Logic Flow:**  
      The code reads through a loop (maximum iterations reduced from 100000 to 2200) to locate the specified cross-section ID (`XFROM`). It uses several file commands:
      - `READ`, `BACKSPACE`, and error handling (`ERR=888`, `END=9000`).

- **Handling Different Cards:**  
      Depending on card type read (`D1`, `NC`, `C2`, `E2`, `X1`, `C3`, `E3`), the subroutine adjusts its behavior:
      - When encountering card types for adjustment (`C3`, `C4`), the subroutine may backtrack in the file stream.
      - Error messages are produced if a cross-section is not found or if stations are in the wrong order.

- **Error Handling:**  
      Labeled sections (e.g., `9000` and `9020`) generate error messages if data is missing or misordered.

---

### 4. Processing Channel Sections

- **Natural vs. Power Function Channels:**
      - **Natural Channels (KCOND = 0):**  
            After data input, subroutine calls perform normalization, hydraulic radius calculation, and error checking.  
            - Calls the helper subroutine `IRRSECT` to compute area, radius, and top width.
            - Uses iterative loops for detailed calculations (e.g., using a linear interpolation for 1/25 of a full cross-section).

      - **Power Function Channels (KCOND = 1):**  
            The code bypasses the natural channel processing and branches directly to section 6666.
            - A loop computes parameters (depth, wetted perimeter, area, top width) using a separate method (`POWER` subroutine call).
            - Adjusts production of secondary detailed outputs if required by additional flags (`IDETAIL`).

- **Normalization Process:**  
      Regardless of channel type, the subroutine uses normalization to scale:
      - Hydraulic radius.
      - Cross-sectional area (by dividing with full cross-section area `AFULL`).
      - Top width and flow (depending on the normalization flag `KNORM`).

---

### 5. Additional Calculations & Subroutine Calls

- **Subroutine Calls:**
      - `IRRSECT`: For computing irregular channel section properties.
      - `POWER`: For calculating parameters in power function channels.
      - `TRANNORM`: Normalizes the channels for Transport if needed.
      - `CHECKSTCH`: Verifies that specific station points match the computed cross-section.

- **Error and Warning Messages:**  
      Various formatted `WRITE` statements (using format labels such as `1690`, `1700`, `2700`, etc.) are employed to output information and warnings regarding hydraulic parameters. These outputs include:
      - Channel length, elevation, maximum depth, and Manning's n.
      - Normalized dimensionless curves printed in tabulated forms.

- **Tolerance Checks:**  
      The subroutine includes checks for small differences in computed length versus input (`XLEN` compared to `XLENIN`) and terminates or issues warnings if discrepancies exceed 1%.

---

### 6. Output Generation

- **Formatted Output:**  
      The subroutine uses a series of formatted `WRITE` statements to print detailed information:
      - **For Natural Channels:**  
            A section prints natural cross-section curves and dimensionless parameters normalized by depth or area.
      - **For Power Function Channels:**  
            Additional details such as the channel exponent, computed flow rate, and hybrid methodology are displayed.
      
- **Final Section:**  
      After normalization, the computed curves are printed iteratively for a set number of stations. The last point is forced to zero to assist in proper formatting in the printing routine.

- **Error Reporting Labels:**  
      Specific error messages (using format labels such as `9000`, `9020`, etc.) notify the user if there is an error in input data or processing order.

---

## Summary

The `GETCUR` subroutine in Fortran is responsible for:

- Reading and verifying input data for channel cross-sections.
- Handling both natural and power function channels through conditional branching.
- Computing key hydraulic parameters (area, top width, hydraulic radius, flow).
- Normalizing calculations to be independent of the measurement system (U.S. customary or metric).
- Invoking helper subroutines for detailed section calculations and normalization.
- Producing detailed formatted output and error messages that assist in debugging and validating input data.

This extensive summary encapsulates the structure, logic, and critical functions of the `GETCUR` subroutine, making it easier to understand its role within the larger hydraulic modeling system.
(2-26) AREA CALCULATIONS.
C     WCH, 2/7/95.  OPTION TO INCREASE NUMBER OF ALLOWABLE CROSS SECTION
c      POINTS WITH PARAMETER STATEMENT.  LEAVE AT 100 FOR NOW.
C     CIM, 9/8/00.  Major changes for option of adding additional
C                   detail for irregular sections in TRANSPORT.
C                   Move some of calculations to new subroutine IRRSECT.
C                   Modified check for STCHL and STCHR.
C     WCH, 7/6/01.  Need top width for new Transport quality calcs.
C                   Create new QCURVE(NATUR,4,I) array to store
C                   normalized flows for Transport and use same
C                   QCURVE(NATUR,3,I) array for top width for both
C                   Extran and Transport.  
C     WCH, 3/14/02. Add metric fixes for calling from Transport.  
C     WCH, 3/15/02. Check for zero value of XFROM.
C     WCH, 4/16/02. Allow default for zero SLOPE.
C     WCH (CIM), 7/20/04. Fix format variable KK for Transport 
C                   output table. 
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'FLODAT.INC'
      INCLUDE 'BALINES.INC'
CIM### 9/9/00 New common block
      INCLUDE 'NEWTR.INC'
      CHARACTER KDNUM*10
C#### WCH, 2/7/95.  INCREASE DIMENSION FROM 100 TO 300.
C  IF CHANGED, CHANGE IN CHECKSTCH AND IRRSECT ALSO!!
      PARAMETER (NXSPTS=100)
      DIMENSION ELSTA(2,NXSPTS)
      LOGICAL GOTOO,FAILED,OKL,OKR
C=======================================================================
C     JNO      = CONDUIT INPUT ORDER NUMBER FROM EXTRAN OR TRANSPORT
C     XFROM    = CROSS SECTION IDENTIFICATION NUMBER,OR
C                POWER FUNCTION EXPONENT
C     SLOPE    = AVERAGE CHANNEL SLOPE
C     METRIC   = US CUSTOMARY OR METRIC UNITS
C     KNORM    = IF 0 - CALLED FROM EXTRAN, IF 1 - CALLED FROM TRANSPORT
C     KCOND    = IF 0 - READ NATURAL CROSS SECTIONS
C     KCOND    = IF 1 - CREATE POWER FUNCTION CHANNELS
C     AFULL    = AREA WHEN CONDUIT IS FULL
C     DEEP     = MAXIMUM DEPTH
C     TWFULL   = TOP WIDTH WHEN CONDUIT IS FULL
C     XLEN     = CONDUIT LENGTH
C     RN0      = CHANNEL MANNING'S N
C     RFULL    = HYDRAULIC RADIUS WHEN CONDUIT IS FULL
C     NSTOP    = NUMBER OF ERRORS
C     IDNUM    = CONDUIT NUMBER
C     KDNUM    = CONDUIT NAME
C     KSTOP    = 0.... PRINT NORMALIZED CURVES
C     KSTOP    = 1.... DO NOT PRINT NORMALIZED CURVES
C     NATUR    = NUMBER (SEQUENCE NO.) OF NATURAL CHANNEL AND/OR OF
C                POWER FUNCTION CHANNEL. PASSED IN FLODAT.INC. 
C=======================================================================
C     EXTRAN USES EITHER U.S. CUSTOMARY OR METRIC UNITS INTERNALLY.
C     TRANSPORT USES U.S. CUSTOMARY UNITS INTERNALLY.  BUT TRANSPORT
C     USES METRIC UNITS WITHIN GETCUR FOR NATURAL SECTIONS, IF METRIC=2.
C=======================================================================
      IF(KNORM.EQ.0) KMET = METRIC
      IF(KNORM.EQ.1) KMET = 1
C=======================================================================
C     INITIALIZE AS REQUIRED
C=======================================================================
      NQC(JNO)            = NATUR
      GOTOO               = .FALSE.
      FAILED              = .FALSE.
      NUMST               = 0
      NUMQ(NATUR)         = 26
      PXSECR              = 1.0
      POW                 = XFROM
      QCURVE(NATUR,1,1)   = 0.0
      QCURVE(NATUR,2,1)   = 0.0
      QCURVE(NATUR,3,1)   = 0.0
Cwch, 7/6/01
      QCURVE(NATUR,4,1)   = 0.0
      IF(KCOND.EQ.0) XLEN = 0.0
Cwch, 4/16/02. Does it matter if slope is zero since it's only used
C     to develop a rating curve?  Try allowing a default of 0.01.
      IF(KCOND.EQ.0.AND.SLOPE.LE.0.0) THEN
                                      IF(JCE.EQ.0) WRITE(N6,9600) IDNUM
                                      IF(JCE.EQ.1) WRITE(N6,9601) KDNUM
	                                SLOPE = 0.01
C                                      STOP
                                      ENDIF
      RTSLOP              = SQRT(SLOPE)
      IF(KNORM.EQ.1.AND.KCOND.EQ.0) TWFULL = 0.0
C=======================================================================
C FOR POWER FUNCTION CHANNELS, GO RIGHT TO CALCULATIONS.
C=======================================================================
      IF(KCOND.EQ.1) GO TO 6666
C=======================================================================
C READ CARDS. WHEN NC OR GR CARD IS ENCOUNTERED, INCORPORATE VALUES.
C             WHEN X1 CARD IS ENCOUNTERED, CHECK IF NAME IS RIGHT.
C             IF X1 CARD IS OKAY, USE IT. IF NOT, FORGET IT.
C=======================================================================
C     READ THE CROSS-SECTION DATA
C=======================================================================
C REDUCE LENGTH OF DO LOOP FROM 100000 TO 2200, WCH, 8/28/92
C#######################################################################
Cwch, 3/115/02 Follow CIM suggestion re. checking for zero XFROM.
	IF(XFROM.EQ.0) THEN
	     WRITE (N6,9005) JNO
	     WRITE(*,9005)   JNO
	     NSTOP = NSTOP + 1
	     ENDIF
      DO 300 KREAD = 1,2200
      READ(N5,*,ERR=888,END=9000) CC
cimbridges
      IF(CC.EQ.'D1') then
cimbridges   additional time saving error check
      write(n6,*) 'ERROR - Cross-section ',XFROM,' was not found'
      write(n6,*) '        Input data may be missing from',
     a' cross-section data.'
      write(n6,*) '        More likely cause is the order of sections',
     a'        in cross-section cards is not the same as they appear ',
     b'in the C1 cards.'
      stop 'Cross-section card not found'
      endif
      IF(CC.EQ.'NC'.OR.CC.EQ.'C2'.OR.CC.EQ.'E2') THEN
           BACKSPACE N5
           READ(N5,*,ERR=888) CC,XNL,XNR,XNCH
           IF(XNCH.LE.0.0) WRITE(N6,201)
           ENDIF
      RN0 = XNCH
      IF(XNCH.LE.0.0) WRITE(N6,201)
      IF(CC.EQ.'X1'.OR.CC.EQ.'C3'.OR.CC.EQ.'E3') THEN
           BACKSPACE N5
           READ(N5,*,ERR=888) CC,SECNO,NUMST,STCHL,STCHR,
     +                                 XLOBL,XLOBR,XLEN,PX,PSXECE
           IF(SECNO.EQ.XFROM) THEN
                              GOTOO                = .TRUE.
                              IF(PX.NE.0.0) PXSECR =  PX
                              ENDIF
           ENDIF
C
      IF(CC.EQ.'GR'.OR.CC.EQ.'C4'.OR.CC.EQ.'E4'.AND.NUMST.NE.0) THEN
           BACKSPACE N5
           DO 210 II = 1,NUMST,5
           IEND = II + 4
           IF(IEND.GT.NUMST) IEND = NUMST
           READ(N5,*,ERR=888) CC,((ELSTA(K,I),K=1,2),I=II,IEND)
  210      CONTINUE
cim 9/8/00  check that at least one station matches STCHL and STCHR
      CALL CHECKSTCH(ELSTA,STCHL,NUMST,'STCHL')
      CALL CHECKSTCH(ELSTA,STCHR,NUMST,'STCHR')
C=======================================================================
C WCH MODIFICATION
C ALLOW ONLY ONE SET OF C3-C4 LINES, IF DESIRED.
C BACKSPACE THE NUMBER OF LINES REQUIRED.
C NREQ = NUMBER OF C4 LINES PLUS ONE C3 LINE.
C=======================================================================
C NEW CORRECTION, 8/92, WCH.  BACKSPACE ONLY IF USING
C  DESIRED C3-C4 LINES.
C=======================================================================
          IF(GOTOO) THEN
                    NREQ = NUMST/5
                    IF(NREQ*5.LT.NUMST) NREQ = NREQ + 1
                    NREQ = NREQ + 1
                    DO 240 J = 1,NREQ
  240               BACKSPACE N5
                    ENDIF
          IF(GOTOO) GO TO 1200
          ENDIF
  300 CONTINUE
C#######################################################################
C IF ARRIVE HERE, HAVEN'T BEEN ABLE TO FIND MATCH FOR NATURAL CHANNEL.
C WRITE ERROR MESSAGE.  WCH, 8/28/92
C#######################################################################
      WRITE(N6,9040) XFROM, SECNO
      NSTOP = NSTOP+1
      RETURN
C=======================================================================
C     IRREGULAR CHANNEL CALCS FOR EXTRAN OR TRANSPORT CHANNEL ROUTING.
C                       TRANSPORT IS NORMALIZED BY AREA.
C                       EXTRAN IS NORMALIZED BY DEPTH.
C=======================================================================
C     MODIFY BY FACTORS ON X1 LINE
C=======================================================================
 1200 CONTINUE
      IF(PXSECR.NE.1.0.OR.PSXECE.NE.0.0) THEN
                                         DO 1300 I  = 1,NUMST
                                         ELSTA(2,I) = PXSECR*ELSTA(2,I)
 1300                                    ELSTA(1,I) = PSXECE+ELSTA(1,I)
C#### WCH (RED), 9/93.
C#### RED (WCH), 12/31/93.  SHOULD BE PXSECR, NOT PSXECR!!
                                         STCHL      = PXSECR*STCHL
                                         STCHR      = PXSECR*STCHR
                                         ENDIF
C=======================================================================
C     ALL REQUIRED DATA IN PLACE. CREATE CURVES AS REQUIRED.
C     DUMMY OUT OVERBANK ROUGHNESS IF IT WAS NOT SPECIFIED.
C=======================================================================
      IF(XNR.EQ.0.0) XNR = XNCH
      IF(XNL.EQ.0.0) XNL = XNCH
C=======================================================================
C     FIND MIN AND MAX STAGE POINTS.
C     CHECK IF STATIONS ARE IN THE WRONG ORDER.
C=======================================================================
      ELMIN =  99999.0
      ELMAX = -99999.0
      DO 1400 I = 1,NUMST
      IF(ELSTA(1,I).GT.ELMAX) ELMAX = ELSTA(1,I)
      IF(ELSTA(1,I).LT.ELMIN) ELMIN = ELSTA(1,I)
      IF(I.GT.1) THEN
C#### WCH, 2/24/93.  MAKE ERROR CHECK SLIGHTLY MORE LIBERAL.
              IF(ELSTA(2,I)-ELSTA(2,I-1).LT.-0.001) FAILED=.TRUE.
              IF(FAILED) GO TO 9020
              ENDIF
 1400 CONTINUE
C=======================================================================
C     ESTABLISH RANGE ON CURVES.
C=======================================================================
      IF(DEEP.LE.0.0)         DEEP = ELMAX - ELMIN
      IF(DEEP.GT.ELMAX-ELMIN) DEEP = ELMAX - ELMIN
      WIDE        = ELSTA(2,NUMST)-ELSTA(2,1)
      ELDEL       = DEEP/25.0
cim### 9/8/00 change for transport  detailed output
      ELEVN        = ELMAX
C=======================================================================
C     DO A FEW BASIC CHECKS.
C=======================================================================
      IF(XNCH.EQ.0.0) THEN
                      WRITE(N6,1405)
                      NSTOP = NSTOP + 1
                      RETURN
                      ENDIF
CIM Change code here to check length against
CIM length on C1 lines.  Adopt convention that
CIM length on C1 line should be used.  For now
CIM stop program run if it doesn't
c      IF(XLEN.EQ.0.0) THEN
c                      WRITE(N6,1406)
c                      NSTOP = NSTOP + 1
c                      RETURN
c                      ENDIF
C=======================================================================
Cwch 3/14/02.Must worry about metric or not when calling from Transport.
C     Error found by Sandy Elliot. 
C=======================================================================
      CMETGET = 1.0
	IF(KNORM.EQ.1.) CMETGET = CMET(1,METRIC)
      IF(ABS((XLEN-XLENIN/CMETGET)/XLEN).GT.0.01) THEN
          IF (KNORM.EQ.0) THEN
C This is EXTRAN
             SELECT CASE (IWLEN)
               CASE (0)
                 WRITE(N6,7020) XLENIN, XLEN
                 nstop = nstop + 1
               CASE (1)
                 WRITE(N6,7040) XLENIN, XLEN
                 XLENIN = XLEN
               CASE (2)
                 WRITE(N6,7050) XLENIN, XLEN
             END SELECT
             ELSE
C This is TRANSPORT
Cwch, 3/14/02
             WRITE(N6,7030) XLENIN/CMETGET, XLEN
             nstop = nstop + 1
             ENDIF
          endif
      IF(WIDE.EQ.0.0) THEN
                      WRITE(N6,1407)
                      NSTOP = NSTOP + 1
                      RETURN
                      ENDIF
      IF(DEEP.EQ.0.0) THEN
                      WRITE(N6,1408)
                      NSTOP = NSTOP + 1
                      RETURN
                      ENDIF
      IF(NUMST.LE.1)  THEN
                      WRITE(N6,1409)
                      NSTOP = NSTOP + 1
                      RETURN
                      ENDIF
C=======================================================================
C     FIND STAGES, TOTAL AREAS, FLOWS AND TOP WIDTHS FOR EACH STAGE.
CIM### 9/8/00  MOVE MUCH OF CALCULATIONS TO SUBROUTINE IRRSECT
C=======================================================================
CIM  FIRST CALL FOR FULL CROSS SECTION
Cwch, 3/14/02. No metric conversions have occured for Transport.  Hence,
C     use full metric units in IRRSECT.  Use METRIC, not KMET as param. 
      CALL IRRSECT(0,ELDEL,ELMIN,KNORM,TWFULL,
     .STCHL,STCHR,XNL,XNCH,XNR,NUMST,RTSLOP,METRIC,elsta)
CIM DO BELOW ONLY IF IDETAIL equals 1
      IF(IDETAIL.EQ.1) THEN
CIM  SECOND CALL FOR SECTION BELOW 1/25 AFULL
cim  linearly interpolate to find first guess for depth
cim  corresponding to 1/25 afull
           I = 1
           AREAT = QCURVE(NATUR,2,26)/25.0
1500       I = I + 1
           IF (QCURVE(NATUR,2,I).LE.AREAT) GO TO 1500
C      WRITE(N6,*) I,AREAT,QCURVE(NATUR,2,I),QCURVE(NATUR,2,I-1)
           RATIO = (AREAT-QCURVE(NATUR,2,I-1))/
     .                    (QCURVE(NATUR,2,I)-QCURVE(NATUR,2,I-1))
           DEEP2 = ((I-2)+RATIO)*0.04*DEEP
cim below iterate to find DEEP2 that matches target area within 0.05%
           ERRMAX = 0.0005*AREAT
           ERRMIN = AREAT
           CLOSEST = 0.0
           ICOUNT=0
1505       CONTINUE
           ELDEL = DEEP2/25.0
C      WRITE(N6,*) RATIO,DEEP2,ELDEL
           CALL IRRSECT(1,ELDEL,ELMIN,KNORM,TWFULL2,
     .       STCHL,STCHR,XNL,XNCH,XNR,NUMST,RTSLOP,METRIC,ELSTA)
CIM   may need to iterate until qcurv2(natur,2,26) = 1/25 afull
           EDELTA = AREAT - QCURV2(NATUR,2,26)
cim      write(n6,*) icount,DEEP2,areat,qcurv2(natur,2,26),edelta
           IF ((ABS(EDELTA).GT.ERRMAX).AND.(ICOUNT.LT.100)) THEN
              IF (ABS(EDELTA).LT.ERRMIN) THEN
                 ERRMIN = ABS(EDELTA)
                 CLOSEST = DEEP2
                 ENDIF
CIM   NEW GUESS FOR DEEP2
              ICOUNT = ICOUNT + 1
              IF (ICOUNT.GE.100) THEN
                 DEEP2 = CLOSEST
                 GO TO 1505
                 ENDIF
              IF (ICOUNT.EQ.1) THEN
                 SLIPE = 0.0016/(QCURV2(NATUR,2,26)-QCURV2(NATUR,2,25))
                 DEEP2OLD = DEEP2
                 AREAOLD = QCURV2(NATUR,2,26)
                 DEEP2 = DEEP2 + EDELTA*SLIPE
                ELSE
                 IF (QCURV2(NATUR,2,26).NE.AREAOLD)
     .           SLIPE = (DEEP2-DEEP2OLD)/(qcurv2(natur,2,26)-AREAOLD)
                 DEEP2OLD = DEEP2
                 AREAOLD = QCURV2(NATUR,2,26)
                  ENDIF
              DEEP2 = DEEP2 + EDELTA*SLIPE
              GO TO 1505
              ENDIF
           ENDIF
CIM### 9/8/00 End of major changes.
C=======================================================================
C=======================================================================
C     CALCULATE AREA, HYDRAULIC RADIUS, AND TOP WIDTH FOR
C               POWER FUNCTION CROSS SECTIONS
C=======================================================================
C     POWER FUNCTION CHANNELS WILL HAVE SKIPPED NATURAL CROSS SECTION
C       INPUT AND CALCS.
C=======================================================================
 6666 CONTINUE
C=======================================================================
C	When called from Transport, metric conversions have already 
C     occured.  Hence, even if METRIC = 2, internal calcs for power-
C     function channels from Transport are in U.S. units. 
C=======================================================================
      IF(KCOND.EQ.1) THEN
           PEW        = 1.0/POW
           DEPTH      = 0.0
           DINC       = DEEP/25.0
           WETPER     = 0.0
           DO 2500 I  = 2,26
           DEPTH      = DEPTH + DINC
           CALL POWER(DEPTH,DEEP,TWFULL,WP,POW,DINC)
           WETPER = WETPER + WP
C#### WCH (RED), 12/16/94.  FIX INTERMEDIATE SLICE (2-26) CALCS FOR
C     POWER FUNCTION CHANNELS.
C     REMOVE TWFULL FROM AREA CALCULATIONS.
C####           AREA   = TWFULL*DEPTH*(1.0 - 1.0/(1.0 + POW))
           TWIDTH = TWFULL*DEPTH**PEW/DEEP**PEW
C#### WCH (RED), 12/16/94.  ADD TWIDTH TO AREA CALCULATIONS.
           AREA   = TWIDTH*DEPTH*(1.0 - 1.0/(1.0 + POW))
CIM### 9/8/00 MOVE RADIUS CALC BELOW AREA CALC !!???
           RADIUS = AREA/WETPER
           FLOW   = CMET(9,KMET)/RN0*RADIUS**0.666667*RTSLOP*AREA
           QCURVE(NATUR,1,I) = RADIUS
           QCURVE(NATUR,2,I) = AREA
Cwch, 7/6/01. Use same array for top width and new array 4 for flow.
Cwch           IF(KNORM.EQ.0) QCURVE(NATUR,3,I) = TWIDTH
Cwch           IF(KNORM.EQ.1) QCURVE(NATUR,3,I) = FLOW
           QCURVE(NATUR,3,I) = TWIDTH
           IF(KNORM.EQ.1) QCURVE(NATUR,4,I) = FLOW
 2500      CONTINUE
CIM ### 9/8/00
           IF(KNORM.EQ.1.AND.IDETAIL.EQ.1) THEN
CIM  This is for added detail below 1/25 of full area
cim  First find depth that corresponds to 1/25 afull
              AREAT = QCURVE(NATUR,2,26)/25.0
cim   solve directly for deep2 that corresponds to Areat
              PEW        = 1.0/POW
              DEEP2=((DEEP**PEW*AREAT)/(TWFULL*(1.0 - 1.0/(1.0 + POW))))
     1          **(1.0/(1.0+PEW))
              DINC2       = DEEP2/25.0
              TWFULL2= TWFULL*DEEP2**PEW/DEEP**PEW
cim           AREA   = TWFULL2*DEEP2*(1.0 - 1.0/(1.0 + POW))
cim      WRITE(N6,*) deep2,areat,area
              DEPTH      = 0.0
              WETPER     = 0.0
              DO 2550 I  = 2,26
              DEPTH      = DEPTH + DINC2
              CALL POWER(DEPTH,DEEP2,TWFULL2,WP,POW,DINC2)
              WETPER = WETPER + WP
C#### WCH (RED), 12/16/94.  FIX INTERMEDIATE SLICE (2-26) CALCS FOR
C     POWER FUNCTION CHANNELS.
C     REMOVE TWFULL FROM AREA CALCULATIONS.
C####           AREA   = TWFULL*DEPTH*(1.0 - 1.0/(1.0 + POW))
              TWIDTH = TWFULL2*DEPTH**PEW/DEEP2**PEW
C#### WCH (RED), 12/16/94.  ADD TWIDTH TO AREA CALCULATIONS.
              AREA   = TWIDTH*DEPTH*(1.0 - 1.0/(1.0 + POW))
CIM MOVE RADIUS CALC BELOW AREA CALC.
              RADIUS = AREA/WETPER
C
              FLOW   = CMET(9,KMET)/RN0*RADIUS**0.666667*RTSLOP*AREA
              QCURV2(NATUR,1,I) = RADIUS
              QCURV2(NATUR,2,I) = AREA
              QCURV2(NATUR,3,I) = FLOW
 2550         CONTINUE
              ENDIF
           ENDIF
CIM### 9/8/00 End of changes for power function.
C=======================================================================
C     CALCULATE EQUIVALENT HYDRAULIC RADIUS,
C     NORMALIZE CURVES, AND FIND MAX VALUES OF AREA AND EQUIV. HYD. RAD.
C=======================================================================
Cwch 4/14/02. For natural channels, Transport is working here in 
C     metric.  For power functions, Transport is working here in 
C     U.S. units. 
C=======================================================================
      IF(KCOND.EQ.0) CONST  =  CMET(9,METRIC)/RN0*RTSLOP
      IF(KCOND.EQ.1) CONST  =  CMET(9,KMET)/RN0*RTSLOP
C=======================================================================
C     Normalization process is independent of U.S. or metric units. 
C     QFULL, AFULL, RFULL etc. are passed to Transport in argument list
C     of call statement.  For natural channels, convert from metric 
C     to U.S. customary units in Sub. INTRAN, right after call 
C     statement.  For power function channels, no conversion is needed
C     in Sub. INTRAN.  
C=======================================================================
      AFULL  =  QCURVE(NATUR,2,26)
      IF(KNORM.EQ.0) QMAX   =  QCURVE(NATUR,1,26)
Cwch, 7/6/01. Use new array 4, not 3, for Transport flow. 
      IF(KNORM.EQ.1) QMAX   =  QCURVE(NATUR,4,26)
      IF(KCOND.EQ.0) RFULL  =  (QMAX/(AFULL*CONST))**1.5
      IF(KCOND.EQ.1) RFULL  =  QCURVE(NATUR,1,26)
Cwch, 7/6/01. Save TWFULL for Transport also.
Cwch      IF(KNORM.EQ.0) TWFULL =  QCURVE(NATUR,3,26)
      TWFULL =  QCURVE(NATUR,3,26)
C
      DO 1600 I = 2,26
      IF(KCOND.EQ.1) QCURVE(NATUR,1,I) = QCURVE(NATUR,1,I)/RFULL
      IF(KCOND.EQ.0) THEN
C=======================================================================
C NOTE THAT HERE FOR EXTRAN, QCURVE-1 IS CONVERTED FROM FLOW TO HYD RAD.
Cwch, 7/6/01. For Transport, use QCURVE4 instead of 3. 
C=======================================================================
           IF(KNORM.EQ.0) QCURVE(NATUR,1,I) =
     +  ((QCURVE(NATUR,1,I)/(CONST*QCURVE(NATUR,2,I)))**1.5)/RFULL
           IF(KNORM.EQ.1) QCURVE(NATUR,1,I) =
     +  ((QCURVE(NATUR,4,I)/(CONST*QCURVE(NATUR,2,I)))**1.5)/RFULL
           ENDIF
      QCURVE(NATUR,2,I) = QCURVE(NATUR,2,I)/AFULL
Cwch      IF(KNORM.EQ.0) QCURVE(NATUR,3,I) = QCURVE(NATUR,3,I)/TWFULL
Cwch      IF(KNORM.EQ.1) QCURVE(NATUR,3,I) = QCURVE(NATUR,3,I)/QMAX
      QCURVE(NATUR,3,I) = QCURVE(NATUR,3,I)/TWFULL
      IF(KNORM.EQ.1) QCURVE(NATUR,4,I) = QCURVE(NATUR,4,I)/QMAX
 1600 CONTINUE
CIM### 9/8/00
CIM   FOR ADDED DETAIL BELOW 1/25 AREA
cim   NOTE THAT THIS IS NORMALIZED TO AFULL, QMAX, RFULL
      IF (KNORM.EQ.1.AND.IDETAIL.EQ.1) THEN
C
         DO 1650 I = 2,26
         IF(KCOND.EQ.1) QCURV2(NATUR,1,I) = QCURV2(NATUR,1,I)/RFULL
         IF(KCOND.EQ.0) THEN
C=======================================================================
C NOTE THAT HERE FOR EXTRAN, QCURVE-1 IS CONVERTED FROM FLOW TO HYD RAD.
C=======================================================================
            QCURV2(NATUR,1,I) =
     +  ((QCURV2(NATUR,3,I)/(CONST*QCURV2(NATUR,2,I)))**1.5)/RFULL
            ENDIF
         QCURV2(NATUR,2,I) = QCURV2(NATUR,2,I)/AFULL
         QCURV2(NATUR,3,I) = QCURV2(NATUR,3,I)/QMAX
 1650    CONTINUE
         ENDIF
C======================================================================
C     CONVERT TO NORMALIZED AREA IF CALLED BY TRANSPORT
CIM### 9/8/00   CALCULATIONS MOVED TO SUBROUTINE TRANNORM
C======================================================================
      IF(KNORM.EQ.1) THEN
CIM  FIRST CALL FOR FULL SECTION
           CALL TRANNORM(0,DEEP2,DEEP)
CIM  SECOND CALL FOR SECTION BELOW 1/25 OF AFULL
           IF (IDETAIL.EQ.1) THEN
              CALL TRANNORM(1,DEEP2,DEEP)
cim correct radius, depth, and q for qcurve(natur,*,2) for true,
cim not interpolate, values at 0.04 afull.
              QCURVE(NATUR,1,2) = QCURV2(NATUR,1,26)
              QCURVE(NATUR,2,2) = QCURV2(NATUR,2,26)
Cwch, 7/6/01              QCURVE(NATUR,3,2) = QCURV2(NATUR,3,26)
              QCURVE(NATUR,4,2) = QCURV2(NATUR,3,26)
              ENDIF
           END IF
C======================================================================
C     PRINT CROSS SECTION OR CONDUIT INFORMATION FOR NATURAL CHANNELS.
C     THE 100th IRREGULAR STATION POINT IS SET = 0 TO GIVE
C     CORRECT OUTPUT IN PRINTING ROUTINE.
C     These prints should be OK for both Extran and Transport since
C     metric units have not been converted for Transport.  
C======================================================================
      IF(KCOND.EQ.0) THEN
           IF(JCE.EQ.0) WRITE(N6,1690) IDNUM
           IF(JCE.EQ.1) WRITE(N6,1691) KDNUM
           WRITE(N6,1696)  XFROM,NATUR
           IF(METRIC.EQ.1) THEN
                WRITE(N6,1700) XLEN,ELEVN,SLOPE,DEEP,
     +                               XNL,STCHL,AFULL,XNCH,RFULL
                WRITE(N6,1701) XNR,STCHR,TWFULL,QMAX
                IF(KSTOP.EQ.0) WRITE(N6,1710) NUMST,PSXECE,PXSECR
                ELSE
                WRITE(N6,2700) XLEN,ELEVN,SLOPE,DEEP,
     +                               XNL,STCHL,AFULL,XNCH,RFULL
                WRITE(N6,2701) XNR,STCHR,TWFULL,QMAX
                IF(KSTOP.EQ.0) WRITE(N6,2710) NUMST,PSXECE,PXSECR
                ENDIF
C
C CIM HERE CHECK THAT STCHL AND STCHR ARE ELSTA STATIONS
      OKL = .FALSE.
	OKR = .FALSE.
	DO I = 1,NUMST
	IF (ELSTA(2,I).EQ.STCHL) OKL = .TRUE.
	IF (ELSTA(2,I).EQ.STCHR) OKR = .TRUE.
	IF (OKL.AND.OKR) EXIT
	ENDDO
	IF (.NOT.OKL) THEN
	  WRITE(N6,7000) 'LEFT ','STCHL'
	  NSTOP = NSTOP + 1
	ENDIF
	IF (.NOT.OKR) THEN
	  WRITE(N6,7000) 'RIGHT','STCHR'
	  NSTOP = NSTOP + 1
	ENDIF
C#### WCH, 2/7/95.  CHANGE ELSTA TO VARIABLE DIMENSION.
C#### REPLACE 100 IN STATEMENTS BELOW BY NXSPTS.
C#### MAX. TIMES THROUGH LOOP = NXSPTS/5 = ILOPPX
           ELSTA(1,NXSPTS) = 0.0
           ELSTA(2,NXSPTS) = 0.0
           ILOPPX          = NXSPTS/5
           DO 1303 I    = 1,ILOPPX
           II           = 5*(I-1)+1
           III          = 5*(I-1)+2
           IF(III.GT.NUMST)   III = NXSPTS
                               IV = 5*(I-1)+3
           IF(IV.GT.NUMST)     IV = NXSPTS
                              IIV = 5*(I-1)+4
           IF(IIV.GT.NUMST)   IIV = NXSPTS
                             IIIV = 5*(I-1)+5
           IF(IIIV.GT.NUMST) IIIV = NXSPTS
           IF(KSTOP.EQ.0) WRITE(N6,1302) (ELSTA(K,II),K=1,2),
     +                (ELSTA(K,III),K=1,2),(ELSTA(K,IV),K=1,2),
     +                (ELSTA(K,IIV),K=1,2),(ELSTA(K,IIIV),K=1,2)
           IF(IIIV.GE.NUMST) GO TO 1304
 1303      CONTINUE
 1304      CONTINUE
           ENDIF
C======================================================================
C     PRINT CONDUIT INFORMATION FOR POWER FUNCTION CHANNELS
Cwch 3/14/02.  Here, need to back convert to metric for printout when
C     calling from Transport. 
C======================================================================
      IF(KCOND.EQ.1) THEN
                     CMETGET = 1.0
	IF(KNORM.EQ.1) CMETGET = CMET(1,METRIC)
	     IF(JCE.EQ.0) WRITE(N6,2690) IDNUM
           IF(JCE.EQ.1) WRITE(N6,2691) KDNUM
           IF(METRIC.EQ.1) THEN
                WRITE(N6,3700) XLEN,POW,DEEP,
     +                               RN0,AFULL,RFULL,TWFULL,NATUR
                ELSE
                WRITE(N6,3710) XLEN/CMETGET,POW,DEEP/CMETGET,
     +       RN0,AFULL/CMETGET**2,RFULL/CMETGET,TWFULL/CMETGET,NATUR
                ENDIF
           ENDIF
C=======================================================================
C     WRITE CROSS SECTION DIMENSIONLESS CURVES
C=======================================================================
Cwch, 7/6/01      IF(KNORM.EQ.0.AND.QCURVE(NATUR,3,1).EQ.0.0)
      IF(QCURVE(NATUR,3,1).EQ.0.0)
     +                  QCURVE(NATUR,3,1) = QCURVE(NATUR,3,2)
      IF(KSTOP.EQ.0) THEN
           IF(KNORM.EQ.0) WRITE(N6,1702)
           IF(KNORM.EQ.1) WRITE(N6,1703)
	     KK = 3
	     IF(KNORM.EQ.1) KK = 4
           DO 1900 I = 1,8
           II        = I + 9
           III       = I + 18
           WRITE(N6,3333) I,(QCURVE(NATUR,K,I),K=1,KK),II,
     !                    (QCURVE(NATUR,K,II),K=1,KK),III,
     !                    (QCURVE(NATUR,K,III),K=1,KK)
 1900      CONTINUE
           WRITE(N6,3333) 9,(QCURVE(NATUR,K,9),K=1,KK),18,
     !                    (QCURVE(NATUR,K,18),K=1,KK)
CIM### 9/8/00
C  WRITE ADDITIONAL TABLE FOR DETAILED PART OF CURVE
           IF(KNORM.EQ.1.AND.IDETAIL.EQ.1) THEN
CIM 2004 set KK to 3 for this write so correct format 3333 is used. 
Cwch 7/20/04.
              KK = 3 
              WRITE(N6,1704)
              WRITE(N6,1705)
              DO 1920 I = 1,8
              II        = I + 9
              III       = I + 18
              WRITE(N6,3333) I,(QCURV2(NATUR,K,I),K=1,3),II,
     !                    (QCURV2(NATUR,K,II),K=1,3),III,
     !                    (QCURV2(NATUR,K,III),K=1,3)
 1920         CONTINUE
              WRITE(N6,3333) 9,(QCURV2(NATUR,K,9),K=1,3),18,
     !                    (QCURV2(NATUR,K,18),K=1,3)
              ENDIF
           ENDIF
CIM### 9/8/00
C=======================================================================
      RETURN
C=======================================================================
C=======================================================================
C     PRINT POSSIBLE INPUT ERRORS
C=======================================================================
C     DATA COULD NOT BE FOUND
C=======================================================================
 9000 CONTINUE
      WRITE(N6,9010) XFROM
      NSTOP = NSTOP+1
      RETURN
C=======================================================================
C     DATA INPUT IN WRONG ORDER
C=======================================================================
 9020 CONTINUE
      WRITE(N6,9030) XFROM
      NSTOP = NSTOP+1
      RETURN
  888 CALL IERROR
C=======================================================================
  201 FORMAT(/,' ===> WARNING !!!  XNCH LE 0.0.')
 1302 FORMAT(5(F10.2,2X,F10.2,4X))
 1405 FORMAT(/,' ===> ERROR !!! MANNINGS "N" OF CENTER CHANNEL IS 0.0.
     +JOB STOPPED.')
 1406 FORMAT(/,' ===> ERROR !!! CHANNEL LENGTH IS 0.0.  JOB STOPPED.')
 1407 FORMAT(/,' ===> ERROR !!! WIDTH OF CHANNEL IS 0.0.  JOB STOPPED.')
 1408 FORMAT(/,' ===> ERROR !!! DEPTH OF CHANNEL IS 0.0.  JOB STOPPED.')
 1409 FORMAT(/,' ===> ERROR !!! NUMBER OF DATA POINTS LE 1.  JOB STOPPED
     +.')
 1410 FORMAT(/,' ===> ERROR !!! NATURAL CHANNEL LIMITED TO MAXIMUM OF
     *50 INTERIOR DEPRESSIONS.  JOB STOPPED.')
 1690 FORMAT(/,9X,'NATURAL CROSS-SECTION INFORMATION FOR CHANNEL ',
     +       I10,/,9X,53('='))
 1691 FORMAT(/,9X,'NATURAL CROSS-SECTION INFORMATION FOR CHANNEL ',
     +       A10,/,9X,53('='))
 1696 FORMAT(9X,' CROSS-SECTION ID (FROM X1 CARD) : ',F15.2,
     +          ' CHANNEL SEQUENCE NUMBER :',I6,/)
 1700 FORMAT(3X,'LENGTH    :',F12.2,' FT ',T55,
     +          ' MAXIMUM ELEVATION        : ',F10.2,'     FT.',/,
     +3X,       'SLOPE     :',F12.4,' FT/FT',T55,
     +          ' MAXIMUM DEPTH            : ',F10.2,'     FT.',/,
     +3X,       'MANNING N :',F12.3,' TO STATION  ',F10.1,T55,
     +          ' MAXIMUM SECTION AREA     : ',1PG10.2,' SQ. FT.',
     +/,3X,'  "    "  :',0PF12.3,' IN MAIN CHANNEL',T55,
     +' MAXIMUM HYDRAULIC RADIUS : ',F10.2,'     FT.')
 1701  FORMAT(3X,'  "    "  :',F12.3,' BEYOND STATION',F8.1,T55,
     +' MAX TOPWIDTH             : ',F10.2,'     FT.',/,T55,
     +' MAXIMUM UNIFORM FLOW     : ',1PE10.2,' CFS.   ')
 1702 FORMAT(///,26X,' CROSS-SECTION DIMENSIONLESS CURVES ',/,
     +           26X,'       NORMALIZED BY DEPTH',/,
     +      24X,42('-')/
     +      3(' POINT  HYDRAULIC                           '),/,
     +      3('  NO.    RADIUS       AREA     TOPWIDTH     '),/,
     +      3(' -----  ---------    -----     --------     '))
Cwch, 7/6/01
 1703 FORMAT(//,26X,' CROSS-SECTION DIMENSIONLESS CURVES ',/,
     +           26X,'       NORMALIZED BY AREA',/,
     +      24X,42('-')/
     +      3(' POINT  HYDRAULIC                                       '
     +),/,
     +      3('  NO.    RADIUS      DEPTH    TOP WIDTH        FLOW     '
     +),/,
     +      3(' -----  ---------     ----     --------     -------     '
     +))
CIM### 9/8/00
 1704 FORMAT(//,26x,' DETAILED NORMALIZED CURVE FOR PORTION OF SECTION',
     +' BELOW 0.04 AFULL',//)
 1705 FORMAT(//,
     +      24X,42('-')/
     +      3(' POINT  HYDRAULIC                           '),/,
     +      3('  NO.    RADIUS      DEPTH         FLOW     '),/,
     +      3(' -----  ---------     ----     --------     '))
 1710 FORMAT(//,52X,'CROSS-SECTION POINTS',/,
     +          51X,'--------------------',/,10X,
     +'THE FOLLOWING ',I2,' STATIONS WERE READ AND ADJUSTED',
     +F9.3,' FT VERTICALLY AND HORIZONTALLY BY A RATIO OF ',F6.3,//,
     +5('   ELEVATION   STATION    '),/,
     +5('      FT         FT       '),/,
     +5('   ---------   -------    '))
 2690 FORMAT(/,9X,'POWER FUNCTION CROSS-SECTION INFORMATION',
     +       ' FOR CHANNEL ',I10,/,9X,53('='))
 2691 FORMAT(/,9X,'POWER FUNCTION CROSS-SECTION INFORMATION',
     +       ' FOR CHANNEL ',A10,/,9X,53('='))
 2700 FORMAT(3X,'LENGTH    :',F12.1,' METERS.',T55,
     +' MAXIMUM ELEVATION        : ',F10.2,' METERS.',/,
     +3X,'SLOPE     :',F12.4,'  M/M ',T55,
     +' MAXIMUM DEPTH            : ',F10.2,' METERS.',/,
     +3X,'MANNING N :',F12.3,' TO STATION  ',F10.1,T55,
     +' MAXIMUM SECTION AREA     : ',1PG10.2,' SQ.MET.',
     +/,3X,'  "    "  :',0PF12.3,' IN MAIN CHANNEL',T55,
     +' MAXIMUM HYDRAULIC RADIUS : ',F10.2,' METERS.')
 2701  FORMAT(3X,'  "    "  :',F12.3,' BEYOND STATION',F8.1,T55,
     +' MAX TOPWIDTH             : ',F10.2,' METERS.',/,T55,
     +' MAXIMUM UNIFORM FLOW     : ',1PE10.2,' CMS.   ')
 2710 FORMAT(//,52X,'CROSS-SECTION POINTS',/,
     +          51X,'--------------------',/,10X,
     +'THE FOLLOWING ',I2,' STATIONS WERE READ AND ADJUSTED',
     +F9.3,'  M VERTICALLY AND HORIZONTALLY BY A RATIO OF ',F6.3,//,
     +5('   ELEVATION   STATION    '),/,
     +5('    METERS     METERS     '),/,
     +5('   ---------   -------    '))
Cwch, 7/6/01 3333 FORMAT(3(I3,3F12.4,5X))
Cwch  Try CVF "variable format expression" option. Lang.Ref.Manual 11-44.
 3333 FORMAT(3(I3,<KK>F12.4,5X))
 3700 FORMAT(9X,'LENGTH                   :',F12.1,' FEET.',/,
     +       9X,'EXPONENT OF CHANNEL      :',F12.3,/,
     +       9X,'MAXIMUM DEPTH            :',F12.2,' FEET.',/,
     +       9X,'MANNING N                :',F12.3,/,
     +       9X,'MAXIMUM SECTION AREA     :',F12.2,' SQ. FT.',/,
     +       9X,'MAXIMUM HYDRAULIC RADIUS :',F12.3,' FEET.',/,
     +       9X,'MAXIMUM TOP WIDTH        :',F12.2,' FEET.',/,
     +       9X,'CHANNEL SEQUENCE NUMBER  :',I12)
 3710 FORMAT(9X,'LENGTH                   :',F12.1,' METERS.',/,
     +       9X,'EXPONENT OF CHANNEL      :',F12.3,/,
     +       9X,'MAXIMUM DEPTH            :',F12.2,' METERS.',/,
     +       9X,'MANNING N                :',F12.3,/,
     +       9X,'MAXIMUM SECTION AREA     :',F12.2,' SQ.MET.',/,
     +       9X,'MAXIMUM HYDRAULIC RADIUS :',F12.3,' METERS.',/,
     +       9X,'MAXIMUM TOP WIDTH        :',F12.2,' METERS.',/,
     +       9X,'CHANNEL SEQUENCE NUMBER  :',I12)
Cwch, 3/15/02.
 9005 FORMAT(/,' ERROR. Value of cross-section ID on Transport E1 line (
     1BARREL)',/,' or Extran C1 line (STHETA) is zero for input line:',
	2I3,/,' Value on E1 or C1 line must be >>numeric<<, not alphanumeri
	3c',/,' to correspond to SECNO on E3 or C3 or X1 line.',/,
	4' This is most likely cause of error.')
 9010 FORMAT(/,' ===> ERROR !! FAILURE TO FIND DATA FOR SECTION',
     +' (',F10.3,')',/,'               JOB ENDED')
 9011  FORMAT(/,'  ===> ERROR, END OF DATA SET FOUND WHILE READING',
     ! 'AREA/STAGE DATA AT JUNCTION # ',I10)
 9012  FORMAT(/,'  ===> ERROR !!, END OF DATA SET FOUND WHILE READING',
     ! 'VOLUME/STAGE DATA AT JUNCTION # ',I10)
 9030 FORMAT(/,' ===> ERROR !! STATIONS NOT IN CORRECT SEQUENCE',
     +' FOR SECTION (',F10.3,')',/,'                JOB ENDED')
 9040 FORMAT(/,' ===> ERROR !! UNABLE TO MATCH NATURAL CHANNEL CROSS SEC
     *TION DATA FOR CHANNEL',F10.1,/,
     *'        LAST SECNO (LINE C3) READ WAS',F10.1,' $$ JOB ENDED.')
Cwch, 4/16/02.
 9600 FORMAT(/,' ===> ERROR !! . CONDUIT SLOPE WAS ZERO',
     +         ' FOR CONDUIT ',I10,' Assign default of 0.01.')
 9601 FORMAT(/,' ===> ERROR !! . CONDUIT SLOPE WAS ZERO',
     +         ' FOR CONDUIT ',A10,' Assign default of 0.01.')
C 9600 FORMAT(/,' ===> FATAL ERROR !! . CONDUIT SLOPE WAS ZERO',
C     +         ' FOR CONDUIT ',I10)
C 9601 FORMAT(/,' ===> FATAL ERROR !! . CONDUIT SLOPE WAS ZERO',
C     +         ' FOR CONDUIT ',A10)
 7000 FORMAT(' ERROR - THE STATION OF THE ',A5,' BANK OF THE CHANNEL (',
     1A5,') DOES NOT CORRESPOND TO A STATION IN THE CROSS SECTION DATA')
 7020 FORMAT(/,' ===> ERROR !!! CHANNEL LENGTH ON C1 LINE DOES NOT',
     a' MATCH LENGTH ENTERED IN IRREGULAR CHANNEL DATA.',/,
     a         '      LENGTH ON C1 LINE EQUALS      ',F10.3,/,
     a         '      LENGTH IN CHANNEL DATA EQUALS ',F10.3)
 7030 FORMAT(/,' ===> ERROR !!! CHANNEL LENGTH ON E1 LINE DOES NOT',
     a' MATCH LENGTH ENTERED IN IRREGULAR CHANNEL DATA.',/,
     a         '      LENGTH ON E1 LINE EQUALS      ',F10.3,/,
     a         '      LENGTH IN CHANNEL DATA EQUALS ',F10.3)
 7040 FORMAT(/,' ===> WARNING !!! CHANNEL LENGTH ON C1 LINE DOES NOT',
     a' MATCH LENGTH ENTERED IN IRREGULAR CHANNEL DATA.',/,
     a' LENGTH ON C3/E1 LINE IS USED (IWLEN = 1)',/,
     a         '      LENGTH ON C1 LINE EQUALS      ',F10.3,/,
     a         '      LENGTH IN CHANNEL DATA EQUALS ',F10.3)
 7050 FORMAT(/,' ===> WARNING !!! CHANNEL LENGTH ON C1 LINE DOES NOT',
     a' MATCH LENGTH ENTERED IN IRREGULAR CHANNEL DATA.',/,
     a' LENGTH ON C1 LINE IS USED (IWLEN = 2)',/,
     a         '      LENGTH ON C1 LINE EQUALS      ',F10.3,/,
     a         '      LENGTH IN CHANNEL DATA EQUALS ',F10.3)

C=======================================================================
      END
``` 
