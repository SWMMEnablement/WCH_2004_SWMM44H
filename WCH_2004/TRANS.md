```fortran 
      SUBROUTINE TRANS
C	TRANSPORT BLOCK
C=======================================================================
C     UNIVERSITY OF FLORIDA TRANSPORT MODEL
C
C     This large subroutine runs the main computational loops of the
C     Transport.   Updated December, 1990 BY R.E.D.
C     Updated 4/6/93 by WCH for metric conversion on output interface
C       file.
C     WCH (CDM - Chuck Moore), 8/93.  Check for subscript out of range
C       and not enough R1 lines.
C     WCH, 10/6/93.  Include storage unit pollutants in initial quality
# Transport Subroutine (TRANS) Overview

This document summarizes the functionality and structure of the TRANS subroutine from the University of Florida Transport Model. The code, originally written in Fortran, implements the main computational loops for water quality and flow simulation. Its extensive history of modifications is documented within the comments.

---

## 1. Initialization and Declarations

- **Inclusions:**  
      The subroutine begins by including several header files (e.g., TAPES.INC, INTER.INC, STIMER.INC, etc.) which provide definitions for global variables, file I/O routines, and simulation parameters.

- **Variable Dimensions:**  
      Arrays are allocated for flow variables (QI, QO, QO1, QO2), surge storage, pollutant tracking (PFILEI, PFILEO), and more. Also, physical parameters such as slopes, distances, and geometry parameters are defined. Comments indicate adjustments—for example, changing array dimensions to accommodate a variable `MQUAL` (number of qualities).

- **Constants and Equivalences:**  
      Variables are associated using FORTRAN’s EQUIVALENCE statements to allow shared memory for different representations. A linear interpolation function, `QLINTP`, is defined to compute intermediate flow or quality values.

---

## 2. Data Input and Preprocessing

- **Initial Data Input:**  
      The subroutine calls `INTRAN` to read simulation inputs from an interface file. It then rewinds several scratch file streams (NSCRT1, NSCRT2, NSCRT7) to prepare for further processing.

- **Array Initialization:**  
      Arrays such as XNT (accumulation variables) and XNT27 (for type 27 quality split elements) are zeroed out. This step ensures that previous runs or uninitialized memory do not affect the current simulation.

- **Calculation of Initial Conduit Volumes:**  
      A loop over network elements initializes conduit variables including storage lengths, rates, and pollutant loads. Modifications in the comments reflect improvements such as changing volume/barrel calculations and the incorporation of storage unit pollutant loads.

---

## 3. Generating Output and Interface Data

- **Interface File Write-Out:**  
      The subroutine writes a header line for an interfacing model. It converts flow quantities using a metric conversion factor (`CMET`) and formats pollutant data appropriately.

- **Time-Step and Quality Data:**  
      An outer loop over time steps and an inner loop over network elements perform the simulation updates. These updates include:
      - Updating simulation time and flow accumulators.
      - Interpolating between successive inputs to compute intermediate values.
      - Routing quality parameters through the network and updating pollutant concentrations.

- **Handling R1 Input Lines:**  
      Two modes are provided:
      - **Multiple R1 Lines (IFLIP = 0):** Data is read per element with pollutant checking.
      - **Single R1 Line (IFLIP = 1):** A consolidated input read occurs.  
      The code rigorously checks for correct identification (using an 'R1' marker) and handles errors by writing to error streams and executing a stop.

---

## 4. Main Computational Loops

- **Time Loop:**  
      The outer loop iterates over a defined number of time steps (`NDT`), updating simulation time (TIME, TIMEHR, TIMDAY) and processing input data according to the current time.

- **Element Loop:**  
      For each time step, the subroutine loops over elements (using indices based on the internal network structure). Key operations include:
      - **Flow Routing:**  
            The calculation involves summing upstream flows, determining if a conduit is surcharging, and adjusting flow capacities.
      - **Quality Routing:**  
            Quality parameters (e.g., pollutant concentrations) are routed through each element with linear interpolation based on elapsed time.
      - **Design Adjustments:**  
            If elements (such as pipes) are found to be surcharging, the subroutine can invoke design routines:
            - For circular conduits, diameters are increased.
            - For rectangular conduits, widths are adjusted.
            - Replacements may occur if an element’s design is not suitable.
            
- **Additional Calculations:**  
      The code includes logic to handle monthly and hourly variations such as:
      - Dry weather flow adjustments.
      - Base-flow modifications.
      - Surcharging statistics and output accumulation (using arrays such as XNT).

---

## 5. Output Generation and Post-Processing

- **Output Tapes and Scratch Files:**  
      After updating flows and qualities, the program writes:
      - **Interface Outputs:**  
            Generated with appropriate metric conversions for use by an interfacing model.
      - **Scratch Tape Outputs:**  
            Detailed records for printouts, including depth measurements and flow summaries.
      
- **Quality and Hydrograph Print Routines:**  
      Conditional calls to external routines (`PRINTQ`, `PRINTF`) are made to produce formatted outputs of the hydrographs and pollutant loadings.

- **Final Simulation Summary:**  
      A section writes a final summary including:
      - Ending date and simulation total time.
      - Presentation of updated element parameters in a tabulated format with detailed format statements.
      
- **Formats:**  
      Extensive FORMAT statements are included to ensure that output is well-aligned and contains descriptive headings. These formats are customized based on whether the metric system is used and whether the conduit design conventions differ.

---

## 6. Error Handling and Termination

- **Error Codes:**  
      The program includes error checking in several places. For instance, if a misidentified R1 line is encountered, or if an array subscript goes out of range, the subroutine writes an error message and calls the IERROR routine.
      
- **Finalization:**  
      At the end, the subroutine calls `OTRAIN` to carry out any final processing before returning, ensuring that the simulation ends normally and outputs are correctly written.

---

## 7. Historical Modifications and Annotations

- **Revision History:**  
      Inline comments indicate updates made by various authors over time:
      - Inclusion of metric conversions.
      - Adjustments to handle additional quality components and storage unit pollutants.
      - Updates to the design routine to prevent surcharging.
      - Improvements to output formatting.
      
- **Legacy Comments:**  
      Throughout the subroutine, legacy documentation remains, providing context for modifications (e.g., changes made in 1990, 1993, 1994, and updates by authors such as R.E.D., WCH, and others).

---

## Conclusion

The TRANS subroutine is a comprehensive routine central to the transport model simulation. It:

- Initializes and validates simulation inputs.
- Performs iterative flow and quality routing with interpolation.
- Adjusts conduit parameters dynamically via design modifications.
- Outputs detailed simulation results for interface and print purposes.

The code’s structure highlights a robust approach to water quality and flow simulation in a complex hydraulic network, reflecting decades of iterative improvement and adaptation.

C     James L. Martin, AScI Corp., 10/93.  Provide code for writing out
C       WASP linkage file.
C     RED, 11/29/93.  Eliminate third argument in FIRST call statement.
C     RED, 12/31/93.  Fix loop in R1 read statement for IFLIP = 1.
C     WCH (Steve Merrill, Brown and Caldwell), 3/28/94.  Add provision
C       to store depths from storage units (using I2 input lines).
C     WCH, 5/12/94.  Include number of barrels in initial volume and
C       mass computation.
C     WCH, 10/14/94.  Allow depth prints without hydrograph prints.
C     WCH, 8/7/96.  Increase field width for time step print out.
C     WCH, 8/7/96.  Rewind scratch files.
C     WCH, 8/29/00. Improve output for design routine.
C     CIM, 9/8/00.Changes for outputing head and velocity
C          to interface file.
C     WCH, 2/15/01. Change time variables to double precision. 
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
      INCLUDE 'NEWTR.INC'
C=======================================================================
      DIMENSION QI(NET),QO(NET),SURGE2(NET),IAMFL(NET),
     1          WELL2(NET),QO1(NET),QO2(NET),OUTTAP(NTHO),
Change dimensions to MQUAL
     1          PFILEI(MQUAL,NTHI),PFILEO(MQUAL,NTHO)
      CHARACTER BLANK*1,ASTER*1,QQQ*1,BMJ*10
Cwch, 2/15/01.  Make time variables double precision
      REAL*8 TIMEHR,TFLOW,TREF,THR
      EQUIVALENCE (QO(1),Q(1,2,2)),(QI(1),Q(1,1,2)),(SURGE2(1),P2(1))
      EQUIVALENCE (QO1(1),QMAX(1)),(QO2(1),QFULL(1)),(WELL2(1),ROUGH(1))
      DATA ASTER/'*'/,BLANK/' '/
C=======================================================================
C     DEFINE STATEMENT FUNCTION FOR LINEAR INTERPOLATION.
C=======================================================================
      QLINTP(Q1,Q2,T1,T2,T) = Q1 + (Q2-Q1)*(T-T1)/(T2-T1)
C=======================================================================
C     CALL SUBROUTINE INTRAN FOR DATA INPUT
C=======================================================================
      DO 500 I  = 1,NET
      NODSGN(I) = 0
 500  CONTINUE
      CALL INTRAN
      NSCRT1   = NSCRAT(1)
      NSCRT2   = NSCRAT(2)
      NSCRT7   = NSCRAT(7)
C=======================================================================
C#### WCH, 8/7/96.  REWIND THESE FILES!  FYI, NSCRAT(1) IS USED EARLIER
C     IN SUBROUTINE FILTH.
C=======================================================================
      IF(NSCRT1.GT.0) REWIND NSCRT1
      IF(NSCRT2.GT.0) REWIND NSCRT2
      IF(NSCRT7.GT.0) REWIND NSCRT7
C
      OLDDAY   = NDAY
CIMT  change from 50 to 10+6*MQUAL
      DO 100 I = 1,(10+6*MQUAL)
 100  XNT(I)   = 0.0
CIMQP INITIALIZE ARRAY FOR TYPE 27 QUALITY SPLIT ELEMENTS
      DO I=1,NE+1
	DO J=1,MQUAL
	XNT27(I,J) = 0.0
	ENDDO
	ENDDO
CIMQP
C=======================================================================
C     CALCULATE INITIAL CONDUIT VOLUME
C=======================================================================
      DO 1000 I  = 1,NE
      M          = JR(I)
      IAMFL(M)   = 0
      SURLEN(M)  = 0.0
      RANQ(M)    = 0.0
      QR(M)      = 0.0
      QBIG(M)    = 0.0
      SBIG(M)    = 0.0
      SMAL(M)    = 100000.0
      SMEAN(M)   = 0.0
      SPEAK(M)   = 0.0
      EMAX(M)    = 0.0
	DO J=0,MQUAL
      STOTAL(M,J)  = 0.0
	ENDDO
      KITMAX(M)  = 0
      KITER(M)   = 0
      DO 110  J  = 1,2
      ISTIME(M,J)= 0
  110 JSTIME(M,J)= 0
      IF(NPOLL.GT.0) THEN
CIMT   CHANGE upper range from 8 to 2*MQUAL
                     DO 1100 K  = 1,2*MQUAL
 1100                RANK(M,K)  = 0.0
                     DO 1150 K  = 1,NPOLL
CIMT    change from 4 to MQUAL
                     K1         = K + MQUAL
 1150                RANK(M,K1) = SCOUR(M,K)
                     ENDIF
      IF(NTYPE(M).EQ.22) THEN
           IS     = KSTORE(M)
           XNT(5) = XNT(5) + STORE(IS)
C#### WCH, 10/6/93.  INCLUDE INITIAL POLLUTANT LOAD.
           IF(NPOLL.GT.0) THEN
                DO 1155 K = 1,NPOLL
CIMT  CHANGE FROM 12 to (8+MQUAL)
 1155           XNT((8+MQUAL)+K) = XNT((8+MQUAL)+K) +
     1                       CPOL2(M,1,K)*STORE(IS)*28.3E-06
                ENDIF
           ENDIF
      IF(NTYPE(M).GT.18) GO TO 1000
C#### WCH, 5/12/94. INCLUDE NUMBER OF BARRELS IN VOLUME/MASS CALC.
      AAA        = 0.5*(A(M,1,1)+A(M,2,1))*DIST(M)*BARREL(M)
      XNT(5)     = XNT(5) + AAA
      IF(NPOLL.GT.0) THEN
               DO 1160 K  = 1,NPOLL
CIMT CHANGE FROM 12 to (8+MQUAL)
               XNT((8+MQUAL)+K)  = XNT((8+MQUAL)+K) +
     1                        CPOL2(M,1,K)*AAA*28.3E-06
 1160          CONTINUE
               ENDIF
 1000 CONTINUE
C=======================================================================
C     GENERATE FIRST LINE OF OUTPUT TAPE TO BE USED BY INTERFACING MODEL
C=======================================================================
      IF(NEXT.GT.0.AND.NOUTS.GT.0) THEN
              DELTA     = 0.0
              DO 1666 J = 1,NOUTS
              IF(JCE.EQ.0.AND.NOE(M).NE.JN(J))  GO TO 1666
              IF(JCE.EQ.1.AND.KOE(M).NE.KJN(J)) GO TO 1666
C#### WCH, 4/6/93.  ADD METRIC CONVERSION FOR OUTPUT INTERFACE FILE.
              OUTTAP(J) = QO(M)/CMET(8,METRIC)
              IF(NPOLL.GT.0) THEN
                         DO 1620 K  = 1,NPOLL
C#### WCH, 4/6/93.  ADD METRIC CONVERSION FOR OUTPUT INTERFACE FILE.
 1620                    PFILEO(K,J) = CPOL2(M,2,K)*QO(M)/CMET(8,METRIC)
                         ENDIF
              GO TO 1667
 1666         CONTINUE
CIM### 9/8/00    move 1667 to here
 1667         CONTINUE
CIM### 9/8/00    GET HEAD AND VELOCITIES TO LAST 2 ARRAY SPACES
              IF (NPOLL2.GT.NPOLL) CALL GETHV(PFILEO)
CIM### 9/8/00   CHANGE NPOLL TO NPOLL2
              IF(NPOLL2.LE.0) WRITE(NEXT) JULDAY,TIMDAY,
     +                       DELTA,(OUTTAP(K),K=1,NOUTS)
              IF(NPOLL2.GT.0) WRITE(NEXT) JULDAY,TIMDAY,DELTA,
     +           (OUTTAP(K),(PFILEO(J,K),J=1,NPOLL2),K=1,NOUTS)
              ENDIF
C=======================================================================
C     BEGIN MAIN LOOPS OF PROGRAM
C     OUTER LOOP ON TIME, INNER LOOP ON ELEMENT NUMBER
C=======================================================================
      TREF   = 0.0
      TIME   = TIMDAY
      TFLOW  = TIMDAY/3600.0
      JDAY   = JULDAY
      TMDAY  = TIMDAY
      WRITE(*,23)  NDT
      DO 200 N = 1,NDT
cim   need to write to 6 not * to get carriage control on writes to console
      WRITE(6,22) N
C=======================================================================
C     UPDATE TIME OF DAY.
C     TIME   = RUNNING TIME OF SIMULATION, SECONDS.
C     TIMEHR = RUNNING TIME IN HRS, BEGINNING AT TZERO.
C     TIMDAY = TIME OF DAY IN HRS.
C=======================================================================
      CALL STIME(DT)
      CALL DATED
      TIME   = TIME + DT
      TIMEHR = TIME/3600.0
      IF(NINPUT.GT.0) THEN
                      DO 4510 I  = 1,NE
                      RNOFF(I)   = 0.0
                      IF(NPOLL.GT.0) THEN
                                     DO 4511 K  = 1,NPOLL
 4511                                PLUTO(K,I) = 0.0
                                     ENDIF
 4510                 CONTINUE
                      ENDIF
C=======================================================================
C     READ INPUT FROM INTERFACE FILE AT REQUIRED TIMES.
C     PERFORM LINEAR INTERPOLATION FOR INTERMEDIATE VALUES.
C=======================================================================
      IF(NCNTRL.EQ.0) THEN
 4515 IF(JULDAY.GT.JDAY.OR.(JULDAY.EQ.JDAY.AND.TIMDAY.GE.TMDAY)) THEN
              DO 4523 I = 1,LOCATS
              QF1(I)    = QF2(I)
              IF(NPOLL.GT.0) THEN
                             DO 4520 J = 1,NPOLL
 4520                        PF1(J,I)  = PF2(J,I)
                             ENDIF
 4523         CONTINUE
              IF(NQUAL.LE.0) READ(LAST,END=205) JDAY,TMDAY,
     +                       DELTA,(QF2(I),I=1,LOCATS)
              IF(NQUAL.GT.0) READ(LAST,END=205) JDAY,TMDAY,
     +           DELTA,(QF2(I),(PFILEI(J,I),J=1,NQUAL),I=1,LOCATS)
              JYEAR = JDAY/1000
              IF (JYEAR.LT.100) then
                   JDAY = JDAY - JYEAR*1000
                   JYEAR = JYEAR + 1900
                   JDAY = JDAY + JYEAR*1000
                   ENDIF
              TREF = TIMEHR
              CALL NTIME(JDAY,TMDAY,TFILE)
              IF(TFILE.LT.0.0) GO TO 4515
              IF(TFILE.LT.DT/10.0) TFILE = 0.0
              TFILE = TFILE/3600.0
              IF(NPOLL.GT.0) THEN
                             DO 4530 I = 1,LOCATS
                             DO 4540 J = 1,NPOLL
                             KPOL      = IPOLX(J)
                             IF(KPOL.NE.0) PF2(J,I) = PFILEI(KPOL,I)
 4540                        CONTINUE
 4530                        CONTINUE
                             ENDIF
C=======================================================================
C             CONTINUE READING BEYOND THE END OF THE INTERFACE FILE
C=======================================================================
              GO TO 9320
  205         JDAY      = 9999999
              TMDAY     = 0.0
              TREF      = TZERO/3600.0 + DT*FLOAT(NDT)/3600.0
              DO 9300 I = 1,LOCATS
              QF1(I)    = 0.0
              QF2(I)    = 0.0

C*************** MODIFIED FOLLOWING STATEMENTS - TRANSPOSITION ERROR ****
C*************** CHANGED PF1(I,J) TO PF1(J,I), PF2 ALSO              ****
C*************** JANUARY 1992    -   LISA BENSON, CSC                ****
CIMT CHANGE UPPER RANGE OF J FROM NQUAL TO NPOLL to be consistent
CIMT with other loops in this routine concerning PF1 and PF2.
              IF(NQUAL.GT.0) THEN
                             DO 9310 J  = 1,NPOLL
                             PF1(J,I)   = 0.0
 9310                        PF2(J,I)   = 0.0
                             ENDIF
 9300         CONTINUE
              ENDIF
 9320 THR       = TIMEHR - TREF
      DO 4570 I = 1,LOCATS
      NNEED     = NLOC(I)
      BMJ       = KAN(I)
      IF(NNEED.LT.0) GO TO 4570
      NS2 = NIN(NNEED,BMJ)
      QQ1 = QF1(I)
      QQ2 = QF2(I)
      IF(TFILE.EQ.0.0) RNOFF(NS2) = QQ2*QCONV
      IF(TFILE.GT.0.0) RNOFF(NS2) = QLINTP(QQ1,QQ2,0.0,TFILE,THR)*QCONV
      XNT(1) = XNT(1) + RNOFF(NS2)*DT
      IF(NPOLL.GT.0) THEN
                     DO 4560 J = 1,NPOLL
                     QQ1       = PF1(J,I)
                     QQ2       = PF2(J,I)
                     IF(TFILE.EQ.0.0) PLUTO(J,NS2) = QQ2*QCONV
                     IF(TFILE.GT.0.0) PLUTO(J,NS2) = QLINTP(QQ1,QQ2,
     +                                 0.0,TFILE,THR)*QCONV
 4560                CONTINUE
                     ENDIF
 4570 CONTINUE
      ENDIF
C=======================================================================
C     Read input from R1 lines at required times.
C=======================================================================
      IF(NINPUT.GT.0) THEN
      IF(TIMEHR.LE.TFLOW) GO TO 4650
 4610 TFLW      = TFLOW
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP R1 <<<<<<<<<<<<
C=======================================================================
C     Input flow and water quality on multiple R1 lines.
C=======================================================================
      IF(IFLIP.EQ.0) THEN
           DO 4640 I = 1,NINPUT
           QE1(I)    = QE2(I)
           IF(NPOLL.GT.0) THEN
                DO 4620 J = 1,NPOLL
 4620           PE1(J,I)  = PE2(J,I)
                ENDIF
C#### WCH (CDM), 8/93.  ADD CHECK FOR CORRECT CC NAME.
           IF(NPOLL.EQ.0) THEN
                READ(N5,*,ERR=888) CC,TFLOW,QE2(I)
                ELSE
                READ(N5,*,ERR=888) CC,TFLOW,QE2(I),(PE2(J,I),J=1,NPOLL)
                ENDIF
           IF(CC.NE.'R1') THEN
                WRITE(N6,9150) TIMEHR,CC
                WRITE(*,9150)  TIMEHR,CC
                STOP
                ENDIF
 4640      CONTINUE
           ENDIF
C=======================================================================
C     INPUT FLOW AND WATER QUALITY ON ONE R1 LINE
C=======================================================================
      IF(IFLIP.EQ.1) THEN
           DO 4645 I = 1,NINPUT
           IF(NPOLL.GT.0) THEN
                     DO 4625 J = 1,NPOLL
 4625                PE1(J,I)  = PE2(J,I)
                     ENDIF
 4645      QE1(I)    = QE2(I)
C#### WCH (CDM), 8/93.  ADD CHECK FOR CORRECT CC NAME.
           IF(NPOLL.EQ.0) THEN
C#### RED (WCH), 12/31/93. FIX MISSING DO LOOP.  (SHOULDN'T BE SAME AS
C####                      FOR IFLIP = 0.)
C
C####           READ(N5,*,ERR=888) CC,TFLOW,QE2(I)
C####           ELSE
C####           READ(N5,*,ERR=888) CC,TFLOW,QE2(I),(PE2(J,I),J=1,NPOLL)
C####           ENDIF
C
                READ(N5,*,ERR=888) CC,TFLOW,(QE2(I),I=1,NINPUT)
                ELSE
                READ(N5,*,ERR=888) CC,TFLOW,(QE2(I),
     1                         (PE2(J,I),J=1,NPOLL),I=1,NINPUT)
                ENDIF
           IF(CC.NE.'R1') THEN
                WRITE(N6,9150) TIMEHR,CC
                WRITE(*,9150)  TIMEHR,CC
                STOP
                ENDIF
           ENDIF
C=======================================================================
      IF(TFLOW.LT.TIMEHR) GO TO 4610
 4650 DO 4670 I  = 1,NINPUT
      NNEED      = KORDER(I)
      BMJ        = BORDER(I)
      NS2        = NIN(NNEED,BMJ)
C#### WCH (CDM), 8/93.  ADD CHECK FOR SUBSCRIPT OUT OF RANGE.
      IF(NS2.LE.0.OR.NS2.GT.NET) WRITE (N6,9100) TFLOW,TIMEHR,NS2,
     1                       I,NNEED,BMJ
      QQ1        = QE1(I)
      QQ2        = QE2(I)
      FLOW       = QLINTP(QQ1,QQ2,TFLW,TFLOW,TIMEHR)*CMET(8,METRIC)
      XNT(1)     = XNT(1)     + FLOW*DT
C#### WCH (CDM), 8/93.  ADD CHECK FOR SUBSCRIPT OUT OF RANGE.
      IF(NS2.GT.0.AND.NS2.LE.NET) RNOFF(NS2) = RNOFF(NS2) + FLOW
      IF(NPOLL.GT.0) THEN
             DO 4660 J = 1,NPOLL
             QQ1       = PE1(J,I)
             QQ2       = PE2(J,I)
C#### WCH (CDM), 8/93.  ADD CHECK FOR SUBSCRIPT OUT OF RANGE.
             IF(NS2.GT.0.AND.NS2.LE.NET) PLUTO(J,NS2) = PLUTO(J,NS2) +
     +                     QLINTP(QQ1,QQ2,TFLW,TFLOW,TIMEHR)*FLOW
 4660        CONTINUE
             ENDIF
 4670 CONTINUE
      ENDIF
C=======================================================================
C     NNOTE, PRECEDING QUALITY INPUTS HAVE UNITS OF CFS*CONCENTRATION
C            FLOW HAS UNITS OF CFS.
C=======================================================================
C     BEGIN INNER LOOP ON ELEMENT NUMBER
C     M = CURRENT ELEMENT NUMBER,(INTERNAL NUMBER).
C=======================================================================
      DO 150 I = 1,NE
      M        = JR(I)
      NTPE    = NTYPE(M)
      KCIM    = KLASS(NTPE)
C=======================================================================
C     STORE INPUT HYDROGRAPHS AND POLLUTOGRAPHS
C           FOR DESIRED ELEMENTS IF INFLEW = 0 (DEFAULT).
C=======================================================================
      IF(NNYN.GT.0.AND.INFLEW.EQ.0) THEN
                    DO 70 J = 1,NNYN
                    IF(JCE.EQ.0.AND.NOE(M).NE.NYN(J)) GO TO 70
                    IF(JCE.EQ.1.AND.KOE(M).NE.KYN(J)) GO TO 70
                    IF(NPOLL.GT.0) THEN
                                   DO 68 K = 1,NPOLL
                                   CPPP(K) = 0.0
                                   IF(RNOFF(M).GT.0.0) CPPP(K) =
     +                                        PLUTO(K,M)/RNOFF(M)
   68                              CONTINUE
                                   WRITE (NSCRT1) RNOFF(M),
     +                                            (CPPP(K),K=1,NPOLL)
                                   ELSE
                                   WRITE (NSCRT1) RNOFF(M)
                                   ENDIF
                    GO TO 71
   70               CONTINUE
                    ENDIF
   71 INPUT     = M
      DO 4680 K = 1,NPOLL
 4680 POLDWF(K) = 0.0
      DUMY1     = 0.0
C=======================================================================
C     CORRECT DWF FOR TEMPORAL VARIATIONS
C     CORRECT SEWAGE FOR DAILY AND HOURLY VARIATION
C
C     WDWF(INPUT,1) IS B O D IN MG/L * CFS
C     WDWF(INPUT,2) IS SS IN MG/L * CFS
C     WDWF(INPUT,3) IS COLIFORM IN MPN/L * CFS
C=======================================================================
      IF(NFILTH.GT.0) THEN
                    KHR = JHR + 1
      IF(KHR.LT.0)  KHR = 1
      IF(KHR.GT.24) KHR = 24
C=======================================================================
C     Update the day of the week(KDAY) for dry weather flow.
C=======================================================================
      IF(NDAY.NE.OLDDAY) THEN
                         KDAY = KDAY + 1
                         IF(KDAY.GT.7) KDAY = 1
                         OLDDAY = NDAY
                         ENDIF
cim monthly variation
      IF (NTPE.EQ.19) THEN
                       BFF = BFFMONTH(2,NINT(GEOM3(INPUT)))
	ELSE
	   BFF = 1.0
	ENDIF
cim
      IF(NPOLL.GT.0)  THEN
                      WDWF1     = WDWF(INPUT,1)*DVBOD(KDAY)*DVDWF(KDAY)
                      WDWF2     = WDWF(INPUT,2)*DVSS(KDAY)*DVDWF(KDAY)
                      WDWF3     = WDWF(INPUT,3)*DVDWF(KDAY)
cim monthly variation
                       WDWF1 = WDWF1 * BFF
                       WDWF2 = WDWF2 * BFF
                       WDWF3 = WDWF3 * BFF
cim
                      POLDWF(1) = WDWF1*HVBOD(KHR)*HVDWF(KHR)
                      POLDWF(2) = WDWF2*HVSS(KHR)*HVDWF(KHR)
                      POLDWF(3) = WDWF3*HVCOLI(KHR)*HVDWF(KHR)
CIMT  21 = (8+3*MQUAL+1), 22 = (8+3*MQUAL+2), 23 = (8+3*MQUAL+3)
                      XNT(8+3*MQUAL+1)   = XNT(8+3*MQUAL+1) + POLDWF(1)
                      XNT(8+3*MQUAL+2)   = XNT(8+3*MQUAL+2) + POLDWF(2)
                      XNT(8+3*MQUAL+3)   = XNT(8+3*MQUAL+3) + POLDWF(3)
                      ENDIF
      DDWF      = QDWF(INPUT)*DVDWF(KDAY)
cim monthly variation
     *            * BFF
      DUMY1     = DDWF*HVDWF(KHR)
      XNT(2)    = XNT(2) + DUMY1*DT
      ELSE
      DUMY1     = 0.0
      ENDIF
C=======================================================================
C     Sum upstream flows.
C=======================================================================
      TOTAL   = 0.0
      DO 80 J = 1,3
      NNEED   = INUE(M,J)
      IF(NNEED.GT.NE) GO TO 80
      NTPEU     = NTYPE(NNEED)
C=======================================================================
C     HERE IF UPSTREAM ELEMENT IS OF FLOW DIVIDER TYPE.
C     MODIFY TO INCLUDE NEW TYPE 27 QUALITY SPLITTER TYPE (CIM 4/99)
C=======================================================================
      IF(NTPEU.LE.20.OR.NTPEU.EQ.27) THEN
                    TOTAL = TOTAL + QO(NNEED)
                    ELSE
                    L     = GEOM3(NNEED)
                    BMJ   = KGEOM(NNEED)
                    QQ    = QO2(NNEED)
                    IF(JCE.EQ.0.AND.NOE(M).EQ.L)   QQ = QO1(NNEED)
                    IF(JCE.EQ.1.AND.KOE(M).EQ.BMJ) QQ = QO1(NNEED)
                    TOTAL = TOTAL + QQ
                    ENDIF
   80 CONTINUE
      KFULL = 2
C=======================================================================
C     Assume If QI = exactly QFULL, conduit has been determined
C                    to be surcharging.
C=======================================================================
      IF(KCIM.LE.2) THEN
                 IF(BARREL(M).GE.1.0) QI(M) = TOTAL/BARREL(M)
                 IF(BARREL(M).LT.1.0) QI(M) = TOTAL*BARREL(M)
                 IF(IAMFL(M).EQ.1.OR.QI(M).EQ.QFULL(M)) THEN
                                           QI(M) = QFULL(M)
                                           KFULL = 1
                                           ITER  = 0
                                           ELSE
                                           ITER  = NITER
                                           ENDIF
                 ELSE
                                          QMANHO = 0.0
CIM  CHANGE FOR VARIABLE BASE FLOW IN TRANSPORT  TYPE 19 ONLY
CIM                 IF(NTPE.EQ.19.OR.NTPE.EQ.22) QMANHO = DIST(M)
CIM 2/96 CALL BFFMONTH TO GET CORRECT FACTOR
            IF(NTPE.EQ.19) QMANHO = DIST(M)*BFFMONTH(1,NINT(GEOM3(M)))
cim type 22???? not documented then remove, inconsistent elsewhere
CIM
cim         IF(NTPE.EQ.19.OR.NTPE.EQ.22) QMANHO = DIST(M)
                 QI(M)     = TOTAL + RNOFF(M) + SURGE2(M)/DT + DUMY1 +
     +                       QINFIL(M) + QMANHO
                 XNT(3)    = XNT(3)    + QINFIL(M)*DT
                 XNT(4)    = XNT(4)    + QMANHO*DT
                 RANQ(M)   = RANQ(M)   + RNOFF(M) + DUMY1 +
     +                       QINFIL(M) + QMANHO
                 IF(NPOLL.GT.0) THEN
                                DO 1200 KK = 1,NPOLL
 1200                           RANK(M,KK) = RANK(M,KK) + PLUTO(KK,M)
                                ENDIF
CIM  REMOVE THE NTPE.EQ.22 TO REMAIN CONSISTANCY EVERYWHERE
CIM  ALTERNATIVE IS TO ADD EVERYWHERE
CIM                 IF(NTPE.EQ.19.OR.NTPE.EQ.22) THEN
                 IF(NTPE.EQ.19) THEN
                 IF(NPOLL.GT.0) THEN
                                DO 1250 KK = 1,NPOLL
CIMT                                IF(KK.EQ.1) PMAN = GEOM1(M)
CIMT                                IF(KK.EQ.2) PMAN = SLOPE(M)
CIMT                                IF(KK.EQ.3) PMAN = ROUGH(M)
CIMT                                IF(KK.EQ.4) PMAN = GEOM2(M)
                   PMAN = PMANN(M,KK)
CIM  CHANGE FOLLOWING TO INCLUDE MONTHLY BASE FLOW FACTORS
	IF (NTPE.EQ.19)  PMAN = PMAN*BFFMONTH(1,NINT(GEOM3(M)))
CIM
CIMT  30 becomes (10+5*MQUAL)
 1250              XNT((10+5*MQUAL)+KK) = XNT((10+5*MQUAL)+KK) + PMAN
                                ENDIF
                 ENDIF
                 IF(QI(M).LT.0.0) QI(M) = 0.0
                 ITER = 0
                 ENDIF
C=======================================================================
C     Route flow through element.
C=======================================================================
  115 KKK    = 0
  116 WSLOPE = 0.0
      CALL ROUTE(DELQ,WSLOPE)
      IF(NTPE.EQ.22) GO TO 130
      KKK  = KKK  + 1
      ITER = ITER + 1
      IF(ITER.GT.NITER) GO TO 116
C=======================================================================
C     SURCHARGE ROUTINE.
C     CHECK THE ELEMENTS DOWNSTREAM FROM A NON-CONDUIT
C     FOR POSSIBLE SURCHARGING.
C=======================================================================
  130                     DENOM = QFULL(M)
      IF(QFULL(M).EQ.0.0) DENOM = 1.0
      IF(DELQ/DENOM.GT.EMAX(M)) EMAX(M) = DELQ/DENOM
                           KITER(M)  = KITER(M) + KKK
      IF(KKK.GT.KITMAX(M)) KITMAX(M) = KKK
      SMEAN(M)  = SMEAN(M)  + WSLOPE
      STOTAL(M,0) = STOTAL(M,0) + QO(M)*DT
      IF(WSLOPE.EQ.0.0)           WSLOPE   = SLOPE(M)
      IF(WSLOPE.GT.SBIG(M))       SBIG(M)  = WSLOPE
      IF(WSLOPE.LT.SMAL(M))       SMAL(M)  = WSLOPE
      IF(KCIM.EQ.3.AND.I.LT.NE) THEN
      NODO      = 0
      II        = 1
 1162 MI        = JR(I+II)
C=======================================================================
C     CHECK TO SEE IF NEXT ELEMENT ROUTED IS DOWNSTREAM ELEMENT.
C=======================================================================
      IF(INUE(MI,1).EQ.M) GO TO 1163
 1164 II = II+1
      IF(I+II-NE) 1162,1162,1170
 1163 IFD    = 1
      IF(NODO.EQ.0) SURTMP = 0.0
      IAMFL(MI) = 0
      NTPEI       = NTYPE(MI)
      KI        = KLASS(NTPEI)
C=======================================================================
C     IS CURRENT ELEMENT A FLOW DIVIDER ?
C     SET PARAMETERS FOR FLOW DIVIDER.
C     MODIFY TO ADD NEW TYPE 27  CIM 4/99
C=======================================================================
      IF(NTPE.LE.20.OR.NTPE.EQ.25.OR.NTPE.EQ.27)          GO TO 1165
      IF(JCE.EQ.0.AND.NTPE.EQ.22.AND.GEOM3(M).LE.0.0) GO TO 1165
      IF(JCE.EQ.1.AND.NTPE.EQ.22.AND.KGEOM(M).EQ.' ') GO TO 1165
      IFD = 2
      L   = GEOM3(M)
      BMJ = KGEOM(M)
      IF(MI.EQ.NIN(L,BMJ)) GO TO 1164
      IF(KI.EQ.3) GO TO 1167
      IF(BARREL(MI).GE.1.0) QINN = QO2(M)/BARREL(MI)
      IF(BARREL(MI).LT.1.0) QINN = QO2(M)*BARREL(MI)
      GO TO 1166
 1165 IF(KI.EQ.3) GO TO 1169
      IF(BARREL(MI).GE.1.0) QINN = QO(M)/BARREL(MI)
      IF(BARREL(MI).LT.1.0) QINN = QO(M)*BARREL(MI)
 1166 IF(QINN.LE.QMAX(MI).AND.IFD.EQ.1) GO TO 1169
      IF(QINN.LE.QMAX(MI))              GO TO 1167
C=======================================================================
C     IF CONDUIT CAPACITY EXCEEDED, STORE EXCESS AT UPSTREAM NON-CONDUIT
C     CONDUIT ASSUMED TO FLOW FULL AT UPSTREAM END.
C     QFULL BASED ON CONDUIT SLOPE.
C=======================================================================
      IF(NDESN.EQ.0.OR.NODSGN(MI).EQ.1) THEN
         IF(ISTIME(MI,1).EQ.0) THEN
                               ISTIME(MI,1) = JULDAY
                               ISTIME(MI,2) = TIMDAY
                               ENDIF
         JSTIME(MI,1) = JULDAY
         JSTIME(MI,2) = TIMDAY
         QFULL(MI)    = P1(MI)*SQRT(SLOPE(MI))
         IF(BARREL(MI).GE.1.0) SURGE2(M) = (QINN - QFULL(MI)) *
     +                                      DT*BARREL(MI) + SURTMP
         IF(BARREL(MI).LT.1.0) SURGE2(M) = (QINN-QFULL(MI))*DT + SURTMP
         QRATIO       = QINN/QFULL(MI)
         SURTMP       = SURGE2(M)
         SURLEN(MI)   = SURLEN(MI) + DT
         IF(SURTMP.GT.SPEAK(MI)) SPEAK(MI) = SURGE2(M)
         IF(QRATIO.GT.QR(MI))       QR(MI) = QRATIO
         ENDIF
C======================================================================
C     DESIGN ROUTINE.
C     INCREASE CONDUIT DIMENSION UNTIL SURCHARGE ELIMINATED.
C     INCREASE DIAMETER OF CIRCULAR CONDUITS BY STANDARD AMOUNTS (NTPEI=1).
C     INCREASE WIDTH OF RECTANGULAR CONDUIT BY 0.5 FT (NTPEI=2).
C     IF NOT CIRCULAR OR RECT., REPLACE BY CIRCULAR OF EQUAL AREA, USING
C     STANDARD PIPE SIZES.
C=======================================================================
      IF(NDESN.GT.0.AND.NODSGN(MI).EQ.0) THEN
                     SURTMP     = 0.0
                     KSTORE(MI) =  1
                     IF(NTPEI.EQ.1) THEN
                            DEL = 0.25
                            IF(GEOM1(MI).GE.3.0) DEL = 0.50
                            IF(GEOM1(MI).LT.1.0) DEL = 1.0-GEOM1(MI)
                            GEOM1(MI) = GEOM1(MI) + DEL
C#### RED, 11/29/93.  ELIMINATE THIRD PARAMETER IN FIRST ARG. LIST.
                            CALL FIRST(MI,1)
                            IF(JCE.EQ.0) WRITE(N6,480)
     +                                   NOE(MI),DEL,GEOM1(MI)
                            IF(JCE.EQ.1) WRITE(N6,481)
     +                                   KOE(MI),DEL,GEOM1(MI)
                            GO TO 1166
                            ENDIF
                     IF(NTPEI.EQ.2) THEN
                            GEOM2(MI) = GEOM2(MI) + 0.50
C#### RED, 11/29/93.  ELIMINATE THIRD PARAMETER IN FIRST ARG. LIST.
                            CALL FIRST(MI,1)
                            IF(JCE.EQ.0) WRITE(N6,400) NOE(MI),GEOM2(MI)
                            IF(JCE.EQ.1) WRITE(N6,401) KOE(MI),GEOM2(MI)
                            GO TO 1166
                            ENDIF
                     NTYPE(MI) = 1
                     NTPEI       = 1
                     GE        = 1.12838*SQRT(AFULL(MI))
                     IF(GE.LE.3.0) THEN
                           GEOM1(MI) = FLOAT(IFIX(GE*100.)/25+1)*0.25
                           ELSE
                           GEOM1(MI) = FLOAT(IFIX(GE*100.)/50+1)*0.50
                           ENDIF
C#### RED, 11/29/93.  ELIMINATE THIRD PARAMETER IN FIRST ARG. LIST.
                     CALL FIRST(MI,1)
                     IF(JCE.EQ.0) WRITE(N6,415) NOE(MI),GEOM1(MI)
                     IF(JCE.EQ.1) WRITE(N6,416) KOE(MI),GEOM1(MI)
                     GO TO 1166
                     ENDIF
      IF(IFD.EQ.1) GO TO 1168
      IF(BARREL(MI).GE.1.0) QO2(M) = QFULL(MI)*BARREL(MI)
      IF(BARREL(MI).LT.1.0) QO2(M) = QFULL(MI)
      QO(M)  = QO1(M) + QO2(M)
C=======================================================================
C     CHECK CAPACITY OF SECOND ELEMENT DOWNSTREAM FROM FLOW-DIVIDER.
C=======================================================================
 1167 L   = GEOM3(M)
      BMJ = KGEOM(M)
      MI  = NIN(L,BMJ)
      NTPEI = NTYPE(MI)
      KI  = KLASS(NTPEI)
      IF(KI.EQ.3) GO TO 1169
      IF(BARREL(MI).GE.1.0) QINN = QO1(M)/BARREL(MI)
      IF(BARREL(MI).LT.1.0) QINN = QO1(M)
      IFD  = 1
      GO TO 1166
 1168 IF(BARREL(MI).GE.1.0) THEN
                            QO1(M) = QFULL(MI) * BARREL(MI)
                            QO(M)  = QO1(M)    + QO2(M)
                            ELSE
                            IF(NODO.EQ.0) THEN
                                          QO1(M)    = QFULL(MI)
                                          QO2(M)    = QI(M)-QFULL(MI)
                                          IAMFL(MI) = 1
                                          ENDIF
                            IF(NODO.EQ.1) THEN
                                          QO2(M)    = QFULL(MI)
                                          IAMFL(MI) = 1
                                          ENDIF
                            QO(M) = QO1(M) + QO2(M)
                            ENDIF
      GO TO 1170
 1169 IF(NTPE.NE.22) SURGE2(M) = SURTMP
      IF(NTPE.EQ.22) SURGE2(M) = SURGE2(M) + SURTMP
 1170 CONTINUE
      IF(BARREL(MI).LT.1.0) THEN
                            NODO = NODO + 1
                            GO TO 1164
                            ENDIF
      ENDIF
C======================================================================
C     Multiply flow by number of barrels if barrels > 1.0
C=======================================================================
      IF(BARREL(M).GE.1.0) THEN
                           QI(M) = QI(M)*BARREL(M)
                           QO(M) = QO(M)*BARREL(M)
                           ENDIF
      IF(QO(M).GT.QBIG(M)) QBIG(M) = QO(M)
C=======================================================================
C     Route quality parameters through element.
C=======================================================================
      IF(NPOLL.GT.0) CALL QUAL(1)
C=======================================================================
C     STORE DESIRED OUTFLOWS TO BE GENERATED ONTO OUTPUT TAPE
C     POLLUTANTS ON INTERFACE FILE HAVE UNITS OF CFS*CONCENTRATION.
C=======================================================================
      IF(NORDER(I).EQ.1) THEN
                         XNT(7)    = XNT(7) + QO(M)*DT
                         IF(NPOLL.GT.0) THEN
                         DO 5555 K = 1,NPOLL
CIMT    8 is ok
 5555                    XNT(8+K)  = XNT(8+K) +
     +                               DT*CPOL2(M,2,K)*QO(M)
                         ENDIF
                         ENDIF
CIMQP   ADD TOTAL OUTFLOW CONCENTRATION PRINTOUT FOR ALL ELEMENTS
                         DO K=1,NPOLL
                         STOTAL(M,K) = STOTAL(M,K)+
     +                                 DT*CPOL2(M,2,K)*QO(M)
	                   ENDDO
      IF(NOUTS.GT.0.AND.NEXT.GT.0) THEN
               DO 6666 J = 1,NOUTS
               IF(JCE.EQ.0.AND.NOE(M).NE.JN(J))  GO TO 6666
               IF(JCE.EQ.1.AND.KOE(M).NE.KJN(J)) GO TO 6666
C#### WCH, 4/6/93.  ADD METRIC CONVERSION FOR OUTPUT INTERFACE FILE.
               OUTTAP(J) = QO(M)/CMET(8,METRIC)
               IF(NPOLL.GT.0) THEN
                        DO 6620 K  = 1,NPOLL
C#### WCH, 4/6/93.  ADD METRIC CONVERSION FOR OUTPUT INTERFACE FILE.
 6620                   PFILEO(K,J) = CPOL2(M,2,K)*QO(M)/CMET(8,METRIC)
                        ENDIF
 6666          CONTINUE
               ENDIF
C=======================================================================
C     STORE DESIRED OUTFLOWS TO BE GENERATED ON SCRATCH TAPE
C                                      FOR PRINTING PURPOSES
C=======================================================================
      IF(NNPE.GT.0) THEN
                    DO 122 J = 1,NNPE
                    IF(JCE.EQ.0.AND.NOE(M).NE.NPE(J)) GO TO 122
                    IF(JCE.EQ.1.AND.KOE(M).NE.KPE(J)) GO TO 122
                    IF(NPOLL.EQ.0) WRITE (NSCRT2) QO(M)
                    IF(NPOLL.GT.0) WRITE (NSCRT2) QO(M),
     +                            (CPOL2(M,2,K),K=1,NPOLL)
                    GO TO 124
  122               CONTINUE
                    ENDIF
  124 CONTINUE
C=======================================================================
C     STORE DESIRED DEPTHS TO BE GENERATED ON SCRATCH TAPE
C                                    FOR PRINTING PURPOSES
C=======================================================================
      IF(NSURF.GT.0) THEN
                     DO 132 J = 1,NSURF
                     IF(JCE.EQ.0.AND.NOE(M).NE.JSURF(J)) GO TO 132
                     IF(JCE.EQ.1.AND.M.NE.JSURF(J))      GO TO 132
C#######################################################################
C#### WCH (Steve Merrill), 3/28/94.  ADD IF STATEMENT TO GET DEPTHS
C       FROM STORAGE UNITS.
C=======================================================================
                     IF(NTYPE(M).LE.18) THEN
                          A1   = A(M,1,2)/AFULL(M)
                          A2   = A(M,2,2)/AFULL(M)
                          A1   = 0.5 * (DEPTH(A1) + DEPTH(A2))
                          A1   = A1*GEOM1(M)
C#######################################################################
C#### WCH, 3/28/94.  HERE, STORE LAST DEPTH FROM STORAGE IN A1 TO BE
C       PLACED ON NSCRAT7.  STORAGE NODES ARE IDENTIFIED SIMPLY ON
C       INPUT LINE I2.
C=======================================================================
                          ELSE IF(NTYPE(M).EQ.22) THEN
                               IS = KSTORE(M)
                               A1 = DEPTHL(IS)
                          ENDIF
                     WRITE(NSCRT7) A1
                     GO TO 134
  132                CONTINUE
                     ENDIF
  134 CONTINUE
C=======================================================================
C     STORE INPUT HYDROGRAPHS AND POLLUTOGRAPHS
C           FOR DESIRED ELEMENTS IF INFLEW = 1 (NEW OPTION).
C=======================================================================
      IF(NNYN.GT.0.AND.INFLEW.EQ.1) THEN
                    DO 170 J = 1,NNYN
                    IF(JCE.EQ.0.AND.NOE(M).NE.NYN(J)) GO TO 170
                    IF(JCE.EQ.1.AND.KOE(M).NE.KYN(J)) GO TO 170
                    IF(NPOLL.GT.0) THEN
                                   DO 168 K = 1,NPOLL
                                   CPPP(K) = 0.0
                                   IF(QI(M).GT.0.0) CPPP(K) =
     +                                        CPOL1(M,2,K)*QI(M)
  168                              CONTINUE
                                   WRITE(NSCRT1) QI(M),
     +                                           (CPPP(K),K=1,NPOLL)
                                   ELSE
                                   WRITE(NSCRT1) QI(M)
                                   ENDIF
                    GO TO 171
  170               CONTINUE
                    ENDIF
  171 CONTINUE
C=======================================================================
C     REPLACE VALUES AT OLD TIME STEP BY VALUES AT NEW ONE
C=======================================================================
C#### JLM, 10/93.  MOVE TO OUTSIDE OF SPATIAL DO-LOOP.
C####      A(M,1,1) = A(M,1,2)
C####      A(M,2,1) = A(M,2,2)
C####      IF(NPOLL.GT.0) THEN
C####                     DO 126 IP     = 1,NPOLL
C####                     CPOL1(M,1,IP) = CPOL1(M,2,IP)
C####  126                CPOL2(M,1,IP) = CPOL2(M,2,IP)
C####                     ENDIF
C####      IF(BARREL(M).GE.1.0) Q(M,1,1)  = Q(M,1,2)/BARREL(M)
C####      IF(BARREL(M).LT.1.0) Q(M,1,1)  = Q(M,1,2)
C####      IF(BARREL(M).GE.1.0) Q(M,2,1)  = Q(M,2,2)/BARREL(M)
C####  150 IF(BARREL(M).LT.1.0) Q(M,2,1)  = Q(M,2,2)
C
  150 CONTINUE
C
C#######################################################################
         IF(ISUMRY.GT.0) CALL LINK(1)
C=======================================================================
C     REPLACE VALUES AT OLD TIME STEP BY VALUES AT NEW ONE
C#### NOTE:  THIS COMPUTATION WAS MOVED UNTIL AFTER
C                 COMPLETION OF THE SPATIAL LOOP, JLM 10/93
C=======================================================================
      DO 160 I=1,NE
      M = JR(I)
      A(M,1,1) = A(M,1,2)
      A(M,2,1) = A(M,2,2)
      IF(NPOLL.GT.0) THEN
                     DO 126 IP     = 1,NPOLL
                     CPOL1(M,1,IP) = CPOL1(M,2,IP)
  126                CPOL2(M,1,IP) = CPOL2(M,2,IP)
                     ENDIF
      IF(BARREL(M).GE.1.0) Q(M,1,1)  = Q(M,1,2)/BARREL(M)
      IF(BARREL(M).LT.1.0) Q(M,1,1)  = Q(M,1,2)
      IF(BARREL(M).GE.1.0) Q(M,2,1)  = Q(M,2,2)/BARREL(M)
  160 IF(BARREL(M).LT.1.0) Q(M,2,1)  = Q(M,2,2)
C=======================================================================
C     GENERATE OUTPUT TAPE TO BE USED BY INTERFACING MODEL
C=======================================================================
      IF(NEXT.GT.0.AND.NOUTS.GT.0) THEN
CIM### 9/8/00   GET HEADS AND VELOCITIES
      IF (NPOLL2.GT.NPOLL) CALL GETHV(PFILEO)
CIM### 9/8/00   CHANGE NPOLL TO NPOLL+2  (3 places)
                     IF(NPOLL2.LE.0) WRITE (NEXT) JULDAY,TIMDAY,
     +                              DT,(OUTTAP(K),K=1,NOUTS)
                     IF(NPOLL2.GT.0) WRITE (NEXT) JULDAY,TIMDAY,
     +                              DT,(OUTTAP(K),(PFILEO(J,K),J=1,
     +                              NPOLL2),K=1,NOUTS)
                     ENDIF
  200 CONTINUE
C=======================================================================
C     CALL NEW OUTPUT SUBROUTINE
C=======================================================================
      CALL OTRAIN
C=======================================================================
C     WRITE NEW ELEMENT TABLE.
C=======================================================================
      IF(NDESN.GT.0) THEN
                     WRITE (N6,930)
C#### RED, 11/29/93.  ELIMINATE THIRD PARAMETER IN FIRST ARG. LIST.
                     CALL FIRST(MI,2)
                     KARIGE = 0
                     IF(METRIC.EQ.1) WRITE (N6,916) KARIGE
                     IF(METRIC.EQ.2) WRITE (N6,919) KARIGE
                     DO 300 I = 1,NE
                     NTPE       = NTYPE(I)
                     IF(NTPE.LE.18) THEN
                           QQQ = BLANK
                           IF(KSTORE(I).EQ.1) QQQ = ASTER
                           DIST(I)  = DIST(I)/CMET(1,METRIC)
                           GEOM1(I) = GEOM1(I)/CMET(1,METRIC)
                           GEOM2(I) = GEOM2(I)/CMET(1,METRIC)
                           GEOM3(I) = GEOM3(I)/CMET(1,METRIC)
                           AFULL(I) = AFULL(I)/CMET(1,METRIC)**2.0
                           QFULL(I) = QFULL(I)/CMET(8,METRIC)
                           QMAX(I)  = QMAX(I)/CMET(8,METRIC)
                           IF(JCE.EQ.0) WRITE(N6,931) NOE(I),NTPE,QQQ,
     +                           NAME(NTPE),SLOPE(I),DIST(I),ROUGH(I),
     +                           GEOM1(I),GEOM2(I),GEOM3(I),BARREL(I),
     +                           AFULL(I),QFULL(I),QMAX(I),SCF(I)
                           IF(JCE.EQ.1) WRITE(N6,932) KOE(I),NTPE,QQQ,
     +                           NAME(NTPE),SLOPE(I),DIST(I),ROUGH(I),
     +                           GEOM1(I),GEOM2(I),GEOM3(I),BARREL(I),
     +                           AFULL(I),QFULL(I),QMAX(I),SCF(I)
                           ENDIF
  300                CONTINUE
                     ENDIF
C=======================================================================
C     PRINT SELECTED INPUT AND OUTPUT HYDROGRAPHS AND POLLUTOGRAPHS.
C=======================================================================
      IF(NPOLL.LE.0.AND.JPRINT.GT.0) CALL PRINTF(0)
C#### WCH, 10/14/94.  ALLOW DEPTH PRINTING WITHOUT HYDROGRAPH PRINTS.
      IF(JPRINT.EQ.0.AND.NSURF.GT.0) CALL PRINTF(1)
      IF(NPOLL.GT.0.AND.JPRINT.GT.0) THEN
                                     CALL PRINTQ
                                     CALL PRINTF(1)
                                     ENDIF
      WRITE(*,4850)
      WRITE(N6,4850)
C=======================================================================
C#### WCH, 8/7/96.  INCREASE FIELD WIDTH FOR TIME STEP NUMBER TO 7.
   22 FORMAT('+',5X,I7)
   23 FORMAT(/,' Beginning loop through ',I7,' time steps.',/,
     +         ' Time step # ',/)
  400 FORMAT(' INCREASE WIDTH OF EXT ELEMENT ',I10,' BY 0.50 TO ',F7.3,
     1' FT.')
  401 FORMAT(' INCREASE WIDTH OF EXT ELEMENT ',A10,' BY 0.50 TO ',F7.3,
     1' FT.')
  415 FORMAT(' REPLACE EXT ELEMENT ',I10,' BY CIRCULAR CONDUIT OF DIAMET
     1ER ',F7.3,'FT.')
  416 FORMAT(' REPLACE EXT ELEMENT ',A10,' BY CIRCULAR CONDUIT OF DIAMET
     1ER ',F7.3,'FT.')
  480 FORMAT(' INCREASE DIAMETER OF EXT ELEMENT ',I10,' BY ',F6.2,
     1' TO ',F8.3,'FT.')
  481 FORMAT(' INCREASE DIAMETER OF EXT ELEMENT ',A10,' BY ',F6.2,
     1' TO ',F8.3,'FT.')
  910 FORMAT (//, ' THE TOTAL SIMULATION TIME =',F12.1,' SECONDS.'/,
     1            '                            ',F12.2,' MINUTES.',/,
     2            '                            ',F12.3,'   HOURS.',/,
     3            '         THE TIME STEP(DT) =',F12.1,' SECONDS.')
Cwch, 8/29/00.  Align headings better with output.
  916 FORMAT(I1,/,
     1' *****************************************************',/,
     1' *           TRANSPORT ELEMENT PARAMETERS            *',/,
     1' *                                                   *',/,
     *' * CAUTION: COLUMN HEADINGS ARE FOR CONDUITS.  REFER *',/,
     2' * TO USERS MANUAL FOR MEANING FOR NON-CONDUITS.     *',/,
     1' *****************************************************',//,
	19X,'EXT.                      SLOPE  DISTANCE  MANNING    GEOM1  G
     3EOM2  GEOM3  NUMBER   AFULL     QFULL     QMAX   SUPER-CRITICAL'
     3,/,9X'ELE. TY                  (FT/FT)   (FT)   ROUGHNESS   (FT)
     3 (FT)   (FT)    OF    (SQ.FT)    (CFS)    (CFS)   FLOW WHEN LESS'
     4,/,9X,
	4'NUM. PE TYPE NAME
     5         BARRELS                              THAN 95% FULL?',/,
	59X,'---- -- -----------       -------  ------  --------   ------
     6-----  ----- -------  -------   ------  -------  --------------')
  919 FORMAT(I1,/,
     1' *****************************************************',/,
     1' *           TRANSPORT ELEMENT PARAMETERS            *',/,
     1' *                                                   *',/,
     *' * CAUTION: COLUMN HEADINGS ARE FOR CONDUITS.  REFER *',/,
     2' * TO USERS MANUAL FOR MEANING FOR NON-CONDUITS.     *',/,
     1' *****************************************************',//,
     29X,'EXT.                       SLOPE  DISTANCE  MANNING    GEOM1
     2GEOM2  GEOM3  NUMBER   AFULL     QFULL     QMAX   SUPER-CRITICAL'
     3,/,9X,'ELE. TY                     (M/M)    (M)   ROUGHNESS    (M)
     4    (M)    (M)    OF      (SQ.M)    (CMS)    (CMS)   FLOW WHEN LES
     4S',/,
     49X,'NUM. PE TYPE NAME
     5            BARRELS                              THAN 95% FULL?'
     5,/,9X,
     6'---- -- -----------       -------  ------  --------   ------  ---
     6--  ----- -------  -------   ------  -------  --------------')
  925 FORMAT (//,' THE ENDING DATE (YR/MO/DAY)....',I4,'/',I2,'/',I2,/,
     1           ' THE ENDING TIME OF DAY.........',F9.3,' SECONDS.',//)
  930 FORMAT(/,1H1,/,
     1' **********************************************************',/,
     1' *         HYDRAULIC DESIGN ROUTINE FINAL RESULTS.        *',/,
     1' *  FINAL CONDUIT DIMENSIONS. ASTERISK INDICATES ALTERED  *',/,
     2' *  CONDUITS.  ==> NOTE, PIPE SIZES ARE INCREASED BY U.S. *',/,
     2' *                 STD. SIZES (E.G., 6 INCHES) AND MAY    *',/,
     2' *                 NOT AGREE WITH METRIC STANDARDS.       *',/,
     2' **********************************************************',/)
  931 FORMAT(1X,I10,I5,1X,A1,A16,F8.5,F9.2,F9.4,F9.3,2F7.3,F6.1,
     +                                           6X,3G9.2,5X,A4)
  932 FORMAT(1X,A10,I5,1X,A1,A16,F8.5,F9.2,F9.4,F9.3,2F7.3,F6.1,
     +                                           6X,3G9.3,5X,A4)
 4750 FORMAT(//,11X,A80,/,11X,A80)
 4850 FORMAT(/,1X,'Transport simulation ended normally.')
C#### WCH (CDM), 8/93.
 9100 FORMAT(/,' WARNING. While reading R1 lines, subscript NS2 in Sub.
     1Trans out of range (LE.0 or GT.NET) at TIMHR =',F7.3,' hrs.',/,
     2' Input flow time =',F7.3,' hrs.  Subscript NS2 =',I4,/,
     3' Occurs for input element no.',I3,' (Sequence no.)  Parameters NN
     3EED =',I4,' and BMS =',I4)
 9150 FORMAT(/,' ERROR. R1 line not found in Sub. TRANS for simulation t
     1ime =',F7.3,' hrs.',/,' Line found had ID =',A2,' Run stopped.')
C=======================================================================
      RETURN
  888 CALL IERROR
      END
``` 
