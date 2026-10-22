      SUBROUTINE IRRSECT(ICALL,ELDEL,ELMIN,KNORM,TWFULL,
     .STCHL,STCHR,XNL,XNCH,XNR,NUMST,RTSLOP,METRIC,elsta)
CIM   BLOCK TRANSPORT
CIM   CALLED BY GETCUR
CIM  SUBROUTINE IRRSECT
CIM    Extracted from subrouting GETCUR by C.Moore
CIM    to allow finer level of detail at low flows for TRANSPORT
CIM    if ICALL = 0, Normal call for full section,
CIM                  Values stored in QCURVE array
CIM       ICALL = 1, Second call for partial section,
CIM                  Values stored in QCURV2 array
Cwch, 7/6/01. Use QCURVE(3) for top width in both Extran and Trans.
C     Use QCURVE(4) and QCURV2(3) for Transport flows. 
C     Increase dimensions where needed. 
Cwch, 3/14/02. Use full metric units with Transport since there has
C     been no conversion in GETCUR up to point where this subroutine
C     is called. Hence, 
CIM  C. Moore 9/8/00
      INCLUDE 'TAPES.INC'
      INCLUDE 'FLODAT.INC'
      DIMENSION QCURV(4,26)
C WCH MODIFICATION.  NEW ARRAY QTEMP(50)
      DIMENSION QTEMP(50)
cim next statement is needed for iteration in getcur to work
      Double precision elevn
C#### WCH, 2/7/95.  INCREASE DIMENSION FROM 100 TO 300.
cim  note, if following is changed, parameter must also be changed in
cim  subroutine GETCUR
      PARAMETER (NXSPTS=100)
      DIMENSION ELSTA(2,NXSPTS)
c      write(n6,*) icall,eldel,elmin
C=======================================================================
C     FIND STAGES, TOTAL AREAS, FLOWS AND TOP WIDTHS FOR EACH STAGE.
C=======================================================================
C##### RED, 6/2/93.  MOVE TWFULL=0 INSIDE DO-LOOP.
      QCURV(1,1)   = 0.0
      QCURV(2,1)   = 0.0
      QCURV(3,1)   = 0.0
	QCURV(4,1)   = 0.0
      ELEVN = ELMIN
      DO 1500 I         = 2,26
      TWFULL            = 0.0
      ELEVN             = ELEVN + ELDEL
      DO J = 1,4
         QCURV(J,I) = 0.0
	   ENDDO
      SUMWP             = 0.0
C=======================================================================
C     FIND AREA AND FLOW IN DEPRESSIONS
C=======================================================================
      DO 1450 J = 1,50
 1450 QTEMP(J)  = 0.0
      J         = 0
      ITEMP     = 0
C
      DO 1490 K = 2,NUMST
C=======================================================================
C WCH MODIFICATION
C MUST COMPUTE FLOW FOR COMPLEX SECTION USING TOTAL AREA AND WETTED
C   PERIMETER.  SUM UNTIL HAVE DRY SEGMENT OR UNTIL MANNING N CHANGES.
C ACCOUNT FOR POSSIBLE "SAWTOOTH" CROSS SECTION.
C USE ARRAY QTEMP TO TEMPORARILY SUM FLOWS UNTIL ALL DEPRESSIONS
C   ACCOUNTED FOR.
C=======================================================================
C  WCH, 2/24/93.  GET RID OF CHECK FOR EQUALITY OF STATIONS.
C  THIS GIVES CORRECT HYDRAULIC RADIUS FOR SECTIONS WITH VERTICAL SIDES.
C  (OLD LINE):  IF(ELSTA(2,K).EQ.ELSTA(2,K-1)) GO TO 1490
C#######################################################################
      IF(ELSTA(1,K-1).GT.ELSTA(1,K)) THEN
           SU = ELSTA(1,K-1)
           SD = ELSTA(1,K)
           ELSE
           SU = ELSTA(1,K)
           SD = ELSTA(1,K-1)
           ENDIF
C=======================================================================
C INCREMENT QTEMP INDEX J EACH TIME ENTER NEW "DEPRESSION" OR CHANGED N.
C IF CROSS SECTION POINT LOWER THAN WATER LEVEL (ELEVN), ACCUMULATE
C AREA, WIDTH AND WETTED PERIMETER.  FINALLY COMPUTE FLOW (STMT 1480)
C AFTER REACHING CROSS SECTION POINT ABOVE WATER LEVEL.
C=======================================================================
cim   change .le. to .lt in next statement  to handle case where
cim   sd = su = elevn (yes, it happened) which caused zero divide
cim   in calculation of RATIO
      IF(SD.LT.ELEVN) THEN
           IF(ITEMP.LE.0) J = J+1
           IF(J.GT.50) THEN
                WRITE(N6,1410)
                NSTOP = NSTOP + 1
                ENDIF
           ITEMP = 1
C=======================================================================
C IF LOWER ELEV (SD) LT WATER ELEVN, THEN THIS IS WET SEGMENT.
C ACCUMULATE AREA, WETTED PERIMETER AND TWIDTH FOR THIS STAGE
C=======================================================================
           WIDTH  = ABS(ELSTA(2,K)   - ELSTA(2,K-1))
           WPERIM = SQRT(WIDTH*WIDTH + (SU-SD)*(SU-SD))
           IF(ELEVN.GT.SU) THEN
                AREA   = WIDTH*((ELEVN-SU) + (ELEVN-SD))/2.0
                TWIDTH = WIDTH
                SUMWP  = SUMWP + WPERIM
                ELSE
                RATIO  = (ELEVN-SD)/(SU-SD)
                AREA   = WIDTH*(SU-SD)/2.0*RATIO*RATIO
                TWIDTH = WIDTH*RATIO
                SUMWP  = SUMWP + WPERIM*RATIO
                ENDIF
C=======================================================================
C USE QTEMP TO ACCUMULATE AREA UNTIL READY TO COMPUTE FLOW FOR DEPRESS.
C USE QCURVE-2 TO SUM TOTAL AREA.
C=======================================================================
C##### RED, 6/2/93.  ADD FOLLOWING LINE:
           TWFULL            = TWFULL   + TWIDTH
           QTEMP(J)          = QTEMP(J) + AREA
           QCURV(2,I) = QCURV(2,I) + AREA
Cwch, 7/6/01           IF(KNORM.EQ.0) QCURV(3,I) = QCURV(3,I) +TWIDTH
           QCURV(3,I) = QCURV(3,I) +TWIDTH
C
C=======================================================================
C IF LOWER ELEV (SD) GT ELEVN, THEN DRY SEGMENT.  END FLOW ACCUMULATION.
C OR, IF CHANGE TO NEW MANNINGS N, ALSO END FLOW ACCUM.
C THIS ASSUMES NO CONTRIBUTION TO WETTED PERIMETER ALONG VERTICAL
C DIVIDING LINE IN WATER COLUMN AT LOCATION OF N-TRANSITION.
C=======================================================================
C#### WCH, 8/25/94.  TRY DIFFERENT LOGIC HERE TO ACCOUNT BOTH FOR
C     VERTICAL SIDES AT TRANSITION POINT AND ALSO GENERALLY NOT TO
C     INCLUDE AREA BEYOND TRANSITION POINT IN CURRENT FLOW CALCULATION.
C     IF VERTICAL WALL OCCURS AT TRANSITION, VERTICAL WALL IS ASSUMED
C     TO HAVE MAIN-CHANNEL N.
C     WHEN AT VERTICAL-WALL N-TRANSITION:
C       GOING DOWN FROM LEFT, CALCULATE ONLY WHEN FIRST REACH TOP.
C       GOING UP AT RIGHT, DON'T CALCULATE UNTIL REACH TOP.
C
C####           IF(ELSTA(2,K-1).EQ.STCHL.AND.XNL.NE.XNCH.
C####     *         OR.ELSTA(2,K-1).EQ.STCHR.AND.XNR.NE.XNCH) GO TO 1480
C
           IF(ELSTA(2,K).EQ.STCHL) THEN
               IF(XNL.NE.XNCH.AND.ELSTA(2,K).NE.ELSTA(2,K-1)) GO TO 1480
               ENDIF
           IF(ELSTA(2,K).EQ.STCHR) THEN
               IF(K.EQ.NUMST) GO TO 1480
               IF(XNR.NE.XNCH.AND.ELSTA(2,K).NE.ELSTA(2,K+1)) GO TO 1480
               ENDIF
C=======================================================================
C HOLD OFF ON FLOW CALCULATIONS UNTIL REACH CROSS SECTION POINT THAT IS
C HIGHER THAN WATER LEVEL (ELEVN).
C=======================================================================
           IF(K.LT.NUMST) GO TO 1490
C=======================================================================
C END OF IF-BLOCK FOR CROSS SECTION POINT LESS THAN WATER LEVEL.
C=======================================================================
C
           ENDIF
C
C=======================================================================
      IF(ITEMP.LE.0) GO TO 1490
C=======================================================================
C FIND FLOW
C=======================================================================
 1480 RN = XNCH
C=======================================================================
C#### WCH, 11/30/93.  TRY STRICTLY LT AND GT FOR THESE ROUGHNESS
C  TRANSITIONS.  AVOID PROBLEM IF TRANSITION IS EXACTLY AT LOCATION
C  OF VERTICAL SIDE, WHERE VERTICAL SIDE IS IN MAIN CHANNEL.
C
C####           IF(ELSTA(2,K-1).LE.STCHL) RN = XNL
C####           IF(ELSTA(2,K-1).GE.STCHR) RN = XNR
C=======================================================================
      IF(ELSTA(2,K-1).LT.STCHL) RN = XNL
C#### WCH, 8/25/94.  CHANGE TO CORRESPOND TO ALTERED LOGIC ABOVE.
C####      IF(ELSTA(2,K-1).GT.STCHR) RN = XNR
      IF(ELSTA(2,K).GT.STCHR) RN = XNR
C
      RADIUS   = QTEMP(J)/SUMWP
      AREA     = QTEMP(J)
Cwch 3/14/02. There have been no metric conversions in GETCUR input
C     for Transport.  Hence, all Transport computations are using
C     units as input in GETCUR.  Hence, use Manning constant as function
C     of METRIC, not KMET as set in GETCUR. Convert later, as needed, in
C     GETCUR. 
      QTEMP(J) = CMET(9,METRIC)/RN*RADIUS**0.666667*RTSLOP*AREA
      ITEMP    = 0
      SUMWP    = 0.0
C
 1490 CONTINUE
C=======================================================================
C NOW STORE SUM OF DEPRESSION FLOWS OR FLOWS FOR DIFFERENT N-VALUES
C   AS TOTAL FLOW FOR THIS DEPTH.
C=======================================================================
      FLOW     = 0.0
      IF(J.GT.0) THEN
                 DO 1495 LL = 1,J
 1495            FLOW = FLOW + QTEMP(LL)
                 ENDIF
      IF(KNORM.EQ.0) QCURV(1,I) = FLOW
Cwch, 7/6/01      IF(KNORM.EQ.1) QCURV(3,I) = FLOW
      IF(KNORM.EQ.1) QCURV(4,I) = FLOW
c      if (i.eq.26) write(n6,1499) i,elevn,qcurv(1,i),qcurv(2,i),
c     .qcurv(3,i)
c 1499 format(i10,4f20.15)
 1500 CONTINUE

CIM   IF ICALL = 0, NORMAL CALL FOR FULL SECTION, VALUES STORED
CIM                 IN QCURVE ARRAY
CIM              1, SECOND CALL FOR PARTIAL SECTION, VALUES STORED
CIM                 IN QCURVE2 ARRAY
Cwch, 7/6/01.  Use QCURVE(4) for Transport flow. 
      IF (ICALL.EQ.0) THEN
           DO 3000 I=1,26
           DO 3000 J=1,4
 3000      QCURVE(NATUR,J,I) = QCURV(J,I)
           ELSE
           DO 3050 I=1,26
           QCURV2(NATUR,1,I) = QCURV(1,I)
           QCURV2(NATUR,2,I) = QCURV(2,I)
 3050      QCURV2(NATUR,3,I) = QCURV(4,I)
           ENDIF
      RETURN
 1410 FORMAT(/,' ===> ERROR !!! NATURAL CHANNEL LIMITED TO MAXIMUM OF',
     *' 50 INTERIOR DEPRESSIONS.  JOB STOPPED.')
      END

