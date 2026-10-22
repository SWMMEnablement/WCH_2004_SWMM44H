      FUNCTION RADH(AA)
C     TRANSPORT BLOCK
C	CALLED BY QUAL AT LINE 55 AND PSI AT LINE 48
C=======================================================================
C     FUNCTION TO COMPUTE THE HYDRAULIC RADIUS FOR A GIVEN FLOW AREA.
C
C     UPDATED (NEW COMMON) BY R.E.D., MAY, 1988.
C     CHECK FOR MAGNITUDE OF ASIN ARGUMENT, WCH, 12/5/94.
C     CHECK FOR MAXIMUM AREA, WCH, 10/2/96.
C     Modified for additional detail for Irr. Section 9/8/99 CIM
C     Use to calculate top width, WIDTHV, if NWQ = 1. Transmit 
C     through Common in TRANQ.INC.  
C     Make some minor corrections to RADH calcs for circle for extreme
C       THETO values.  WCH, 7/11/01. 
C     Add types 14 and 15 to if statements for type 16. WCH, 3/14/02. 
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      INCLUDE 'FLODAT.INC'
CIM### 9/8/00 Add NEWTR
      INCLUDE 'NEWTR.INC'
Cwch, 7/6/01.
      INCLUDE 'TRANWQ.INC'
      REAL L1
C=======================================================================
C     PARAMETERS NEEDED ARE:
C
C     AA = AREA OF FLOW FOR WHICH RADH IS TO BE CALCULATED
C=======================================================================
      NTPE  = NTYPE(M)
      AF    = AFULL(M)
      IF(AA.LE.0.0) THEN
                    RADH   = 0.0
	              WIDTHV = 0.0
	              WIDTHM = 0.0
                    RETURN
                    ENDIF
C#### WCH, 10/2/96.  DON'T ALLOW AREA TO BE GREATER THAN AFULL.
      IF(AA.GT.AFULL(M)) AA = AFULL(M)
C=======================================================================
C     NO CALCULATIONS FOR NON-CONDUITS
C=======================================================================
      IF(NTPE.GE.19) THEN
                   RADH = 0.0
                   RETURN
                   ENDIF
C=======================================================================
C     RECTANGULAR PIPE
C=======================================================================
      IF(NTPE.EQ.2) THEN
                  XL   = GEOM2(M)
                  DIAM = GEOM1(M)
                  RADH = AA/(XL+2.0*(AA/AF)*DIAM)
	            WIDTHV = GEOM2(M)
	            WIDTHM = GEOM2(M)
                  RETURN
                  ENDIF
C=======================================================================
C     HYDRAULIC RADIUS FOR MODIFIED BASKET HANDLE CONDUIT.
C=======================================================================
      IF(NTPE.EQ.10) THEN
                   DD = (GEOM1(M)+GEOM2(M)/2.0)*DEPTH(AA/AFULL(M))
	             WIDTHM = GEOM2(M)
                   IF(DD.LE.GEOM1(M)) THEN
                            RADH = AA/(GEOM2(M)+2.0*DD)
	                      WIDTHV = GEOM2(M)
                            ELSE
                            CATHY = DD-GEOM1(M)
C#### WCH, 12/5/94.  CHECK FOR ARG > 1.0.
C     IF ARG > 1, LET THETO = MAX = PI/2.
                            ARG = 2.0*CATHY/GEOM2(M)
                            IF(ARG.GE.1.0) THEN
                               THETO = 1.5707963
                               ELSE
                               THETO = ASIN(ARG)
                               ENDIF
C####                            THETO = ASIN(2.0*CATHY/GEOM2(M))
                            PER   = 2.0*GEOM1(M)+GEOM2(M)*(1.0+THETO)
						  RADH  = AA/PER
	                      WIDTHV = GEOM2(M)*COS(THETO)
                            ENDIF
                   RETURN
                   ENDIF
C=======================================================================
C     HYDRAULIC RADIUS FOR RECTANGULAR, TRIANGULAR BOTTOM.
C=======================================================================
      IF(NTPE.EQ.11) THEN
                   DD = GEOM1(M)*DEPTH(AA/AFULL(M))
                   IF(DD.LE.GEOM3(M)) THEN
                         PER   = DD/P5(M)
                         RADH  = AA/PER
					   WIDTHV = 2.0*SQRT((PER/2.0)**2-DD**2)
                         ELSE
                         CATHY = DD-GEOM3(M)
                         PER    = 2.0*CATHY+GEOM3(M)/P5(M)
                         RADH   = AA/PER
	                   WIDTHV = GEOM2(M)
                         ENDIF
	             WIDTHM = WIDTHV
                   RETURN
                   ENDIF
C=======================================================================
C     HYDRAULIC RADIUS FOR RECTANGULAR, ROUND BOTTOM.
C=======================================================================
      IF(NTPE.EQ.12) THEN
                   IF(AA.GT.P6(M)) THEN
                         CATHY = AA-P6(M)
                         PER   = 2.0*CATHY/GEOM2(M)+GEOM3(M)*P5(M)
                         RADH  = AA/PER
	                   WIDTHV = GEOM2(M)
		               WIDTHM = GEOM2(M)
                         RETURN
                         ELSE
                         DIAM = 2.0*GEOM3(M)
                         AF   = 3.14159 *GEOM3(M)*GEOM3(M)
                         GO TO 105
                         ENDIF
                   ENDIF
C=======================================================================
C    HYDRAULIC RADIUS OF TRAPEZOIDAL CHANNEL
C    YYY = depth.
C=======================================================================
      IF(NTPE.EQ.13) THEN
                   YYY = (-GEOM2(M) + SQRT(GEOM2(M)**2 + 4.0 *
     1                          AA/GEOM3(M))) * GEOM3(M)/2.0
Cwch, 7/11/01.  Whoa!  How long has this error been here?!
Cwch                   RADH = AA/(GEOM2(M) + YYY +P5(M))
                   RADH = AA/(GEOM2(M) + YYY*P5(M))
	             WIDTHV = GEOM2(M) + 2.0*YYY/GEOM3(M)
	             WIDTHM = WIDTHV
                   RETURN
                   ENDIF
C=======================================================================
C    HYDRAULIC RADIUS OF PARABOLIC, POWER FUNCTION OR NATURAL CHANNEL
Cwch 3/14/02. Change IF to include types 14-16. 
C=======================================================================
      IF(NTPE.LE.16.AND.NTPE.GE.14) THEN
           M1              = NQC(M)
           RFULL           = QCURVE(M1,1,26)
           QCURVE(M1,1,26) = 1.0
           ASPOT           = AA/AF
           I               = 1   + IFIX(ASPOT/0.04)
Cwch, 7/6/01. Add optional computation of width.
           IF(NWQ.EQ.1) THEN
                   TWFULL           = QCURVE(M1,3,26)
                   QCURVE(M1,3,26) = 1.0
                   DELTA           = (ASPOT - 0.04*FLOAT(I-1))/0.04
                   WIDTHV          = (QCURVE(M1,3,I) + (QCURVE(M1,3,I+1)
     1                                          - QCURVE(M1,3,I))*DELTA)
                   WIDTHV          = WIDTHV*TWFULL
	             WIDTHM          = WIDTHV
                   QCURVE(M1,3,26) = TWFULL
	             ENDIF
CIM### 9/8/00 detail for irregular sections
           IF (I.GE.2.OR.IDETAIL.EQ.0) THEN
                   DELTA           = (ASPOT - 0.04*FLOAT(I-1))/0.04
                   RADH            = (QCURVE(M1,1,I) + (QCURVE(M1,1,I+1)
     1                                          - QCURVE(M1,1,I))*DELTA)
                   RADH            = RFULL*RADH
                   QCURVE(M1,1,26) = RFULL
cim      write(n6,*) 'RADH1',aa,af,aspot,i,qcurve(m1,1,i),qcurve(m1,1,i+1),
cim     +delta,radh,rfull
                   RETURN
                   ELSE  ! Here for detailed portion of curve
                   I               = 1   + IFIX(ASPOT/0.0016)
                   DELTA           = (ASPOT - 0.0016*FLOAT(I-1))/0.0016
                   M1              = NQC(M)
                   RADH            = (QCURV2(M1,1,I) + (QCURV2(M1,1,I+1)
     +                                          - QCURV2(M1,1,I))*DELTA)
                   RADH            = RFULL*RADH
                   QCURVE(M1,1,26) = RFULL
cim      write(n6,*) 'RADH2',aa,af,aspot,i,qcurv2(m1,1,i),qcurv2(m1,1,i+1),
cim     +delta,radh,rfull
                   RETURN
                   ENDIF
CIM### 9/8/00  End of changes
cim      write(n6,*) 'RADH1',aa,af,aspot,i,qcurve(m1,1,i),qcurve(m1,1,i+1),
cim     +delta,radh,rfull
           ENDIF
C=======================================================================
C     CIRCULAR PIPE, NTPE = 1.
C     ASSUME EQUIVALENT CIRCULAR PIPE FOR ODD SHAPES AND USER-DEFINED, 
C       I.E., TYPES 3-9 AND 17-18.
C     BOTTOM OF LIST, SO HERE AS DEFAULT.
C=======================================================================
      IF(NTPE.EQ.1) THEN
                  DIAM = GEOM1(M)
                  ELSE
                  DIAM = SQRT(4.0*AF/3.14159)
                  ENDIF
C=======================================================================
  105 ALPHA = AA/AF
      IF(ALPHA.GE.0.04) THEN
                        DALPHA = ANORM(1,2) - ANORM(1,1)
                        I      = IFIX(ALPHA/DALPHA) + 1
                        D1     = DNORM(1,I)+(ALPHA-ANORM(1,I))/DALPHA*
     1                           (DNORM(1,I+1)-DNORM(1,I))
                        ELSE
                        CALL CIRCLE(ALPHA,PS,D1,2)
                        ENDIF
      D1 = D1 * DIAM
      RR = DIAM/2.0
C=======================================================================
C     HERE IF FLOW IS BELOW HALF-WAY MARK
Cwch 7/11/01. Misc. code shortcuts. 
C=======================================================================
      IF(D1-RR.LT.0.0) THEN
                       D2 = RR-D1
	                 RADH   = 0.0
	                 WIDTHV = 0.0
	                 WIDTHM = 0.0
                       IF(ABS(RR-D2).LE.1.0E-3) RETURN
C     ABOVE: PIPE ~EMPTY.
                       L1    = SQRT(RR**2-D2**2)
                       ARG   = L1/D2
                       THETO = 2.0*ATAN(ARG)
                       S     = RR*THETO
                       IF(S.LE.1.0E-4) RETURN
C     ABOVE: AGAIN, PIPE ~EMPTY.
C     HERE IF USUAL CASE OF WATER IN BOTTOM OF PIPE. 
                       RADH   = AA/S
	                 WIDTHV = 2.0*SQRT(RR**2-(RR-D1)**2)
	                 WIDTHM = WIDTHV
                       RETURN
                       ENDIF
C=======================================================================
C     HERE IF FLOW IS AT HALF-WAY MARK
C=======================================================================
      IF(ABS(D1-RR).LT.0.001) THEN
                       RADH = RR/2.0
	                 WIDTHV = 2.0*RR
	                 WIDTHM = WIDTHV
                       RETURN
                       ENDIF
C=======================================================================
C     HERE IF FLOW IS OVER HALF-WAY MARK
C=======================================================================
      IF(D1-RR.GT.0.0) THEN
                       D2     = D1-RR
	                 WIDTHM = 2.0*RR
                       IF(ABS(RR-D2).LE.1.0E-3) THEN
C     PIPE ~FULL
                                              RADH = RR/2.0
	                                        WIDTHV = 0.0
                                              RETURN
                                              ENDIF
                       L1    = SQRT(RR**2-D2**2)
                       THETO = 2.0*ATAN2(L1,D2)
                       S     = RR*THETO
                       IF(S.LE.1.0E-4) THEN
C     AGAIN, PIPE ~FULL
                                        RADH = RR/2.0
	                                  WIDTHV = 0.0
                                        RETURN
                                        ENDIF
                       S     = 2.0*3.14159*RR-S
                       RADH = AA/S
	                 WIDTHV = 2.0*SQRT(RR**2-D2**2)
                       RETURN
                       ENDIF
      END
