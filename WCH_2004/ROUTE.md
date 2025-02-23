```fortran 
      SUBROUTINE ROUTE(DELQ,WSLOPE)
C     TRANSPORT BLOCK
C     CALLED BY DWLOAD NEAR LINE 170
C               INITAL NEAR LINE 195
C               TRANS NEAR LINE 511
C=======================================================================
C     ROUTING SUBPROGRAM FOR FLOW THROUGH SEWER ELEMENTS.
C     IN THE PROGRAM, ALPHA REPRESENTS NORMALIZED AREA, A/AFULL.
C                     PSI AND PS REPRESENT NORMALIZED FLOW, Q/QFULL.
C=======================================================================
C     UPDATED SEPTEMBER 1981 BY W.C.H. AND S.J.N.
C     UPDATED SEPTEMBER 1988 BY R.E.D.
C     UPDATED SEPTEMBER 1989 BY R.E.D.
C     UPDATED AUGUST 1993 BY CHARLES I. MOORE, CDM, ANNANDALE, VA FOR
C       INSTALLATION OF TYPE 26 TABULAR FLOW DIVIDER.  INSTALLED BY
C       WCH.
C     CHECK ON VALUE OF DH, 9/23/93.  WCH (RED).
C     USE STORE() INSTEAD OF STORL() FOR BACKWATER ELEMENT, WCH, 10/6/93.
C     ADD PROBABLY UNNECESSARY CHECK ON RANGE FOR L, WCH, 10/6/93.
C     SET DEFALUT VALUE FOR QO2(M) BEFORE IF STMT, RED, 12/31/93.
C     Changes to have additional level of detail in TRANSPORT irregular
C       section calculations.  CIM 9/8/00
C     Use QCURVE(4), not (3), for flow. No change to QCURV2. WCH, 7/6/01.
C     Use parameter QMINRTE for minimum flow for flow routing. Also
C       go through same extra effort for power function channels, types
C       14 and 15 as for natural channels, type 16 since all can have
C       IDETAIL = 1. WCH 3/14/02. 
C     Move a couple of error prints to better location. WCH (LR), 
C       6/11/02.
C     Correction to QCURVE2 index. WCH (CIM), 7/20/04.
C=======================================================================
C     MEANING OF A AND Q ARRAY DIMENSIONS, E.G., A(M,1,2):
C     A OR Q(CHANNEL,1-UPSTREAM 2-DOWNSTREAM,
C                    1-START OF TIME STEP 2-END OF TIME STEP)
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'NAMES.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      INCLUDE 'TST.INC'
      INCLUDE 'FLODAT.INC'
      INCLUDE 'FSPLIT.INC'
CIM### 9/8/00 add NEWTR
      INCLUDE 'NEWTR.INC'
      DIMENSION QI(NET),QO(NET),WELL1(NET),WELL2(NET),PUMP(NET),
     +          QO1(NET),QO2(NET)
      CHARACTER BMJ*10
      EQUIVALENCE (QO(1),Q(1,2,2)),(QI(1),Q(1,1,2)),(PUMP(1),DIST(1))
      EQUIVALENCE (QO1(1),QMAX(1)),(QO2(1),QFULL(1))
      EQUIVALENCE (WELL1(1),SLOPE(1)),(WELL2(1),ROUGH(1))
      DATA WT/0.60/,WD/0.60/
      DATA NUMERR/0/
Cwch 3/14/02. Set flows and areas to zero for Q < 0.00001 cfs. 
      DATA QMINRTE/1E-5/
C=======================================================================
C     USE GENERALIZED WEIGHTS OF TIME AND SPACE DERIVATIVES OF
C     CONTINUITY EQUATION.
C     WT = WEIGHT ON TIME DERIVATIVE.
C     WD = WEIGHT ON SPATIAL DERIVATIVE.
C          THESE CONSTANTS APPEAR IN DEFINITIONS OF C1(M) AND C2.
C     NOTE, WT  = WD = 0.50 CORRESPOND TO ORIGINAL VERSION OF TRANSPORT MODEL.
C           WT  = WD = 0.55 APPEAR TO GIVE BEST ATTENUATION OF HYDROGRAPHS.
C     M         = ELEMENT NUMBER
C     KLASS(NTPE) = 1 FOR CONDUIT WITH FUNCTIONAL Q-A RELATIONSHIP.
C     KLASS(NTPE) = 2 FOR CONDUIT WITH TABULAR Q-A RELATIONSHIP.
C     KLASS(NTPE) = 3 FOR ELEMENT OTHER THAN CONDUIT.
C                   COMPUTE TOTAL HEAD SLOPE AND ASSOCIATED PARAMETERS.
C     KFULL = 1 IF CONDUIT IS FULL AT UPSTREAM END.
C     KFULL = 2 IF CONDUIT IS NOT FULL AT UPSTREAM END.
C=======================================================================
      NTPE   = NTYPE(M)
      K      = KLASS(NTPE)
C=======================================================================
C     PRINT OF ERRORS STOPS WHEN NUMBER OF ERRORS EXCEED 50
C=======================================================================
      IF(NUMERR.EQ.50.AND.NPRINT.GT.0) THEN
                                        NUMERR = NUMERR + 1
                                        WRITE(N6,930)
                                        ENDIF
C=======================================================================
C     NO ITERATIONS REQUIRED FOR CONDUITS WITH SURCHARGED FLOW.
C     NO ITERATIONS REQUIRED FOR CONDUITS WITH SUPER-CRITICAL FLOW.
C=======================================================================
      IF(K.LE.2)     THEN
           IF(KFULL.EQ.1) THEN
                  C1(M)   = AFULL(M)/QFULL(M)*DXDT(M)*WT/WD
                  QMAX(M) = P4(M)*QFULL(M)
                  WSLOPE  = SLOPE(M)
                  DELQ    = 0.0
                  ELSE
                  IF(SCF(M).NE.GNO) THEN
                            ITER    = 0
                            WSLOPE  = SLOPE(M)
                            DELQ    = 0.0
                            ELSE
C=======================================================================
C                           NON-SUPERCRITICAL FLOW
C=======================================================================
                            IF(ITER.LE.NITER) THEN
                               QOLD = QFULL(M)
                               A1   = A(M,1,1)/AFULL(M)
                               A2   = A(M,2,1)/AFULL(M)
                               DV   = (VEL(Q(M,1,1),A(M,1,1))**2 -
     +                                 VEL(Q(M,2,1),A(M,2,1))**2)
     +                                /DIST(M)/64.4
                               WSLOPE = SLOPE(M) + P2(M)*(DEPTH(A1) -
     +                                  DEPTH(A2)) + DV
                               ELSE
                               A1 = A(M,1,2)/AFULL(M)
                               A2 = A(M,2,2)/AFULL(M)
                               DV = (VEL(Q(M,1,2),A(M,1,2))**2 -
     +                               VEL(Q(M,2,2),A(M,2,2))**2)
     +                              /DIST(M)/64.4
                               WSLOPE = SLOPE(M) + P2(M)*(DEPTH(A1) -
     +                                  DEPTH(A2)) + DV
                               ENDIF
C=======================================================================
                            IF(WSLOPE.LE.SLOPE(M)) WSLOPE = SLOPE(M)
                                                OMEGA = 0.50
                            IF(ITER.GT.NITER+3) OMEGA = 0.33
                            QFULL(M) = P1(M)*SQRT(WSLOPE)
                            QFULL(M) = (1.0-OMEGA)*QOLD+OMEGA*QFULL(M)
                            DELQ     = ABS(QFULL(M)-QOLD)
                            QOLD     = QFULL(M)
                            IF(DELQ.LT.EPSIL*QFULL(M)) ITER = 0
                            IF(ITER-NITER.GE.NITER-1)  ITER = 0
                            C1(M)   = AFULL(M)/QFULL(M)*DXDT(M)*WT/WD
                            QMAX(M) = P4(M)*QFULL(M)
                            ENDIF
C=======================================================================
                  ENDIF
           ENDIF
C=======================================================================
C     ROUTINE FOR CONDUIT WITH FUNCTIONAL Q-A RELATIONSHIP.
C     FIND NEW UPSTREAM AREA.
C=======================================================================
      IF(K.EQ.1) THEN
Cwch 3/14/02.  Use QMINRTE for minimum flow for routing, not 1E-20. 
      IF(QI(M).EQ.0.0.AND.Q(M,2,1).LE.QMINRTE) THEN
                                               A(M,2,2) = 0.0
                                               Q(M,2,2) = 0.0
                                               RETURN
                                               ENDIF
      IF(KFULL.EQ.1) THEN
                     A(M,1,2) = AFULL(M)
                     ELSE
                     C2    = -Q(M,1,2)/QFULL(M)
                     ALPHA = A(M,1,1)/AFULL(M)
                     CALL NEWTON(ALPHA,PS,0.0,C2,KFLAG)
                     IF(KFLAG.EQ.2) THEN
                              NUMERR = NUMERR + 1
                              IF(NPRINT.GE.1.AND.NUMERR.LT.50) THEN
                                 IF(JCE.EQ.0) WRITE(N6,910)
     +                                        TIME,N,NOE(M),A(M,1,1)
                                 IF(JCE.EQ.1) WRITE(N6,911)
     +                                        TIME,N,KOE(M),A(M,1,1)
                                 ENDIF
                              ALPHA = A(M,1,1)/AFULL(M)
                              ENDIF
                     A(M,1,2) = ALPHA*AFULL(M)
                     ENDIF
C=======================================================================
C     ASSIGN VALUES TO CONSTANTS AND SOLVE FOR DOWNSTREAM Q AND A.
C=======================================================================
      C2    = (1.0-WD)*Q(M,2,1)-(1.0-WD)*Q(M,1,1)-WD*Q(M,1,2)
      C2    = C2 + DXDT(M)*((1.0-WT)*A(M,1,2) -
     +                      (1.0-WT)*A(M,1,1)-WT*A(M,2,1))
      C2    = C2/QFULL(M)/WD
      ALPHA = A(M,2,1)/AFULL(M)
C=======================================================================
C     DOWNSTREAM Q AND A MAY NOW BE CALCULATED.
C=======================================================================
      CALL NEWTON(ALPHA,PS,C1(M),C2,KFLAG)
      IF(KFLAG.EQ.1) THEN
                     A(M,2,2) = ALPHA*AFULL(M)
                     Q(M,2,2) = PS*QFULL(M)
                     RETURN
                     ENDIF
C=======================================================================
C     DETERMINE REASON FOR NON-CONVERGENCE.
C     THEN USE DEFAULT OPTIONS TO DETERMINE Q AND A.
C
C     SEE IF LINE-C1*ALPHA-C2 INTERSECTS Q-A CURVE.
C     CONTINUITY EQUATION TRIES TO FORCE Q.GT.QMAX.
C     LET DOWNSTREAM FLOW BE QFULL UNLESS UPSTREAM Q IS GT QFULL.
C=======================================================================
      NUMERR = NUMERR + 1
      IF(NPRINT.GE.1.AND.NUMERR.LT.50) THEN
         WRITE(N6,900)
         IF(JCE.EQ.0) WRITE(N6,901) TIME,NOE(M),QFULL(M),AFULL(M),
     +                DXDT(M),Q(M,1,1),Q(M,1,2),Q(M,2,1),A(M,1,1),
     +                A(M,1,2),A(M,2,1),ALPHA,PS,C2,N
         IF(JCE.EQ.1) WRITE(N6,1901) TIME,KOE(M),QFULL(M),AFULL(M),
     +                DXDT(M),Q(M,1,1),Q(M,1,2),Q(M,2,1),A(M,1,1),
     +                A(M,1,2),A(M,2,1),ALPHA,PS,C2,N
         ENDIF
      IF((-C1(M)*ALFMAX(NTPE)-C2).GT.P4(M)) THEN
Cwch, 6/11/02. (LR)  Error print should be here, not after ELSE below.
C     Also, don't need another addition to error count?
         IF(NPRINT.GE.1.AND.NUMERR.LT.50) WRITE(N6,903)
C                                  NUMERR = NUMERR + 1
         IF(Q(M,1,2).GT.QFULL(M)) THEN
                                  Q(M,2,2) = Q(M,1,2)
                                  A(M,2,2) = A(M,1,2)
                                  ELSE
                                  Q(M,2,2) = QFULL(M)
                                  A(M,2,2) = AFULL(M)
                                  ENDIF
         RETURN
         ELSE
C=======================================================================
C     CONTINUITY EQUATION TRIES TO FORCE Q. LT. ZERO.
C     LET DOWNSTREAM FLOW BE ZERO.
C=======================================================================
         IF(C2.GT.0.0) THEN
                       NUMERR = NUMERR + 1
                       IF(NPRINT.GE.1.AND.NUMERR.LT.50) WRITE(N6,904)
                       Q(M,2,2) = 0.0
                       A(M,2,2) = 0.0
                       RETURN
                       ENDIF
C=======================================================================
C     REASON FOR NON-CONVERGENCE UNDETERMINED.
C     LET NEW Q AND A EQUAL VALUES AT PREVIOUS TIME STEP.
C=======================================================================
         IF(NPRINT.GE.1.AND.NUMERR.LT.50) WRITE(N6,902)
                   NUMERR   = NUMERR + 1
                   A(M,2,2) = A(M,2,1)
                   Q(M,2,2) = Q(M,2,1)
                   RETURN
                   ENDIF
      ENDIF
C=======================================================================
C     END OF IF-REALM FOR CONDUITS WITH FUNCTIONAL Q-A RELATIONSHIP.
C
C     ROUTINE FOR CONDUITS WITH TABULAR Q-A RELATIONSHIP.
C=======================================================================
      IF(K.EQ.2)   THEN
Cwch 3/14/02.  Use QMINRTE for minimum flow for routing, not 1E-20. 
                   IF(QI(M).EQ.0.0.AND.Q(M,2,1).LE.QMINRTE) THEN
                                               A(M,2,2) = 0.0
                                               Q(M,2,2) = 0.0
                                               RETURN
                                               ENDIF
Cwch, 3/14/02. Also need this for power function channels. 
           IF(NTPE.LE.16.AND.NTPE.GE.14) THEN
                   KK     = NQC(M)
                   DALPHA = 0.04
                   MMM    = 26
CIM### 9/8/00 CIMDETAIL detail cal for irregular sections
                   IF(IDETAIL.EQ.1) THEN
                        DALPH2 = 0.0016
CIM MM = 26 + 26 -2 DUPLICATES (1 AND 26)
                        MMM    = 50
                        ENDIF
                   ELSE   ! types other than 16
                   DALPHA = ANORM(NTPE,2) - ANORM(NTPE,1)
                   MMM    = MM(NTPE)
                   ENDIF
C=======================================================================
C     CALCULATE UPSTREAM AREA.
C=======================================================================
           IF(KFULL.EQ.1) THEN
                     A(M,1,2) = AFULL(M)
                     ELSE
                     PS = Q(M,1,2)/QFULL(M)
                     CALL FINDA(PS,A(M,1,2))
                     ENDIF
C=======================================================================
C     CALCULATE DOWNSTREAM Q AND A.
C=======================================================================
                                    ISIGN =  1
           IF(A(M,1,2).LE.A(M,1,1)) ISIGN = -1
           ICHK = 1
           I    = IOLD(M)
           C2   = (1.0-WD)*Q(M,2,1)-(1.0-WD)*Q(M,1,1)-WD*Q(M,1,2)
           C2   = C2 + DXDT(M)*((1.0-WT)*A(M,1,2) -
     +                     (1.0-WT)*A(M,1,1) - WT*A(M,2,1))
           C2   = C2/QFULL(M)/WD
C=======================================================================
C     CALCULATE SLOPE OF LINE SEGMENT I OF Q-A CURVE.
Cwch, 3/14/02.  Also do IDETAIL stuff for power function channels. 
C=======================================================================
C     For parabolic, power function, and natural channels, NTPE=16.
C     From Sub GETCUR:
C     QCURVE(1)  = hydraulic radius
C     QCURVE(2)  = area
C     QCURVE(3)  = top width
C     QCURVE(4)  = flow
C     For detail at low water:
C     QCURVE2(1) = hydraulic radius
C     QCURVE2(2) = area
C     QCURVE2(3) = flow
C=======================================================================
  120      IF(NTPE.LE.16.AND.NTPE.GE.14) THEN
                   KK    = NQC(M)
CIM### 9/8/00 DETAIL  First is normal case for type 16
                 IF(IDETAIL.EQ.0) THEN
                   SLUPE = (QCURVE(KK,4,I+1) - QCURVE(KK,4,I))/DALPHA
                 ELSE  !IDETAIL = 1
CIM REFINED ESTIMATE FOR LOW FLOW
                   IF (I.GE.26) THEN
                    ICIM = I-24
                    SLUPE = (QCURVE(KK,4,ICIM+1) - QCURVE(KK,4,ICIM))
     1                      /DALPHA
                   ELSE
                    SLUPE = (QCURV2(KK,3,I+1) - QCURV2(KK,3,I))/DALPH2
                   ENDIF
                 ENDIF !IDETAIL
CIM### 9/8/00 DETAIL

                   ELSE     ! this is types other than 16
                   SLUPE = (QNORM(NTPE,I+1) - QNORM(NTPE,I))/DALPHA
                   ENDIF
           IF(SLUPE+C1(M).EQ.0.0) GO TO 130
C=======================================================================
C     COMPUTE ALPHA CORRESPONDING TO INTERSECTION OF LINE SEGMENT
C     I OF Q-A CURVE WITH LINE -C1*ALPHA-C2.
C     CHECK TO SEE IF ALPHA IS IN PROPER RANGE.
Cwch, 3/14/02.  Also do extra stuff for power function channels. 
C=======================================================================
           IF(NTPE.LE.16.AND.NTPE.GE.14) THEN
                KK    = NQC(M)
CIM### 9/8/00 DETAIL FIRST IS NORMAL CASE FOR IDETAIL = 0
                IF (IDETAIL.EQ.0) THEN
                  ALPHA = (-QCURVE(KK,4,I)-C2+SLUPE*FLOAT(I-1)*0.04) /
     +                                     (SLUPE + C1(M))
                  ELSE
cim  refined low flow estimates 4/96
                  IF (I.GE.26) THEN
                    ICIM = I-24
              ALPHA = (-QCURVE(KK,4,ICIM)-C2+SLUPE*FLOAT(ICIM-1)*0.04) /
     +                                     (SLUPE + C1(M))
                    ELSE
CIM Here, should use 3, not 4.
C                ALPHA = (-QCURV2(KK,4,I)-C2+SLUPE*FLOAT(I-1)*0.0016) /
Cwch 7/20/04
                ALPHA = (-QCURV2(KK,3,I)-C2+SLUPE*FLOAT(I-1)*0.0016) /
     +                                     (SLUPE + C1(M))
                    ENDIF
                  ENDIF
                ELSE  !  Types other than 16
                ALPHA = (-QNORM(NTPE,I)-C2+SLUPE*ANORM(NTPE,I))/
     1                                                    (SLUPE+C1(M))
                ENDIF
           IF(ALPHA.GT.1.0.OR.ALPHA.LT.0.0) GO TO 125
           L  =  IFIX(ALPHA/DALPHA) + 1
           IF(NTPE.LE.16.AND.NTPE.GE.14) THEN
                IF(L.GT.25) L = 25
                KK    = NQC(M)
CIM### 9/8/00 DETAIL
                IF(L.GE.2.OR.IDETAIL.EQ.0) THEN
                  PS    = QCURVE(KK,4,L) + (ALPHA - FLOAT(L-1)*0.04 )/
     1                   DALPHA * (QCURVE(KK,4,L+1) - QCURVE(KK,4,L))
                  ELSE  ! DETAILED FOR LOWER PORTION OF CURVE
                  L2 = IFIX(ALPHA/DALPH2) + 1
              PS    = QCURV2(KK,3,L2) + (ALPHA - FLOAT(L2-1)*0.0016 )/
     1                DALPH2 * (QCURV2(KK,3,L2+1) - QCURV2(KK,3,L2))
                  ENDIF
                ELSE   !types other than 16
C#### WCH, 10/6/93.  CHECK RANGE ON L.  IF AT MAX, USE QFULL.
                IF(L.LT.MMM) THEN
                    PS    = QNORM(NTPE,L) + (ALPHA-ANORM(NTPE,L))/DALPHA
     1                          * (QNORM(NTPE,L+1) - QNORM(NTPE,L))
                    ELSE
                    PS    = QNORM(NTPE,MMM)
                    ENDIF
                ENDIF
           IF(ABS(PS+C1(M)*ALPHA+C2).LE.EPSIL) GO TO 150
C=======================================================================
C     TRY NEXT LINE SEGMENT.
C=======================================================================
  125      I = I+ISIGN
  126      IF(I.GT.0.AND.I.LT.MMM) GO TO 120
C=======================================================================
C     CONDITION FOR ICHK = 2
C=======================================================================
           IF(ICHK.EQ.2) THEN
                NUMERR = NUMERR + 1
                IF(NPRINT.GE.1.AND.NUMERR.LT.50) THEN
                     WRITE(N6,900)
                     IF(JCE.EQ.0) WRITE(N6,901)TIME,NOE(M),QFULL(M),
     1                 AFULL(M),DXDT(M),Q(M,1,1),Q(M,1,2),Q(M,2,1),
     2                 A(M,1,1),A(M,1,2),A(M,2,1),ALPHA,PS,C2,N
                     IF(JCE.EQ.1) WRITE(N6,1901)TIME,KOE(M),QFULL(M),
     1                 AFULL(M),DXDT(M),Q(M,1,1),Q(M,1,2),Q(M,2,1),
     2                 A(M,1,1),A(M,1,2),A(M,2,1),ALPHA,PS,C2,N
                     ENDIF
                IF((-C1(M)*ALFMAX(NTPE)-C2).GT.P4(M)) THEN
Cwch, 6/11/02. (LR)  Error print should be here, not after ELSE below.
                     IF(NPRINT.GE.1.AND.NUMERR.LT.50) WRITE(N6,903)
                     IF(Q(M,1,2).GT.QFULL(M)) THEN
                          Q(M,2,2) = Q(M,1,2)
                          A(M,2,2) = A(M,1,2)
                          ELSE
                          Q(M,2,2) = QFULL(M)
                          A(M,2,2) = AFULL(M)
                          NUMERR = NUMERR + 1
                          ENDIF
                     RETURN
                     ELSE
                     IF(C2.GT.0.0) THEN
                          IF(NPRINT.GE.1.AND.NUMERR.LT.50) WRITE(N6,904)
                          Q(M,2,2) = 0.0
                          A(M,2,2) = 0.0
                          NUMERR = NUMERR + 1
                          RETURN
                          ENDIF
                     IF(NPRINT.GE.1.AND.NUMERR.LT.50) WRITE(N6,902)
                     A(M,2,2) = A(M,2,1)
                     Q(M,2,2) = Q(M,2,1)
                     NUMERR = NUMERR + 1
                     RETURN
                     ENDIF
                ENDIF
C=======================================================================
C     END OF ICHK = 2 CONDITIONAL STATEMENTS
C=======================================================================
C     IF I HAS REACHED ZERO OR MMM START AT IOLD AND GO OTHER WAY
C=======================================================================
           ISIGN = -ISIGN
           I     = IOLD(M)+ISIGN
           ICHK  = 2
           GO TO 126
C=======================================================================
C     LINE-C1*ALPHA-C2 AND LINE SEGMENT ARE PARALLEL.
C     CHECK TO SEE IF THEY ARE CO-LINEAR.
C=======================================================================
  130      IF(NTPE.LE.16.AND.NTPE.GE.14) THEN
                KK = NQC(M)
CIM### 9/8/00 DETAIL Normal calculations were IDETAIL = 0
              IF(IDETAIL.EQ.0) THEN
                IF(ABS(C2+QCURVE(KK,4,I)-SLUPE*FLOAT(I-1)*.04).GT.EPSIL)
     1                                                         GO TO 125
                ALPHA = QCURVE(KK,2,I) + DALPHA/2.0
                L     = IFIX(ALPHA/DALPHA) + 1
                PS    = QCURVE(KK,4,L) + (ALPHA - FLOAT(I-1)*0.04)/
     1                DALPHA * (QCURVE(KK,4,L+1) - QCURVE(KK,4,L) )
             ELSE   ! This is detailed caculations section
                IF (I.GE.26) THEN
                ICIM = I-24
          IF(ABS(C2+QCURVE(KK,4,ICIM)-SLUPE*FLOAT(ICIM-1)*.04).GT.EPSIL)
     1                                                         GO TO 125
                ALPHA = QCURVE(KK,2,ICIM) + DALPHA/2.0
                L     = IFIX(ALPHA/DALPHA) + 1
                ELSE
                ICIM = I
          IF(ABS(C2+QCURV2(KK,3,I)-SLUPE*FLOAT(I-1)*.0016).GT.EPSIL)
     1                                                         GO TO 125
                ALPHA = QCURV2(KK,2,I) + DALPH2/2.0
                L2    = IFIX(ALPHA/DALPH2) + 1
                ENDIF
                IF (L.GE.2) THEN
                PS = QCURVE(KK,4,L) + (ALPHA - FLOAT(ICIM-1)*0.04)/
     1               DALPHA * (QCURVE(KK,4,L+1) - QCURVE(KK,4,L))
                ELSE
                PS = QCURV2(KK,3,L2) + (ALPHA - FLOAT(ICIM-1)*0.0016)/
     1                DALPH2 * (QCURV2(KK,3,L2+1) - QCURV2(KK,3,L2) )
                ENDIF
                ENDIF  ! End of detailed calculations
                ELSE  !types other than 16
              IF(ABS(C2 + QNORM(NTPE,I) - SLUPE*ANORM(NTPE,I)).GT.EPSIL)
     1                                                         GO TO 125
                ALPHA = ANORM(NTPE,I)    + DALPHA/2.0
                L     = IFIX(ALPHA/DALPHA) + 1
                PS    = QNORM(NTPE,L)    + (ALPHA-ANORM(NTPE,L))/DALPHA
     +                       * (QNORM(NTPE,L+1) - QNORM(NTPE,L))
                ENDIF
 150       IOLD(M) = I
C=======================================================================
C     IF VERY LOW FLOW, OBTAIN REFINED CALCULATION FOR CIRCULAR PIPE
C=======================================================================
           IF(ALPHA.LT.0.04.AND.NTPE.EQ.1) THEN
               CALL NEWTON(ALPHA,PS,C1(M),C2,KFLAG)
               IF(KFLAG.EQ.1) THEN
                              A(M,2,2) = ALPHA*AFULL(M)
                              Q(M,2,2) = PS*QFULL(M)
                              RETURN
                              ENDIF
               IF(NPRINT.GE.1.AND.NUMERR.LT.50) THEN
                    WRITE(N6,900)
               IF(JCE.EQ.0) WRITE(N6,901)TIME,NOE(M),QFULL(M),AFULL(M),
     +                      DXDT(M),Q(M,1,1),Q(M,1,2),Q(M,2,1),A(M,1,1),
     +                      A(M,1,2),A(M,2,1),ALPHA,PS,C2,N
               IF(JCE.EQ.1) WRITE(N6,1901)TIME,KOE(M),QFULL(M),AFULL(M),
     +                      DXDT(M),Q(M,1,1),Q(M,1,2),Q(M,2,1),A(M,1,1),
     +                      A(M,1,2),A(M,2,1),ALPHA,PS,C2,N
                    NUMERR = NUMERR + 1
                    ENDIF
               IF((-C1(M)*ALFMAX(NTPE)-C2).GT.P4(M)) THEN
                    IF(Q(M,1,2).GT.QFULL(M)) THEN
                                        Q(M,2,2) = Q(M,1,2)
                                        A(M,2,2) = A(M,1,2)
                                        ELSE
                                        IF(NPRINT.GE.1.AND.NUMERR.
     +                                            LT.50) WRITE(N6,903)
                                        Q(M,2,2) = QFULL(M)
                                        A(M,2,2) = AFULL(M)
                                        NUMERR = NUMERR + 1
                                        ENDIF
                    RETURN
                    ELSE
                    IF(C2.GT.0.0) THEN
                             IF(NPRINT.GE.1.AND.
     +                                 NUMERR.LT.50) WRITE(N6,904)
                             Q(M,2,2) = 0.0
                             A(M,2,2) = 0.0
                             NUMERR   = NUMERR + 1
                             RETURN
                             ENDIF
                    IF(NPRINT.GE.1.AND.NUMERR.LT.50) WRITE(N6,902)
                    A(M,2,2) = A(M,2,1)
                    Q(M,2,2) = Q(M,2,1)
                    NUMERR = NUMERR + 1
                    RETURN
                    ENDIF
               ENDIF
           A(M,2,2) = ALPHA * AFULL(M)
           Q(M,2,2) =    PS * QFULL(M)
           RETURN
           ENDIF
C===================================================================
C	END OF IF-REALM FOR CHANNELS WITH FUNCTIONAL Q-A RELATIONSHIP.
C===================================================================
C
C=========> ELEMENT IS NOT A CONDUIT.
C
C====================================================================
      NGOTO = NTPE - 18
C====================================================================
C     ERROR ===> CONDUITS SHOULD BE KLASS 1 OR 2  (NTPE < 19)
C====================================================================
      IF(NGOTO.LT.1) THEN
                     QO(M)  = QI(M)
                     NUMERR = NUMERR + 1
                     IF(NPRINT.GT.0.AND.NUMERR.LT.50) WRITE(N6,905) M
                     RETURN
                     ENDIF
C====================================================================
C     MANHOLE ===> SIMPLY TRANSLATE FLOW WITH NO TIME DELAY (NTPE = 19)
C====================================================================
      IF(NGOTO.EQ.1) THEN
                     QO(M) = QI(M)
                     RETURN
                     ENDIF
C====================================================================
C     LIFT STATION ===> PUMPS ASSUMED TO BE PUMPING AT CONSTANT RATE
C     FORCE MAIN ASSUMED TO REMAIN FULL AT ALL TIMES RESULTING IN NO
C     VOLUME IN WET WELL INITIALLY IS HALF THE CAPACITY(GEOM1).
C     TIME DELAY.  (NTPE = 20)
C====================================================================
      IF(NGOTO.EQ.2) THEN
                     WELL2(M) = WELL2(M) + QI(M)*DT
                     IF(WELL2(M).GE.GEOM1(M)) THEN
                             IF(WELL2(M).GE.PUMP(M)*DT) THEN
                                         QO(M)    = PUMP(M)
                                         WELL2(M) = WELL2(M)-PUMP(M)*DT
                                         GEOM2(M) = 1.0
                                         RETURN
                                         ELSE
                                         QO(M)    = WELL2(M)/DT
                                         WELL2(M) = 0.0
                                         GEOM2(M) = 0.0
                                         RETURN
                                         ENDIF
                     ENDIF
                     IF(GEOM2(M).LE.0.0) THEN
                             QO(M)    = 0.0
                             GEOM2(M) = 0.0
                             RETURN
                             ELSE
                             IF(WELL2(M).GE.PUMP(M)*DT) THEN
                                         QO(M)    = PUMP(M)
                                         WELL2(M) = WELL2(M)-PUMP(M)*DT
                                         GEOM2(M) = 1.0
                                         RETURN
                                         ELSE
                                         QO(M)    = WELL2(M)/DT
                                         WELL2(M) = 0.0
                                         GEOM2(M) = 0.0
                                         RETURN
                                         ENDIF
                             ENDIF
                     ENDIF
C====================================================================
C     ROUTINE FOR TYPE 21 AND TYPE 24 FLOW DIVIDERS.
C     TYPE 21 MAY BE USED FOR A SIMPLE OVERFLOW STRUCTURE.
C     TYPE 24 MAY BE USED WITH A CUNNETTE SECTION DOWNSTREAM.
C====================================================================
      IF(NGOTO.EQ.3.OR.NGOTO.EQ.6) THEN
                     QO(M) = QI(M)
                     IF(GEOM1(M).GT.0.0)   THEN
                          IF(QI(M).LE.GEOM1(M)) THEN
                                            QO1(M) = QI(M)
                                            QO2(M) = 0.0
                                            ELSE
                                            QO1(M) = GEOM1(M)
                                            QO2(M) = QI(M)-GEOM1(M)
                                            ENDIF
                          ELSE
C====================================================================
C     MODIFICATION MADE 9/26/89 TO ALLOW GEOM1(M) TO INDICATE
C     A PIPE THAT WILL ONLY ALLOW QFULL(M) TO PASS TO GEOM3(M).
C====================================================================
                          L      = GEOM3(M)
                          BMJ    = KGEOM(M)
                          NEXRAM = NIN(L,BMJ)
                          IF(QI(M).LE.QFULL(NEXRAM)) THEN
                                            QO1(M) = QI(M)
                                            QO2(M) = 0.0
                                            ELSE
                                            QO1(M) = QFULL(NEXRAM)
                                            QO2(M) = QI(M)-QFULL(NEXRAM)
                                            ENDIF
                          ENDIF
                     RETURN
                     ENDIF
C====================================================================
C     STORAGE ELEMENT. ADAPTED FROM STORAGE/TREATMENT BLOCK.
C====================================================================
      IF(NGOTO.EQ.4) THEN
                     KSTOR = KSTORE(M)
                     QINST = QI(M)
                     CALL TSTORG
                     QO1(M) = QOUST1
                     QO2(M) = QOUST2
                     QO(M)  = QOUST1 + QOUST2
                     RETURN
                     ENDIF
C====================================================================
C     ROUTINE FOR TYPE 23 FLOW DIVIDER.
C     USE WITH SIDE WEIR OR OTHER WEIR TYPE DIVERSION.
C     DIST  = MAX FLOW WITHOUT FLOW OVER WEIR.
C     SLOPE = MAXIMUM FLOW THROUGH WHOLE STRUCTURE.
C     ROUGH = WEIR CONSTANT TIMES WEIR LENGTH.
C     GEOM1 = WEIR HEIGHT.
C     GEOM2 = DEPTH IN STRUCTURE AT TIME OF MAXIMUM FLOW.
C     GEOM3 = DOWNSTREAM EXT. ELE. NUM. INTO WHICH GOES UNDIVERTED FLOW.
C             FLOW OVER WEIR IS THE DIVERTED FLOW.
C====================================================================
      IF(NGOTO.EQ.5) THEN
                     QO(M) = QI(M)
                     IF(QI(M).GT.SLOPE(M).AND.JCE.EQ.0) WRITE(N6,920)
     +                                     TIME,NOE(M),QI(M)
                     IF(QI(M).GT.SLOPE(M).AND.JCE.EQ.1) WRITE(N6,921)
     +                                     TIME,KOE(M),QI(M)
                     IF (QI(M).LE.DIST(M)) THEN
                                           QO2(M) = 0.0
                                           QO1(M) = QI(M)
                                           RETURN
                                           ENDIF
                     DH     = (QI(M)-DIST(M))/(SLOPE(M)-DIST(M)) *
     +                                        (GEOM2(M)-GEOM1(M))
C#### WCH (RED), 9/93.  CHECK FOR LARGE ENOUGH VALUE OF DH.
C#### RED (WCH), 12/31/93.  SET DEFAULT VALUE FOR QO2(M)  (= 0.0).
                                       QO2(M) = 0.0
                     IF(DH.GT.1.0E-10) QO2(M) = ROUGH(M)*DH*SQRT(DH)
                     QO1(M) = QI(M) - QO2(M)
                     QO1(M) = AMAX1(QO1(M),DIST(M))
                     QO2(M) = QI(M) - QO1(M)
                     RETURN
                     ENDIF
C====================================================================
C     ROUTINE FOR BACKWATER ELEMENT.
C     ELEMENT ACTS AS FLOW DIVIDER.
C     GEOM3 = ELEMENT NUMBER OF DOWNSTREAM STORAGE UNIT.
C
C     IF BACKWATER EXTENDS ALL THE WAY UP TO ELEMENT,
C     THEN WHOLE FLOW IS DIVERTED TO STORAGE ELEMENT.
C     ASSUME LENGTH OF BACKWATER IS PROPORTIONAL TO SQRT OF STORAGE
C     VOLUME.
C====================================================================
      IF(NGOTO.EQ.7) THEN
               QO(M)  = QI(M)
               L      = GEOM3(M)
               BMJ    = KGEOM(M)
               L      = NIN(L,BMJ)
               KSTOR  = KSTORE(L)
               MINT   = MINTS(KSTOR)
C#### WCH, 10/6/93.  CHANGE STORL() TO STORE().
               QO1(M) = QI(M)*SQRT(STORE(KSTOR)/TSTORE(KSTOR,MINT))
               IF(QO1(M).GT.QI(M)) QO1(M) = QI(M)
               QO2(M) = QI(M) - QO1(M)
               RETURN
               ENDIF
C#######################################################################
C      WCH, 8/93.
C      ROUTINE FOR TABULAR DIVERSION  NTYPE = 26
C      WRITTEN BY CHUCK MOORE, CDM, 8/93
C=======================================================================
      IF(NGOTO.EQ.8) THEN
         QO(M) = QI(M)
         KIN   = KINOUT(M)
         DO 400 IM=1,NSPLIT(KIN)
         INEXT = IM
         IF (SPLITIN(KIN,IM).GE.QI(M)) GO TO 405
  400    CONTINUE
C=======================================================================
C  PRINT WARNING IF INFLOW EXCEEDS MAXIMUM TABULAR VALUE.
C=======================================================================
         WRITE(N6,940) TIME,N
         IF(JCE.EQ.0) WRITE(N6,1030) NOE(M)
         IF(JCE.EQ.1) WRITE(N6,1031) KOE(M)
         WRITE(N6,1035) QI(M),SPLITIN(KIN,NSPLIT(KIN))
         UNDIV = SPLITOUT(KIN,NSPLIT(KIN))
         GO TO 410
  405    IF(INEXT.EQ.1) THEN
            UNDIV = SPLITOUT(KIN,INEXT)
            ELSE
            RRATIO = (QI(M)-SPLITIN(KIN,INEXT-1))/
     1             (SPLITIN(KIN,INEXT)-SPLITIN(KIN,INEXT-1))
            UNDIV = SPLITOUT(KIN,INEXT-1) +
     1      RRATIO*(SPLITOUT(KIN,INEXT)-SPLITOUT(KIN,INEXT-1))
            ENDIF
  410    DIVERT = QI(M) - UNDIV
         QO1(M) = UNDIV
         QO2(M) = DIVERT
         RETURN
      ENDIF

C#######################################################################
C      WCH, 4/99
C      ROUTINE FOR QUALITY FLOW SPLITTER NTYPE = 27
C====================================================================
C       SIMPLY TRANSLATE FLOW WITH NO TIME DELAY
C       SAME AS MANHOLE(NTPE = 19)
C====================================================================
      IF(NGOTO.EQ.9) THEN
                     QO(M) = QI(M)
                     RETURN
                     ENDIF
C====================================================================
  900 FORMAT(/,' ===> ITERATION FOR DOWNSTREAM FLOW HAS NOT CONVERGED.',
     +/,'      Q(CHANNEL,1-UPSTREAM 2-DOWNSTREAM,1-START 2-END)',/,3X
     1,'TIME  ELEMENT  QFULL  AFULL  DXDT  Q(M,1,1) Q(M,1,2) Q(M,2,1) A(
     2M,1,1) A(M,1,2) A(M,2,1)  ALPHA     PS     C2 TIME STEP')
  901 FORMAT(1PE8.2,I10,0PF9.1,F7.2,F7.1,3F9.2,3F9.3,3F7.3,I10)
 1901 FORMAT(1PE8.2,A10,0PF9.1,F7.2,F7.1,3F9.2,3F9.3,3F7.3,I10)
  902 FORMAT(/,' ===> REASON FOR NON-CONVERGENCE UNDETERMINED. USE Q,A V
     +ALUES AT PREVIOUS TIME STEP.')
  903 FORMAT(/,' ===> CONTINUITY EQN TRIES TO FORCE Q.GT.QMAX. USE QFULL
     + UNLESS UPSTREAM Q.GT.QFULL - THEN USE UPSTREAM VALUE.')
  904 FORMAT(/,' ===> CONTINUITY EQN TRIES TO FORCE Q < 0.0   USE',
     +         ' ZERO FLOW.')
  905 FORMAT(/,' ===> ERROR !! CONDUITS SHOULD BE CLASS 1 OR 2. M= ',I5)
  910 FORMAT(/,' ===> WARNING !!  NEWTON UNABLE TO FIND AREA GIVEN FLOW.
     + TIME = ',F7.1,', TIME STEP=',I3,/,
     +'       EXT. ELE. NUM.=',I8,', USE OLD UPSTREAM AREA = ',F6.2)
  911 FORMAT(/,' ===> WARNING !!  NEWTON UNABLE TO FIND AREA GIVEN FLOW.
     + TIME = ',F7.1,', TIME STEP=',I3,/,
     +'       EXT. ELE. NUM.= ',A10,', USE OLD UPSTREAM AREA = ',F6.2)
  920 FORMAT(/,' ===> WARNING !!. TIME = ',E14.7,' ELEMENT',I9,
     +  '      MAX FLOW TO TYPE 23 DIVIDER EXCEEDED. INFLOW = ',E14.7)
  921 FORMAT(/,' ===> WARNING !!. TIME = ',E14.7,' ELEMENT ',A10,
     +  '      MAX FLOW TO TYPE 23 DIVIDER EXCEEDED. INFLOW = ',E14.7)
  930 FORMAT(/,' ====> ALLOWABLE ERRORS IN ROUTE EXCEEDED.',/,
     +         ' ====> PRINTOUT OF ERRORS ENDED.')
C#### WCH (CDM), 8/93.
  940 FORMAT(' WARNING - INFLOW TO TYPE 26 FLOW DIVIDER EXCEEDS MAXIMUM
     1INFLOW SPECIFIED IN TABLE.',/,' TIME =',F7.1,' SEC., TIME STEP ='
     2 ,I5)
 1030 FORMAT(' TABULAR FLOW SPLIT ELEMENT # ',I10)
 1031 FORMAT(' TABULAR FLOW SPLIT ELEMENT # ',A10)
 1035 FORMAT('+',40X,', INFLOW = ',F10.3,' CFS, MAXIMUM IN TABLE = ',
     1 F10.3,' CFS.')
C=======================================================================
      END
``` 
