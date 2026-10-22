      SUBROUTINE GQUAL(J,AREA0,AREA1,FLOW0,VSURO,QINT)
C	RUNOFF BLOCK
C	CALLED BY GUTTER NEAR LINE 223
C=======================================================================
C     GQUAL was updated by W.HUBER and R.DICKINSON March, 1988.
C     GQUAL was last updated December, 1990.
C     Updated 7/28/93 by WCH to correct calculation for average
C       inflow flux during surcharging.
C     WCH, 1/23/97.  Minor change in averaging process for exponential
C       argument for quality routing, and route for storage elements.
C     WCH, 2/10/01. Modify routing to permit BMP fraction and first-
C       order decay removal in channel/pipes.  Note: some comment lines
C       and old code deleted.
C=======================================================================
C     This subroutine routes quality in channel J for the flow values
C     computed in subroutine GUTTER.  NQS quality C's are calculated.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'QUALTY.INC'
      INCLUDE 'GRWTR.INC'
      INCLUDE 'NEW88.INC'
Cwch, 2/10/01
      INCLUDE 'OVERLAND.INC'
      DIMENSION FLUX(MQUAL),FLUXAV(MQUAL),FLUX0(MQUAL,NG)
      EQUIVALENCE (FLUX0,CDOT)
C=======================================================================
C     Check flow and set zeros if below minimum.
C=======================================================================
C#### WCH, 1/23/97.  STILL NEED TO ROUTE IF WATER BEING STORED BEHIND
C     WEIR ETC., EVEN IF NO OUTFLOW.
C=======================================================================
C####      IF(OUTFLW(J).LE.1.0E-9) THEN
      IF(OUTFLW(J).LE.1.0E-9.AND.WTYPE(J).EQ.-1) THEN
                              DO 10 K   = 1,NQS
                              C(K,J)    = 0.0
                              FLUX0(K,J)= 0.0
   10                         CONTINUE
                              RETURN
                              ELSE
                              DO 15 K = 1,NQS
                              FLUX(K) = 0.0
   15                         CONTINUE
                              ENDIF
C=======================================================================
C     Compute inputs from groundwater.
C=======================================================================
      IF(NOGWSC.GT.0) THEN
                DO 20 IA = 1,NOGWSC
                IF(JCE.EQ.0.AND.NGWTOG(IA).NE.NAMEG(J)) GO TO 20
                IF(JCE.EQ.1.AND.KGWTOG(IA).NE.KAMEG(J)) GO TO 20
                DO 22 K   = 1,NQS
                GWQ(K)    = GWQ(K)  + GWFLOW(IA)*CGWQ(K)*DELT
   22           FLUX(K)   = FLUX(K) + GWFLOW(IA)*CGWQ(K)
   20           CONTINUE
                ENDIF
C=======================================================================
C     Compute inputs from upstream channels.
C     Inputs are instantaneous at the end of the time step.
C     Concentration is in mg/L, so FLUX is in mg/L * cfs.
C=======================================================================
      QACT    = QIN(J)
      DO 50 K = 1,NCP
      L       = NGTOG(K,J)
      IF(L.EQ.0) GO TO 60
      DO 40 M = 1,NQS
      FLUX(M) = FLUX(M) + C(M,L) * OUTFLW(L)
   40 CONTINUE
   50 CONTINUE
   60 CONTINUE
C=======================================================================
C     Add mass input from adjacent watersheds: POFF in mg/sec.
C     POFF is the instantaneous value at the end of the time step.
C     Here, convert FLUX to units of mg/s * ft3/L = mg/L * cfs.
C     FACT3 = 28.3 L/ft3.
C=======================================================================
      DO 80 K  = 1,NCP
      L        = NWTOG(K,J)
      IF(L .EQ.0) GO TO 90
      DO 70 M  = 1,NQS
      ND       = NDIM(M) + 1
      DO 70 JJ = 1,N1
      IF(N1.EQ.1) I = KLAND(L)
      IF(N1.GT.1) I = JJ
      FLUX(M)  = FLUX(M) + POFF(I,M,L)/FACT3(ND)
   70 CONTINUE
   80 CONTINUE
   90 CONTINUE
C=======================================================================
C     OUTFLW is the instantaneous flow at the end of a time step.
C
C     If NPG(J) is a dummy channel calculate concentration and return.
C=======================================================================
      IF(NPG(J).EQ.3) THEN
                      DO 100 M = 1,NQS
  100                 C(M,J)   = FLUX(M) / OUTFLW(J)
                      RETURN
                      ENDIF
C=======================================================================
C     Account for surcharge contribution.
C     Compute the average inflow FLUX.
Cwch, 2/10/01.  Keep FLUXAV as flux, not concentration.
C=======================================================================
      IF(QSUR(J).LE.0.0.AND.VSURO.LE.0.0) THEN
             DO 135 M  = 1,NQS
             FLUXAV(M) = 0.0
C             IF(QACT.GT.0.0) FLUXAV(M) = 0.5*(FLUX0(M,J)+FLUX(M))/QACT
             IF(QACT.GT.0.0) FLUXAV(M) = 0.5*(FLUX0(M,J)+FLUX(M))
  135        CONTINUE
             ELSE
C=======================================================================
C#### Calculations with surcharge altered by WCH, 7/28/93.
C     VSURO   --> Previous time step surcharge storage, ft3.
C     QSUR(J) --> Current time step surcharge storage (*volume*), ft3.
C     QADD    --> Change in surcharge storage (flow rate), cfs.
C                 QADD > 0 ==> Draining from stored surcharge.
C                 QADD < 0 ==> Adding to stored surcharge.
C     QIN(J)  --> The average total potential inflow, cfs.
C     QACT    --> The average actual inflow (after surcharge volume
C                 subtracted or added), cfs.
C-======================================================================
             QADD = (VSURO - QSUR(J)) / DELT
             QACT = QIN(J) + QADD
C=======================================================================
C     Compute total potential flux in.
C     FLUXIN --> Average potential flux in without surcharge, mg/L*cfs.
C     CVSUR  --> Mass of stored surcharge, mg/L * ft3 = mg * ft3/L.
C       Caution with units of mass and concentration!
C=======================================================================
             DO 120 M = 1,NQS
             FLUXIN   = (FLUX0(M,J)+FLUX(M))/2.0
C=======================================================================
C    Case 1: Surcharge volume (and load) is draining (diminishing).
C    Add load to average potential flux.
C    Stored surcharge load has no decay or loss.  Use old concentration
C      (CVSUR/VSURO) over the time step.
C    SRFLUX has units mg/L * cfs.
C=======================================================================
             IF(QADD.GE.0.0) THEN
                  SRFLUX     = QADD*CVSUR(M,J)/VSURO
                  FLUXAV(M)  = FLUXIN + SRFLUX
                  CVSUR(M,J) = CVSUR(M,J) - SRFLUX * DELT
                  ENDIF
C=======================================================================
C     Case 2: Adding to surcharge volume (and load).
C     Subtract load stored in surcharge from average potential flux.
C     Add to load stored in surcharge.
C     Remember, QADD will be negative here, so SRFLUX will be positive.
C=======================================================================
            IF(QADD.LT.0) THEN
                 SRFLUX     = - QADD * FLUXIN/QIN(J)
                 FLUXAV(M)  = FLUXIN - SRFLUX
                 CVSUR(M,J) = CVSUR(M,J) + SRFLUX * DELT
                 ENDIF
C=======================================================================
Cwch, 2/10/01.  Keep FLUXAV as flux, not concentration.
C             FLUXAV(M)  = FLUXAV(M)/QACT
Cwch, 2/10/01. Allow decay within stored surcharge.
C=======================================================================
            DEKAY        = QDECAY(M)*CVSUR(M,J)*DELT
	      CVSUR(M,J)   = CVSUR(M,J) - DEKAY
	      SUMREMG(M,2) = SUMREMG(M,2) + DEKAY
  120       IF(CVSUR(M,J).LT.0.0) CVSUR(M,J) = 0.0
            ENDIF
C=======================================================================
C     Compute current values of channel parameters for routing.
Cwch, 2/10/01.  Compute DENOM separately from ARG to include QDECAY.
C=======================================================================
      V0   = GLEN(J) * AREA0
      V1   = GLEN(J) * AREA1
      TWOV = V0 + V1
C      ARG  = 0.0
      DENOM1 = 0.0
C=======================================================================
C#### WCH, 1/23/97.  CHANGE FORM OF AVERAGE Q/V SLIGHTLY, TO ENSURE
C     ARG = 0 WHEN Qin = 0 (BY CONTINUITY, DV/DT = Qin - Q).
C=======================================================================
C####      IF(V0.GT.0.0) ARG = FLOW0/V0
C####      IF(V1.GT.0.0) ARG = ARG + OUTFLW(J)/V1
C####      ARG               = ARG*DELT/2.0
      IF(TWOV.GT.0.0) DENOM1 = (FLOW0+OUTFLW(J))/TWOV
C=======================================================================
C     Allow for case in which conduit is only draining, with no inflow.
C#### WCH, 7/28/93.  NEED FACTOR OF 2 FOR DV/V CALC TO GET AVG. VOL.
C=======================================================================
      IF(TWOV.GT.0.0)  DENOM1 = DENOM1 + 2.0 * (V1-V0)/TWOV/DELT
      IF(DENOM1.LT.0.0)   DENOM1 = 0.0
C=======================================================================
C     Move loop on pollutants up, to include QDECAY(M).
C=======================================================================
      DO 150 M = 1,NQS
	ND    = NDIM(M) + 1
	DENOM = DENOM1 + QDECAY(M)
	ARG   = DENOM*DELT
C=======================================================================
C     EXXP => 0 ==> Big ARG, big Qin/V, concentration => Cin.
C     EXXP >  0 ==> Smaller ARG, smaller Qin/V, and
C      concentration between Cin and Co.
C     EXXP => 1 ==> ARG => 0, Qin/V => 0, concentration => Co.
C=======================================================================
                      EXXP = 0.0
      IF(ARG.LT.10.0) EXXP = EXP(-ARG)
C=======================================================================
C     If there is no initial volume, conc. must equal TOP/DENOM.
C=======================================================================
      IF(V0.LT.1.0E-5) EXXP = 0.0
C=======================================================================
C     Compute final parameters.
C     Routing assumes complete mixing within channel/pipe.
C     FLUX0  = Array that stores the computed flux for the next step,
C       mg/L * cfs.
C     WCH = Co = C(M,J) = Concentration at the beginning of the time
C       step, mg/L.
C     Then C(M,J) will be computed for end of time step value.
C     TOP = mg/L*cfs / ft3 = mg/L per s.
C     DENOM = cfs per ft3 = 1/s.  So TOP/DENOM = mg/L.
C=======================================================================
C      IF(QINT.LE.0.0.AND.EXXP.EQ.0.0) THEN
C                                      DO 170 M   = 1,NQS
C  170                                 FLUX0(M,J) = FLUX(M)
C                                      RETURN
C                                      ENDIF
C      DO 150 M = 1,NQS
      WCH = C(M,J)
	TOP = 0.0
	IF(TWOV.GT.0.0) TOP = FLUXAV(M)*(1-GREMOVE(M,J))/TWOV*2.0
C=======================================================================
CXXX     If EXXP = 0, use instantaneous inflow concentration.
C=======================================================================
CXXX      IF(EXXP.EQ.0.0) C(M,J) = FLUX(M)/QINT
C=======================================================================
C     The following equation uses average quantities over the
C     time step to eliminate stability problems.
C
C     The equation integrated is:
C
C     V*dC/dt = Qin*Cin - Q*C - C*dV/dt - K*C*V + L
C
C     (See User's Manual, Appendix IX.)
Cwch, 2/10/01   Include decay, K = QDECAY,
Cwch, 2/10/01.  Allow BMP removal, L = -GREMOVE*Qin*Cin
C     The solution is:
C
C     C = TOP/DENOM [1 - EXP(-ARG)] + Co EXP(-ARG)
C
C     where
C
C     TOP = Qin*Cin*(1-GREMOVE)/V, mg/L*cfs / ft3 = mg/L / s
C     DENOM = Q/V + (1/V)dV/dt + K, cfs/ft3 = 1/s
C     ARG = DENOM*DELT = [Q/V + (1/V)dV/dt + K]*DELT
C
C     Here, we use average values for the time step.
C
C=======================================================================
      C(M,J) = WCH*EXXP
      IF(DENOM.GT.0.0) C(M,J) = TOP/DENOM*(1.0-EXXP) + C(M,J)
C=======================================================================
Cwch, 2/10/01.
C     Keep track of removals by BMP fraction and decay.
C     SUMREMG = mg/s * sec = mg.
C     Multiply by FACT3(ND) = 28.3 L/ft3 to convert mg/L*cfs to mg/s.
C=======================================================================
      SUMREMG(M,1) = SUMREMG(M,1) + GREMOVE(M,J)*FLUXAV(M)*
     1                              FACT3(ND)*DELT
      SUMREMG(M,2) = SUMREMG(M,2) + QDECAY(M)*(V0*WCH+V1*C(M,J))/2.0*
     1                              FACT3(ND)*DELT
C=======================================================================
C     Define FLUX0(M,J) for the next time step.
C=======================================================================
  150 FLUX0(M,J) = FLUX(M)
      RETURN
      END
