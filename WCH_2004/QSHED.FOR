      SUBROUTINE QSHED
C	RUNOFF BLOCK
C	CALLED BY SUBROUTINE HYDRO NEAR LINE 357
C=======================================================================
C     Enter QSHED for time-step water quality calculations.
C=======================================================================
C     Last updated in December, 1992 by WCH for typo at statement 620.
C     Updated by WCH, 9/3/93 to include quality load from
C       infiltration/inflow.
C     Updated by WCH (from RED), 9/23/93 to correct subscript order
C       in WSNOW.
C     Add error check for negative flow and correct GO TO, WCH, 9/29/93.
C     Use DMEAN for multiplication for loads, consistent with summations
C       for flow volumes, WCH, 11/30/93.
C     Major changes to account for subcatchment to subcatchment flows,
C       WCH, 12/20/00.
C     Include summers, SUMNL(J,K,#), for each land use.  Waste a 
C       subscript or two to be consistent with SUM(K,#) numbering 
C       scheme. Include SUMNL in QUALTY.INC.  WCH, 12/20/00. 
C     For erosion, use rainfall for appropriate subcat, suggested by 
C       Ching L. Chen.  WCH, 5/8/01. 
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'QUALTY.INC'
C#### WCH, 9/93
      INCLUDE 'RDII.INC'
Cwch, 12/20/00
      INCLUDE 'OVERLAND.INC'
      DOUBLE PRECISION ATTEN,POW,DTHR2,RP
C=======================================================================
C     Initialize several parameters.
C     If have erosion, perform buildup-washoff for NQS-1 constituents.
C     NQSS = NQS - 1 ---> This subtraction was performed in QHYDRO.
C=======================================================================
      DTHR2 = DELT/7200.0
      DELT2 = DELT/2.0
C#### WCH, 9/29/93.  INITIALIZE ERROR PRINT COUNTER.
      IF(MTIME.EQ.1) KERR = 0
C=======================================================================
C     Time step calculations for each subcatchment.
C=======================================================================
      DO 1000 N     = 1,NOW
C=======================================================================
C     Compute instantaneous runoff rate for total subcatchment (in/hr).
C=======================================================================
Cwch, 12/20/00. Move RUN2 calc outside loop on land uses. 
C=======================================================================
      IF(WAREA(N).NE.0.0) THEN
           RUN2 = WFLOW(N)/WAREA(N)*43200.0
           ELSE
           RUN2 = 0.0
           ENDIF
Cwch, 12/20/00.  Can't allow negative rainfall inflow (rain - losses)
C     for purposes of inputting rainfall load.
      IF(RFLOW(N).LT.0.0) RFLOW(N) = 0.0
C=======================================================================
C     Begin inner loop on number of quality land uses, N1. 
C=======================================================================
      DO 1000 JJ    = 1,N1
      IF(N1.EQ.1) J = KLAND(N)
      IF(N1.GT.1) J = JJ
      LAH = 0
C=======================================================================
C     If runoff rate is small, perform buildup.
C     Note, 0.000005 in/hr = 1.16E-10 ft/s
C=======================================================================
      IF(RUN2.LT.0.000005) GO TO 510
C=======================================================================
C     Perform buildup if this is first 'WET' timestep.
C=======================================================================
      IF(TBUILD(J,N).LE.0.0) GO TO 600
      LAH = 1
      GO TO 530
C=======================================================================
C     TBUILD = time since last runoff or sweeping.
C=======================================================================
  510 TBUILD(J,N) = TBUILD(J,N) + DELT
C=======================================================================
C     Here, check for sweeping, etc.
C     JOULE = Julian day of year.
C     No sweeping if wrong dates or if snow is present on imperv area.
C=======================================================================
      JOULE = JULDAY - NYEAR*1000
      IF(JOULE-KLNBGN.LT.0)           GO TO 560
      IF(JOULE-KLNBGN.EQ.0)           GO TO 530
      IF(JOULE-KLNBGN.GT.0) THEN
               IF(JOULE.GE.KLNEND)    GO TO 560
C#### WCH (RED), 9/93. TRANSPOSE SUBSCRIPTS IN WSNOW.
               IF(WSNOW(3,N).GT.0.0)  GO TO 560
C=======================================================================
C     Increment street sweeping time and check to see if should sweep.
C=======================================================================
               TCLEAN(J,N) = TCLEAN(J,N) + DELT
               IF(TCLEAN(J,N).LT.CLFREQ(J)) GO TO 560
               ENDIF
C=======================================================================
C     Time to sweep.  First perform buildup for each constituent.
C     Buildup for some constituents only if snow is present.
C=======================================================================
  530 DO 550 K = 1,NQSS
      IF(KALC(J,K).EQ.4) GO TO 550
      IF(KALC(J,K).EQ.0) KC = METHOD(J)
      IF(KALC(J,K).GT.0) KC = KALC(J,K) - 1
      OPS      = PSHED(J,K,N)
      IF(LINKUP(J,K).EQ.0) GO TO 540
C#### WCH (RED), 9/93.  TRANSPOSE SUBSCRIPTS IN WSNOW.
      IF(WSNOW(1,N).LE.0.0.AND.WSNOW(3,N).LE.0.0) GO TO 550
  540 CALL BUILD(N,K,1,KC,CULIM(J,K,N),CPOW(J,K,N),CCOEF(J,K,N),
     1                TBUILD(J,N),PSHED(J,K,N))
      SUM(K,2) = SUM(K,2) + (PSHED(J,K,N) - OPS)
Cwch, 12/20/00
	SUMNL(J,K,2) = SUMNL(J,K,2) + (PSHED(J,K,N) - OPS)
C=======================================================================
C     Regenerate catchbasin loading.
C=======================================================================
      IF(BASINS(N).GT.0.0) THEN
                           OPB = PBASIN(K,N)
                           PBASIN(K,N) = PBASIN(K,N) + TBUILD(J,N)
     1                                    * (PPBASN(K,N)/DRYBSN)
                           IF(PBASIN(K,N).GT.PPBASN(K,N))
     1                                       PBASIN(K,N) = PPBASN(K,N)
                           SUM(K,4) = SUM(K,4) + (PBASIN(K,N) - OPB)
Cwch, 12/20/00
						 SUMNL(J,K,4) = SUMNL(J,K,4)+(PBASIN(K,N)-OPB)
                           ENDIF
  545 CONTINUE
  550 CONTINUE
      TBUILD(J,N) = 0.0
C=======================================================================
C     Check to see if only needed to buildup in order to
C     wash off, not to sweep.
C=======================================================================
      IF(LAH.EQ.1) GO TO 600
C=======================================================================
C     Now, finally sweep streets.
Cwch, 12/20/00. Rename REMOVE to REMOV.
C=======================================================================
      DO 555 K     = 1,NQSS
      REMOV        = AVSWP(J)     * REFF(J,K) * PSHED(J,K,N)
      SUM(K,5)     = SUM(K,5)     + REMOV
Cwch, 12/20/00
      SUMNL(J,K,5) = SUMNL(J,K,5) + REMOV
  555 PSHED(J,K,N) = PSHED(J,K,N) - REMOV
      TCLEAN(J,N)  = 0.0
C=======================================================================
C     No washoff during dry weather.
Cwch, 12/20/00 Also reset subcat to subcat loads.
C=======================================================================
  560 DO 570 K    = 1,NQS
          IF(JJ.EQ.1) THEN
		POFF2C0(K,N) = 0.0
	    POFF2C1(K,N) = 0.0	
		ENDIF	
  570 POFF(J,K,N) = 0.0
Cwch, 12/20/00
      IF(ISC2SC.EQ.1) GO TO 960
      GO TO 1000
C=======================================================================
C     PERFORM WASHOFF CALCULATIONS.  ULTIMATELY, CALC LOAD RATE
C        (E.G.,MG/SEC) OF EACH CONSTITUENT WASHED OFF.
C     Total of three options:
C        KWASH = 0, modified exponential washoff.
C              = 1, rating curve, no limit.
C              = 2, rating curve with limit from buildup equation.
C=======================================================================
  600 IF(RUN2.LT.0.000005) GO TO 560
      DO 800 K    = 1,NQSS
      TEMPLD(J,K) = 0.0
      IF(KWASH(J,K).GT.0) GO TO 620
C=======================================================================
C     Here, modified exponential washoff.
C=======================================================================
                             RP = RUN2
      IF(WASHPO(J,K).NE.1.0) RP = RUN2**WASHPO(J,K)
C=======================================================================
C     Use trapezoidal rule for incremental integration within exponent.
C=======================================================================
      POW          = RCOEF(J,K)*DTHR2*(RP+OLDQP(J,K,N))
      OLDQP(J,K,N) = RP
C=======================================================================
C     Compute modified exponential decay.
C=======================================================================
                     ATTEN = 0.0
      IF(POW.LT.35.) ATTEN = DEXP(-POW)
C=======================================================================
C     Calc washoff load rate (e.g., mg/sec, mpn/sec).
C=======================================================================
      TEMPLD(J,K) = RCOEFX(J,K)*RP*PSHED(J,K,N)*ATTEN
C=======================================================================
C     Decrease constituent surface load (E.G, mg, mpn)
C=======================================================================
      DPSHED = TEMPLD(J,K) * DELT
C=======================================================================
C     Here, have attempted to remove more than PSHED.
C=======================================================================
      IF(DPSHED.GT.PSHED(J,K,N))  THEN
                                  TEMPLD(J,K)  = PSHED(J,K,N)/DELT
                                  DPSHED       = PSHED(J,K,N)
                                  ENDIF
                                  PSHED(J,K,N) = PSHED(J,K,N) - DPSHED
      IF(PSHED(J,K,N).LT.1.0E-25) PSHED(J,K,N) = 0.0
      GO TO 700
C=======================================================================
C     Here, compute washoff load rate (e.g., mg/sec, mpn/sec)
C     using a rating curve with no upper limit, or a rating
C     curve with upper limit given by buildup equation.
C=======================================================================
C#######################################################################
C     WCH, 9/29/93.  ERROR CHECK.
C#######################################################################
  620 SCFLOW  =  PLAND(J,N)*WFLOW(N)
      IF(SCFLOW.LT.0.0) THEN
           IF(KERR.LE.200) THEN
                WRITE(N6,9000) TIME,J,K,N,PLAND(J,N),WFLOW(N),SCFLOW
                KERR = KERR + 1
                ELSE
                IF(KERR.EQ.201) WRITE(N6,9001)
                KERR = KERR + 1
                ENDIF
           ENDIF
C#######################################################################
C#### WCH, 12/3/92.  Change to "EQ.1"
      IF(KWASH(J,K).EQ.1) THEN
C####                  SCFLOW      = PLAND(J,N)*WFLOW(N)
                    TEMPLD(J,K) = RCOEF(J,K)*SCFLOW**WASHPO(J,K)
                    ELSE
C=======================================================================
C     Here if KWASH = 2
C=======================================================================
C#### WCH, 9/29/93.  SHOULD BE GO TO 700, NOT 800.
                    IF(PSHED(J,K,N).LE.0.0) GO TO 700
C####                    SCFLOW      = PLAND(J,N)*WFLOW(N)
                    TEMPLD(J,K) = RCOEF(J,K)*SCFLOW**WASHPO(J,K)
                    PP          = (OLDQP(J,K,N) + TEMPLD(J,K))*DELT2
                    IF(PP.GE.PSHED(J,K,N)) THEN
                         TEMPLD(J,K) = PSHED(J,K,N)/DELT2 - OLDQP(J,K,N)
                         IF(TEMPLD(J,K).LT.0.0) TEMPLD(J,K)  = 0.0
                                                PSHED(J,K,N) = 0.0
                         ELSE
                         PSHED(J,K,N) = PSHED(J,K,N) - PP
                         ENDIF
                    OLDQP(J,K,N)      = TEMPLD(J,K)
                    ENDIF
C#### WCH, 11/30/93.  MULT BY DMEAN, NOT DELT.
  700 SUM(K,6) = SUM(K,6) + TEMPLD(J,K)*DMEAN
Cwch, 12/20/00
	SUMNL(J,K,6) = SUMNL(J,K,6) + TEMPLD(J,K)*DMEAN
C=======================================================================
C     Compute catchbasin contribution.
C=======================================================================
      IF(BASINS(N).GT.0.0) THEN
         PP          = PBASIN(K,N) * PLAND(J,N) * WFLOW(N) / BASINS(N)
         PBASIN(K,N) = PBASIN(K,N) - PP * DELT
         IF(PBASIN(K,N).LT.0.0) THEN
                                PP          = PBASIN(K,N) / DELT + PP
                                PBASIN(K,N) = 0.0
                                ENDIF
C#### WCH, 11/30/93.  MULT BY DMEAN, NOT DELT.
         SUM(K,7)     = SUM(K,7)     + PP*DMEAN
Cwch, 12/20/00
	   SUMNL(J,K,7) = SUMNL(J,K,7) + PP*DMEAN
         TEMPLD(J,K)  = TEMPLD(J,K) + PP
         ENDIF
  800 CONTINUE
C=======================================================================
C     Simulate erosion by the universal soil loss equation.
C     Erosion index by method of Wischemier and Smith.
C=======================================================================
      IF(IROS.EQ.1) THEN
                    TEMPLD(J,NQS) = 0.0
                    IF(CNSTNT(N).LE.0.0) GO TO 900
Cwch, 5/8/01. Use rainfall plus melt for this subcatchment, in case of 
C     multiple gages, not rain from last subcat in WSHED.  Use RFLOW, 
C     rainfall in cfs, inserted for subcat to subcat flows.  Divide by 
C     area and convert to in./hr. 
Cwch                    RNINHR = RINE*43200.0
                    RNINHR = RFLOW(N)/WAREA(N)*43200.0  
                    IF(RNINHR.LT.0.01) GO TO 900
                    Y = (9.16+3.31*ALOG10(RNINHR))*RNINHR/3600.0
                    TEMPLD(J,NQS) = Y*RAINIT*CNSTNT(N)
C#### WCH, 11/30/93.  MULT BY DMEAN, NOT DELT.
                    SUM(NQS,6)    = SUM(NQS,6) + TEMPLD(J,NQS)*DMEAN
Cwch, 12/20/00
			 SUMNL(J,NQS,6) = SUMNL(J,NQS,6) + TEMPLD(J,NQS)*DMEAN
                    IF(IROSAD.GT.0) TEMPLD(J,IROSAD) = TEMPLD(J,IROSAD)
     +                                               + TEMPLD(J,NQS)
                    ENDIF
C=======================================================================
  900 DO 950 K    = 1,NQS
      POFF(J,K,N) = TEMPLD(J,K)
      ND          = NDIM(K) + 1
C=======================================================================
C     Add fractions from other constituents.
C=======================================================================
      DO 930 L    = 1,NQS
      IF(L.NE.K) THEN
                 PP          = TEMPLD(J,L) * F1(K,L)
                 POFF(J,K,N) = POFF(J,K,N) + PP
C#### WCH, 11/30/93.  MULT BY DMEAN, NOT DELT.
                 SUM(K,8)     = SUM(K,8)     + PP*DMEAN
Cwch, 12/20/00
			   SUMNL(J,K,8) = SUMNL(J,K,8) + PP*DMEAN
                 ENDIF
  930 CONTINUE
C=======================================================================
C     Add precipitation load = Flow*concentration.
C     CONCRN                 = Precip concentration*conversion factor.
Cwch, 12/20/00. If have subcat to subcat flows, add rain load later,
C     for subcatchments receiving upstream flows.
C     Here use WFLOW, not RFLOW (rainfall inflow) because this load
C     is load that *leaves* the subcatchment and is like washoff.  
C     Otherwise, would have to do mass balance with volume of water
C     on subcatchment.  Do this later only for surface inflow from
C     another subcatchment.  
C=======================================================================
		IF(NWTOW(1,N).EQ.0) THEN
		     PP          = PLAND(J,N)*WFLOW(N)*CONCRN(J,K)*FACT3(ND)
		     POFF(J,K,N) = POFF(J,K,N) + PP
C#### WCH, 11/30/93.  MULT BY DMEAN, NOT DELT.
Cwch, 12/20/00. Don't need 940 line number on SUM(K,9) line.
		     SUM(K,9)     = SUM(K,9)     + PP*DMEAN
Cwch, 12/20/00 
			 SUMNL(J,K,9) = SUMNL(J,K,9) + PP*DMEAN
	         ENDIF
C#######################################################################
C     WCH, 9/93.  Add contribution from infiltration/inflow.
C     CONCII = constant concentration in I/I.
C=======================================================================
      IF(RRMAX.GT.0.0.AND.JJ.EQ.1) THEN
           IF(FLOWII(N).LE.0.0) GO TO 950
           PP          = FLOWII(N)*CONCII(K)*FACT3(ND)
           POFF(J,K,N) = POFF(J,K,N) + PP
C#### WCH, 11/30/93.  MULT BY DMEAN, NOT DELT.
           SUMRDII(K)  = SUMRDII(K) + PP*DMEAN
           ENDIF
  950 CONTINUE
C
	IF(ISC2SC.EQ.0)     GOTO 1000
  960	IF(NWTOW(1,N).EQ.0) GOTO 1000
C=======================================================================
Cwch, 12/20/00.  To account for upstream inflow, must do mass balance
C     on inflowing mass and mass stored in subcat water volume.  Sigh.
C=======================================================================
C     Use similar scheme to that of GQUAL for channel/pipes.
C     Must calculate new subcatchment water volume.
C     Then, do calcs in new Subroutine SHEDQUAL
C=======================================================================
	IF(JJ.EQ.1) THEN
		VOL1 = 0.0
		DO LL = 1,4
			VOL1 = VOL1 + WDEPTH(LL,N)*WAR(LL,N)
			ENDDO
		ENDIF
	DO K = 1,NQS
          ND = NDIM(K) + 1
	    IF(VOL1.GT.0.0) THEN
	      CALL SHEDQUAL(VOL1,WVOL0(N),WFLOW(N),WFLOW0(N),RFLOW(N),
     1	QDECAY(K),REMOVE(J,K),DELT,POFF2C1(K,N),POFF2C0(K,N),
     2	ENFILW(N),CONCRN(J,K)*FACT3(ND),COFF(J,K,N),XLINFIL,XLREMOV,
     3	XLDECAY)
C=======================================================================
C     Keep track of losses by infiltration, removal fraction, and decay.
C     SUMREM(1) = mg lost by infiltration.
C     SUMREM(2) = mg lost by removal fraction.
C     SUMREM(3) = mg lost by first-order decay.
C     Also must sum rainfall load, SUM(K,9).
C     Losses are averages over the time step, so multiply by DELT, not
C     DMEAN.  
C     Add routed load to generated load and save old value.
C     Convert concentration to flux by multiplying by instanteous flow.
C     Here, apportion over each land use by PLAND.
C     Save for each land use even though only differences will be
C     by fraction area and by different REMOVE and CONCRN.  
C=======================================================================
		  PP = PLAND(J,N)*RFLOW(N)*CONCRN(J,K)*FACT3(ND)
	      SUM(K,9)      = SUM(K,9)     + PP*DELT
	      SUMNL(J,K,9)  = SUMNL(J,K,9) + PP*DELT
	      SUMREM(J,K,1) = SUMREM(J,K,1) + 
     1                      XLINFIL*DELT*PLAND(J,N)
		  SUMREM(J,K,2) = SUMREM(J,K,2) + 
     1                      XLREMOV*DELT*PLAND(J,N)
	      SUMREM(J,K,3) = SUMREM(J,K,3) + 
     1                      XLDECAY*DELT*PLAND(J,N)
		POFF(J,K,N)   = POFF(J,K,N) + COFF(J,K,N)*WFLOW(N)*PLAND(J,N)
C
	      ELSEIF (WVOL0(N).GT.0.0) THEN
C=======================================================================
C	If subcatchment is just dry, need to account for remaining 
C     pollutants.  Add to summations done in Sub. HYDRO at last time 
C     step.  Assume mass left in old volume is what remains.
C=======================================================================
	         SUMWAT(K) = SUMWAT(K) + WVOL0(N)*PLAND(J,N)*COFF(J,K,N)
               SUMNL(J,K,11) = SUMNL(J,K,11) + 
     1                      WVOL0(N)*PLAND(J,N)*COFF(J,K,N)
               COFF(J,K,N) = 0.0	
            ENDIF				  
	    ENDDO
C=======================================================================
C     Save outflow and volume for old values for next time step.
C=======================================================================
	IF(JJ.EQ.N1) THEN
		WFLOW0(N) = WFLOW(N)
		WVOL0(N)  = VOL1
	    ENDIF
C=======================================================================
C     End loop on subcatchments and land uses. 
C=======================================================================
 1000 CONTINUE
C=======================================================================
Cwch, 12/20/00. Save desired subcatch outflow concentrations for 
C     printing.  Have already saved flows in Sub. WSHED.
C     Need to do this only for subcats that flow to other subcats but
C     array will allow for any to be saved. 
C=======================================================================
      IF(MSUBC.GT.0) THEN
          IF(JSTART(1).GT.0) THEN
              DO 1600 L = 1,NDET
              IF(JULDAY.GE.JSTART(L).AND.JULDAY.LE.JSTOP(L)) GO TO 1605
 1600         CONTINUE
              GO TO 1650
 1605         CONTINUE
              ENDIF
C=======================================================================
C     Need to compute flow-weighted average concentration over 
C     land uses.
C     Array POFF2C0(MQUAL,NW) is reset at each time step near beginning
C     of this subroutine.  Use it as dummy array to hold concentrations.
C=======================================================================
           DO 1620 M = 1,MSUBC
	     N = ISUBC(M)
      	 DO 1620 K = 1,NQSS
               ND = NDIM(K) + 1
	         POFF2C0(K,M) = 0.0
		     DO 1610 JJ = 1,N1
		         IF(N1.EQ.1) J = KLAND(N)
		         IF(N1.GT.1) J = JJ
 1610              POFF2C0(K,M) = POFF2C0(K,M) + POFF(J,K,N)
C=======================================================================
C     POFF has units of mg/s.  Divide by WFLOW (cfs) to get mg/ft3.
C     Divide also by FACT3(ND), usually = 28.3 L/ft3, to get mg/L. 
C=======================================================================
	         IF(WFLOW(N).GT.0.0) THEN
			    POFF2C0(K,M) = POFF2C0(K,M)/WFLOW(N)/FACT3(ND)
				ELSE
				POFF2C0(K,M) = 0.0
				ENDIF  
 1620          CONTINUE				    
		 WRITE(NSCRAT(9),ERR=1777) ((POFF2C0(K,M),K=1,NQS),M=1,MSUBC)
 1650      CONTINUE
           ENDIF
C=======================================================================
Cwch, 12/20/00. Determine if subcatchment receives flow from upstream.  
C     If so, save inflow loads at previous time step (POFF2C0) and 
C     compute inflow loads at this time step (POFF2C1). 
C     Sum over all land uses.  Need to do this here instead of at 
C     beginning of QSHED since POFF gets altered in Sub. GUTTER.  Note
C     that POFF2C0 and POFF2C1 will be zero during first time step.  
C=======================================================================
	IF(ISC2SC.EQ.1) THEN
		DO 1700 N = 1,NOW
	        IF(NWTOW(1,N).EQ.0) GOTO 1700
	        DO JJ = 1,NQSS
			     POFF2C0(JJ,N) = POFF2C1(JJ,N)
			     POFF2C1(JJ,N) = 0.0
			     ENDDO
			DO 1670 LL = 1,NCP
			IF(NWTOW(LL,N).EQ.0) GOTO 1700
			NWW = NWTOW(LL,N)
			DO 1670 J = 1,NQSS
				DO 1670 K = 1,JLAND
 1670	 			POFF2C1(J,N) = POFF2C1(J,N) + POFF(K,J,NWW)
 1700			CONTINUE
		ENDIF
C=======================================================================
      RETURN
C=======================================================================
 1777 WRITE(N6,9777)
      STOP ' Error in QSHED writing subcat quality to NSCRAT(9).'
C=======================================================================
C#### WCH, 9/29/93.
 9000 FORMAT(' $$$$$ WARNING!! SERIOUS ERROR FROM QSHED. SHOULD NOT HAVE
     1 PLAND OR WFLOW < 0. ON-SCREEN ERROR 1230 LIKELY. RUN CONTINUES.',
     2/, ' TIME=',F7.1,' SEC. J,K,N =',3I4,' PLAND(J,N),WFLOW(N),SCFLOW
     3=',3E14.6)
 9001 FORMAT (' $$$$$ LIMIT OF 200 NEGATIVE FLOW ERROR MESSAGES FROM QSH
     1ED REACHED.  NO MORE PRINTED.')
 9777 FORMAT(/,' ===> ERROR !!  WRITING SUBCATCHMENT QUALITY DATA ON THE
     + NSCRAT(9) FILE IN SUB. QSHED.')
      END
