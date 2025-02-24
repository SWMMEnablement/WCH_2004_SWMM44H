```fortran 
	SUBROUTINE QUALSOLN(IP,QAVG,CQAVG,DVDT,VOL,DECAYY,ERODE,
     1 DEPOS,REMOVE,XLDECAY)
C=======================================================================
C	Routine to evaluate analytical solution to Transport pollutant
C       routing using complete mixing solutions.  
C     Called from Sub. QUAL.  Replaces code at end of QUAL.
C     Created 7/6/01 by Wayne Huber in order to provide linked DO-BOD-
C       NOD simulation. 
## Extensive Summary of the QUALSOLN Subroutine

This section provides an extensive overview of the Fortran subroutine `QUALSOLN` used to solve the transport of pollutants using a complete-mixing analytical solution. The summary explains the procedure, the parameters involved, and how the solution is applied to different water quality constituents.

---

### Overview

- **Purpose:**  
	The subroutine calculates the outflow concentration of various pollutants by integrating mass balance equations. It avoids numerical instability by using average values over the time step.

- **Context:**  
	The routine is called from the larger water quality simulation (referred to as Sub. QUAL) and is similar in approach to that used in Sub. SHEDQUAL. It handles both independent and linked constituent processes such as decay, removal, settling, and reaeration.

- **Method:**  
	The integration uses the equation:  
	C = C_new * [1 − exp(-ARG)] + C_old * exp(-ARG)  
	where:
	- C_new is derived from inflow loads and first-order decay.
	- C_old represents the concentration from the previous time step.
	- ARG is a product involving the decay coefficient and the time step.

---

### Main Sections and Calculations

1. **Initialization and Inclusion of Common Blocks:**
	 - Includes external files for common variables (`TAPES.INC`, `TRANWQ.INC`, `HUGO.INC`).
	 - Sets initial variables such as denominator (`DENOM`) and numerator (`TOP`).

2. **Parameter Setup for Transport:**
	 - **Denom Calculation:**  
		 The denominator is computed as `(QAVG + DVDT) / VOL` to adjust for flow fluctuations.
	 - **Average Conditions:**  
		 The routine computes current channel parameters including surface area, average depth, and velocity for use in linked pollutant evaluations.

3. **Pollutant-Specific Branching:**
	 - **Ordinary Constituents (General Case):**
		 - Adjusts `DENOM` by adding the decay coefficient.
		 - Applies additional terms for constituents not linked to upstream processes.
	 - **Special Cases:**
		 - **CBOD (Carbonaceous BOD):**  
			 Uses a specific decay transformation and adjusts for settling effects if applicable.
		 - **NBOD (Nitrogenous Oxygen Demand):**  
			 Similar treatment as for CBOD with its own decay constant.
		 - **NO3-N:**  
			 Involves an extra linkage term to account for decay from NOD, ensuring coupling between these constituents.
		 - **DO (Dissolved Oxygen):**  
			 Considers the oxygen deficit generated from BOD and NOD decay, and includes calculations for reaeration, sediment oxygen demand, and associated removal terms.

4. **Numerical Integration and Exponential Term:**
	 - **Exponential Decay Factor:**  
		 A term `EXXP` is computed as exp(-ARG) (or set to 1 when ARG equals zero). This factor modulates the blend between the new (incoming) and old (existing) concentration values.
	 - **Weighted Average:**  
		 The final concentration (`C2NEXT`) is computed by blending the incoming load (normalized by the adjusted decay and removal factors) with the previous concentration.

5. **Continuity and Mass Balance:**
	 - The routine tracks overall mass lost or transformed by individual processes:
		 - **Removal:**  
			 Mass fraction removed due to BMP (Best Management Practices).
		 - **Settling and Decay:**  
			 Quantifies mass removal due to settling and first-order decay.
		 - **Linked Processes:**  
			 Adjusts for secondary effects (e.g., NOD decay contributing to NO3-N, oxygen deficit contributions from both CBOD and NBOD).

6. **Handling Special Conditions:**
	 - The subroutine ensures that:
		 - Negative concentrations or denominators are corrected to avoid division errors.
		 - Special cases like evaporation (though not active in the current transport block) are addressed by checking for negative `DENOM`.

---

### Final Processing and Return

- **Output Update:**  
	Updated concentrations are stored in the common block `CPOL2`, where:
	- The first index represents the element.
	- The second index differentiates between start and end-of-step concentrations.
	- The third index corresponds to the pollutant type.
	
- **Mass Tracking:**  
	Auxiliary variables accumulate removal, settling, decay, and reaeration contributions over the time step to maintain mass balance in the simulation. These are essential for subsequent continuity checks and overall water quality assessments.

- **Return Statement:**  
	Once all calculations and updates are complete, the routine finishes with a return, passing control back to the calling subroutine.

---

### Summary

This subroutine is central to pollutant transport simulation. It:
- Calculates changes in pollutant concentration over each time step using complete-mixing assumptions.
- Integrates multiple processes such as decay, settling, reaeration, and BMP removal.
- Handles both independent and linked constituents for water quality modeling.
- Adjusts dynamically based on current flow and depth, ensuring that pollutant concentrations are updated accurately while conserving mass.

This detailed summary encapsulates the structure and purpose of the `QUALSOLN` subroutine, providing a foundational understanding for further modifications or documentation.

C     Correct for non-calc when there is zero flow and thus, zero
C       DENOM.  WCH, 8/6/03.
C=======================================================================
C	CAUTION!
C     THERE MAY BE SOME IDENTICAL NAMES USED IN ARGUMENT LIST AS IN 
C     TRANSPORT COMMON.  BE VERY CAREFUL IF LATER DECIDE TO INCLUDE  
C     VARIOUS LABELED COMMONS.
C     Caution. Most Transport Common blocks not accessed.
C=======================================================================
      INCLUDE 'TAPES.INC'
	INCLUDE 'TRANWQ.INC'
	INCLUDE 'HUGO.INC'
C=======================================================================
C
C     IP      = Constituent number.
C     M       = Element number (should be channel/pipe or element
C               with storage). (HUGO.INC)
C     QAVG    = Average outflow from element, cfs.
C               "Average"  means average of beginning and end of time
C               step.
C     CQAVG   = Average incoming load to element, cfs * mg/L.
C     DVDT    = dV/dt = change in volume per time step, cfs.
C	VOL     = Average volume of water in element, ft3.
C     DECAYY  = First-order decay coefficient, 1/sec.
C     DT      = Time step, sec. (HUGO.INC)
C     ERODE   = Load from erosion, cfs*mg/L.
C     DEPOS	= Deposition, cfs*mg/L.
C	REMOVE  = BMP removal fraction, affecting all loads.
C   CPOL2 values are in Common in HUGO.INC
C     CPOL2(M,1,IP)  = Downstream concentration at beginning of 
C               time step, mg/L.
C     CPOL2(M,2,IP)  = Downstream concentration at end of time 
C               step, mg/L.
C
C     XLREMOV = mg/L*ft3 removed by removal fraction.
C     XLDECAY = mg/L*ft3 removed by first-order decay.
C     XLSETL  = mg/L*ft3 removed by settling.
C=======================================================================
C     The following equation uses average quantities over the
C     time step to avoid stability problems.
C
C     The equation integrated is:
C
C     V*dC/dt = (Qin*Cin + Load)*(1 - Remove) - Q*C - C*dV/dt - Vs*As 
C         - K*C*V + [possible linkages among CBOD, NOD, NO3-N & DO]
C
C     (See User's Manual, Appendix IX.)
C
C     C       = outflow concentration, mg/L
C     Vs      = settling velocity, ft/s
C     As      = surface area, ft2
C     Q       = QAVG = outflow rate, cfs
C     Qin*Cin = CQAVG = incoming load, cfs*mg/L
C     Load    = other loads, cfs*mg/L
C     K       = DECAYY, 1/s
C
C     Note that evaporation does not remove mass.  Outflow by evaporation
C     is accounted for in (1/V)dV/dt term.  Hence, evaporation can increase
C     concentrations, especially when outflow (and volume or depth) are
C     very small. 
C     No evaporation in Transport Block, July 2001.  
C
C     Use average values for the time step in order to integrate 
C     equation with constant coefficients.  
C
C     C = Cnew [1 - EXP(-ARG)] + Cold EXP(-ARG) + [Possible additional
C         terms reflecting linked CBOD, NOD, NO3-N, DO]
C
C     where 
C     Cnew  = TOP/DENOM
C     ARG   = DENOM*DT,
C     TOP   = [Qin*Cin + Load]*(1-REMOVE)/V
C     DENOM = [Q/V + (1/V)dV/dt + Vs/D + K]
C     D     = Average depth, ft
C     Cold  = C at last time step.
C     Cnew, Cold and C have units of mg/ft3
C=======================================================================
C     Compute current values of channel parameters for routing.
C     For linked DO simulation, need surface area, average depth,
C     and velocity.
C=======================================================================
C     DENOM can be negative when there is evaporation.  
C     But no evaporation in Transport, as of July 2001.  Hence, should
C     not have negative DENOM. 
C=======================================================================
C     This subroutine is only called when VOL > 0.0.  Hence, there are 
C     no checks for zero divide with VOL. 
C=======================================================================
      DENOM = 0.0
	TOP   = 0.0
	DENOM = (QAVG + DVDT)/VOL
C=======================================================================
C     Determine several parameters used by constituents during
C     evaluation for IP = 1.
C=======================================================================
      IF(IP.EQ.1) CALL QUALPARM(ASURF,ASSETL,DBAR,DBARSETL,VELL,CS,WAVG)
      IF(NWQ.EQ.1.AND.IP.LE.4) THEN
         SELECT CASE (IP)
	      CASE (1)  ! CBOD
C=======================================================================
C     Here, simulate ultimate, carbonaceous BOD = CBOD.
C     Use same solution form as for other constituents -- no "upstream"
C     linkages.
C=======================================================================
            DECAYY  = DEKD(M)*THETA(1)**(TWATER(M)-20.0)
	      DECAYYD = DECAYY
            DENOM   = DENOM + DECAYY
	      IF(DBARSETL.GT.0.0) 
     1	       DENOM = DENOM + (1.0-FDIS(1))*VSETL(1)/DBARSETL
	      IF(DENOM.LT.0.0) DENOM = 0.0
	      DENBOD = DENOM
	      TOP = (CQAVG+ERODE-DEPOS)*(1.0-REMOVE)/VOL
            TOPBOD = TOP
            CASE (2)  ! NBOD
C=======================================================================
C     Here, simulate nitrogenous oxygen demand = NOD.
C     This may be simulated as TKN or NH3-N, but will be called NOD
C     in output. 
C     Use same solution form as for other constituents -- no "upstream" 
C     linkages.
C=======================================================================
            DECAYY  = DEKN(M)*THETA(2)**(TWATER(M)-20.0)
	      DECAYYN = DECAYY
            DENOM   = DENOM + DECAYY
	      IF(DBARSETL.GT.0.0) DENOM = DENOM + 
     1          (1.0-FDIS(2))*VSETL(2)/DBARSETL
	      IF(DENOM.LT.0.0) DENOM = 0.0
	      DENNOD = DENOM
	      TOP = (CQAVG+ERODE-DEPOS)*(1.0-REMOVE)/VOL
            TOPNOD = TOP
            CASE (3)  ! NO3-N
C=======================================================================
C     Here, simulate NO3-N.
C     This is linked to decay of NOD, so must evaluate fully linked
C     integral solution.
C=======================================================================
            DENOM  = DENOM + DECAYY
	      IF(DBARSETL.GT.0.0) DENOM = DENOM + 
     1          (1.0-FDIS(3))*VSETL(3)/DBARSETL
	      IF(DENOM.LT.0.0) DENOM = 0.0
	      TOP = (CQAVG+ERODE-DEPOS)*(1.0-REMOVE)/VOL
	      CASE (4)  ! DO
C=======================================================================
C     Here, simulate dissolve oxygen deficit DEFICIT = Csat - DO.
C     This is linked to decay of BOD and NOD, so must evaluate fully
C     linked integral solution.
C     Calculate the reaeration coefficient = decay coef. for DEFICIT.
C     Units of TOP are mg/L * ft3/s.  Units of SOD are mg/ft2-sec.
C     For same units for settling load, divide by average depth
C     (ft) and divide by 28.3 L/ft3. 
C=======================================================================
           CALL REAERATE(DECAY2,DBAR,VELL,WIND(M),TAIR(M),TWATER(M),REA,
     1THETA(3),KWIND,KOVAR,GNUW,GNUA,RHOW,RHOA,DMOLEC,XLAMBDA,ZEE,CAPPA,
     2 GAMMAZ,UTSTAR,PRESURE,UCSTAR,CAPPA3,CDRAG,IYZ,DELG1,ZZERO,USTAR,
     3 CAYEL,DECAY2W,SALINITY,G1,ZAA,N6,DECAY2F)
	      DECAYY = DECAY2
	      DENOM  = DENOM + DECAY2
	      SEDMD  = 0.0
	      IF(DBARSETL.GT.0.0) SEDMD = SOD(M)/DBARSETL/28.3
C=======================================================================
C     Do not include ERODE and DEPOS for DEFICIT, for now.
C     Do not include REMOVE for DEFICIT.
C     Assume upstream and incoming deficit load is QAVG*CS - CQAVG
C     where CQAVG is "load" of incoming DO.
C=======================================================================
            DEFLOAD = QAVG*CS - CQAVG
	      IF(DEFLOAD.LT.0.0) DEFLOAD = 0.0
	      TOP = DEFLOAD/VOL + SEDMD
	      END SELECT
C=======================================================================
	   ELSE
C=======================================================================
C     Here for "ordinary" constituent.
C=======================================================================
         DENOM = DENOM + DECAYY
	   IF(DBARSETL.GT.0.0) DENOM = DENOM + 
     1          (1.0-FDIS(IP))*VSETL(IP)/DBARSETL
	   IF(DENOM.LT.0.0) DENOM = 0.0
	   TOP = (CQAVG+ERODE-DEPOS)*(1.0-REMOVE)/VOL
         ENDIF
C=======================================================================
C     EXXP => 0 ==> Big ARG, big Qin/V, concentration => Cin.
C     EXXP >  0 ==> Smaller ARG, smaller Qin/V, and
C      concentration between Cin and Co.  
C     EXXP => 1 ==> ARG => 0, Qin/V => 0, concentration => Co.
C     EXXP > 1.0 for negative DENOM (increase concentration because
C      of evaporation).  But no evap in Transport yet.
C=======================================================================
                      EXXP = 0.0
				    ARG  = DENOM*DT
	IF(ARG.EQ.0.0)  EXPP = 1.0
      IF(ARG.LT.10.0.AND.ARG.GT.0.0) EXXP = EXP(-ARG)
C=======================================================================
C     For all cases except DO, first part of solution is the same.
C=======================================================================
      C2NEXT = 0.0
	CZERO  = CPOL2(M,1,IP)
	IF(NWQ.EQ.1.AND.IP.EQ.4) CZERO = CS - CPOL2(M,1,4)
	IF(CZERO.LT.0.0) CZERO  = 0.0
	IF(DENOM.NE.0.0) C2NEXT = TOP/DENOM*(1.0-EXXP) + CZERO*EXXP
Cwch, 8/6/03
      IF(DENOM.EQ.0.0) C2NEXT = CZERO
C=======================================================================
      IF(NWQ.EQ.1.AND.IP.LE.4) THEN
	   SELECT CASE (IP)
	     CASE (1)  ! CBOD
	     EXPBOD = EXXP
	     CPOL2(M,2,1) = C2NEXT
	     CASE (2)  ! NBOD
	     EXPNOD = EXXP
	     CPOL2(M,2,2) = C2NEXT
	     CASE (3)  ! NO3-N
C=======================================================================
C     Add linkage to NOD decay to NO3-N.
C=======================================================================
           PART2 = 0.0
	     PART3 = 0.0
	     IF(DENOM.NE.0.0.AND.DENNOD.NE.0.0) THEN
		    PART2 = DECAYYN*TOPNOD/DENOM/DENNOD*(1.0 - EXXP)
	        IF(ABS(DENOM-DENNOD).GT.0.0001) THEN
	           PART3 = DECAYYN/(DENOM-DENNOD)*
     1     	   (CPOL2(M,1,2)-TOPNOD/DENNOD)*(EXPNOD-EXXP)
	           ELSE
C=======================================================================
C     Use solution for lim->(DENOM->DENNOD)
C=======================================================================
                 PART3 = DECAYYN*(CPOL2(M,1,2)-TOPNOD/DENNOD)*(-DT*EXXP)
	           ENDIF
              ENDIF
	     CPOL2(M,2,3) = C2NEXT + PART2 + PART3
           CASE (4)  ! DO	
C=======================================================================
C     Add linkage to NOD and BOD decay to DEFICIT.  First, BOD.
C=======================================================================
           PART4 = 0.0
	     PART5 = 0.0
	     IF(DENOM.NE.0.0.AND.DENBOD.NE.0.0) THEN
		    PART4 = DECAYYD*TOPBOD/DENOM/DENBOD*(1.0 - EXXP)
	        IF(ABS(DENOM-DENBOD).GT.0.0001) THEN
	           PART5 = DECAYYD/(DENOM-DENBOD)*
     1              (CPOL2(M,1,1)-TOPBOD/DENBOD)*(EXPBOD-EXXP)
	           ELSE
C=======================================================================
C     Use solution for lim->(DENOM->DENBOD)
C=======================================================================
                 PART5 = DECAYYD*(CPOL2(M,1,1)-TOPBOD/DENBOD)*(-DT*EXXP)
	           ENDIF
              ENDIF
C=======================================================================
C     Similarly for NOD, but need to multiply by 
C     stoichiometric constant, TKN to NO3-N, 64/14 = 4.57.
C=======================================================================
           PART6 = 0.0
	     PART7 = 0.0
	     IF(DENOM.NE.0.0.AND.DENNOD.NE.0.0) THEN
		    PART6 = DECAYYN*TOPNOD/DENOM/DENNOD*(1.0 - EXXP)
	        IF(ABS(DENOM-DENNOD).GT.0.0001) THEN
	           PART7 = DECAYYN/(DENOM-DENNOD)*
     1              (CPOL2(M,1,2)-TOPNOD/DENNOD)*(EXPNOD-EXXP)
	           ELSE
C=======================================================================
C     Use solution for lim->(DENOM->DENNOD)
C=======================================================================
                 PART7 = DECAYYN*(CPOL2(M,1,2)-TOPNOD/DENNOD)*(-DT*EXXP)
	           ENDIF
              ENDIF
	     CPOL2(M,2,4) = CS - (C2NEXT + PART4 + PART5 
     1                    + 4.57*(PART6 + PART7))
           IF(CPOL2(M,2,4).LT.0.0) CPOL2(M,2,4) = 0.0
           END SELECT
C=======================================================================
	   ELSE
C=======================================================================
C     For "ordinary" pollutants, just use C2NEXT previously computed. 
C=======================================================================
	   CPOL2(M,2,IP) = C2NEXT
	   ENDIF
C=======================================================================
C     For continuity, try to keep track of losses (mg/L * ft3/s)*sec =
C     mg/L * ft3.  Processes include:
C       -removal fraction (XLREMOV)
C       -settling (XLSETL)
C       -decay (XLDECAY) -- sum in Sub. QUAL
C       -NOD decay to NO3-N (XLNO3SRC)
C       -reaeration (XLREAER)
C       -sediment oxygen demand (XLSEDOD)
C       -For DO: deficit source from CBOD (XLBOD2DO)
C                deficit source from NOD  (XLNOD2DO)
C=======================================================================
      XLREMOV(IP) = XLREMOV(IP) + REMOVE*(CQAVG + ERODE - DEPOS)*DT	
	CAVG        = (CPOL2(M,1,IP)+CPOL2(M,2,IP))/2.0
	XLSETL(IP)  = XLSETL(IP) + ASSETL*(1-FDIS(IP))*VSETL(IP)*CAVG*DT	
      IF(NWQ.EQ.1.AND.(IP.EQ.3.OR.IP.EQ.4)) THEN
	   SELECT CASE (IP)
	     CASE (3)  ! NO3-N
C=======================================================================
C     For NO3-N, keep track of mass added by NOD decay.
C=======================================================================
           XLNO3SRC = XLNO3SRC 
     1       + DECAYYN*VOL*DT*(CPOL2(M,1,2)+CPOL2(M,2,2))/2.0
           XLDECAY  = DECAYY*VOL*CAVG*DT
	     CASE (4)  ! DO
C=======================================================================
C     For DO continuity check, calculate mass of DO from reaeration by
C     knowing deficits.  Use current Cs for old and current time step. 
C     Also need deficit source from BOD, NOD, and sediment OD. 
C=======================================================================
	                        DEFICIT = CS-CAVG
	     IF(DEFICIT.LT.0.0) DEFICIT = 0.0
	     XLREAER  = XLREAER + DECAY2*VOL*DEFICIT*DT
	     XLSEDOD  = XLSEDOD + VOL*SEDMD*DT
	     XLBOD2DO = XLBOD2DO
     1       + DECAYYD*VOL*DT*(CPOL2(M,1,1)+CPOL2(M,2,1))/2.0
	     XLNOD2DO = XLNOD2DO
     1       + DECAYYN*VOL*DT*(CPOL2(M,1,2)+CPOL2(M,2,2))/2.0*4.57
           END SELECT
	     ELSE
           XLDECAY = DECAYY*VOL*CAVG*DT
	     ENDIF
C=======================================================================
      RETURN
	END
``` 
