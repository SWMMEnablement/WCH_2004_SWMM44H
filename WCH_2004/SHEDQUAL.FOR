	SUBROUTINE SHEDQUAL(VOL1,VOL0,WFLOW,WFLOW0,QRAIN,
     1 DECAY,REMOVE,DELT,POFF2C1,POFF2C0,ENFILW,CRAIN,COFF,
     2 XLINFIL,XLREMOV,XLDECAY)
C=======================================================================
C     Routine to perform quality routing through a subcatchment.
C     Written by WCH, 12/20/00.
C     Called from Sub. QSHED, near end.
C     Perform routing similar to Sub. GQUAL for channel/pipes.  
C     See SWMM User's Manual, Appendix IX.  
C     Route quality *only* from upstream subcatchment(s).
C     Washed-off pollutants added to outflow, in Sub. QSHED.
C     But, add rainfall contribution, unless this is most upstream 
C       subcatchment.  
C     Note that do same calcs for all land use fractions.  Since have
C       no way of apportioning flow and volume other than by same
C       land use fraction, fraction would divide out in numerator (flow)
C       and denominator (volume) and hence not affect computation.
C       Only effect of land use is different removal fraction for 
C       different land use and different rainfall concentration.  
C=======================================================================
C
C	VOL1    = Volume of water (over all subareas) on subcatchment, ft3.
C     VOL0    = Volume of water on subcatchment at last time step, ft3.
C     WFLOW   = Subcatchment outflow, cfs.
C     WFLOW0  = Subcatchment outflow at last time step, cfs.
C     PLAND   = Fraction of subcatchment area with current land use.
C     DECAY   = First order decay coef. for this pollutant and land use,
C               1/sec.
C     REMOVE  = BMP removal fraction for this pollutant and land use, 
C               based on inflow load.
C     DELT    = Time step, sec.
C     POFF2C1 = Incoming load from upstream, mg/sec.
C     POFF2C0 = Incoming load at last time step, mg/sec.
C     ENFILW  = Infilration outflow rate, avg. over time step, cfs. 
C     QRAIN   = Rain/snowmelt inflow rate, cfs, avg. over time step.
C     CRAIN   = Rain/snowmelt concentration, mg/L * 28.3 L/ft3 = mg/ft3.
C     COFF    = Concentration of outflow, mg/s / cfs = mg/ft3, for this
C               pollutant and land use.  
C               Input is old value.  Output is new value (to be found 
C               by this subroutine).
C     XLENFIL = mg/s removed by infiltration.
C     XLREMOV = mg/s removed by removal fraction.
C     XLDECAY = mg/s removed by first-order decay.
C=======================================================================
C
C	CAUTION!
C     SOME IDENTICAL NAMES USED IN ARGUMENT LIST AS IN COMMON.  BE VERY
C     CAREFUL IF LATER DECIDE TO INCLUDE VARIOUS LABELED COMMONS.
C
	INCLUDE 'TAPES.INC'
C=======================================================================
C     The following equation uses average quantities over the
C     time step to avoid stability problems.
C
C     The equation integrated is:
C
C     V*dC/dt = Qin*Cin + Qrain*Crain - Q*C - C*dV/dt - Qinfil*C 
C         - K*C*V + L
C
C     (See User's Manual, Appendix IX.)
C
C     Here, L = - REMOVE*(inflow load)
C     Q       = outflow rate
C     Qin*Cin = POFF2C
C     K       = DECAY
C
C     Note that evaporation does not remove mass.  Outflow by evaporation
C     is accounted for in (1/V)dV/dt term.  Hence, evaporation can increase
C     concentrations, especially when outflow (and volume or depth) are
C     very small.  
C
C     C = Cnew [1 - EXP(-ARG)] + Cold EXP(-ARG)
C
C     where 
C     Cnew  = TOP/DENOM
C     ARG   = DENOM*DELT,
C     TOP   = [Qin*Cin + Qrain*Crain]*(1-REMOVE)/V
C     Numerator of TOP has units of mg/s
C     DENOM = [Q/V + (1/V)dV/dt + Qinfil/V + K]
C     Cold  = C at last time step.
C     Cnew, Cold and C have units of mg/ft3
C
C     Here, we use average values for the time step.  
C
C=======================================================================
C     Assume flow rates and volumes are apportioned in accordance with
C     PLAND fraction.  
C=======================================================================
C     Compute average inflow loads, etc.
C=======================================================================
	CONCOLD = COFF
	POFFAVG = (POFF2C1+POFF2C0)/2.0
C=======================================================================
C     Compute current values of channel parameters for routing.
C=======================================================================
      TWOV  = VOL0 + VOL1
      DENOM = DECAY
C=======================================================================
C     Use form of average Q/V that ensures ARG = 0 when Qin = 0 
C     (by continuity, dV/dt = Qin + Qrain - Q - Qinfil - Qevap).  
C=======================================================================
      IF(TWOV.GT.0.0) DENOM = (WFLOW0+WFLOW)/TWOV
C=======================================================================
C     Allow for case in which surface is only draining, with no inflow.
C     Need factor of 2 in numerator for DV/V calc to get avg. vol.
C     Likewise when adding infiltration outflow.  
C     DENOM can be negative when there is evaporation.  
C=======================================================================
      IF(TWOV.GT.0.0)  DENOM = DENOM + 2.0*(VOL1-VOL0)/TWOV/DELT + 
     +   2.0*ENFILW/TWOV
C=======================================================================
C     EXXP => 0 ==> Big ARG, big Qin/V, concentration => Cin.
C     EXXP >  0 ==> Smaller ARG, smaller Qin/V, and
C      concentration between Cin and Co.  
C     EXXP => 1 ==> ARG => 0, Qin/V => 0, concentration => Co.
C     EXXP > 1.0 for negative DENOM (increase concentration because
C      of evaporation).    
C=======================================================================
                      EXXP = 0.0
				    ARG  = DENOM*DELT
      IF(ARG.LT.20.0) EXXP = EXP(-ARG)
C=======================================================================
C     Two inflow sources are upstream inflow and rainfall.
C=======================================================================
				    TOP = 0.0
	IF(TWOV.GT.0.0) TOP = (POFFAVG+QRAIN*CRAIN)*(1-REMOVE)/(TWOV/2.0)
C=======================================================================
C     Final outflow concentration (mg/ft3) is weighted average of 
C     inflow and old outflow.
C=======================================================================
					 COFF = CONCOLD*EXXP
	IF(DENOM.GT.0.0) COFF = TOP/DENOM*(1-EXXP) + COFF
C=======================================================================
C     For continuity, try to keep track of losses (mg/s) by:
C       -infiltration
C       -removal fraction
C       -decay.
C=======================================================================
      XLINFIL = ENFILW*(COFF+CONCOLD)/2.0
      XLREMOV = REMOVE*(POFFAVG + QRAIN*CRAIN)	
      XLDECAY = DECAY*(CONCOLD*VOL0+COFF*VOL1)/2.0	
	RETURN
	END


