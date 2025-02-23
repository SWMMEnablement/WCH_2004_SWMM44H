```fortran 
      SUBROUTINE GAMP(SUCT,SMD,SMDMAX,IFLAG,FU,F,HYDCON,WDEPTH,
     1           TR,DELT,TIME,RI,RLOSS,UL,SVAP,KAMEW,NAMEW,FLOW2P)
C     RUNOFF BLOCK
C     CALLED BY WSHED NEAR LINE 300
C=======================================================================
C     Green-Ampt infiltration routine,  written FEB 1979 by R.G.MEIN.
C
C     Theory as given in Mein & Larson, Water Resources Research,
C                                       Vol. 9, NO. 2, 384-394, 1973
C=======================================================================
C     Redistribution rate depends on soil hydraulic conductivity
C     and is considered only for the surface region.
C#######################################################################
C     WCH (CDM), 10/5/93.  CHANGE EVAP TO SVAP FOR CONSISTENCY, 5 
C       LOCATIONS.
C     WCH, 4/5/00. Declare TIME as double precision to match delcarion 
# GAMP Fortran Subroutine – Comprehensive Summary

This document provides an extensive overview of the **GAMP** Fortran subroutine. The routine is designed to simulate surface infiltration and runoff following the **Green-Ampt** infiltration theory, with additional modifications to account for various hydrological processes.

## Overview

- **Purpose:**  
      Simulate runoff and infiltration on a watershed using the Green-Ampt method. It accounts for soil surface conditions, rainfall intensity, infiltration limits, evaporation effects, and the redistribution of water.

- **Key Concepts:**
      - **Green-Ampt Infiltration:** Uses a mathematical model based on soil suction and moisture deficits.
      - **Surface Saturation:** Determines whether the soil is saturated at the beginning or during the time interval.
      - **Time Step Management:** Adjusts infiltration and redistribution based on elapsed time during a rainfall event.
      - **Numerical Convergence:** Utilizes a Newton-Raphson iteration to ensure solutions converge when calculating potential infiltration volume.

## Code Structure

The subroutine is structured with several major segments:

1. **Variable Declarations and Inclusions:**
       - Common include file (`TAPES.INC`) brings in shared constants and settings.
       - Variables include double precision values (e.g., `RI`, `RLOSS`, `TIME`) and character strings for names (e.g., `KAMEW`).

2. **Statement Function for the Green-Ampt Equation:**
       - Defines `F22(X)` – a function used in the Newton-Raphson iteration for solving infiltration volume.

3. **Conditional Branching Based on Surface Conditions:**
       - **Unsaturated Surface:** Handles infiltration when the surface is not fully saturated. Adjusts soil moisture and potential evaporation losses.
       - **Saturated Surface:** Deals with scenarios where the water input exceeds the infiltration capacity. Implements logic to either prevent further infiltration or to redistribute water through a saturation interface.

4. **Decision Paths for Water Balance:**
       - Determines water application strategy based on rainfall intensity in relation to the hydraulic conductivity (`HYDCON`).
       - Adjusts calculations for evaporation and surface water accumulation.

5. **Newton-Raphson Iteration:**
       - Iterative section (labeled `300`) to compute the infiltration volume.  
       - Convergence is monitored by comparing successive estimates (`F21` and `F22(F21)`).

6. **Handling Convergence Failure:**
       - When iteration does not meet the convergence criteria, the code issues warnings and defaults to assuming half of the available water infiltrates.

7. **Output and Return Statements:**
       - Adjusts volume and state variables (`FU`, `F`, `SMD`, etc.) accordingly upon reaching state saturation or solution convergence.
       - Uses formatted output for diagnostic messages if convergence fails.

## Key Variables and Parameters

- **Input Parameters:**
      - `SUCT`, `SMD`, `HYDCON`: Soil suction, moisture content, and hydraulic conductivity.
      - `IFLAG`: Flag indicating initial surface condition (saturated or unsaturated).
      - `TIME`, `TR`, `DELT`: Time parameters for simulation step control.
      
- **Calculated Variables:**
      - `R1`: Effective water input after accounting for evaporation and other losses.
      - `FUMAX`: Maximum possible infiltrated water volume based on soil parameters.
      - `RVOL`: Rainfall volume adjusted for the time step.
      - `FS`: Additional water that can potentially infiltrate when soil conditions change.
      
- **Diagnostic Outputs:**
      - `NAMEW`, `KAMEW`: Naming variables for diagnostic messages in case of convergence issues.

## Functional Flow

1. **Initial Checks:**  
       The subroutine first checks whether the surface is saturated. If it is, it jumps to a routine to recalculate volume under saturated conditions.

2. **Desaturation and Redistribution:**  
       When unsaturated, the routine decreases the remaining rain (`TR`) and applies infiltration. During dry periods, moisture is redistributed, affecting the volume that can infiltrate.

3. **Depth and Evaporation Adjustments:**  
       Adjustments are made to account for evaporation losses and overland flow (`FLOW2P`) in both low and high-intensity rainfall conditions.

4. **Convergence Iteration:**  
       Iteratively solves for infiltration volume using a loop that updates an estimate until convergence is reached. In absence of convergence, a fallback scenario is used.

## The Fortran Code Listing

```fortran
                  SUBROUTINE GAMP(SUCT,SMD,SMDMAX,IFLAG,FU,F,HYDCON,WDEPTH,
             1           TR,DELT,TIME,RI,RLOSS,UL,SVAP,KAMEW,NAMEW,FLOW2P)
C     RUNOFF BLOCK
C     CALLED BY WSHED NEAR LINE 300
C=======================================================================
C     Green-Ampt infiltration routine,  written FEB 1979 by R.G.MEIN.
C
C     Theory as given in Mein & Larson, Water Resources Research,
C                                       Vol. 9, NO. 2, 384-394, 1973
C=======================================================================
C     Redistribution rate depends on soil hydraulic conductivity
C     and is considered only for the surface region.
C#######################################################################
C     WCH (CDM), 10/5/93.  CHANGE EVAP TO SVAP FOR CONSISTENCY, 5 
C       LOCATIONS.
C     WCH, 4/5/00. Declare TIME as double precision to match delcarion 
C       in TIMER.INC..$SELECTION_PLACEHOLDER$
C     WCH, 12/20/00.  Add rerouted overland flow to rain-evap calc. 
C     WCH, 4/11/02. Correct use of RI to R1 in several places, to 
C       account for rerouted overland flow. 
C=======================================================================
                  INCLUDE 'TAPES.INC'
                  DOUBLE PRECISION RI,RLOSS,SVAP,TIME
                  CHARACTER KAMEW*10
C=======================================================================
C     Statement function for Green-Ampt equation.
C=======================================================================
                  F22(X) = X-(X-F-C1*ALOG(X+C1)+C2)/(1.0-C1/(X+C1))
C=======================================================================
                  C1    = SUCT*SMD
                  TBS   = 6.0
Cwch, 12/20/00. Add rerouted water from impervious areas.
Cwch, 4/11/02.  Add standing water to potential infiltration, 
C     as in HORTON.
                  R1    = RI - SVAP + FLOW2P + WDEPTH/DELT
                  FUMAX = UL*SMDMAX
                  RVOL  = R1*DELT 
C=======================================================================
C     Check for surface saturation.
C=======================================================================
                  IF(IFLAG.EQ.1)   GO TO 200
C=======================================================================
C     Surface unsaturated at the beginning of the time step.
C=======================================================================
                  TR = TR - DELT
C=======================================================================
C     Evaporation depletes any rainfall or standing water.
Cwch, 4/11/02. Set RLOSS = total water available for loss.
C=======================================================================
                  IF(R1.LE.0.0) THEN
                                                            RLOSS = RI + FLOW2P + WDEPTH/DELT
                                                            IF(FU.LE.0.0) RETURN
C=======================================================================
C                   Deplete soil moisture during dry period.
C=======================================================================
                                                            DF  = UL*DELT/90000.
                                                            DEP = DF*FUMAX
                                                            F   = F  - DEP
                                                            FU  = FU - DEP
                                                            IF(FU.LE.0.0) THEN
                                                                                                      FU  = 0.0
                                                                                                      F   = 0.0
                                                                                                      SMD = SMDMAX
                                                                                                      RETURN
                                                                                                      ENDIF
C=======================================================================
C                   If sufficient time elapsed since rain, redistribute.
C=======================================================================
                                                            IF(TR.LE.0.0) THEN
                                                                                                      SMD = (FUMAX-FU)/UL
                                                                                                      F   = 0.0
                                                                                                      ENDIF
                                                            RETURN
                                                            ENDIF
C=======================================================================
C     Low intensity, all infiltrates.  Rain intensity > 0.0
C=======================================================================
                  IF(R1.LE.HYDCON) THEN
                                                                   FU = FU + RVOL
                                                                   IF(FU.GT.FUMAX) FU = FUMAX
                                                                   F                  = F + RVOL
Cwch, 4/11/02. Loss should be R1 plus evap, not RI. 
                                                                   RLOSS              = R1 + SVAP
                                                                   IF(TR.LE.0.0) THEN
                                                                                                             SMD = (FUMAX-FU)/UL
                                                                                                             F   = 0.0
                                                                                                             ENDIF
                                                                   RETURN
                                                                   ENDIF
C=======================================================================
C     Rain (minus evap) intensity > Hyd. conductivity.
C=======================================================================
                  TR = 900.0*TBS/UL
                  FS = HYDCON*C1/(R1-HYDCON)-F
C=======================================================================
C     Soil still wet from last rain, surface saturates.
C=======================================================================
                  IF(FS.LE.0.0) THEN
                                                            IFLAG = 1
                                                            GO TO 200
                                                            ENDIF
C=======================================================================
C     All water infiltrates.
C=======================================================================
                  IF(FS.GE.RVOL) THEN
Cwch, 4/11/02. Loss should be R1 plus evap, not RI. 
                                                             RLOSS = R1 + SVAP
                                                             F     = F  + RVOL
                                                             FU    = FU + RVOL
                                                             IF(FU.GT.FUMAX) FU = FUMAX
                                                             RETURN
                                                             ENDIF
C=======================================================================
C     Surface saturates during interval.
C=======================================================================
                  F  = F + FS
                  TS = FS/R1
                  C2 = C1*ALOG(F+C1)-HYDCON*(DELT-TS)
                  GO TO 300
C=======================================================================
C     Surface is saturated, so compute vol of potential infiltration.
C=======================================================================
      200 TR = 900.0*TBS/UL
                  TS = 0.0
                  IF(C1.LE.0.0) THEN
                                                            F2 = HYDCON*DELT+F
                                                            ELSE
                                                            C2 = C1*ALOG(F+C1) - HYDCON*DELT
                                                            GO TO 300
                                                            ENDIF
C=======================================================================
C     Excess water at surface.  Corrected December, 1990.
Cwch 4/11/02. RVOL includes WDEPTH.  Should not add here.
C=======================================================================
C  210 IF((F2-F).LE.(WDEPTH+RVOL)) THEN
      210 IF((F2-F).LE.RVOL) THEN
                                                                                                      RLOSS = (F2-F)/DELT + SVAP
                                                                                                      FU    = F2 - F      + FU
                                                                                                      F     = F2
C---- old code                    FU    = F2
                                                                                                      IF(FU.GT.FUMAX) FU = FUMAX
                                                                                                      RETURN
C=======================================================================
C     Rain+surface water infiltrates.  Corrected December, 1990.
C=======================================================================
                                                                                                      ELSE
Cwch, 4/11/02. Loss should be R1 + evap. R1 includes all available water. 
C                                  RLOSS = WDEPTH/DELT + RI
                                                                                                      RLOSS = R1 + SVAP
                                                                                                      F     = F  + AMAX1(WDEPTH+RVOL,0.0)
                                                                                                      IFLAG = 0
                                                                                                      FU    = FU + AMAX1(WDEPTH+RVOL,0.0)
C--- old code                     FU    = F
                                                                                                      IF(FU.GT.FUMAX) FU = FUMAX
                                                                                                      RETURN
                                                                                                      ENDIF
C=======================================================================
C     Newton-Raphson iteration to solve for infiltration volume.
C=======================================================================
      300 F21      = F
                  DO 310 I = 1,11
                  F2       = F22(F21)
                  IF(ABS(F2-F21).LT.0.0001)   GO TO 320
                  F21      = F2
      310 CONTINUE
C=======================================================================
C     No convergence.
C=======================================================================
                  F2 = F + (DELT-TS)*R1/2.0
                  IF(JCE.EQ.0) WRITE(N6,315) NAMEW,TIME,IFLAG,SMD,F,R1
                  IF(JCE.EQ.1) WRITE(N6,316) KAMEW,TIME,IFLAG,SMD,F,R1
C=======================================================================
C     Convergence, return to call point.
C=======================================================================
      320 IF(IFLAG.EQ.0) THEN
                                                             RLOSS = (F2 - F + FS)/DELT + SVAP
                                                             FU    = F2 - F  + FU
                                                             F     = F2
                                                             IFLAG = 1
C--- old code        FU    = F2
                                                             IF(FU.GT.FUMAX) FU = FUMAX
                                                             RETURN
                                                             ENDIF
                  GO TO 210
C=======================================================================
315   FORMAT(' ===> !! Warning, No convergence of Green-Ampt equation. F
             1or subcat. input # ',I10,' Time =',F9.0,' seconds',/,
             2 ' IFLAG =',I2,' SMD =',F6.4,' F(ft) =',F7.4,' Rain-Evap(ft/s) =',
             3 E12.5,' ===>  Assume infil. vol. = half of rain-evap.')
316   FORMAT(' ===> !! Warning, No convergence of Green-Ampt equation. F
             1or subcat. input name  ',A10,' Time =',F9.0,' seconds',/,
             2 ' IFLAG =',I2,' SMD =',F6.4,' F(ft) =',F7.4,' Rain-Evap(ft/s) =',
             3 E12.5,' ===>  Assume infil. vol. = half of rain-evap.')
C=======================================================================
                  END
```

## Conclusion

The **GAMP** subroutine is a detailed hydrological calculation model that simulates various physical processes such as infiltration, surface saturation, and evaporation. Modifications and conditional logic within the code ensure that it accurately reflects the Green-Ampt infiltration theory while also handling edge cases such as convergence failures. This comprehensive markdown summary captures the intent, structure, and key operational details of the subroutine.

C     WCH, 12/20/00.  Add rerouted overland flow to rain-evap calc. 
C     WCH, 4/11/02. Correct use of RI to R1 in several places, to 
C       account for rerouted overland flow. 
C=======================================================================
      INCLUDE 'TAPES.INC'
      DOUBLE PRECISION RI,RLOSS,SVAP,TIME
      CHARACTER KAMEW*10
C=======================================================================
C     Statement function for Green-Ampt equation.
C=======================================================================
      F22(X) = X-(X-F-C1*ALOG(X+C1)+C2)/(1.0-C1/(X+C1))
C=======================================================================
      C1    = SUCT*SMD
      TBS   = 6.0
Cwch, 12/20/00. Add rerouted water from impervious areas.
Cwch, 4/11/02.  Add standing water to potential infiltration, 
C     as in HORTON.
      R1    = RI - SVAP + FLOW2P + WDEPTH/DELT
      FUMAX = UL*SMDMAX
      RVOL  = R1*DELT 
C=======================================================================
C     Check for surface saturation.
C=======================================================================
      IF(IFLAG.EQ.1)   GO TO 200
C=======================================================================
C     Surface unsaturated at the beginning of the time step.
C=======================================================================
      TR = TR - DELT
C=======================================================================
C     Evaporation depletes any rainfall or standing water.
Cwch, 4/11/02. Set RLOSS = total water available for loss.
C=======================================================================
      IF(R1.LE.0.0) THEN
                    RLOSS = RI + FLOW2P + WDEPTH/DELT
                    IF(FU.LE.0.0) RETURN
C=======================================================================
C                   Deplete soil moisture during dry period.
C=======================================================================
                    DF  = UL*DELT/90000.
                    DEP = DF*FUMAX
                    F   = F  - DEP
                    FU  = FU - DEP
                    IF(FU.LE.0.0) THEN
                                  FU  = 0.0
                                  F   = 0.0
                                  SMD = SMDMAX
                                  RETURN
                                  ENDIF
C=======================================================================
C                   If sufficient time elapsed since rain, redistribute.
C=======================================================================
                    IF(TR.LE.0.0) THEN
                                  SMD = (FUMAX-FU)/UL
                                  F   = 0.0
                                  ENDIF
                    RETURN
                    ENDIF
C=======================================================================
C     Low intensity, all infiltrates.  Rain intensity > 0.0
C=======================================================================
      IF(R1.LE.HYDCON) THEN
                       FU = FU + RVOL
                       IF(FU.GT.FUMAX) FU = FUMAX
                       F                  = F + RVOL
Cwch, 4/11/02. Loss should be R1 plus evap, not RI. 
                       RLOSS              = R1 + SVAP
                       IF(TR.LE.0.0) THEN
                                     SMD = (FUMAX-FU)/UL
                                     F   = 0.0
                                     ENDIF
                       RETURN
                       ENDIF
C=======================================================================
C     Rain (minus evap) intensity > Hyd. conductivity.
C=======================================================================
      TR = 900.0*TBS/UL
      FS = HYDCON*C1/(R1-HYDCON)-F
C=======================================================================
C     Soil still wet from last rain, surface saturates.
C=======================================================================
      IF(FS.LE.0.0) THEN
                    IFLAG = 1
                    GO TO 200
                    ENDIF
C=======================================================================
C     All water infiltrates.
C=======================================================================
      IF(FS.GE.RVOL) THEN
Cwch, 4/11/02. Loss should be R1 plus evap, not RI. 
                     RLOSS = R1 + SVAP
                     F     = F  + RVOL
                     FU    = FU + RVOL
                     IF(FU.GT.FUMAX) FU = FUMAX
                     RETURN
                     ENDIF
C=======================================================================
C     Surface saturates during interval.
C=======================================================================
      F  = F + FS
      TS = FS/R1
      C2 = C1*ALOG(F+C1)-HYDCON*(DELT-TS)
      GO TO 300
C=======================================================================
C     Surface is saturated, so compute vol of potential infiltration.
C=======================================================================
  200 TR = 900.0*TBS/UL
      TS = 0.0
      IF(C1.LE.0.0) THEN
                    F2 = HYDCON*DELT+F
                    ELSE
                    C2 = C1*ALOG(F+C1) - HYDCON*DELT
                    GO TO 300
                    ENDIF
C=======================================================================
C     Excess water at surface.  Corrected December, 1990.
Cwch 4/11/02. RVOL includes WDEPTH.  Should not add here.
C=======================================================================
C  210 IF((F2-F).LE.(WDEPTH+RVOL)) THEN
  210 IF((F2-F).LE.RVOL) THEN
                                  RLOSS = (F2-F)/DELT + SVAP
                                  FU    = F2 - F      + FU
                                  F     = F2
C---- old code                    FU    = F2
                                  IF(FU.GT.FUMAX) FU = FUMAX
                                  RETURN
C=======================================================================
C     Rain+surface water infiltrates.  Corrected December, 1990.
C=======================================================================
                                  ELSE
Cwch, 4/11/02. Loss should be R1 + evap. R1 includes all available water. 
C                                  RLOSS = WDEPTH/DELT + RI
                                  RLOSS = R1 + SVAP
                                  F     = F  + AMAX1(WDEPTH+RVOL,0.0)
                                  IFLAG = 0
                                  FU    = FU + AMAX1(WDEPTH+RVOL,0.0)
C--- old code                     FU    = F
                                  IF(FU.GT.FUMAX) FU = FUMAX
                                  RETURN
                                  ENDIF
C=======================================================================
C     Newton-Raphson iteration to solve for infiltration volume.
C=======================================================================
  300 F21      = F
      DO 310 I = 1,11
      F2       = F22(F21)
      IF(ABS(F2-F21).LT.0.0001)   GO TO 320
      F21      = F2
  310 CONTINUE
C=======================================================================
C     No convergence.
C=======================================================================
      F2 = F + (DELT-TS)*R1/2.0
      IF(JCE.EQ.0) WRITE(N6,315) NAMEW,TIME,IFLAG,SMD,F,R1
      IF(JCE.EQ.1) WRITE(N6,316) KAMEW,TIME,IFLAG,SMD,F,R1
C=======================================================================
C     Convergence, return to call point.
C=======================================================================
  320 IF(IFLAG.EQ.0) THEN
                     RLOSS = (F2 - F + FS)/DELT + SVAP
                     FU    = F2 - F  + FU
                     F     = F2
                     IFLAG = 1
C--- old code        FU    = F2
                     IF(FU.GT.FUMAX) FU = FUMAX
                     RETURN
                     ENDIF
      GO TO 210
C=======================================================================
315   FORMAT(' ===> !! Warning, No convergence of Green-Ampt equation. F
     1or subcat. input # ',I10,' Time =',F9.0,' seconds',/,
     2 ' IFLAG =',I2,' SMD =',F6.4,' F(ft) =',F7.4,' Rain-Evap(ft/s) =',
     3 E12.5,' ===>  Assume infil. vol. = half of rain-evap.')
316   FORMAT(' ===> !! Warning, No convergence of Green-Ampt equation. F
     1or subcat. input name  ',A10,' Time =',F9.0,' seconds',/,
     2 ' IFLAG =',I2,' SMD =',F6.4,' F(ft) =',F7.4,' Rain-Evap(ft/s) =',
     3 E12.5,' ===>  Assume infil. vol. = half of rain-evap.')
C=======================================================================
      END
``` 
