```fortran 
      SUBROUTINE SNOW(TA,SMIMED,J,K,II,RIN,RINE,ASC,WINDY,KOMPUT)
C     RUNOFF BLOCK
C     CALLED BY WSHED NEAR LINE 254, 266, 278, 290
C=======================================================================
C     This subroutine adds/deletes water from snow pack, performs
C       redistribution of snow, calls AREAL and MELT, and routes 
C       liquid water through snow pack.
C     Updated 4/7/94 by WCH to indicate if snow is present in 
C       catchment.  Put new variable KWIKSN in SUBCAT.INC.  
C=======================================================================
      # SNOW Subroutine Documentation

      This document provides a complete and extensive summary of the SNOW subroutine, originally written in Fortran, which is responsible for handling snow accumulation, redistribution, melting, and the routing of meltwater in a watershed simulation.

      ## Overview

      The SNOW subroutine performs the following major tasks:
      - Adds or removes water from the snowpack.
      - Redistributes snow among various subareas.
      - Checks and sets the presence of snow in the catchment.
      - Converts excess snow to immediate melt.
      - Calculates the melt rate and routes meltwater.

      The subroutine relies on several included files for configuration and simulation parameters:
      - **TAPES.INC**
      - **TIMER.INC**
      - **STIMER.INC**
      - **DETAIL.INC**
      - **SUBCAT.INC**

      ## Detailed Breakdown

      ### 1. Handling Snow Addition and Early Termination

      - **Water Addition:**  
            If the rain input (RIN) is negative—implying snowfall—the subroutine adds the snowfall (scaled by the time increment DELT) to the snowpack.  
      - **Early Return:**  
            If the resulting snowpack value is zero or less, the subroutine exits immediately to avoid further processing.

      ### 2. Snow Presence Indicator and Redistribution

      - **Snow Presence Flag:**  
            A flag (**KWIKSN**) is set to indicate that snow is present, which influences other parts of the watershed simulation.
      - **Redistribution:**  
            Depending on the simulation mode (indicated by ISNOW) and subarea parameters, the routine redistributes snow from one subarea to others.  
            - Excess snow in a given subarea is calculated and partially moved to adjacent areas.
            - A portion of this excess, based on predefined fractions (SFRAC), is rerouted to:
                  - Other impervious areas.
                  - Pervious subareas.
                  - Out-of-system routing (tracked via a counter).

      ### 3. Immediate Melt and Snowpack Depletion

      - **Immediate Melt Trigger:**  
            In some cases, such as when new snowfall isn’t present or for normally bare areas, the routine bypasses redistribution and forces an immediate melt.
      - **Snowpack Depletion:**  
            Once melt is initiated, the subroutine adjusts both the snow water content (WSNOW) and related flow parameters (FW) accordingly.

      ### 4. Melt Calculations and Routing

      - **Melt Rate Determination:**  
            The subroutine computes the melt rate (SMELT) by calling auxiliary routines:
            - **CALL AREAL:** For areal melt distribution.
            - **CALL MELT:** To determine the melt rate based on meteorological parameters like temperature (TA) and wind (WINDY).
      - **Meltwater Routing:**  
            Meltwater is routed through the snowpack:
            - The computed melt is added to cumulative melt water (FW).
            - The routine ensures that the melt does not exceed the available snow.
            - Adjustments are made to keep runoff and residual snow values consistent.

      ## Important Variables

      - **TA:** Air temperature.
      - **RIN/RINE:** Rainfall input; a negative value of RIN indicates snowfall.
      - **WSNOW:** Array holding snowpack water content.
      - **DELT:** Time step for simulation.
      - **ISNOW:** Indicator for simulation type or snow condition.
      - **SNCP:** Snow cover percentage used when determining the fractional coverage.
      - **SFRAC:** Array of fractions used for redistributing snow.
      - **KWIKSN:** Snow presence flag.
      - **SMIMED:** Melt rate adjustment variable.
      - **RI:** Temporary variable used for routing calculations.
      - **FW:** Array representing the flow of water.
      - **CNT(9):** Tally for the out-of-system water from snow redistribution.

      ## Code Listing

      Below is the full Fortran code for the SNOW subroutine, which the above summary describes in detail:

      ```fortran
                        SUBROUTINE SNOW(TA,SMIMED,J,K,II,RIN,RINE,ASC,WINDY,KOMPUT)
      C     RUNOFF BLOCK
      C     CALLED BY WSHED NEAR LINE 254, 266, 278, 290
      C=======================================================================
      C     This subroutine adds/deletes water from snow pack, performs
      C       redistribution of snow, calls AREAL and MELT, and routes 
      C       liquid water through snow pack.
      C     Updated 4/7/94 by WCH to indicate if snow is present in 
      C       catchment.  Put new variable KWIKSN in SUBCAT.INC.  
      C=======================================================================
                        INCLUDE 'TAPES.INC'$SELECTION_PLACEHOLDER$
                        INCLUDE 'TIMER.INC'
                        INCLUDE 'STIMER.INC'
                        INCLUDE 'DETAIL.INC'
                        INCLUDE 'SUBCAT.INC'
      C=======================================================================
      C     Add new snow to snow pack.
      C=======================================================================
                        IF(RIN.LT.0.0) WSNOW(II,J) = WSNOW(II,J)-RIN*DELT
                        IF(WSNOW(II,J).LE.0.0) RETURN
      C#######################################################################
      C#### WCH, 4/7/94.  Here, indicate presence of snow in order to set
      C       WET time step in Sub. HYDRO.
      C=======================================================================
                        KWIKSN = 1
      C
                        IF(K.EQ.2.OR.K.EQ.4) GO TO 110
      C=======================================================================
      C     Can redistribute snow (e.g., plow) off subareas 1 and 3 for 
      C     continuous simulation (ISNOW=2).  This is only done once since
      C     only call Sub. SNOW once for subareas 1 and 3.
      C=======================================================================
                        IF(ISNOW.EQ.2) GO TO 105
      C=======================================================================
      C     If ISNOW = 1 and there is no new snow (RIN >= 0), RETURN.
      C     Else, there is snow on "normally bare" area.  It is all immediately
      C     melted at statement 115. 
      C=======================================================================
                        IF(RIN.GE.0.0) RETURN
                        IF(RIN.LT.0.0) GO TO 115
      C=======================================================================
      C     IMPERVIOUS AREA WITH DEPRESSION STORAGE (K=1).
      C     PERFORM SNOW CALCS ONLY FOR ISNOW=2 (CONTINUOUS MELT).
      C     NORMALLY WITH NO SNOW BUT MAY HAVE UP TO WEPLOW FT WATER EQUIV.
      C
      C     CHECK FOR REDISTRIBUTION OF SNOW (PLOWING).
      C     Arrive here only for subarea 1 or 3 and for continuous simulation.
      C=======================================================================
            105 IF(WSNOW(3,J).LT.WEPLOW(J)) GO TO 110
                        EXC = WSNOW(3,J)-WEPLOW(J)
      C=======================================================================
      C     MOVE EXCESS SNOW TO:
      C        OTHER IMPERVIOUS IN SUBCATCHMENT,
      C        (FRACTIONS HAVE BEEN MULTIPLIED BY (WAR(1,J)+WAR(3,J))/WAR(KX,JX)
      C        EARLIER.)
      C=======================================================================
                        WSNOW(1,J) = WSNOW(1,J)+SFRAC(1,J)*EXC
      C=======================================================================
      C     PERVIOUS IN SUBCATCHMENT,
      C=======================================================================
                        WSNOW(2,J) = WSNOW(2,J)+SFRAC(2,J)*EXC
      C=======================================================================
      C     PERVIOUS IN LAST SUBCATCHMENT,
      C=======================================================================
                        WSNOW(2,NOW) = WSNOW(2,NOW)+SFRAC(3,J)*EXC
      C=======================================================================
      C     OUT OF SYSTEM, (KEEP TALLY ON THIS QUANTITY),
      C=======================================================================
                        CNT(9) = CNT(9) + SFRAC(4,J)*EXC
      C=======================================================================
      C        CONVERT TO IMMEDIATE MELT.
      C=======================================================================
                        SMIMED     = SFRAC(5,J)*EXC/DELT
                        WSNOW(3,J) = WEPLOW(J)
      C=======================================================================
      C     SNOW MELT CALCULATIONS FOR ALL AREAS.
      C=======================================================================
            110 SMELT = 0.0
                        RI    = RINE
      C=======================================================================
      C     Upon entering this subroutine, RINE = rain, if raining, or
      C     zero if snowing. 
      C     CEASE SNOW CALCS WHEN AMT LT 0.001 IN. (0.00008 FT).
      C=======================================================================
                        IF(WSNOW(II,J).GT.0.00008) GO TO 120
            115 SMIMED      = SMIMED + (WSNOW(II,J) + FW(II,J))/DELT
                        WSNOW(II,J) = 0.0
                        FW(II,J)    = 0.0
                        IF(ISNOW.EQ.2) COLDC(II,J) = 0.0
                        ASC = 0.0
                        RETURN
      C=======================================================================
      C     DETERMINE FRACTION OF AREA THAT IS SNOW COVERED = ASC.
      C=======================================================================
            120                           ASC = 1.0
                        IF(ISNOW.EQ.1.AND.K.EQ.2) ASC = SNCP(J)
                        IF(ASC.LE.0.0) RETURN
      C
                        IF(ISNOW.NE.2) GO TO 130
                        IF(K.EQ.2) CALL AREAL(WSNOW(II,J),SI(II,J),NEWSNO(II,J),ASC,
                   1                  AWE(II,J),SBA(II,J),SBWS(II,J),RIN,ADCP,DELT)
      C
                        IF(K.EQ.4) CALL AREAL(WSNOW(II,J),SI(II,J),NEWSNO(II,J),ASC,
                   1                  AWE(II,J),SBA(II,J),SBWS(II,J),RIN,ADCI,DELT)
      C
      C     DETERMINE MELT RATE (FT/SEC) = SMELT.
      C     MELT RETURNS SMELT, IF MELT OCCURS, OR ELSE ADDS TO COLD CONTENT.
      C
                                                                   DHM = DH(II,J)
            130 IF(ISNOW.EQ.1) DHM = DHMAX(II,J)
                        IF(WSNOW(II,J).GT.0.0) CALL MELT(DHM,TBASE(II,J),RIN,TA,DELT,
                   1SMELT,COLDC(II,J),ATI(II,J),WINDY,ASC,WSNOW(II,J),KOMPUT)
      C=======================================================================
                        IF(SMELT.LE.0.0) RETURN
      C=======================================================================
      C     ROUTE MELT THROUGH SNOW PACK.
      C     Return snowmelt to Sub. WSHED through RI, in COMMON "Detail.inc"
      C=======================================================================
                                                                                          RI = SMELT*DELT*ASC
                        IF(RI.GT.WSNOW(II,J)) RI = WSNOW(II,J)
                        WSNOW(II,J) = WSNOW(II,J) - RI
                        FW(II,J)    = FW(II,J) + RI + RINE*DELT
                        RI          = FW(II,J) - FWFRAC(II)*WSNOW(II,J)
                        IF(RI.LT.0.0) RI = 0.0
                        FW(II,J)         = FW(II,J) - RI
                        RI               = RI / DELT
                        RETURN
                        END
      ```

      The SNOW subroutine plays a vital role in simulating the hydrologic processes of snow accumulation, redistribution, melting, and runoff, ensuring that the watershed model responds accurately to varying meteorological and terrain conditions.

      INCLUDE 'TIMER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
C=======================================================================
C     Add new snow to snow pack.
C=======================================================================
      IF(RIN.LT.0.0) WSNOW(II,J) = WSNOW(II,J)-RIN*DELT
      IF(WSNOW(II,J).LE.0.0) RETURN
C#######################################################################
C#### WCH, 4/7/94.  Here, indicate presence of snow in order to set
C       WET time step in Sub. HYDRO.
C=======================================================================
      KWIKSN = 1
C
      IF(K.EQ.2.OR.K.EQ.4) GO TO 110
C=======================================================================
C     Can redistribute snow (e.g., plow) off subareas 1 and 3 for 
C     continuous simulation (ISNOW=2).  This is only done once since
C     only call Sub. SNOW once for subareas 1 and 3.
C=======================================================================
      IF(ISNOW.EQ.2) GO TO 105
C=======================================================================
C     If ISNOW = 1 and there is no new snow (RIN >= 0), RETURN.
C     Else, there is snow on "normally bare" area.  It is all immediately
C     melted at statement 115. 
C=======================================================================
      IF(RIN.GE.0.0) RETURN
      IF(RIN.LT.0.0) GO TO 115
C=======================================================================
C     IMPERVIOUS AREA WITH DEPRESSION STORAGE (K=1).
C     PERFORM SNOW CALCS ONLY FOR ISNOW=2 (CONTINUOUS MELT).
C     NORMALLY WITH NO SNOW BUT MAY HAVE UP TO WEPLOW FT WATER EQUIV.
C
C     CHECK FOR REDISTRIBUTION OF SNOW (PLOWING).
C     Arrive here only for subarea 1 or 3 and for continuous simulation.
C=======================================================================
  105 IF(WSNOW(3,J).LT.WEPLOW(J)) GO TO 110
      EXC = WSNOW(3,J)-WEPLOW(J)
C=======================================================================
C     MOVE EXCESS SNOW TO:
C        OTHER IMPERVIOUS IN SUBCATCHMENT,
C        (FRACTIONS HAVE BEEN MULTIPLIED BY (WAR(1,J)+WAR(3,J))/WAR(KX,JX)
C        EARLIER.)
C=======================================================================
      WSNOW(1,J) = WSNOW(1,J)+SFRAC(1,J)*EXC
C=======================================================================
C     PERVIOUS IN SUBCATCHMENT,
C=======================================================================
      WSNOW(2,J) = WSNOW(2,J)+SFRAC(2,J)*EXC
C=======================================================================
C     PERVIOUS IN LAST SUBCATCHMENT,
C=======================================================================
      WSNOW(2,NOW) = WSNOW(2,NOW)+SFRAC(3,J)*EXC
C=======================================================================
C     OUT OF SYSTEM, (KEEP TALLY ON THIS QUANTITY),
C=======================================================================
      CNT(9) = CNT(9) + SFRAC(4,J)*EXC
C=======================================================================
C        CONVERT TO IMMEDIATE MELT.
C=======================================================================
      SMIMED     = SFRAC(5,J)*EXC/DELT
      WSNOW(3,J) = WEPLOW(J)
C=======================================================================
C     SNOW MELT CALCULATIONS FOR ALL AREAS.
C=======================================================================
  110 SMELT = 0.0
      RI    = RINE
C=======================================================================
C     Upon entering this subroutine, RINE = rain, if raining, or
C     zero if snowing. 
C     CEASE SNOW CALCS WHEN AMT LT 0.001 IN. (0.00008 FT).
C=======================================================================
      IF(WSNOW(II,J).GT.0.00008) GO TO 120
  115 SMIMED      = SMIMED + (WSNOW(II,J) + FW(II,J))/DELT
      WSNOW(II,J) = 0.0
      FW(II,J)    = 0.0
      IF(ISNOW.EQ.2) COLDC(II,J) = 0.0
      ASC = 0.0
      RETURN
C=======================================================================
C     DETERMINE FRACTION OF AREA THAT IS SNOW COVERED = ASC.
C=======================================================================
  120                           ASC = 1.0
      IF(ISNOW.EQ.1.AND.K.EQ.2) ASC = SNCP(J)
      IF(ASC.LE.0.0) RETURN
C
      IF(ISNOW.NE.2) GO TO 130
      IF(K.EQ.2) CALL AREAL(WSNOW(II,J),SI(II,J),NEWSNO(II,J),ASC,
     1                  AWE(II,J),SBA(II,J),SBWS(II,J),RIN,ADCP,DELT)
C
      IF(K.EQ.4) CALL AREAL(WSNOW(II,J),SI(II,J),NEWSNO(II,J),ASC,
     1                  AWE(II,J),SBA(II,J),SBWS(II,J),RIN,ADCI,DELT)
C
C     DETERMINE MELT RATE (FT/SEC) = SMELT.
C     MELT RETURNS SMELT, IF MELT OCCURS, OR ELSE ADDS TO COLD CONTENT.
C
                     DHM = DH(II,J)
  130 IF(ISNOW.EQ.1) DHM = DHMAX(II,J)
      IF(WSNOW(II,J).GT.0.0) CALL MELT(DHM,TBASE(II,J),RIN,TA,DELT,
     1SMELT,COLDC(II,J),ATI(II,J),WINDY,ASC,WSNOW(II,J),KOMPUT)
C=======================================================================
      IF(SMELT.LE.0.0) RETURN
C=======================================================================
C     ROUTE MELT THROUGH SNOW PACK.
C     Return snowmelt to Sub. WSHED through RI, in COMMON "Detail.inc"
C=======================================================================
                            RI = SMELT*DELT*ASC
      IF(RI.GT.WSNOW(II,J)) RI = WSNOW(II,J)
      WSNOW(II,J) = WSNOW(II,J) - RI
      FW(II,J)    = FW(II,J) + RI + RINE*DELT
      RI          = FW(II,J) - FWFRAC(II)*WSNOW(II,J)
      IF(RI.LT.0.0) RI = 0.0
      FW(II,J)         = FW(II,J) - RI
      RI               = RI / DELT
      RETURN
      END
``` 
