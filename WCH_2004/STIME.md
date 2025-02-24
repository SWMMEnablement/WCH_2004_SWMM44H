```fortran 
      SUBROUTINE STIME(DELTA)
C#######################################################################
C     JULDAY = JULIAN DAY
C     TIMDAY = TIME OF DAY IN SECONDS
C     DELTA  = NEW TIME STEP IN SECONDS
C#######################################################################
      INCLUDE 'STIMER.INC'
      # STIME Fortran Subroutine Overview

      This document describes the Fortran subroutine STIME, which updates a simulation time by adding a time step and adjusting both the time of day and the Julian day appropriately. Below is an extensive summary of its functionality:

      ## Purpose
      - **Update Time:** Increases the simulation time by a given time step `DELTA` (in seconds).
      - **Adjust Day Transition:** Properly handles transitions when the time of day exceeds 86,400 seconds (i.e., midnight).
      - **Correct Date Calculation:** Adjusts the Julian day and computes the calendar year correctly, including leap year checks.
      - **Invoke Date Routine:** When advancing a day requires recalculating date-related parameters, a secondary routine (`DATED`) is called.

      ## Key Variables and Computations
      - **JULDAY:** A combination of the year and the day-of-year; modified to reflect changes after a time step.
      - **TIMDAY:** Represents the time of day in seconds. It is incremented by `DELTA` and, if it exceeds 86,400 seconds, it is reset by subtracting 86,400 seconds.
      - **NYEAR:** Derived from `JULDAY`; if the year is stored as a two- or three-digit number, adjustments are made to convert it into a full four-digit year.
      - **DAY CHECK:** The program checks if the day-of-year has exceeded the number of days available in the year (367 in a leap year, 366 otherwise) and adjusts both `JULDAY` and `NYEAR` accordingly.

      ## Algorithm Flow
      1. **Include External Definitions:** The subroutine starts by including an external file (`STIMER.INC`) for additional constants or declarations.
      2. **Year Adjustment:** The initial `JULDAY` is used to extract the year (`NYEAR`). If `NYEAR` is less than 100, numeric adjustments reformat it to a standard calendar year.
      3. **Time Increment:** `TIMDAY` is incremented by the given `DELTA`.
      4. **Midnight Check Loop:** If `TIMDAY` is greater than or equal to 86,400 seconds:
            - The time is wrapped around by subtracting 86,400 seconds.
            - The `JULDAY` is incremented and checked to see if a new calendar year should begin.
            - A condition checks if the day-of-year exceeds the maximum available days, with consideration for leap years.
            - If a year rollover is detected, the date is reset to the first day of the new year and the `DATED` subroutine is called.
            - The loop repeats as long as `TIMDAY` remains equal to or above the 86,400 threshold.
      5. **Exit:** The subroutine returns control after processing all necessary adjustments.

      ## Full Code Listing

      ```fortran
                  SUBROUTINE STIME(DELTA)
      C#######################################################################
      C     JULDAY = JULIAN DAY
      C     TIMDAY = TIME OF DAY IN SECONDS
      C     DELTA  = NEW TIME STEP IN SECONDS
      C#######################################################################
                  INCLUDE 'STIMER.INC'
                  NYEAR = JULDAY/1000

                  IF (NYEAR.LT.100) THEN
                          JULDAY = JULDAY - NYEAR*1000
                          NYEAR = NYEAR + 1900
                          JULDAY = JULDAY + NYEAR*1000
                  ENDIF

                  TIMDAY = TIMDAY + DELTA

       100  IF(TIMDAY.GE.86400.0) THEN
                                     TIMDAY = TIMDAY - 86400.0
                                     JULDAY = JULDAY + 1
                                     IDAY   = JULDAY - 1000*(JULDAY/1000)
                                     IF(MOD(NYEAR,4).EQ.0.AND.IDAY.EQ.367.OR.
              +             MOD(NYEAR,4).NE.0.AND.IDAY.EQ.366) THEN
                                                                                      NYEAR  = NYEAR + 1
                                                                                      JULDAY = NYEAR*1000 + 1
                                     ENDIF
                                     CALL DATED
                  ENDIF

                  IF(TIMDAY.GE.86400.0) GO TO 100

                  RETURN
                  END
      ```

      This comprehensive markdown serves as both a summary of the subroutine’s logic and a complete presentation of the original code, ready for inclusion in the document.
      IF (NYEAR.LT.100) THEN
           JULDAY = JULDAY - NYEAR*1000
           NYEAR = NYEAR + 1900
           JULDAY = JULDAY + NYEAR*1000
           ENDIF
      TIMDAY = TIMDAY + DELTA
 100  IF(TIMDAY.GE.86400.0) THEN
                TIMDAY = TIMDAY - 86400.0
                JULDAY = JULDAY + 1
                IDAY   = JULDAY - 1000*(JULDAY/1000)
                IF(MOD(NYEAR,4).EQ.0.AND.IDAY.EQ.367.OR.
     +             MOD(NYEAR,4).NE.0.AND.IDAY.EQ.366) THEN
                                         NYEAR  = NYEAR + 1
                                         JULDAY = NYEAR*1000 + 1
                                         ENDIF
                CALL DATED
                ENDIF
      IF(TIMDAY.GE.86400.0) GO TO 100
      RETURN
      END
``` 
