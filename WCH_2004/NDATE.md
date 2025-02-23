```fortran 
      SUBROUTINE NDATE(TIME,JDAY,TMDAY)
C#######################################################################
C     THIS SUBROUTINE CALCULATES THE NEW JULIAN DAY AND TIME OF DAY
C     RESULTING FROM ADDING TIME TO THE EXISTING JULDAY AND TIMDAY
C
C     JDAY  = NEW JULIAN DAY
C     TMDAY = NEW TIME OF DAY IN SECONDS
C     TIME  = TIME INCREMENT IN SECONDS
C#######################################################################
      INCLUDE 'STIMER.INC'
C=======================================================================
      TMDAY = TIMDAY + TIME
      JDAY  = JULDAY
 100  IF(TMDAY.GE.86400.0) THEN
                TMDAY = TMDAY - 86400.0
                JDAY  = JDAY + 1
                JYEAR = JDAY/1000
                IDAY  = JDAY - 1000*JYEAR
                IF (JYEAR.LT.100) JYEAR = JYEAR + 1900
                IF(MOD(JYEAR,4).EQ.0.AND.IDAY.GE.367.OR.
# NDATE Fortran Subroutine Documentation

The following markdown provides an extensive summary of the Fortran subroutine `NDATE`, which calculates a new Julian day and time-of-day by adding an increment (in seconds) to the current time. This subroutine is typically used in time-stepping models to keep track of date transitions.

## Overview

The `NDATE` subroutine accomplishes the following primary tasks:
- **Time Increment**: It adds a given time increment (in seconds) to the current "time of day."
- **Day Transition**: When the incremented time exceeds a full day (86400 seconds), the subroutine adjusts the time and advances the Julian day accordingly.
- **Year Correction**: When crossing over into a new year, it recalculates the Julian day and corrects for leap-year conditions.

## Detailed Explanation

1. **Input Variables**:
      - `TIME`: The time increment to be added (in seconds).
      - `JDAY`: The current Julian day, which will be updated.
      - `TMDAY`: The current time-of-day in seconds.

2. **Include File**:
      - The subroutine includes `STIMER.INC`, which likely declares and defines constants or other time-related variables needed by the routine.

3. **Time and Day Calculation**:
      - The new time-of-day (`TMDAY`) is computed by summing the current time-of-day and the increment.
      - If `TMDAY` exceeds or equals 86400 seconds (one day), the subroutine subtracts a full day (86400 seconds) and increments the Julian day (`JDAY`).

4. **Year Calculation and Leap-Year Consideration**:
      - When the day transitions to a new day, the year (`JYEAR`) and day-of-year (`IDAY`) portions are recalculated.
      - If the computed year (`JYEAR`) is less than 100, it is corrected by adding 1900.
      - The subroutine then checks for leap-year conditions. In the original code, a placeholder existed to handle the scenario when the current year is not a leap year and the day-of-year exceeds the last valid day for non-leap years.

5. **Modified Leap-Year Handling**:
      - The revised condition for handling non-leap years is as follows:
        
        -+-+-+-+-+
        IF(MOD(JYEAR,4).NE.0.AND.IDAY.GE.366) THEN
                                      JYEAR  = JYEAR + 1
                                      JDAY   = JYEAR*1000 + 1
        ENDIF
        -+-+-+-+-+

      - **Explanation**:
        - The condition `MOD(JYEAR,4).NE.0` checks if `JYEAR` is not divisible by 4 (i.e. not a leap year).
        - The `AND.IDAY.GE.366` part ensures that if the day-of-year reaches or exceeds 366 in a non-leap year, the year is incremented and the Julian day is reset to the start of the new year.
        
6. **Loop and Return**:
      - The subroutine repeatedly adjusts the time-of-day and Julian day until the time-of-day is less than 86400 seconds.
      - Finally, it returns after updating the date values accordingly.

## Complete Fortran Code (with Rewritten Modification)

Below is the complete Fortran code in a markdown code block with the updated leap-year handling inserted at the placeholder:
                  JYEAR  = JYEAR + 1
                                         JDAY   = JYEAR*1000 + 1
                                         ENDIF
                ENDIF
      IF(TMDAY.GE.86400.0) GO TO 100
      RETURN
      END
``` 
