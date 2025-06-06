```fortran 
      SUBROUTINE DATED
C#######################################################################
C     FIND THE YEAR, MONTH, DAY, HOUR, MINUTE AND SECOND CORRESPONDING
C     TO THE EXISTING JULDAY AND TIMDAY
C
C     JULDAY = JULIAN DAY
C     TIMDAY = TIME OF DAY IN SECONDS
C     CORRECTION BY WCH, 4/93 TO FIX CONVERSION FROM TIMDAY IN 
C       SECONDS TO HR/MIN/SEC.
# Fortran DATED Subroutine Summary

This document describes the purpose and functionality of the Fortran subroutine `DATED`, which is designed to convert a given Julian date and seconds-of-day into their corresponding calendar date and time components.

## Overview

- **Purpose**:  
      The subroutine calculates the year, month, day, hour, minute, and second from:
      - `JULDAY`: a Julian day number (in a modified format where the first part represents the year)
      - `TIMDAY`: the time of day given in seconds

- **Leap Year Correction**:  
      The algorithm accounts for leap years by adjusting the days in February. The condition checks if the year is divisible by 4, and if so, sets February's days to 29.

## Detailed Steps

1. **Year Extraction**:
       - The millennium part of `JULDAY` is divided to extract the preliminary year.
       - If the preliminary year is less than 100, it is corrected by adding 1900.
       - The adjusted value of `JULDAY` is then updated.

2. **Month and Day Calculation**:
       - The variable `JEWEL` holds the remaining day count after extracting the year.
       - The subroutine compares `JEWEL` to cumulative day counts for each month.
       - Uses conditional checks (with `IF` statements) to determine the month and the corresponding day (`NDAY`).

3. **Time Conversion**:
       - Converts `TIMDAY` (seconds of the day) into hours, minutes, and seconds.
       - A small fractional correction is added to ensure accurate conversion to an integer hour.
       - Adjustments are made to compute minutes and remaining seconds.

## Notable Aspects

- **Accuracy Adjustment**:  
      A minor fractional adjustment (`+ 0.0001` for hours and `+ 0.01` for seconds) ensures that rounding errors are minimized, particularly in the hour and second calculations.

- **Conditional Flow**:  
      The extensive use of `IF` statements and the use of a `GO TO` statement allow the subroutine to break out of the month-selection sequence once the correct month is determined.

## Integration

- **Include File** (`STIMER.INC`):  
      The subroutine includes an external file (`STIMER.INC`), which may define additional variables or configurations needed for the timing calculations.

## Conclusion

The `DATED` subroutine efficiently converts a combined year-day format and a seconds-of-day count into a human-readable date and time format. This conversion is particularly useful in legacy systems where dates are stored in Julian format, ensuring compatibility and correct time interpretation even with leap years.

      INCLUDE 'STIMER.INC'
C=======================================================================
      NYEAR = JULDAY/1000
      JEWEL = JULDAY - 1000*NYEAR
      IF (NYEAR.LT.100) THEN
           NYEAR  = NYEAR + 1900
           JULDAY = JEWEL + NYEAR*1000
           ENDIF
      IFEB  = 28
      IF((NYEAR/4)*4-NYEAR.EQ.0) IFEB = 29
      IF(JEWEL.LE.31)      THEN
                           MONTH = 1
                           NDAY  = JEWEL
                           GO TO 100
                           ENDIF
      IF(JEWEL.LE.31+IFEB) THEN
                           MONTH = 2
                           NDAY  = JEWEL - 31
                           GO TO 100
                           ENDIF
      IF(JEWEL.LE.62+IFEB) THEN
                           MONTH = 3
                           NDAY  = JEWEL - 31 - IFEB
                           GO TO 100
                           ENDIF
      IF(JEWEL.LE.92+IFEB) THEN
                           MONTH = 4
                           NDAY  = JEWEL - 62 - IFEB
                           GO TO 100
                           ENDIF
      IF(JEWEL.LE.123+IFEB) THEN
                            MONTH = 5
                            NDAY  = JEWEL - 92 - IFEB
                            GO TO 100
                            ENDIF
      IF(JEWEL.LE.153+IFEB) THEN
                            MONTH = 6
                            NDAY  = JEWEL - 123 - IFEB
                            GO TO 100
                            ENDIF
      IF(JEWEL.LE.184+IFEB) THEN
                            MONTH = 7
                            NDAY  = JEWEL - 153 - IFEB
                            GO TO 100
                            ENDIF
      IF(JEWEL.LE.215+IFEB) THEN
                            MONTH = 8
                            NDAY  = JEWEL - 184 - IFEB
                            GO TO 100
                            ENDIF
      IF(JEWEL.LE.245+IFEB) THEN
                            MONTH = 9
                            NDAY  = JEWEL - 215 - IFEB
                            GO TO 100
                            ENDIF
      IF(JEWEL.LE.276+IFEB) THEN
                            MONTH = 10
                            NDAY  = JEWEL - 245 - IFEB
                            GO TO 100
                            ENDIF
      IF(JEWEL.LE.306+IFEB) THEN
                            MONTH = 11
                            NDAY  = JEWEL - 276 - IFEB
                            GO TO 100
                            ENDIF
      IF(JEWEL.LE.337+IFEB) THEN
                            MONTH = 12
                            NDAY  = JEWEL - 306 - IFEB
                            ENDIF
C#### WCH, 4/20/93. CHANGE FRACTION ADDED TO 0.0001 HR.  MUST BE < 1 SEC.
  100 JHR     = IFIX(TIMDAY/3600.0 + 0.0001)
      JSEC    = IFIX(TIMDAY - FLOAT(JHR)*3600.0 + 0.01)
      MINUTE  = JSEC / 60
      JSEC    = JSEC - 60*MINUTE
      RETURN
      END
``` 
