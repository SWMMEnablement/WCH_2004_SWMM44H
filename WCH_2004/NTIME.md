```fortran 
      SUBROUTINE NTIME(JDAY,TMDAY,TRAIN)
C#######################################################################
C     CALCULATE THE TIME DIFFERENCE IN SECONDS BETWEEN JDAY AND TMDAY
C     AND THE EXISTING JULDAY AND TIMDAY
C
C     JDAY   = JULIAN DAY
C     TMDAY  = TIME OF DAY IN SECONDS
C     TRAIN  = TIME DIFFERENCE IN SECONDS (+ or -)
C     TRAIN is positive if JDAY/TMDAY > JULDAY/TIMDAY.
C#######################################################################
      INCLUDE 'STIMER.INC'
      JYEAR = JDAY/1000
      IYEAR = JULDAY/1000
      LDAY  = JDAY   - JYEAR*1000
      KDAY  = JULDAY - IYEAR*1000
      IF (JYEAR.LT.100) THEN
           JYEAR = JYEAR + 1900
      # WCH 2004 SWMM44H: NTIME Subroutine Documentation

      ## Table of Contents
      1. [Introduction](#introduction)
      2. [Code Overview](#code-overview)
      3. [Detailed Code Explanation](#detailed-code-explanation)
      4. [References](#references)
      5. [Appendices](#appendices)

      ## Introduction
      This document provides a complete technical explanation of the NTIME subroutine written in Fortran. The subroutine calculates the time difference (in seconds) between a new date/time (JDAY/TMDAY) and an existing date/time (JULDAY/TIMDAY). It handles different year formats and adjusts for leap years.

      ## Code Overview
      The NTIME subroutine uses mathematical and logical operations to:
      - Adjust two-digit year formats by adding 1900.
      - Determine whether the input date and the existing date span different years.
      - Compute the day difference (accounting for leap years) and the time difference in seconds.
      - Combine these differences to generate the output time difference variable (TRAIN).

      <details>
      <summary>Section Navigation</summary>

      - [Introduction](#introduction)
      - [Code Overview](#code-overview)
      - [Detailed Code Explanation](#detailed-code-explanation)
      - [References](#references)
      - [Appendices](#appendices)

      </details>

      ## Detailed Code Explanation
      Below is the NTIME subroutine with clear indentation, descriptive comments, and Fortran syntax highlighting.

      ```fortran
            SUBROUTINE NTIME(JDAY,TMDAY,TRAIN)
      C#######################################################################
      C     CALCULATE THE TIME DIFFERENCE IN SECONDS BETWEEN JDAY AND TMDAY
      C     AND THE EXISTING JULDAY AND TIMDAY
      C
      C     JDAY   = JULIAN DAY (input parameter)
      C     TMDAY  = TIME OF DAY IN SECONDS (input parameter)
      C     TRAIN  = TIME DIFFERENCE IN SECONDS (+ or -) (output parameter)
      C              TRAIN is positive if JDAY/TMDAY > JULDAY/TIMDAY.
      C#######################################################################
            INCLUDE 'STIMER.INC'
            JYEAR = JDAY/1000
            IYEAR = JULDAY/1000
            LDAY  = JDAY   - JYEAR*1000
            KDAY  = JULDAY - IYEAR*1000
            
            IF (JYEAR.LT.100) THEN
      C           Adjust two-digit year format for JDAY and recalculate
                 JYEAR = JYEAR + 1900
      C           Recalculate JDAY with the updated year value
                 JDAY = LDAY + JYEAR*1000
            ENDIF
            
            IF (IYEAR.LT.100) THEN
      C           Adjust two-digit year format for the existing date
                 IYEAR = IYEAR + 1900
                 JULDAY = KDAY + IYEAR*1000
            ENDIF
            
            JDIFF = 0
            
      C=======================================================================
      C     HANDLE SCENARIOS WHERE IYEAR AND JYEAR ARE DIFFERENT YEARS
      C=======================================================================
            IF(JYEAR.GT.IYEAR) THEN
                 IY    = IYEAR  + 1
                 JY    = JYEAR  - 1
                 IF(JY.GE.IY) THEN
                         DO 100 KYEAR = IY,JY
                             ID = 365
                             IF(MOD(KYEAR,4).EQ.0) ID = 366
                             JDIFF = JDIFF + ID
       100               CONTINUE
                 ENDIF
                 JDIFF = JDIFF + LDAY
                 ID = 365
                 IF(MOD(IYEAR,4).EQ.0) ID = 366
                 JDIFF = JDIFF + ID - KDAY
            ENDIF
            
      C=======================================================================
      C     HANDLE SCENARIOS WHERE IYEAR AND JYEAR ARE DIFFERENT (INVERSE CASE)
      C=======================================================================
            IF(JYEAR.LT.IYEAR) THEN
                 IY    = IYEAR  - 1
                 JY    = JYEAR  + 1
                 IF(IY.GE.JY) THEN
                         DO 200 KYEAR = JY,IY
                             ID = 365
                             IF(MOD(KYEAR,4).EQ.0) ID = 366
                             JDIFF = JDIFF - ID
       200               CONTINUE
                 ENDIF
                 JDIFF = JDIFF - KDAY
                 ID = 365
                 IF(MOD(JYEAR,4).EQ.0) ID = 366
                 JDIFF = JDIFF - ID + LDAY
            ENDIF
            
      C=======================================================================
      C     HANDLE SCENARIOS WHERE IYEAR AND JYEAR ARE THE SAME
      C=======================================================================
            IF(JYEAR.EQ.IYEAR) THEN
                 JDIFF = LDAY - KDAY
            ENDIF
            
      C=======================================================================
      C     CALCULATE THE DIFFERENCE IN SECONDS
      C=======================================================================
            TDIFF = TMDAY - TIMDAY
            TRAIN = FLOAT(JDIFF)*86400.0 + TDIFF
            RETURN
            END
      ```

      ## References
      - [Fortran Language Reference][fortranref] - General information about the Fortran programming language and standards.

      [fortranref]: https://en.wikipedia.org/wiki/Fortran

      ## Appendices

      ### Appendix A: Variable Descriptions
      | Variable | Description                                    | Type    |
      |----------|------------------------------------------------|---------|
      | JDAY     | Input Julian day value                         | Integer |
      | TMDAY    | Input time of day in seconds                   | Integer |
      | TRAIN    | Output time difference in seconds              | Float   |
      | JYEAR    | Calculated year extracted from JDAY            | Integer |
      | IYEAR    | Calculated year extracted from JULDAY          | Integer |
      | LDAY     | Day portion of JDAY                            | Integer |
      | KDAY     | Day portion of JULDAY                          | Integer |

      ### Appendix B: Technical Notes (Collapsible)
      <details>
      <summary>Click to expand detailed technical notes</summary>

      - The subroutine adjusts for two-digit year representations by adding 1900.
      - Leap years are detected by checking if the year modulo 4 equals 0.
      - Day differences are calculated by summing or subtracting the number of days between years.
      - The final time difference in seconds is computed by combining the day difference (converted to seconds) and the raw time difference.

      </details>  JDAY = LDAY + JYEAR*1000
           ENDIF
      IF (IYEAR.LT.100) THEN
           IYEAR = IYEAR + 1900
           JULDAY = KDAY + IYEAR*1000
           ENDIF
      JDIFF = 0
C=======================================================================
C     IYEAR AND JYEAR ARE DIFFERENT YEARS
C=======================================================================
      IF(JYEAR.GT.IYEAR) THEN
           IY    = IYEAR  + 1
           JY    = JYEAR  - 1
           IF(JY.GE.IY) THEN
                   DO 100 KYEAR = IY,JY
                                         ID = 365
                   IF(MOD(KYEAR,4).EQ.0) ID = 366
                   JDIFF = JDIFF + ID
 100               CONTINUE
                   ENDIF
           JDIFF = JDIFF + LDAY
                                 ID = 365
           IF(MOD(IYEAR,4).EQ.0) ID = 366
           JDIFF = JDIFF + ID - KDAY
           ENDIF
C=======================================================================
C     IYEAR AND JYEAR ARE DIFFERENT YEARS
C=======================================================================
      IF(JYEAR.LT.IYEAR) THEN
           IY    = IYEAR  - 1
           JY    = JYEAR  + 1
           IF(IY.GE.JY) THEN
                   DO 200 KYEAR = JY,IY
                                         ID = 365
                   IF(MOD(KYEAR,4).EQ.0) ID = 366
                   JDIFF = JDIFF - ID
 200               CONTINUE
                   ENDIF
           JDIFF = JDIFF - KDAY
                                 ID = 365
           IF(MOD(JYEAR,4).EQ.0) ID = 366
           JDIFF = JDIFF - ID + LDAY
           ENDIF
C=======================================================================
C     IYEAR AND JYEAR ARE THE SAME YEAR
C=======================================================================
      IF(JYEAR.EQ.IYEAR) THEN
                         JDIFF = LDAY - KDAY
                         ENDIF
C=======================================================================
C     CALCULATE DIFFERENCE IN SECONDS
C=======================================================================
      TDIFF = TMDAY - TIMDAY
      TRAIN = FLOAT(JDIFF)*86400.0 + TDIFF
      RETURN
      END
``` 
