```fortran 
      SUBROUTINE GTCOLD(IDO,IGO)
C	TEMP BLOCK
C     CALLED BY TEMP NEAR 110 and 112
C     
C=======================================================================
C     UPDATED 11/92 BY WCH TO CORRECT TYPO AT APPROX. LINE 91
C       AND TO ALLOW NO STATION NUMBER FOR USER-DEFINED INPUT
C     UPDATED 12/92 TO CORRECT DATE PROBLEMS.
C     UPDATED 9/30/93 TO ALLOW ZERO STATION NUMBER AND BETTER END-OF-
C       FILE MESSAGE.  WCH.
C     ENHANCE ERROR MESSAGE FOR READING OF DATA, WCH, 4/18/94.
C     ALTER IOSTAT NUMBER FOR LAHEY, WCH, 8/5/95.
C     READ YEARS AS 4-DIGITS, WCH, 3/28/00.
C     FIX MAJOR UNITS PROBLEM HERE AND IN SUB. TEMP. WCH, 7/27/04.
C     ADD OPTION TO USE ALL KTYPE VALUES FOR USER INPUT. ALLOWS USER
C       TO PROVIDE ANY OR ALL THREE OF TEMP, EVAP, OR WIND. WCH, 7/28/04.
C=======================================================================
C     THIS PROGRAM READS TWO FORMATS
C     IFORM = 0 MEANS POST 1980 FORMAT
C     IFORM = 2 MEANS USER DEFINED FORMAT
C     IDO   = 0 SEARCH FOR STATION NUMBER
C     IDO   = 1 READ STATION DATA
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'TEW.INC'
      # GTCOLD Fortran Routine Documentation

      # GTCOLD Fortran Subroutine Documentation

      This document provides a complete technical description and documentation for the GTCOLD Fortran subroutine. It covers an overview of the subroutine, technical details including code fragments, variable declarations, control flow, and error handling. Supporting materials such as a table of contents, collapsible sections, and reference-style links are included to maintain a clear and precise technical record.

      ---

      ## Table of Contents

      1. [Introduction](#introduction)
      2. [Subroutine Overview](#subroutine-overview)
            - [Purpose and Functionality](#purpose-and-functionality)
            - [Code Structure](#code-structure)
      3. [Technical Details](#technical-details)
            - [Variable Declarations](#variable-declarations)
            - [Control Flow & Logic](#control-flow--logic)
            - [Error Handling](#error-handling)
      4. [Code Listings](#code-listings)
            - [Fortran Routine Header](#fortran-routine-header)
            - [Key Code Fragments](#key-code-fragments)
      5. [Section Navigation](#section-navigation)
      6. [References](#references)
      7. [Appendices](#appendices)
            - [Collapsible Additional Technical Notes](#collapsible-additional-technical-notes)
            - [Table of Key Sections](#table-of-key-sections)

      ---

      ## Introduction

      The subroutine `GTCOLD` is designed to process temperature, evaporation, and wind data from an input file using two data formats:
      - **Format 0**: The post-1980 default format.
      - **Format 2**: A user-defined format.

      This document details the evolution of the subroutine, modifications made over time, and methods used for error handling and data backspacing.

      ---

      ## Subroutine Overview

      ### Purpose and Functionality

      - **Data Reading**: Supports two distinct formats for reading input data.
      - **Date Handling**: Adjusts 2-digit year entries to 4-digit format where necessary.
      - **Error Handling**: Uses backspacing and IO status checks to validate data and handle input errors.
      - **Station Verification**: Confirms the station number and date accuracy prior to processing further data.

      ### Code Structure

      - **Conditional Branches**: Decisions based on flags such as `IDO`, `IFORM`, and `KTYPE`.
      - **Loops**: Iterations over the 31 days of each month.
      - **Read Statements**: Employ different formats and handle possible errors through defined error codes.

      ---

      ## Technical Details

      ### Variable Declarations

      The following variables are declared for use in the subroutine:

      ```fortran
      INTEGER  DAY, YEAR, STA, FF(8), IVALUE(31)
      ```

      - `DAY`, `YEAR`, `STA`: Track the day, year, and station number.
      - `FF(8)`: Used for storing floating-point values temporarily.
      - `IVALUE(31)`: Array for storing daily records.

      ### Control Flow & Logic

      The subroutine employs:
      - **Conditional Reads**: The `IF(IDO.EQ.0)` and `IF(IDO.EQ.1)` branches process station data and daily value readings, respectively.
      - **Loop Constructs**: Iterates to process each day in a month using loops.
      - **Backspacing**: For error correction by repositioning the file pointer if data does not match expectations.
      - **Conditional Checks**: For example, adjusting years below 100 by adding 1900 and verifying if station numbers match.

      ### Error Handling

      Error conditions are checked using:
      - **IOSTAT values**: To determine the success of a read operation.
      - **Custom Error Messages**: Printed using formatted WRITE statements when an error condition is encountered.
      - **Example Error Format**:

      ```fortran
       2000 FORMAT(/,' ===> ERROR !!  READING THE INPUT FILE, UNIT',I3,/,
             1        '  LAHEY ERROR NUMBER =',I5,/,
             2        '  EXECUTION STOPS FROM TEMP BLOCK.')
      ```

      ---

      ## Code Listings

      ### Fortran Routine Header

      Below is the initial part of the Fortran code, which includes subroutine declaration, comments, and inclusion of necessary files:

      ```fortran
              SUBROUTINE GTCOLD(IDO,IGO)
      C  TEMP BLOCK
      C  CALLED BY TEMP NEAR 110 and 112
              INCLUDE 'TAPES.INC'
              INCLUDE 'TEW.INC'
              # GTCOLD Fortran Routine Documentation
      ```

      ### Key Code Fragments

      The following code snippet demonstrates how the subroutine handles different input formats and error checking:

      ```fortran
              IF(IDO.EQ.0)   THEN
                   IF(IFORM.EQ.0) THEN
                        READ(IO,1000,ERR=60,END=440,IOSTAT=IOS) IBUF,ELMTYP,
             +              NEWYR,NEWMON
                        IF(NEWYR.LT.100) NEWYR = NEWYR + 1900
                        IF(ISTA.GT.0.AND.IBUF.NE.ISTA) GO TO 2
                        BACKSPACE IO
                        ISTA = IBUF
                   ENDIF
                   IF(IFORM.EQ.2) THEN
                        NUMPAR = 6
                        IF(KTYPE.EQ.1.OR.KTYPE.EQ.2) NUMPAR = NUMPAR - 1
                        IF(KTYPE.EQ.3.OR.KTYPE.EQ.4) NUMPAR = NUMPAR + 1
                        IF(KTYPE.EQ.6)               NUMPAR = NUMPAR + 2
                        IF(F1.EQ.0)                  NUMPAR = NUMPAR - 1
                        READ(IO,FIRMAT,ERR=60,END=440,IOSTAT=IOS) 
             +             (FF(I),I=1,NUMPAR)
                        IF(F1.NE.0) IBUF = FF(F1)
                        NEWYR = FF(F2)
                        IF(NEWYR.LT.100) NEWYR = NEWYR + 1900
                        NEWMON = FF(F3)
                        NEWDAY = FF(F4)
                        BACKSPACE IO
                   ENDIF
                   RETURN
      ```

      *Note: Additional fragments cover processing of daily records, unit conversions, and final error reporting.*

      ---

      ## Section Navigation

      - [Introduction](#introduction)
      - [Subroutine Overview](#subroutine-overview)
      - [Technical Details](#technical-details)
      - [Code Listings](#code-listings)
      - [References](#references)
      - [Appendices](#appendices)

      ---

      ## References

      - [Fortran Documentation][fortran-doc]
      - [SWMM Model Technical Reference](https://www.epa.gov/water-research/storm-water-management-model-swmm)

      [fortran-doc]: https://gcc.gnu.org/onlinedocs/gfortran/ "GFortran Documentation"

      ---

      ## Appendices

      ### Collapsible Additional Technical Notes

      <details>
        <summary><strong>Additional Technical Notes</strong></summary>
        
        - The subroutine includes legacy updates from 1992 through 2004.
        - It converts 2-digit years to proper 4-digit years for consistency.
        - Unit conversions and error messages are dynamically handled.
        - Detailed comment lines annotate changes and logic at each update iteration.
      </details>

      ### Table of Key Sections

      | Section               | Description                                                        |
      | --------------------- | ------------------------------------------------------------------ |
      | Variable Declarations | Overview of variables and their roles in subroutine processing       |
      | Control Flow          | Detailed flow of logic including loops, conditionals, and reads      |
      | Error Handling        | Formats and message structures for managing file read errors          |

      ---

      ## Table of Contents

      1. [Introduction](#introduction)
      2. [Code Overview](#code-overview)
            - [Subroutine Purpose](#subroutine-purpose)
            - [Code Structure](#code-structure)
      3. [Technical Details](#technical-details)
            - [Variable Declarations](#variable-declarations)
            - [Control Flow](#control-flow)
            - [Fortran Code Blocks](#fortran-code-blocks)
      4. [Section Navigation](#section-navigation)
      5. [References](#references)
      6. [Appendices](#appendices)

      ---

      ## Introduction

      The subroutine `GTCOLD` has been developed to process temperature, evaporation, and wind data from a file. It has been updated over time to fix typos and improve error handling. This documentation provides a detailed markdown conversion preserving the original technical accuracy.

      For further details on Fortran programming, see [Fortran Documentation][fortran-doc].

      ---

      ## Code Overview

      ### Subroutine Purpose

      The main purpose of the subroutine is to:
      - **Read two data formats**:
        - Format 0: Post-1980 default format.
        - Format 2: User-defined format.
      - **Handle variable station numbers and dates.**
      - **Process temperature, evaporation, and wind data** and perform necessary unit conversions.

      ### Code Structure

      The code uses:
      - **Conditional branches** to select record reading based on provided flags.
      - **Loop structures** to iterate over days in a month.
      - **Error handling** using IO status and backspace operations.

      ---

      ## Technical Details

      ### Variable Declarations

      The following snippet shows the declaration of key integer variables necessary for data processing:

      ```fortran
      INTEGER  DAY, YEAR, STA, FF(8), IVALUE(31)
      ```

      This segment defines:
      - `DAY`, `YEAR`, and `STA`: Used for date and station number tracking.
      - `FF(8)`: An array to temporarily store floating-point values.
      - `IVALUE(31)`: An array that holds daily record values for the month.

      ### Control Flow

      The control flow in `GTCOLD` relies on:
      - **INITIAL checks of the IFORM flag** to decide the format.
      - **Conditional READ statements** that divert the process to different branches using `GO TO`.
      - **Backspacing operations** to re-read data when validation fails.
      - **Error handling** through dedicated error message formatting and IO status checks.

      ### Fortran Code Blocks

      Below is an example of the Fortran code block for reading the integer declarations:

      ```fortran
      INTEGER  DAY, YEAR, STA, FF(8), IVALUE(31)
      ```

      Other code block examples, including main subroutine and error handling, are similarly documented throughout the file.

      ---

      ## Section Navigation

      - [Introduction](#introduction)
      - [Code Overview](#code-overview)
      - [Technical Details](#technical-details)
      - [References](#references)
      - [Appendices](#appendices)

      ---

      ## References

      - [Fortran Documentation][fortran-doc]
      - [SWMM Model Technical Reference](https://www.epa.gov/water-research/storm-water-management-model-swmm)

      ---

      ## Appendices

      ### Collapsible Details

      <details>
        <summary><strong>Additional Technical Notes</strong></summary>
        
        - The subroutine includes legacy code updates from 1992 through 2004.
        - Unit conversion is conditionally applied after data collection.
        - Detailed error reporting is provided with specific LAHEY error numbers.
        
      </details>

      ### Tables in MD Format

      | Section           | Description                                                   |
      | ----------------- | ------------------------------------------------------------- |
      | Variable Decls    | Declarations of integers and arrays                          |
      | Control Flow      | Handling of data reading and conditional jumps               |
      | Error Handling    | Procedures for error message formatting and IO backspacing     |

      ### Image Placeholder

      ![Fortran Code Diagram](https://via.placeholder.com/600x200 "Fortran Code Diagram")

      ---

      ## References

      [fortran-doc]: https://gcc.gnu.org/onlinedocs/gfortran/ "GFortran Documentation"

      CHARACTER ELMTYP*4,LERR*30
C=======================================================================
      IF(IDO.EQ.0)   THEN
      IF(IFORM.EQ.0) THEN
C#### WCH, 4/18/94.  ADD IOSTAT
2              READ(IO,1000,ERR=60,END=440,IOSTAT=IOS) IBUF,ELMTYP,
     +              NEWYR,NEWMON
               IF(NEWYR.LT.100) NEWYR = NEWYR + 1900
C#### WCH, 9/30/93.  ALLOW ZERO STATION NUMBER OPTION HERE TOO.
               IF(ISTA.GT.0.AND.IBUF.NE.ISTA) GO TO 2
               IF(IYBEG(1).NE.0.AND.NEWYR.LT.IYBEG(1))  GO TO 2
               IF(IYBEG(2).NE.0.AND.NEWMON.LT.IYBEG(2)) GO TO 2
               BACKSPACE IO
               ISTA = IBUF
               ENDIF
C
      IF(IFORM.EQ.2) THEN
C#######################################################################
C WCH, 11/92  CORRECT TO ALLOW NO STATION NO. ENTERED IF F1=0,
C               AS IN DOCUMENTATION
C#######################################################################
               NUMPAR = 6
               IF(KTYPE.EQ.1.OR.KTYPE.EQ.2) NUMPAR = NUMPAR - 1
	         IF(KTYPE.EQ.3.OR.KTYPE.EQ.4) NUMPAR = NUMPAR + 1
	         IF(KTYPE.EQ.6)               NUMPAR = NUMPAR + 2
               IF(F1.EQ.0)                  NUMPAR = NUMPAR - 1
C#### WCH, 4/18/94.  ADD IOSTAT
4              READ(IO,FIRMAT,ERR=60,END=440,IOSTAT=IOS) 
     +             (FF(I),I=1,NUMPAR)
               IF(F1.NE.0) IBUF = FF(F1)
               NEWYR  = FF(F2)
C#### WCH, 12/92.  NEED 2-DIGIT YEAR.
c              IF(NEWYR.GT.1900) NEWYR = NEWYR - 1900
               IF(NEWYR.LT.100)  NEWYR = NEWYR + 1900
               NEWMON = FF(F3)
               NEWDAY = FF(F4)
               IF(F1.NE.0.AND.IBUF.NE.ISTA) GO TO 4
               IF(IYBEG(1).NE.0.AND.NEWYR.LT.IYBEG(1))  GO TO 4
               IF(IYBEG(2).NE.0.AND.NEWMON.LT.IYBEG(2)) GO TO 4
               IF(IYBEG(3).NE.0.AND.NEWDAY.LT.IYBEG(3)) GO TO 4
               BACKSPACE IO
               ENDIF
      RETURN
C#### WCH, 9/30/93.  ADD ALTERNATE END OF FILE MESSAGE.
  440 WRITE(N6,9440) IO,ISTA,IBUF
      STOP
C=======================================================================
C  ENDIF FOR IDO = 0
C=======================================================================
      ENDIF
C
      IF(IDO.EQ.1) THEN
 7    BACKSPACE IO
      CALL SETZER(BARAY,12,31,-99.0)
      CALL SETZER(SARAY,12,31,-99.0)
      CALL SETZER(EARAY,12,31,-99.0)
      CALL SETZER(WARAY,12,31,-99.0)
C
10    CONTINUE
      IF(IFORM.EQ.0) THEN
C#### WCH, 4/18/94.  ADD IOSTAT
         READ(IO,1000,END=40,ERR=60,IOSTAT=IOS) STA,ELMTYP,
     +             YEAR,MONTH,(IDUM,IDUM,IVALUE(J),J=1,31)
         IF (YEAR.LT.100) YEAR = YEAR + 1900
C
         IF(KTYPE.EQ.0.AND.(ELMTYP.EQ.'TMIN'.OR.ELMTYP.EQ.'TMAX'))
     +              GO TO 15
      IF(KTYPE.EQ.1.AND.ELMTYP.EQ.'EVAP') GO TO 15
      IF(KTYPE.EQ.2.AND.ELMTYP.EQ.'WDMV') GO TO 15
      IF(KTYPE.EQ.3.AND.(ELMTYP.EQ.'TMIN'.OR.ELMTYP.EQ.'TMAX'.
     +                          OR.ELMTYP.EQ.'EVAP')) GO TO 15
      IF(KTYPE.EQ.4.AND.(ELMTYP.EQ.'TMIN'.OR.ELMTYP.EQ.'TMAX'.
     +                          OR.ELMTYP.EQ.'WDMV')) GO TO 15
      IF(KTYPE.EQ.5.AND.(ELMTYP.NE.'EVAP'.OR.ELMTYP.EQ.'WDMV'))GO TO 15
      IF(KTYPE.EQ.6.AND.(ELMTYP.EQ.'TMIN'.OR.ELMTYP.EQ.'TMAX'.
     +              OR.ELMTYP.EQ.'EVAP'.OR.ELMTYP.EQ.'WDMV')) GO TO 15
         GO TO 30
C
  15     CONTINUE
         DO 200 K = 1,31
         IF(IVALUE(K).EQ.-9999) GO TO 200
         IF(ELMTYP.EQ.'EVAP') EARAY(MONTH,K) = FLOAT(IVALUE(K))
         IF(ELMTYP.EQ.'WDMV') WARAY(MONTH,K) = FLOAT(IVALUE(K))
         IF(ELMTYP.EQ.'TMIN') SARAY(MONTH,K) = FLOAT(IVALUE(K))
         IF(ELMTYP.EQ.'TMAX') BARAY(MONTH,K) = FLOAT(IVALUE(K))
  200    CONTINUE
         ENDIF
C
      IF(IFORM.EQ.2) THEN
C#### WCH 11/92
C#### WCH, 4/18/94.  ADD IOSTAT
Cwch, 7/28/04. Allow reading of temperature and/or evap and/or wind.
         READ(IO,FIRMAT,END=40,ERR=60,IOSTAT=IOS) (FF(I),I=1,NUMPAR)
         IF(F1.NE.0) STA = FF(F1)
         YEAR  = FF(F2)
C#### WCH, 12/92.  NEED 2-DIGIT YEAR.
c         IF(YEAR.GT.1900) YEAR = YEAR - 1900
          IF(YEAR.LT.100) YEAR = YEAR + 1900
         MONTH = FF(F3)
         DAY   = FF(F4)
Cwch, 7/28/04         MAXU  = FF(F5)
         IF(MONTH.LE.0.OR.MONTH.GT.12) GO TO 30
         IF(DAY.LE.0.OR.DAY.GT.31)     GO TO 30
Cwch, 7/28/04. More complicated IF statements for multiple input options.
Cwch	   MINU  = FF(F6)
C         IF(KTYPE.EQ.0) BARAY(MONTH,DAY) = FLOAT(MAXU)
C         IF(KTYPE.EQ.0) SARAY(MONTH,DAY) = FLOAT(MINU)
C         IF(KTYPE.EQ.1) EARAY(MONTH,DAY) = FLOAT(MAXU)*CONV
C         IF(KTYPE.EQ.2) WARAY(MONTH,DAY) = FLOAT(MAXU)*CONV
         IF(KTYPE.EQ.0.OR.KTYPE.EQ.3.OR.KTYPE.EQ.4.OR.KTYPE.EQ.6) THEN
              BARAY(MONTH,DAY) = FLOAT(FF(F5))
	        SARAY(MONTH,DAY) = FLOAT(FF(F6))
	        ENDIF
	   IF(KTYPE.EQ.1.OR.KTYPE.EQ.3.OR.KTYPE.EQ.5.OR.KTYPE.EQ.6)
     1        EARAY(MONTH,DAY) = FLOAT(FF(F7))*CONV
	   IF(KTYPE.EQ.2.OR.KTYPE.GE.4)
     1        WARAY(MONTH,DAY) = FLOAT(FF(F8))*CONV
Cwch, 7/2704. Save units conversion for after print-out. 
C     Convert in Sub. TEMP while placing on interface file.
C         IF(KUNIT.EQ.1.AND.KTYPE.EQ.0) THEN
C                   BARAY(MONTH,DAY) = 1.8*BARAY(MONTH,DAY) + 32.0
C#####
C WCH, 11/92 CHANGE BARAY TO SARAY
C#####
C                   SARAY(MONTH,DAY) = 1.8*SARAY(MONTH,DAY) + 32.0
C                   ENDIF
         ENDIF
C=======================================================================
C     SKIP IF THE NEW STATION IS NOT THE STATION SELECTED
C     RETURN IF THE NEW YEAR IS LESS THAN THE SELECTED ENDING YEAR
C=======================================================================
30    CONTINUE
C#######################################################################
C  WCH, 12/92  RETURN IF LAST DAY OF YEAR.
C  ALSO CHECK FOR NO ADDITIONAL RECORDS.
C  END = 40 IN READ MEANS THIS IS LAST YEAR IF REACH END OF FILE.
C#######################################################################
      IF(IFORM.EQ.0) DAY = 31
      IF(MONTH.EQ.12.AND.DAY.EQ.31)  THEN
C#### WCH, 4/18/94.  ADD IOSTAT
             IF(IFORM.EQ.0) READ(IO,1000,END=40,ERR=60,IOSTAT=IOS) 
     +             STA,ELMTYP,YEAR,MONTH,(IDUM,IDUM,IVALUE(J),J=1,31)
             IF (YEAR.LT.100) YEAR = YEAR + 1900
C#### WCH, 4/18/94.  ADD IOSTAT
             IF(IFORM.EQ.2) READ(IO,FIRMAT,END=40,ERR=60,IOSTAT=IOS) 
     +             (FF(I),I=1,NUMPAR)
             IF (FF(F2).LT.100) FF(F2) = FF(F2) + 1900
C  NOTE, IO BACKSPACED AT BEGINNING OF LOOP FOR IDO = 1 (STATEMENT 7).
             RETURN
             ENDIF           
C#### WCH, 11/92
      IF(IFORM.EQ.2.AND.F1.EQ.0) GO TO 33
      IF(STA.NE.ISTA)                            GO TO 40
C#######################################################################
C  WCH, 12/92 CORRECT WRONG SUBSCRIPTS FOR YEAR AND MONTH
C  (3) = DAY, (2) = MONTH, (1) = YEAR
C  ALSO, ALTER THE CHECK FOR ENDING DATE TO CHECK FOR EXACT 
C    DATE FOR IFORM = 2.
C#######################################################################
33    IF(IFORM.EQ.2) THEN
        IF(YEAR.EQ.IYEND(1).AND.MONTH.EQ.IYEND(2).AND.DAY.EQ.IYEND(3)) 
     *                                           GO TO 40
        ENDIF
      IF(IFORM.EQ.0) THEN
        IF(YEAR.EQ.IYEND(1).AND.MONTH.GE.IYEND(2)) GO TO 40
        ENDIF
      IF(IYEND(1).NE.0.AND.YEAR.GT.IYEND(1))     GO TO 40
      IF(YEAR.GT.NEWYR)                          RETURN
      GO TO 10
C=======================================================================
C     ENDIF FOR IDO = 1
C=======================================================================
      ENDIF
C=======================================================================
40    IGO = 1
      RETURN
C=======================================================================
C     ERROR MESSAGE FOR READING INPUT FILE.
C=======================================================================
C#### WCH, 4/18/94.  ADD IOSTAT
C#### WCH, 8/5/95.  ALTER IOS FOR LAHEY.
60    WRITE(N6,2000) IO,MOD(IOS,256)
      WRITE(*,2000)  IO,MOD(IOS,256)
      WRITE(*,2001)
      BACKSPACE IO
      READ(IO,3000)  LERR
      WRITE(N6,3001) LERR
      STOP
C=======================================================================
Cwch, 3/28/00.  Read year as 4 digits.
C1000 FORMAT(3X,I6,2X,A4,4X,I2,I2,7X,31(2I2,I6,2X))
 1000 FORMAT(3X,I6,2X,A4,2X,I4,I2,7X,31(2I2,I6,2X))
C#### WCH, 4/18/94.  ADD IOSTAT
C#### WCH, 8/5/95.  ALTER IOSTAT NUMBER FOR LAHEY.
 2000 FORMAT(/,' ===> ERROR !!  READING THE INPUT FILE, UNIT',I3,/,
     1        '  LAHEY ERROR NUMBER =',I5,/,
     2        '  EXECUTION STOPS FROM TEMP BLOCK.')
C#### WCH, 4/18/94
 2001 FORMAT ('  SEE OUTPUT FILE FOR MORE INFORMATION.')
 3000 FORMAT(A30)
 3001 FORMAT(/,' FIRST 30 CHARACTERS OF THE LAST LINE READ ARE: ',/,
     +         3X,A30 )
C#### WCH, 9/30/93.
 9440 FORMAT (' ERROR FROM TEMP BLOCK.  END OF FILE REACHED ON UNIT',
     1I3,/,' EITHER ISTA > 0 AND CAN''T FIND MATCHING STATION NUMBER ON 
     2DATA FILE OR ELSE CAN''T FIND CORRECT DATES.',/,' ISTA =',I8,
     3 ' STA. NO. ON DATA FILE =',I8,/,' RUN STOPPED FROM TEMP BLOCK.')
C=======================================================================
      END
``` 
