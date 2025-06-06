```fortran 
      SUBROUTINE GTRAIN(IDO,IGO,ILOST,M4)
C     RAIN BLOCK
C     CALLED BY RAIN BLOCK AT NUMEROUS LOCATIONS
C#######################################################################
C     This program reads 15 versions of precipitation files.
C     WCH, 3/2/93.  Precautionary IF statement if IHR = 25.
C     WCH, 8/93.  Add option for recent EarthInfo ASCII files in
C       Subroutine G9RAIN and misc. corrections.
C     WCH, 8/93. Add code to ensure 15-min. data are properly placed
C       on interface file.
C     WCH, 10/11/93.  Revise input format for IFORM = 4 or 6 for 15-min.
C       rainfall.  Include calc. of JULDAY when necessary.
C     WCH, 4/25/94.  Correct computation of Julian date for 15-min.
C       data for IFORM = 0 and 1.
C     WCH, 4/26/94.  Major code modifications to permit averaging of
# GTRAIN Fortran Code Documentation

## Table of Contents
1. [Introduction](#introduction)
2. [Overview of GTRAIN Subroutine](#overview-of-gtrain-subroutine)
      - [Data Input Formats](#data-input-formats)
3. [NCDC Accumulated Rainfall Section](#ncdc-accumulated-rainfall-section)
      - [Special Codes](#special-codes)
4. [Reference-Style Links](#reference-style-links)
5. [Tables and Images](#tables-and-images)
6. [Collapsible Sections and Navigation](#collapsible-sections-and-navigation)
7. [Appendices](#appendices)
8. [References](#references)

## Introduction
This document provides a detailed technical overview of the GTRAIN Fortran subroutine which manages rainfall data processing across multiple formats. The emphasis in this section is on handling NCDC accumulated rainfall and listing the special codes present in the data.

## Overview of GTRAIN Subroutine
The subroutine GTRAIN is designed to read and process precipitation data from various formats including but not limited to NWS fixed-length records, variable-length records, EarthInfo ASCII data, and AES formats. Structured error handling and comprehensive format detection are core components of its design.

### Data Input Formats
- **IFORM = 0:** NWS Post-1980 (TD-3240 fixed-length format)
- **IFORM = 1:** NWS Post-1980 Variable Length Format
- **IFORM = 2:** NWS Pre-1980 Format
- **IFORM = 4 / 6:** NCDC Condensed / EarthInfo Display Formats
- **IFORM = 5, 13, 14, 15:** AES Precipitation Data Formats
- **IFORM = 7 / 8:** EarthInfo Precipitation Data Formats

Each format is treated with specific routines to handle data conversion, time calculation, and error-checking.

## NCDC Accumulated Rainfall Section
### Special Codes
This section describes the handling of accumulated rainfall for NCDC formats. The subroutine processes special codes in the data stream to indicate various conditions such as missing values and meter malfunctions.

**Key Technical Details:**
- **Accumulated Data:** The code computes accumulated rainfall values using internal conversion routines.
- **Special Codes:**
  - `-1` indicates missing precipitation data.
  - `-2` indicates a meter fault or accumulated error.
- **Time Handling:** Both hourly (24 intervals) and 15-minute (96 intervals) data points are supported with robust time conversions.
- **Subroutine Calls:** The routine `RAINCD` is employed throughout to standardize the handling of special codes.

#### Example Fortran Code Snippet
```fortran
            IF(IFORM.EQ.2) THEN
                  IF(XRA(J).EQ.BLK) THEN
                        HOUR(IDAY,J+J1) = 0
                        GO TO 35
                  ENDIF
                  HOUR(IDAY,J+J1) = INTCHR(XRA(J))
35       CONTINUE
            ENDIF
```
*This snippet, part of the NCDC data handling, illustrates how special marker codes are interpreted and converted.*

## Reference-Style Links
For further reading and more detailed technical discussions, please refer to the following:
- [Fortran Programming Resources][fortran]
- [SWMM Model Documentation][swmm]

[fortran]: https://fortran-lang.org "Fortran Language Home"
[swmm]: https://www.epa.gov/water-research/storm-water-management-model-swmm "SWMM Documentation"

## Tables and Images
### Input Formats Overview

| Input Format | Description                                               | Notes                             |
|--------------|-----------------------------------------------------------|-----------------------------------|
| IFORM = 0    | NWS Post-1980 TD-3240 Fixed-Length Format                 | Y2K issues may be present         |
| IFORM = 1    | NWS Post-1980 Variable Length Format                      | Used for 15-minute data           |
| IFORM = 2    | NWS Pre-1980 Format                                       | Not Y2K compliant                 |
| IFORM = 4/6  | NCDC Condensed / EarthInfo Formats                        | Special corrections applied       |
| IFORM = 5    | AES Hourly Data                                           | Requires unit conversion          |
| IFORM = 15   | AES 15-Minute Data                                        | Utilizes special averaging scheme |

**Image Example:**

![GTRAIN Flowchart](https://via.placeholder.com/600x400 "Flowchart of GTRAIN Data Processing")

## Collapsible Sections and Navigation
<details>
  <summary>Detailed Troubleshooting Steps</summary>
  
  **Steps:**
  
  1. **Validation:** Ensure input files conform to the specified format.
  2. **Station Check:** Verify that station IDs match between data and expected configuration.
  3. **Time Handling:** Confirm that the computed time intervals match the data’s intended granularity.
  4. **Error Monitoring:** Use IOSTAT and error formats (e.g., 9700, 9600) to diagnose read errors.
  
  Use the above steps for swift navigation within the troubleshooting process.
</details>

## Appendices
### Appendix A: Error Format Descriptions
The subroutine defines multiple error messages to handle:
- **IO Errors:** Specific formats (e.g., 9700, 9600) capture file read errors with IOSTAT details.
- **Data Integrity:** Checks for valid day/month/year calculations and station consistency.

### Appendix B: Conversion Factors
- **Rainfall Conversion:** Raw values are converted to inches per hour.
- **Time Conversion:** HHMM values are parsed and converted to seconds for accurate interval calculations.

## References
1. [Fortran Language Home](https://fortran-lang.org)
2. [SWMM Model Documentation](https://www.epa.gov/water-research/storm-water-management-model-swmm)
3. [EarthInfo Data Documentation](https://www.example.com/earthinfo-docs)

## Conclusion
The GTRAIN subroutine is a comprehensive solution for processing diverse rainfall data formats. Its meticulous error handling and detailed treatment of accumulated rainfall ensure reliable operation and robust data integrity across various application scenarios.

C     WCH, 5/24/94.  Fix subscript for FLAG1 for IFORM=1 and remove
C       ILOOP (no discernable purpose).
C     WCH, 8/1/95.  Change all station IDs to character from integer.
C       Also, add new AES format.  No longer compare requested ID with
C       ID read on data lines to avoid character comparison
C       complications.
C     WCH, 8/4/95. Alter IOSTAT number for Lahey.
C     WCH, 10/2/96.  Add better error path for initial reads of files.
C     WCH, 12/5/96.  Correct the logic for starting at indicated
C       beginning year when IFORM = 4 or 6.
C     WCH, 12/6/96.  Fix averaging process for 15-min. rainfall
C       for special combinations of hour/minute.
C     WCH, 11/9/99.  Read year as 4 digits to fix Y2K in several spots. 
C     WCH, 11/22/99. Add Y2K compatibility for AES data.  
C     WCH, 2/9/01.  Reset value of NEND every call to GTRAIN.
C        Suggested by Nerkez Gavranovic, Sydney, Australia
C     WCH, 7/28/04. Add option for unformatted read for IFORM=1.
C#######################################################################
C     IFORM = 0 NWS POST 1980 TD-3240 FIXED LENGTH FORMAT
C     IFORM = 1 NWS POST 1980 TD-3240/3260 VARIABLE LENGTH FORMAT
C     IFORM = 2 NWS PRE-1980 FORMAT
C			  Note, IFORM = 2 is CANNOT be Y2K compatible.  
C     IFORM = 3 USER-SPECIFIED INPUT FORMAT AND FILE (SUB. G3RAIN)
C     IFORM = 4 NWS MICROCOMPUTER FORMAT WITH QUOTATION MARKS (RELEASE
C               B, CONDENSED)
C               Note, IFORM = 4 CANNOT be Y2K compatible. 
C     IFORM = 5 HOURLY AES PRECIPITATION DATA (OLDER COMPRESSED FORMAT)
C               Note, IFORM = 5 CANNOT be Y2K compatible.
C     IFORM = 6 NWS MICROCOMPUTER FORMAT (IFORM=4) WITH APOSTROPHES
C     IFORM = 7 EARTH INFO NCDC  DISPLAY COMPUTER FORMAT.
C     IFORM = 8 EARTH INFO ASCII DISPLAY COMPUTER FORMAT.
C         THE FOLLOWING FOUR OPTIONS ARE IN SUBROUTINE G9RAIN:
C     IFORM = 9 EARTH INFO UNPROCESSED ASCII OUTPUT FILE - 1993, 15-MIN.
C     IFORM =10 EARTH INFO UNPROCESSED ASCII OUTPUT FILE - 1993, HOURLY.
C     IFORM =11 EARTH INFO PROCESSED ASCII FILE - 1993, 15-MIN.
C     IFORM =12 EARTH INFO PROCESSED ASCII FILE - 1993, HOURLY.
C
C     IFORM =13 AES PRECIPITATION DATA (NEWER FORMAT, WITH FLAGS).
C               Note, IFORM = 13 CANNOT be Y2K compatible.
C     IFORM =14 HOURLY AES PRECIP. DATA, NEWER FORMAT WITH 4-DIGIT YR.
C     IFORM =15 15-MIN. AES PRECIP. DATA, NEWER FORMAT WITH 4-DIGIT YR.
C=======================================================================
C     IDO   = 0 SEARCH FOR STATION NUMBER
C     IDO   = 1 READ STATION RAINFALL
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'PRECIP.INC'
C#### WCH, 8/1/95.  CHANGE STA TO CHARACTER.
CWCH, 11/22/99. CHANGE XRAIN TO 96
C####      INTEGER DAY,YEAR,STA,XRAIN(24),IHOUR(100),VALUE(100)
      INTEGER DAY,YEAR,XRAIN(96),IHOUR(100),VALUE(100)
      REAL    YRAIN(24)
      CHARACTER*3 XRA(12),ZIM,BIB,BLK
      CHARACTER STNAME*20,UNITS*2,CH(50)*1,FLAG1(100)*1,FLAG2(100)*1
C#### WCH, 8/1/95.  ADD IBUF AND STA. 7/28/04 +ABUF
      CHARACTER IA*1,IM*1,ICODE*1,ELMTYP*4,SQUOTE*1,IBUF*8,STA*8,ABUF*6,
     1 ABUF2*2,ABUF3,IBUF2*6,STA6*6
C#### WCH, 10/11/93 AND 4/26/94
      CHARACTER*1 M3LINE(50),BLANK,IKODE
      DATA IM/'M'/,IA/'A'/,ZIM/'0- '/,BIB/'   '/,BLK/'-  '/
      DATA SQUOTE/''''/,BLANK/' '/
C=======================================================================
C     First find the correct Rainfall station (IDO = 0)
C=======================================================================
      IF(IFORM.EQ.4) THEN
                     M3 = NSCRAT(3)
                     IF(M3.EQ.0) CALL ERROR(104)
                     ENDIF
      IF(IDO.EQ.0)   THEN
C#### WCH, 8/1/95.
      IPRINT = 0
C=======================================================================
      IF(IFORM.EQ.0) THEN
C#### WCH, 10/2/96.  GO TO 970 ON ERROR, NOT BACK TO SAME STATEMENT.
C#### 2              READ(IO,1000,ERR=2,END=40) IBUF,ELMTYP,
2              READ(IO,1000,ERR=970,END=40,IOSTAT=IOS) IBUF,ELMTYP,
     +                                    NEWYR,NEWMON,NEWDAY
                    IF (NEWYR.LT.100) NEWYR = NEWYR + 1900
C#### WCH, 8/1/95.  PRINT NAME INSTEAD OF TESTING EQUALITY.
C####               IF(IBUF.NE.ISTA) GO TO 2
               IF(IPRINT.EQ.0) THEN
                    WRITE(N6,990) IBUF,ISTA
                    WRITE(*,990) IBUF,ISTA
                    IPRINT = 1
                    ENDIF
C
               IF(IYBEG(1).NE.0.AND.NEWYR.LT.IYBEG(1))  GO TO 2
               IF(NEWMON.LT.IYBEG(2).AND.
     +                            NEWYR.EQ.IYBEG(1))    GO TO 2
               IF(NEWMON.LE.IYBEG(2).AND.NEWDAY.LT.IYBEG(3)
     +                       .AND.NEWYR.EQ.IYBEG(1))    GO TO 2
               IF(ELMTYP.EQ.'QPCP') THISTO = 900.0
               IF(ELMTYP.EQ.'HPCP') THISTO = 3600.0
               ENDIF
C=======================================================================
      IF(IFORM.EQ.1) THEN
C#### WCH, 10/2/96.  GO TO 970 ON ERROR, NOT BACK TO SAME STATEMENT.
Cwch, 7/28/04. Add option for unformatted file for IFORM=1
C 3              READ(IO,1060,ERR=970,END=40,IOSTAT=IOS) IBUF,ELMTYP,
3     IF(IFORM1.EQ.0) READ(IO,1060,ERR=970,END=40,IOSTAT=IOS) IBUF,
     +                                 ELMTYP,NEWYR,NEWMON,NEWDAY
	IF(IFORM1.EQ.1) THEN
C 3123      READ(IO,*,ERR=970,END=3124,IOSTAT=IOS) ABUF
C     Check first for two extraneous lines at top of file.
C	     IF(ABUF.EQ.'COOPID'.OR.ABUF.EQ.'------') THEN
C			  GOTO 3123
C	          ELSE
C	          REWIND IO
C	          ENDIF
           READ(IO,*,ERR=970,END=3124,IOSTAT=IOS) IBUF2,ABUF2,
     +                   ELMTYP,ABUF2,NEWYR,NEWMON,NEWDAY

	     IBUF = IBUF2
	     ENDIF
C
                    IF (NEWYR.LT.100) NEWYR = NEWYR + 1900
C#### WCH, 8/1/95.  PRINT NAME INSTEAD OF TESTING EQUALITY.
C####               IF(IBUF.NE.ISTA) GO TO 3
               IF(IPRINT.EQ.0) THEN
                    WRITE(N6,990) IBUF,ISTA
                    WRITE(*,990) IBUF,ISTA
                    IPRINT = 1
                    ENDIF
C
               IF(IYBEG(1).NE.0.AND.NEWYR.LT.IYBEG(1))  GO TO 3
               IF(NEWMON.LT.IYBEG(2).AND.
     +                            NEWYR.EQ.IYBEG(1))    GO TO 3
               IF(NEWMON.LE.IYBEG(2).AND.NEWDAY.LT.IYBEG(3)
     +                       .AND.NEWYR.EQ.IYBEG(1))    GO TO 3
Cwch, 7/28/04. Can't do 15-min yet with IFORM1=1
               IF(ELMTYP.EQ.'QPCP'.AND.IFORM.EQ.1) THEN
	              WRITE(N6,9123)
	              WRITE(*,9123)
 9123 FORMAT(/,' ERROR. Sorry, cannot yet read 15-min. unformatted data 
     1with IFORM=-1. Run will have to stop.')
	               STOP
	               ENDIF
               IF(ELMTYP.EQ.'QPCP') THISTO = 900.0
               IF(ELMTYP.EQ.'HPCP') THISTO = 3600.0
               ENDIF
C=======================================================================
      IF(IFORM.EQ.2) THEN
C#### WCH, 10/2/96.  GO TO 970 ON ERROR, NOT BACK TO SAME STATEMENT.
4              READ(IO,1040,ERR=970,END=40,IOSTAT=IOS) IBUF,NEWYR,
     1              NEWMON,NEWDAY
               IF (NEWYR.LT.100) NEWYR = NEWYR + 1900
C#### WCH, 8/1/95.  PRINT NAME INSTEAD OF TESTING EQUALITY.
C####               IF(IBUF.NE.ISTA) GO TO 4
               IF(IPRINT.EQ.0) THEN
                    WRITE(N6,990) IBUF,ISTA
                    WRITE(*,990) IBUF,ISTA
                    IPRINT = 1
                    ENDIF
C
               IF(IYBEG(1).NE.0.AND.NEWYR.LT.IYBEG(1))  GO TO 4
               IF(NEWMON.LT.IYBEG(2).AND.
     +                            NEWYR.EQ.IYBEG(1))    GO TO 4
               IF(NEWMON.LE.IYBEG(2).AND.NEWDAY.LT.IYBEG(3)
     +                       .AND.NEWYR.EQ.IYBEG(1))    GO TO 4
C#### WCH, 8/93.
               THISTO = 3600.0
               ENDIF
C=======================================================================
C#### WCH, 8/1/95.
CWCH, 11/22/99. ADD 4-DIGIT YEAR AND 15-MIN. DATA OPTION FOR AES.
      IF(IFORM.EQ.5.OR.IFORM.GE.13) THEN
C#### WCH, 10/2/96.  GO TO 970 ON ERROR, NOT BACK TO SAME STATEMENT.
7              IF(IFORM.LE.13) READ(IO,1045,ERR=970,END=40,IOSTAT=IOS) 
     1             IBUF,NEWYR,NEWMON,NEWDAY
	         IF(IFORM.GE.14) READ(IO,1047,ERR=970,END=40,IOSTAT=IOS)
     1             IBUF,NEWYR,NEWMON,NEWDAY
               IF (NEWYR.LT.100) NEWYR = NEWYR + 1900
C#### WCH, 8/1/95.  PRINT NAME INSTEAD OF TESTING EQUALITY.
C####               IF(IBUF.NE.ISTA) GO TO 7
               IF(IPRINT.EQ.0) THEN
                    WRITE(N6,990) IBUF,ISTA
                    WRITE(*,990) IBUF,ISTA
                    IPRINT = 1
                    ENDIF
C
               IF(IYBEG(1).NE.0.AND.NEWYR.LT.IYBEG(1))  GO TO 7
               IF(NEWMON.LT.IYBEG(2).AND.
     +                            NEWYR.EQ.IYBEG(1))    GO TO 7
               IF(NEWMON.LE.IYBEG(2).AND.NEWDAY.LT.IYBEG(3)
     +                       .AND.NEWYR.EQ.IYBEG(1))    GO TO 7
C#### WCH, 8/93.
               THISTO = 3600.0
CWCH, 11/22/99. ADD 15-MIN OPTION.
			 IF(IFORM.EQ.15) THISTO = 900.0
               ENDIF
C=======================================================================
      IF(IFORM.EQ.7) THEN
                     READ(IO,1050,END=40) YEAR
                     IF(YEAR.GT.1900) THEN
c                                      YEAR = YEAR - 1900
                                      ELSE
                                      BACKSPACE IO
                                      READ(IO,1051,ERR=40,END=40) YEAR
c                                      YEAR = YEAR - 1900
                                      ENDIF
                    IF (YEAR.LT.100) YEAR = YEAR + 1900
                     BACKSPACE IO
                     IBUF   = ISTA
                     THISTO = 3600.0
                     NEWYR  = YEAR
                     ENDIF
C=======================================================================
      IF(IFORM.EQ.8) THEN
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1150,ERR=40,END=40) YEAR
                     BACKSPACE IO
                     IBUF   = ISTA
                     IF (YEAR.LT.100) YEAR = YEAR + 1900
                     NEWYR  = YEAR
                     THISTO = 3600.0
                     ENDIF
C=======================================================================
C     Read Rainfall lines 1 to 5 for IFORM = 4 and IFORM = 6.
C=======================================================================
      IF(IFORM.EQ.4) THEN
C=======================================================================
C                    Open rainfall file
C======================================================================
C#### WCH, 8/93.  PERMIT PROCESSED NSCRAT(3) FILE TO BE SAVED.
            IF(JKP(53).NE.2.AND.M3.GT.0.AND.FFNAME(53).NE.'SCRT3.UF')
     +      OPEN(M3,FILE=FFNAME(53),FORM='FORMATTED',STATUS='UNKNOWN')
            IF(JKP(53).NE.2.AND.M3.GT.0.AND.FFNAME(53).EQ.'SCRT3.UF')
     +      OPEN(M3,FORM='FORMATTED',STATUS='SCRATCH')
                     REWIND M3
                     WRITE(*,1090)
                     DO 2000 J = 1,1000000
                     READ(IO,1080,END=2001) CH
C=======================================================================
C                    Change double quotes " in record to single '
C                    so that character strings can be read by
C                    List-Directed I/O
C=======================================================================
                     DO 30 I = 1,50
                     IF(CH(I).EQ.'"') CH(I) = SQUOTE
   30                CONTINUE
                     WRITE(M3,1080) CH
 2000                CONTINUE
 2001                CONTINUE
                     REWIND M3
                     READ(M3,*,ERR=888) LN
                     READ(M3,*,ERR=888) LN,STNAME
C#### WCH, 8/1/95.  READ STATION AS INTEGER AND CONVERT.
                     READ(M3,*,ERR=888) LN,IISTA
                     WRITE(ISTA,'(I8)') IISTA
C
                     READ(M3,*,ERR=888) LN,NEWYR,NEWMON,IEY,IEM
               IF (NEWYR.LT.100) NEWYR = NEWYR + 1900
                     READ(M3,*,ERR=888) LN,ELMTYP
c                     NEWYR = NEWYR - 1900
                     IF(ELMTYP.EQ.'QPCP') THISTO = 900.0
                     IF(ELMTYP.EQ.'HPCP') THISTO = 3600.0
                     ENDIF
      IF(IFORM.EQ.6) THEN
                     M3 = IO
                     REWIND M3
                     READ(M3,*,ERR=888) LN
                     READ(M3,*,ERR=888) LN,STNAME
C#### WCH, 8/1/95.  READ STATION AS INTEGER AND CONVERT.
                     READ(M3,*,ERR=888) LN,IISTA
                     WRITE(ISTA,'(I8)') IISTA
C
                     READ(M3,*,ERR=888) LN,NEWYR,NEWMON,IEY,IEM
               IF (NEWYR.LT.100) NEWYR = NEWYR + 1900
                     READ(M3,*,ERR=888) LN,ELMTYP
c                     NEWYR = NEWYR - 1900
                     IF(ELMTYP.EQ.'QPCP') THISTO = 900.0
                     IF(ELMTYP.EQ.'HPCP') THISTO = 3600.0
                     ENDIF
C=======================================================================
      ENDIF
C=======================================================================
C====> End of IDO = 0 (Executed only once).
C=======================================================================
Cwch, 2/9/01  Apparently it is possible to lose value of NEND between 
C     calls to GTRAIN.  So reset value of NEND every call.  Move to
C     this location between IDO = 0 and IDO = 1.
C     Suggested by Nerkez Gavranovic, Sydney, Australia
C#######################################################################
C    WCH, 8/93.  CHECK FOR COMPATIBILITY OF HYETOGRAPH TIME INTERVALS.
C#######################################################################
      IF(HIST.GT.0.0) THEN
           IF(ABS(HIST-THISTO).GT.0.1) THEN
                WRITE(N6,9110) THISTO,HIST
                WRITE(*,9110)  THISTO,HIST
                STOP
                ENDIF
           ELSE
           HIST = THISTO
           ENDIF
C#### WCH, 4/26/94.
      NEND = 24
      IF(ABS(HIST-900.0).LE.0.1) NEND = 96
C=======================================================================
C     Read the rainfall values (IDO = 1)
C=======================================================================
C     CLEAR THIS YEAR'S MATRIX
C     BACKSPACE FOR A NEW YEAR OR NEW STATION
C     COMPUTE STARTING DAY NUMBER FOR THIS YEAR
C=======================================================================
      IF(IDO.EQ.1) THEN
C=======================================================================
C     IF 8 < IFORM < 13, DO ALL CALCULATIONS IN SUB. G9RAIN
C=======================================================================
CWCH, 11/22/99. ADD OPTION FOR IFORM .GE.13
      IF(IFORM.LT.4.OR.IFORM.EQ.5.OR.IFORM.GE.13) BACKSPACE IO
      CALL SETIA(HOUR,366,27,0)
cim fix set LIMRN values 11/97
C     CALL SETIA(RDAY,3000,1,0)
      CALL SETIA(RDAY,LIMRN,1,0)
C#### WCH, 4/26/94.  INITIALIZE NEW VARIABLE ACODE.
      DO 8 I = 1,366
      DO 8 J = 1,4
    8 ACODE(I,J) = BLANK
      IOK    = 0
C#### WCH, 5/24/94.  DON'T SEE A REASON FOR ILOOP???
C####      ILOOP  = 0
      ILOST  = 1
      NSTORM = 0
      IDAST  = KDATE(0,1,NEWYR)
      IF(NEND.EQ.96) REWIND M4
C=======================================================================
C     Read this year's rainfall.
C=======================================================================
      IF(IFORM.EQ.7) THEN
                     READ(IO,1050,ERR=40,END=40) YEAR
                     IF(YEAR.GT.1900) THEN
c                                      YEAR = YEAR - 1900
                                      ELSE
                                      BACKSPACE IO
                                      READ(IO,1051,ERR=40,END=40) YEAR
c                                      YEAR = YEAR - 1900
                                      ENDIF
                     IF (YEAR.LT.100) YEAR = YEAR + 1900
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     IBUF  = ISTA
                     NEWYR = YEAR
                     IDAST = KDATE(0,1,NEWYR)
                     ENDIF
      IF(IFORM.EQ.8) THEN
                     READ(IO,1150,ERR=40,END=40) YEAR
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     READ(IO,1080,ERR=40,END=40) CH(1)
                     IBUF  = ISTA
                     IF (YEAR.LT.100) YEAR = YEAR + 1900
                     NEWYR = YEAR
                     IDAST = KDATE(0,1,NEWYR)
                     ENDIF
C#### WCH, 4/26/94.  INITIALIZE VARIABLE FOR NEW KODEA OPTION.
      JDOLD = 0
C=======================================================================
10    IF(IFORM.EQ.0) READ(IO,1000,ERR=10,END=40) STA,ELMTYP,YEAR,
     *               MONTH,DAY,IHR,IRAIN,ICODE
C#### WCH, 8/93.  ZERO OUT ALL 100 CELLS OF ARRAY 'VALUE'.
      IF(IFORM.EQ.1) CALL SETIA(VALUE,100,1,0)
Cwch, 7/28/04
C      IF(IFORM.EQ.1) READ(IO,1060,ERR=10,END=40) STA,ELMTYP,YEAR,
      IF(IFORM.EQ.1.AND.IFORM1.EQ.0) READ(IO,1060,ERR=10,END=40) 
     *               STA,ELMTYP,YEAR,MONTH,DAY,NUM,(IHOUR(I),
     *               VALUE(I),FLAG1(I),FLAG2(I),I=1,NUM)
      IF(IFORM.EQ.1.AND.IFORM1.EQ.1) THEN
	     NUM = 24
	     READ(IO,*,ERR=10,END=40) 
     *     STA6,ABUF2,ELMTYP,ABUF2,YEAR,MONTH,DAY,ABUF3,(IHOUR(I),
     *               VALUE(I),FLAG1(I),FLAG2(I),I=1,NUM)
	write(n6,*) STA6,ABUF2,ELMTYP,ABUF2,YEAR,MONTH,DAY,ABUF3,
     *               (IHOUR(I),VALUE(I),FLAG1(I),FLAG2(I),I=1,NUM)
	     ENDIF
      IF(IFORM.EQ.2) READ(IO,1040,ERR=10,END=40) STA,YEAR,MONTH,
     *               DAY,ICARD,XRA
      IF(IFORM.EQ.5) READ(IO,1045,ERR=10,END=40) STA,YEAR,MONTH,
     *               DAY,(XRAIN(I),I=1,24)
C#### WCH, 8/1/95.  ADD NEW AES FORMAT OPTION.
      IF(IFORM.EQ.13) READ(IO,1046,ERR=10,END=40) STA,YEAR,MONTH,
     *               DAY,(XRAIN(I),FLAG1(I),I=1,24)
CWCH, 11/22/99. ADD TWO NEW AES FORMATS.
      IF(IFORM.EQ.14) READ(IO,1047,ERR=10,END=40) STA,YEAR,MONTH,
     *               DAY,(XRAIN(I),FLAG1(I),I=1,24)
      IF(IFORM.EQ.15) READ(IO,1048,ERR=10,END=40) STA,YEAR,MONTH,
     *               DAY,(XRAIN(I),FLAG1(I),I=1,96)
                     IF (YEAR.LT.100) YEAR = YEAR + 1900
      IF(IFORM.EQ.7) THEN
                     READ(IO,1085,ERR=49,END=40) MONTH,DAY,
     +                                           (YRAIN(I),I=1,12)
                     READ(IO,1095,ERR=10,END=40) (YRAIN(I),I=13,24)
                     ILOST = 0
                     IF(IYBEG(1).NE.0.AND.YEAR.LT.IYBEG(1))  GO TO 10
                     IF(MONTH.LT.IYBEG(2).AND.
     +                                     YEAR.EQ.IYBEG(1)) GO TO 10
                     IF(MONTH.LE.IYBEG(2).AND.DAY.LT.IYBEG(3)
     +                                .AND.YEAR.EQ.IYBEG(1)) GO TO 10
                     STA   = ISTA
                     ILOST = 1
                     ENDIF
      IF(IFORM.EQ.8) THEN
                     READ(IO,1155,ERR=40,END=40) MONTH,DAY,XRAIN
                     STA   = ISTA
                     ILOST = 0
                     IF(MONTH.EQ.0) GO TO 10
                     IF(IYBEG(1).NE.0.AND.YEAR.LT.IYBEG(1))  GO TO 10
                     IF(MONTH.LT.IYBEG(2).AND.
     +                                     YEAR.EQ.IYBEG(1)) GO TO 10
                     IF(MONTH.LE.IYBEG(2).AND.DAY.LT.IYBEG(3)
     +                                .AND.YEAR.EQ.IYBEG(1)) GO TO 10
                     ILOST = 1
                     ENDIF
C=======================================================================
C#### 8/1/95.  ADD CHECK FOR ZERO YEAR/MONTH/DAY.  LIKELY CAUSED BY
C     EXTRA CARRIAGE RETURN AT END OF INPUT ASCII DATA FILE.
C=======================================================================
      IF(IFORM.NE.4.AND.IFORM.NE.6) THEN
           IF(YEAR.LE.0.OR.MONTH.LE.0.OR.DAY.LE.0) THEN
                WRITE(N6,9655) IO
                GO TO 40
                ENDIF
           ENDIF
C=======================================================================
C     NCDC Release B Condensed data.
C     IFORM = 4 or 6.  Read Rainfall lines 8 and 9 (first column)
C       for microcomputer format.
C=======================================================================
      IF(IFORM.EQ.4.OR.IFORM.EQ.6) THEN
                     READ(M3,*,END=940,ERR=950,IOSTAT=IOS) LN
                     IF(LN.EQ.10) GO TO 40
                     BACKSPACE M3
C=======================================================================
C     Here, should be reading data line LN = 8.
C=======================================================================
                     READ(M3,*,ERR=950,IOSTAT=IOS) LN,YEAR,MONTH,DAY,
     1                     UNITS,NR
                     IF (YEAR.LT.100) YEAR = YEAR + 1900
CIM    here year 1948 comes over as 948 
CIM    I guess that 2000 will be 000
                     IF (YEAR.LT.1000) THEN
	                     IF (YEAR.GT.500) THEN
	                     YEAR = YEAR + 1000
	                     ELSE
	                     YEAR = YEAR + 2000
	                     ENDIF
	                 ENDIF
c                     YEAR    = YEAR - 900
                     IF(IYEND(1).NE.0.AND.YEAR.EQ.IYEND(1).AND.
     +               MONTH.GE.IYEND(2).AND.DAY.GT.IYEND(3))  GO TO 40
                     IF(IYEND(1).NE.0.AND.YEAR.EQ.IYEND(1).AND.
     +               MONTH.GT.IYEND(2))                      GO TO 40
                     IF(IYEND(1).NE.0.AND.YEAR.GT.IYEND(1))  GO TO 40
C=======================================================================
C      BACKSPACE AND RETURN FOR FIRST ENTRY OF NEXT YEAR IN ORDER
C      RECORD/ANALYZE DATA FOR PRIOR YEAR AND INCREMENT NEWYR.
C      BUT ONLY DO IT IF DATA HAVE BEEN FOUND, IOK=1.
C=======================================================================
                     IF(YEAR.GT.NEWYR.AND.IOK.EQ.1) THEN
                                                    BACKSPACE M3
                                                    RETURN
                                                    ENDIF
                     IF(IYBEG(1).NE.0.AND.YEAR.LT.IYBEG(1))  GO TO 55
                     IF(MONTH.LT.IYBEG(2).AND.
     +                                     YEAR.EQ.IYBEG(1)) GO TO 55
                     IF(MONTH.LE.IYBEG(2).AND.DAY.LT.IYBEG(3)
     +                                .AND.YEAR.EQ.IYBEG(1)) GO TO 55
C=======================================================================
C      ALSO BACKSPACE AND RETURN IF THIS IS FIRST DAY OF STARTING YEAR.
C      IN THIS CASE, NO RAIN WOULD HAVE BEEN READ, IOK=0.
C      DO THIS IN ORDER TO INCREMENT NEWYR AND GET CORRECT IDAST.
C=======================================================================
                     IF(IYBEG(1).NE.0.AND.YEAR.EQ.IYBEG(1)
     1                  .AND.YEAR.GT.NEWYR.AND.IOK.EQ.0) THEN
                          IF(MONTH.EQ.IYBEG(2).AND.DAY.EQ.IYBEG(3)) THEN
                               NEWYR = YEAR
                               ILOST = 0
                               BACKSPACE M3
                               RETURN
                               ENDIF
                          ENDIF
C#### WCH, 5/24/94.  DON'T SEE A REASON FOR ILOOP???
C####                     ILOOP   = ILOOP + 1
C####                     IF(ILOOP.EQ.1)                          GO TO 55
C=======================================================================
C     HERE, FINALLY, READ THE ACTUAL PRECIP. VALUES. (DATA LINE LN = 9)
C     NEED SEPARATE READS FOR HOURLY AND 15-MIN. DATA.
C=======================================================================
C#### WCH, 4/26/94.
C     ALLOW FOR AVERAGING OF NCDC VALUES WITH "A" (ACCUMULATED TOTALS),
C     OR ELSE THE OPTION FOR USING THAT RAINFALL VALUE LUMPED AT THE
C     TIME INDICATED FOR THE "A".
C
C     KODEA = 0, OLD PROCEDURE.  DON'T INCLUDE ACCUMULATED TOTALS, BUT
C                DO KEEP A TOTAL AT END OF EVENT SUMMARY.
C     KODEA = 1, AVERAGE ACCUMULATED TOTAL OVER PRECEDING HOURS SINCE
C                LAST "A", "I", OR "M" VALUE OR FROM BEGINNING OF DAY.
C     KODEA = 2, USE ACCUMULATED TOTAL AS RAINFALL AT TIME OF "A".
C=======================================================================
C     SAVE OLD TIME FOR KODEA = 1 OPTION.
C=======================================================================
                     JD1    = JDATE(DAY,MONTH,YEAR)
                     JULDAY = JD1 + YEAR*1000
                     IHROLD = 1
                     MINOLD = 0
                     RHOURO = 0.0
                     MIN    = 0
                     DO 50 J = 1,NR
                     IF(NEND.LT.96) READ(M3,*,ERR=960,IOSTAT=IOS)
     1                       LN,IHR,IRAIN,ICODE
                     IF(NEND.EQ.96) READ(M3,*,ERR=960,IOSTAT=IOS)
     1                       LN,IHR,MIN,IRAIN,ICODE
C#######################################################################
C  WCH, 3/2/93 AND 10/8/93.
C  LAST ENTRY IS TOTAL FOR DAY.  SKIP CALCULATIONS FOR THIS CASE.
C#######################################################################
                     IF(ICODE.EQ.'0') ICODE = BLANK
                     IF(J.EQ.NR) THEN
                          CALL RAINCD(ICODE,JD1)
                          GO TO 50
                          ENDIF
                     IF(IRAIN.GE.99999) THEN
                                        IRAIN = 0
                                        ICODE = IM
                                        IF(KODEA.EQ.1) THEN
C=======================================================================
C    HERE, SAVE ENDING TIME OF MISSING RAINFALL VALUE, FOR HOURLY DATA
C=======================================================================
C#### WCH, 12/6/96. SEVERAL REPLACED STATEMENTS UP TO COMMENT OF 4/26/94
                                           IF(NEND.EQ.24) THEN
                                               IHROLD = IHR
                                               MINOLD = 0
                                               RHOURO = FLOAT(IHR)*3600.
                                               ELSE
C=======================================================================
C    HERE, SAVE STARTING TIME OF MISSING RAINFALL VALUE, FOR 15-MIN DATA
C=======================================================================
                                               IHRSO  = IHR
                                               MINOLD = MIN - 15
                                               IF(MINOLD.EQ.-15) THEN
                                                  MINOLD = 45
                                                  IHRSO  = IHRSO - 1
                                                  ENDIF
                                               IHROLD = IHRSO + 1
                                               RHOURO = FLOAT(IHRSO)*
     1                                          3600.+FLOAT(MINOLD)*60.
                                               ENDIF
                                           ENDIF
                                        ENDIF
C#######################################################################
C#### WCH, 4/26/94.  MAKE NOTE OF SPECIAL CODES FOR EVENT PRINT-OUT.
C=======================================================================
                     CALL RAINCD(ICODE,JD1)
                     IOK   = 1
                     ILOST = 1
                     IDAY  = KDATE(DAY,MONTH,YEAR) - IDAST
C#### WCH, 8/93
C=======================================================================
C#### WCH, 12/6/96.  NEED TO COMPUTE IHRS AND RECOMPUTE MIN EVEN IF
C     THERE IS NO RAIN, IN EVENT THEY ARE NEEDED FOR AVERAGING OF
C     ACCUMULATED DATA.  SO, PUT IRAIN GT OR EQ 0 CHECKS INSIDE LOOP
C     FOR NEND.EQ.96 (15 MIN DATA).
C=======================================================================
C####                     IF(NEND.EQ.96.AND.IRAIN.GT.0) THEN
                     IF(NEND.EQ.96) THEN
C=======================================================================
C  FOR 15-MIN. DATA, COMPUTE STARTING HOUR AND MINUTE (IHRS, 0-23 AND
C    MIN, 0-45) AND ENDING HOUR (IHR, 1-24).
C    RELEASE-B CONDENSED HAS HOURS GOING FROM 0-24 AND MINUTES
C    FROM 0 - 45.  SMALLEST TIME SHOULD BE HR=0 AND
C    MIN=15 AND LARGEST TIME SHOULD BE HR = 24 AND MIN = 0.  HOWEVER
C    PROTECT AGAINST POSSIBILITY OF HR = 0 AND MIN = 0 IN CODE AND
C    AGAINST POSSIBILITY OF HR = 24 AND MIN > 0.
C    NOTE THAT TIMES IN DATA (IHR,MIN) ARE ENDING TIMES OF RAINFALL.
C=======================================================================
                        IHRS = IHR
                        MIN = MIN - 15
                        IF(MIN.EQ.-15) THEN
                             MIN = 45
                             IHRS = IHRS - 1
                             ENDIF
                        IHR  = IHRS + 1
                        JPJDAY = JULDAY
C#### WCH, 10/8/93.  WORRY ABOUT RAIN ENDING EXACTLY AT MIDNITE AND IHR=0.
C#### JUST IN CASE HR=0 AND MIN=0 START SHOWING UP ON INPUT DATA:
                        IF(IHRS.EQ.-1.AND.MIN.EQ.45) THEN
                           IHR  = 1
                           IHRS = 23
                           IF(JULDAY-(JULDAY/1000)*1000.GT.1) THEN
                               JPJDAY = JPJDAY - 1
                               ELSE
                               JPJDAY = JDATE(31,12,YEAR-1) +
     1                                   (YEAR-1)*1000
                               ENDIF
                           ENDIF
                        RHOUR = FLOAT(IHRS)*3600.0 + FLOAT(MIN)*60.0
C=======================================================================
C#### WCH, 12/6/96.  HERE, PUT IN CHECK FOR RAIN OR NO RAIN.
C=======================================================================
                        IF(IRAIN.GT.0) THEN
C=======================================================================
C  IF KODEA = 1, PLACE AVERAGE ON FILE DURING INTERVENING HOURS.
C  NOTE, EXTRA AVERAGING INTERVAL IS ADDED IN SUB. RAINAVG TO ACCOUNT
C  FOR GETTING TO END OF CURRENT INTERVAL THAT CONTAINS "A" CODE.
C=======================================================================
                             IF(KODEA.EQ.1.AND.ICODE.EQ.IA) THEN
                                CALL RAINAVG(RHOUR,RHOURO,JPJDAY,
     1                            IDAY,IHROLD,MINOLD,NEND,IRAIN,M4)
                                ICODE = ' '
C=======================================================================
C  PLACE 15-MIN. PRECIP. VALUE ON SCRATCH FILE IN UNITS OF INCHES/HOUR.
C=======================================================================
                                ELSE
                                REIN  = FLOAT(IRAIN)/100.0/0.25
                                WRITE(M4) JPJDAY,RHOUR,THISTO,REIN
                                HOUR(IDAY,IHR) = HOUR(IDAY,IHR) + IRAIN
                                ENDIF
C
C#### WCH, 12/6/96.
C####                    ELSE IF(NEND.EQ.96) THEN
C####                    IHR = IHR + 1
                             ELSE IF(KODEA.EQ.1.AND.ICODE.EQ.IA) THEN
C####                    RHOURO = FLOAT(IHR-1)*3600.+FLOAT(MIN)*60.
C=======================================================================
C     HERE HAVE ACCUMULATION CODE ("A") BUT NO RAIN.  UNLIKELY, BUT
C     POSSIBLE.
C=======================================================================
                                  RHOURO = RHOUR
                                  IHROLD = IHR
                                  MINOLD = MIN
                                  ENDIF
                        ENDIF
C=======================================================================
C#### WCH, 10/8/93.  RECOGNIZE POSSIBLE ERROR WHEN 15-MIN RAINFALL ENDS
C       AT MIDNIGHT.  PREVIOUS 15-MIN INCREMENT COUNTED IN FIRST HOUR
C       OF NEXT DAY FOR PRINT-OUT OF HOURLY TOTALS, ONLY.  SHOULD BE
C       CORRECT ON PRECIPITATION INTERFACE FILE.
C=======================================================================
                     IF(IHR.GT.24) IHR = 24
                     IF(NEND.EQ.24) THEN
                        IF(KODEA.EQ.1.AND.ICODE.EQ.IA
     1                              .AND.IRAIN.GT.0) THEN
C=======================================================================
C     SUBTRACT HOUR FROM END HOUR BECAUSE EXTRA INTERVAL IS ADDED IN
C     SUB. RAINAVG.
C=======================================================================
                           RHOUR = FLOAT(IHR-1)*3600.
                           CALL RAINAVG(RHOUR,RHOURO,JPJDAY,
     1                       IDAY,IHROLD,MINOLD,NEND,IRAIN,M4)
                           ICODE = ' '
                           ELSE
                           HOUR(IDAY,IHR) = IRAIN + HOUR(IDAY,IHR)
                           ENDIF
                        IF(KODEA.EQ.1.AND.ICODE.EQ.IA.AND.
     1                                          IRAIN.EQ.0) THEN
                           RHOURO = FLOAT(IHR)*3600.
                           IHROLD = IHR
                           ENDIF
                        ENDIF
C=======================================================================
C                    Special codes for missing rainfall.
C=======================================================================
                     IF(ICODE.EQ.IA.AND.KODEA.EQ.0) THEN
                                     HOUR(IDAY,IHR) = -2
                                     SUM(1) = SUM(1) + FLOAT(IRAIN)*CONV
                                     ENDIF
C#### WCH, 12/6/96.  DON'T DO THIS FOR 15-MIN DATA.
                     IF(NEND.EQ.24.AND.ICODE.EQ.IM) HOUR(IDAY,IHR) = -1
   50                CONTINUE
                     GO TO 52
C=======================================================================
C     Here if need to read data to advance file before arriving at
C     desired start time.
C=======================================================================
   55                CONTINUE
                     ILOST = 0
                     IF(YEAR.GT.NEWYR) THEN
                                       NEWYR = YEAR
                                       BACKSPACE M3
                                       RETURN
                                       ENDIF
                     DO 51 J = 1,NR
C#### WCH, 10/8/93.  NEED SEPARATE READS FOR HOURLY AND 15-MIN.
                     IF(NEND.LT.96) READ(M3,*,ERR=950,IOSTAT=IOS)
     1                    LN,IHR,IRAIN,ICODE
                     IF(NEND.EQ.96) READ(M3,*,ERR=950,IOSTAT=IOS)
     1                    LN,IHR,MIN,IRAIN,ICODE
   51                CONTINUE
                     NEWYR = YEAR
                     IDAST = KDATE(0,1,NEWYR)
   52                CONTINUE
                     ENDIF
C=======================================================================
C     Check for same station.     Check for same year.

C     Add to this year's matrix.  Find the day number and indicate
C                                 unusual conditions.
C     Hour =  -2 means meter stuck.
C     Hour =  -1 means missing data.
C=======================================================================
      IF((IFORM.EQ.4.OR.IFORM.EQ.6).AND.IOK.EQ.0)  GO TO 10
      IF((IFORM.EQ.4.OR.IFORM.EQ.6).AND.IOK.EQ.1)  GO TO 777
C#### WCH, 8/1/95.  ELIMINATE THIS CHECK.
C####      IF(STA.NE.ISTA) GO TO 40
      IF(IYEND(1).NE.0.AND.YEAR.EQ.IYEND(1).AND.MONTH.GE.IYEND(2)
     *                   .AND.DAY.GT.IYEND(3))   GO TO 40
      IF(IYEND(1).NE.0.AND.YEAR.EQ.IYEND(1).AND.MONTH.GT.IYEND(2))
     *                                           GO TO 40
      IF(IYEND(1).NE.0.AND.YEAR.GT.IYEND(1))     GO TO 40
      IF(YEAR.GT.NEWYR)                          RETURN
      IDAY = KDATE(DAY,MONTH,YEAR) - IDAST
C=======================================================================
C#### WCH, 8/1/95.  ADD CHECK FOR SUBSCRIPT.
      IF(IDAY.LE.0.OR.IDAY.GT.366) THEN
           WRITE(N6,9660) IDAY,YEAR,MONTH,DAY,NEWYR
           WRITE (*,9660) IDAY,YEAR,MONTH,DAY,NEWYR
           GO TO 40
           ENDIF
C=======================================================================
C     Special codes for new format of rainfall.
C=======================================================================
C     NCDC, TD-3240, fixed length records (one rainfall value per line).
C=======================================================================
      IF(IFORM.EQ.0) THEN
C=======================================================================
C     SAVE OLD TIME FOR KODEA = 1 OPTION.
C=======================================================================
                     JD1   = JDATE(DAY,MONTH,YEAR)
                     JDNEW = JD1 + YEAR*1000
                     IF(KODEA.EQ.1) THEN
                        IF(JDNEW.GT.JDOLD.OR.JD1.EQ.1) THEN
                           IHROLD = 1
                           MINOLD = 0
                           RHOURO = 0.0
                           JDOLD  = JDNEW
                           ENDIF
                        ENDIF
C
                     IF(IRAIN.GE.99999) THEN
                              IRAIN = 0
                              ICODE = IM
                              IF(KODEA.EQ.1) THEN
C=======================================================================
C    HERE, SAVE ENDING TIME OF MISSING RAINFALL VALUE, FOR HOURLY DATA
C=======================================================================
C#### WCH, 12/6/96. SEVERAL REPLACED STATEMENTS UP TO COMMENT OF 4/26/94
                                   IF(NEND.EQ.24) THEN
                                       IHROLD = IHR
                                       MINOLD = 0
                                       RHOURO = FLOAT(IHR)*3600.
                                       ELSE
C=======================================================================
C    HERE, SAVE STARTING TIME OF MISSING RAINFALL VALUE, FOR 15-MIN DATA
C=======================================================================
                                       IHRSO  = IHR/100
                                       MINOLD = IHR - (IHR/100)*100 - 15
                                       IF(MINOLD.EQ.-15) THEN
                                            MINOLD = 45
                                            IHRSO  = IHRSO - 1
                                            ENDIF
                                       IHROLD = IHRSO + 1
                                       RHOURO = FLOAT(IHRSO)*
     1                                     3600.+FLOAT(MINOLD)*60.
                                       ENDIF
                                   ENDIF
                              ENDIF
C#######################################################################
C#### WCH, 4/26/94.  MAKE NOTE OF SPECIAL CODES FOR EVENT PRINT-OUT.
C=======================================================================
                     CALL RAINCD(ICODE,JD1)
C#### WCH, 8/93
C#### WCH, 12/6/96.  CHECK FOR IRAIN.GT.0 LATER.
C####                     IF(NEND.EQ.96.AND.IRAIN.GT.0) THEN
                     IF(NEND.EQ.96) THEN
C=======================================================================
C  FOR 15-MIN. DATA, COMPUTE STARTING HOUR AND MINUTE (IHRS, 0-23 AND
C    MIN, 0-45) AND ENDING HOUR (IHR, 1-24)
C  FORM OF IHR IS HRMN WHERE HR = HOUR, FROM 0 - 24, AND MN = MIN, E.G.,
C    0045, 1530, 2300.  THIS IS END TIME OF RAINFALL VALUE.
C=======================================================================
                        MIN  = IHR - (IHR/100)*100 - 15
                        IHRS = IHR/100
                        IF(MIN.LT.0) THEN
                           MIN  = 45
                           IHRS = IHRS - 1
                           ENDIF
                        IHR  = IHRS + 1
C#### WCH, 12/6/96.  NEED TO COMPUTE RHOUR HERE.
                        RHOUR = FLOAT(IHRS)*3600.0 + FLOAT(MIN)*60.0
                        IF(IRAIN.GT.0) THEN
C#### WCH, 4/25/94.  DON'T SUBTRACT 1900 FROM YEAR.  ALREADY 2-DIGIT.
                           JULDAY = JDATE(DAY,MONTH,YEAR)
     1                                 + YEAR*1000
C####                RHOUR = FLOAT(IHRS)*3600.0 + FLOAT(MIN)*60.0
C=======================================================================
C  IF KODEA = 1, PLACE AVERAGE ON FILE DURING INTERVENING HOURS.
C=======================================================================
                           IF(KODEA.EQ.1.AND.ICODE.EQ.IA) THEN
                                CALL RAINAVG(RHOUR,RHOURO,JULDAY,
     1                           IDAY,IHROLD,MINOLD,NEND,IRAIN,M4)
                                ICODE = ' '
                                JDOLD = JULDAY
C=======================================================================
C  PLACE 15-MIN. PRECIP. VALUE ON SCRATCH FILE IN UNITS OF INCHES/HOUR.
C=======================================================================
                                ELSE
                                REIN  = FLOAT(IRAIN)/100.0/0.25
                                WRITE(M4) JULDAY,RHOUR,THISTO,REIN
                                HOUR(IDAY,IHR) = HOUR(IDAY,IHR) + IRAIN
                                ENDIF
C#### WCH, 12/6/96
C####                           ENDIF
C####                        ELSE IF(NEND.EQ.96.AND.KODEA.EQ.1.AND.
C####     1                            ICODE.EQ.IA) THEN
                           ELSE IF(KODEA.EQ.1.AND.ICODE.EQ.IA) THEN
                                   IHROLD = IHR
                                   MINOLD = MIN
C####                   RHOURO = FLOAT(IHRS)*3600.+FLOAT(MIN)*60.
                                   RHOURO = RHOUR
                                   ENDIF
                           ENDIF
C=======================================================================
C  ALLOW AVERAGING OF HOURLY RAINFALL ALSO.
C=======================================================================
                     IF(NEND.EQ.24) THEN
                        IF(KODEA.EQ.1.AND.ICODE.EQ.IA
     1                             .AND.IRAIN.GT.0) THEN
                           RHOUR = FLOAT(IHR-1)*3600.
                           CALL RAINAVG(RHOUR,RHOURO,JDNEW,
     2                          IDAY,IHROLD,MINOLD,NEND,IRAIN,M4)
                           ICODE = ' '
                           JDOLD = JDNEW
                           ELSE
                           HOUR(IDAY,IHR) = IRAIN + HOUR(IDAY,IHR)
                           ENDIF
                        IF(KODEA.EQ.1.AND.ICODE.EQ.IA) THEN
                           RHOURO = FLOAT(IHR)*3600.
                           IHROLD = IHR
                           JDOLD = JDNEW
                           ENDIF
                        ENDIF
C
                     IF(ICODE.EQ.IA.AND.KODEA.EQ.0) THEN
                                     HOUR(IDAY,IHR) = -2
                                     SUM(1) = SUM(1) + FLOAT(IRAIN)*CONV
                                     ENDIF
C#### WCH, 12/6/96.  DON'T DO THIS FOR 15-MIN DATA.
                     IF(NEND.EQ.24.AND.ICODE.EQ.IM) HOUR(IDAY,IHR) = -1
                     ENDIF
C=======================================================================
C     NCDC TD-3240/3260, variable length records (data for all hours on
C     one line).  This format is also for "NCDC format" for exporting
C     of data from EarthInfo and Hydrosphere CD-ROMs.
C=======================================================================
      IF(IFORM.EQ.1) THEN
                     IF(KODEA.EQ.1) THEN
                        IHROLD = 1
                        MINOLD = 0
                        RHOURO = 0.0
                        ENDIF
                     ICODE = ' '
C#### WCH, 12/6/96.  PLACE THIS DATE CALCULATION OUTSIDE LOOP.
                     JD1 = JDATE(DAY,MONTH,YEAR)
                     JULDAY = JD1 + YEAR*1000
C
                     DO 8000 I = 1,NUM
                     IRAIN     = VALUE(I)
                     IF(IRAIN.GE.99999) THEN
                              IRAIN = 0
                              ICODE = IM
                              IF(KODEA.EQ.1) THEN
C=======================================================================
C    HERE, SAVE ENDING TIME OF MISSING RAINFALL VALUE, FOR HOURLY DATA
C=======================================================================
C#### WCH, 12/6/96. SEVERAL REPLACED STATEMENTS UP TO COMMENT OF 4/26/94
                                   IF(NEND.EQ.24) THEN
                                       IHR    = IHOUR(I)/100
                                       IHROLD = IHR
                                       MINOLD = 0
                                       RHOURO = FLOAT(IHR)*3600.
                                       ELSE
C=======================================================================
C    HERE, SAVE STARTING TIME OF MISSING RAINFALL VALUE, FOR 15-MIN DATA
C=======================================================================
                                       IHR   = IHOUR(I)
                                       IHRSO = IHR/100
                                       MINOLD = IHR - (IHR/100)*100 - 15
                                       IF(MINOLD.EQ.-15) THEN
                                            MINOLD = 45
                                            IHRSO  = IHRSO - 1
                                            ENDIF
                                       IHROLD = IHRSO + 1
                                       RHOURO = FLOAT(IHRSO)*
     1                                     3600.+FLOAT(MINOLD)*60.
                                       ENDIF
                                   ENDIF
                              ENDIF
C#######################################################################
C#### WCH, 4/26/94.  MAKE NOTE OF SPECIAL CODES FOR EVENT PRINT-OUT.
C=======================================================================
                     IKODE = BLANK
                     IF(ICODE.NE.BLANK)    IKODE = ICODE
C#### WCH, 5/24/94.  CHANGE SUBSCRIPT 1 TO I FOR FLAG1().
                     IF(FLAG1(I).NE.BLANK) IKODE = FLAG1(I)
                     CALL RAINCD(IKODE,JD1)
                     IF(I.EQ.NUM) GO TO 8000
C#######################################################################
C  WCH, 8/93,  Need to perform this time calculation for both 15-min.
C    and hourly data because IHOUR is of form HRMN, e.g.,
C    2330, 0015, etc.
C#######################################################################
                     IHR  = IHOUR(I)
                     MIN  = IHR - (IHR/100)*100 - 15
                     IHRS = IHR/100
                     IF(MIN.LT.0) THEN
                        MIN  = 45
                        IHRS = IHRS - 1
                        ENDIF
                     IHR  = IHRS + 1
C#### WCH, 12/6/96.  CHECK FOR ZERO RAIN LATER.
                     IF(NEND.EQ.96) THEN
C####                     IF(NEND.EQ.96.AND.IRAIN.GT.0) THEN
C#### WCH, 4/25/94.  DON'T SUBTRACT 1900 FROM YEAR.  ALREADY 2-DIGIT.
                          RHOUR = FLOAT(IHRS)*3600.0 + FLOAT(MIN)*60.0
C=======================================================================
C  IF KODEA = 1, PLACE AVERAGE ON FILE DURING INTERVENING HOURS.
C=======================================================================
C#### WCH, 12/6/96
                          IF(IRAIN.GT.0) THEN
C#### WCH, 5/24/94.  CHANGE SUBSCRIPT 1 TO I FOR FLAG1().
                             IF(KODEA.EQ.1.AND.FLAG1(I).EQ.IA) THEN
                                 CALL RAINAVG(RHOUR,RHOURO,JULDAY,
     1                             IDAY,IHROLD,MINOLD,NEND,IRAIN,M4)
C#### WCH, 5/24/94.  CHANGE SUBSCRIPT 1 TO I FOR FLAG1().
                                 FLAG1(I) = ' '
                                 JDOLD = JULDAY
C=======================================================================
C  PLACE 15-MIN. PRECIP. VALUE ON SCRATCH FILE IN UNITS OF INCHES/HOUR.
C=======================================================================
                                 ELSE
                                 REIN  = FLOAT(IRAIN)/100.0/0.25
                                 WRITE(M4) JULDAY,RHOUR,THISTO,REIN
                                 HOUR(IDAY,IHR) = HOUR(IDAY,IHR) + IRAIN
                                 ENDIF
C
C#### WCH, 12/6/96
                             ELSE IF(FLAG1(I).EQ.IA.AND.KODEA.EQ.1) THEN
C#### WCH, 5/24/94.  CHANGE SUBSCRIPT 1 TO I FOR FLAG1().
C####                        ELSE IF(NEND.EQ.96.AND.FLAG1(I).EQ.IA.AND.
C####     1                      KODEA.EQ.1) THEN
                                 IHROLD = IHR
                                 MINOLD = MIN
C####                   RHOURO = FLOAT(IHRS)*3600.+FLOAT(MIN)*60.
                                 RHOURO = RHOUR
                                 ENDIF
                          ENDIF
C=======================================================================
C  ALLOW AVERAGING OF HOURLY RAINFALL ALSO.
C=======================================================================
                     IF(NEND.EQ.24) THEN
C#### WCH, 5/24/94.  CHANGE SUBSCRIPT 1 TO I FOR FLAG1().
                        IF(KODEA.EQ.1.AND.FLAG1(I).EQ.IA
     1                                 .AND.IRAIN.GT.0) THEN
                           RHOUR = FLOAT(IHR-1)*3600.
                           CALL RAINAVG(RHOUR,RHOURO,JDNEW,
     1                          IDAY,IHROLD,MINOLD,NEND,IRAIN,M4)
C#### WCH, 5/24/94.  CHANGE SUBSCRIPT 1 TO I FOR FLAG1().
                           FLAG1(I) = ' '
                           ELSE
                           HOUR(IDAY,IHR) = IRAIN + HOUR(IDAY,IHR)
                           ENDIF
C#### WCH, 5/24/94.  CHANGE SUBSCRIPT 1 TO I FOR FLAG1().
                        IF(KODEA.EQ.1.AND.FLAG1(I).EQ.IA) THEN
                           RHOURO = FLOAT(IHR)*3600.
                           IHROLD = IHR
                           ENDIF
                        ENDIF
C
                     IF(FLAG1(I).EQ.IA.AND.KODEA.EQ.0) THEN
                                     HOUR(IDAY,IHR) = -2
                                     SUM(1) = SUM(1) + FLOAT(IRAIN)*CONV
                                     ENDIF
C#### WCH, 12/6/96.  DON'T DO THIS FOR 15-MIN DATA.
                     IF(NEND.EQ.24.AND.ICODE.EQ.IM) HOUR(IDAY,IHR) = -1
 8000                CONTINUE
                     ENDIF
C=======================================================================
C     NWS Card Deck 488 format.
C     Special codes for old format of rainfall.
C=======================================================================
      IF(IFORM.EQ.2) THEN
                     IF(ICARD.EQ.1) J1 =  0
                     IF(ICARD.EQ.2) J1 = 12
                     DO 35 J = 1,12
                     IF(XRA(J).EQ.ZIM) THEN
                                       HOUR(IDAY,J+J1) = -2
C#### WCH, 4/26/94.
                             CALL RAINCD(IA,IDAY)
                                       GO TO 35
                                       ENDIF
                     IF(XRA(J).EQ.BIB) THEN
                                       HOUR(IDAY,J+J1) = -1
C#### WCH, 4/26/94.
                             CALL RAINCD(IM,IDAY)
                                       GO TO 35
                                       ENDIF
                     IF(XRA(J).EQ.BLK) THEN
                                       HOUR(IDAY,J+J1) = 0
                                       GO TO 35
                                       ENDIF
                     HOUR(IDAY,J+J1) = INTCHR(XRA(J))
35                   CONTINUE
                     ENDIF
C=======================================================================
C     Special codes for AES Canadian data.
C=======================================================================
C#### WCH, 8/1/95.  ADD NEW FORMAT OPTION, IFORM = 13.
CWCH, 11/22/99. ADD FORMAT FOR 4-DIGIT YEAR (IFORM = 14).
      IF(IFORM.EQ.5.OR.IFORM.EQ.13.OR.IFORM.EQ.14) THEN
                     DO 45 J = 1,24
                     IF(XRAIN(J).GE.9999) THEN
                             SUM(1)       = SUM(1)+FLOAT(XRAIN(J))*CONV
                             HOUR(IDAY,J) = -2
C#### WCH, 4/26/94.
                             CALL RAINCD(IA,IDAY)
C#### WCH, 8/1/95.  IF THERE IS A FLAG, PRINT IT.
CWCH, 11/22/99. CHANGE TO .GE.13
                             IF(IFORM.GE.13) CALL RAINCD(FLAG1(J),IDAY)
                             GO TO 45
                             ENDIF
C     Code for missing data is -1. (AES value = -99999)
                     IF(XRAIN(J).LE.-999) THEN
                             HOUR(IDAY,J) = -1
C#### WCH, 4/26/94.
                             CALL RAINCD(IM,IDAY)
C#### WCH, 8/1/95.  IF THERE IS A FLAG, PRINT IT.
CWCH, 11/22/99. CHANGE TO .GE.13
                             IF(IFORM.GE.13) CALL RAINCD(FLAG1(J),IDAY)
                             GO TO 45
                             ENDIF
C#### WCH, 8/1/95.  IF THERE IS A FLAG, PRINT IT.
CWCH, 11/22/99. MAKE .GE.13
                     IF(IFORM.GE.13) CALL RAINCD(FLAG1(J),IDAY)
                     HOUR(IDAY,J)         = XRAIN(J)
45                   CONTINUE
                     ENDIF
C=======================================================================
CWCH, 11/22/99.  AES 15-min precip. data.
C	Now more properly called CMC or NCDA format?
C     AES "ELEM" code = 159 for FIF21 (15-min precip).
C     Take format from Web page at:
C     http://www.cmc.ec.gc.ca/climate/document.htm
C=======================================================================
      IF(IFORM.EQ.15) THEN
                     JD1 = JDATE(DAY,MONTH,YEAR)
                     JULDAY = JD1 + YEAR*1000
	               NUM = 96
C
                     DO 808 I = 1,NUM
                     IRAIN     = XRAIN(I)
C     Don't fully understand AES format, so don't allow for special
C     computations for missing or accumulated data.  
                     IF(IRAIN.LE.-999) THEN
                              IRAIN = 0
                              ICODE = IM
                              ENDIF
C#######################################################################
C#### WCH, 4/26/94.  MAKE NOTE OF SPECIAL CODES FOR EVENT PRINT-OUT.
C=======================================================================
                     IKODE = BLANK
                     IF(FLAG1(I).NE.BLANK) IKODE = FLAG1(I)
                     CALL RAINCD(IKODE,JD1)
C=======================================================================
C     Compute hour and minute of start of interval.
C     IHR  = 1-24 = subscript.
C     IHRS = 0-23 = starting hour.
C=======================================================================
                     IHR  = (I-1)/4 + 1
	               IHRS = IHR - 1
                     MIN  = (I - 1 - IHRS*4)*15
                     RHOUR = FLOAT(IHRS)*3600.0 + FLOAT(MIN)*60.0
                     IF(IRAIN.GT.0) THEN
C=======================================================================
C     Intensity is 15-min. depth/.25 hr and divided by conversion factor
C     of 0.1 to get mm/hr. 
C=======================================================================
                                 REIN  = FLOAT(IRAIN)/0.1/0.25
                                 WRITE(M4) JULDAY,RHOUR,THISTO,REIN
                                 HOUR(IDAY,IHR) = HOUR(IDAY,IHR) + IRAIN
                                 ENDIF
                     IF(FLAG1(I).EQ.IA.AND.KODEA.EQ.0) THEN
                                     HOUR(IDAY,IHR) = -2
                                     SUM(1) = SUM(1) + FLOAT(IRAIN)*CONV
                                     ENDIF
  808                CONTINUE
                     ENDIF
C=======================================================================
C     Earth Info precipitation data.
C     Old format, data retrieved from display.  For new ASCII files,
C       use Subroutine G9RAIN (IFORM = 9-12).
C=======================================================================
      IF(IFORM.EQ.7) THEN
                     DO 60 J       = 1,24
                     HOUR(IDAY,J)  = IFIX(YRAIN(J)*100.0+0.1)
60                   CONTINUE
                     ENDIF
C=======================================================================
C     Earth Info Precipitation data.
C=======================================================================
      IF(IFORM.EQ.8) THEN
                     DO 65 J       = 1,24
                     HOUR(IDAY,J)  = XRAIN(J)
65                   CONTINUE
                     ENDIF
C=======================================================================
 777  HOUR(IDAY,25) = MONTH
      HOUR(IDAY,26) =   DAY
      HOUR(IDAY,27) =  YEAR
      GO TO 10
      ENDIF
C=======================================================================
C     Skip if the new station is not the station selected.
C     Return if the new year is less than the selected ending year.
C=======================================================================
   49 RETURN
C=======================================================================
  940 WRITE(N6,9400) M3
   40 IGO = 1
      RETURN
  888 WRITE(N6,1070)
      STOP
C#######################################################################
C     WCH, 10/11/93.  Additional error messages while reading precip.
C     file.
C=======================================================================
C#### WCH, 8/4/95.  ALTER IOSTAT NUMBER FOR LAHEY, FOR FOUR WRITES.
  950 WRITE(N6,9500) M3,LN,YEAR,MONTH,DAY,NR,MOD(IOS,256)
      WRITE(*,9501)  M3,MOD(IOS,256)
      GO TO 965
  960 WRITE(N6,9600) M3,LN,YEAR,MONTH,DAY,NR,J,IHR,MIN,IRAIN,
     1  MOD(IOS,256)
      WRITE(N6,9601) M3,MOD(IOS,256)
  965 BACKSPACE M3
      READ(M3,9650,END=966) M3LINE
  966 WRITE(N6,9651) M3,M3LINE
3124	write(N6,*) IBUF2,ABUF2,
     +                   ELMTYP,ABUF2,NEWYR,NEWMON,NEWDAY
      STOP
C#### WCH, 10/2/96.  ADD NEW ERROR ROUTINE FOR INITIAL READS.
Cwch, 7/28/04 Just print IOSTAT
C  970 WRITE(N6,9700) IO,MOD(IOS,256),IBUF
C      WRITE(*,9700) IO,MOD(IOS,256),IBUF
  970 WRITE(N6,9700) IO,IOS,IBUF
      WRITE(*,9700) IO,IOS,IBUF
      STOP 'Run stopped from Sub. GTRAIN.'
C=======================================================================
C#### WCH, 8/1/95.  NEW FORMAT STATEMENT 990.
  990 FORMAT(/,' STATION ID ON PRECIP. DATA INPUT FILE = ',A8,/,
     1' REQUESTED STATION ID = ',A8,'  CHECK TO BE SURE THEY MATCH.',/)
C#### WCH, 8/1/95.  CHANGE IBUF FORMAT FROM I6 TO A6.
CWCH, 11/9/99. READ YEAR (3RD PARAMETER) AS 4 DIGITS, NOT 2.  
 1000 FORMAT(3X,A6,2X,A4,2X,I4,I2,I4,3X,I2,3X,I5,A1)
C#### WCH, 8/1/95.  CHANGE IBUF FORMAT FROM I6 TO A6.
 1040 FORMAT(A6,3I2,I1,12A3)
C#### WCH, 8/1/95.  CHANGE IBUF FORMAT FROM I8 TO A7 AND ALTER NEXT
C     FIELD ALSO. ADD NEW 1046 FOR NEW AES FORMAT.
 1045 FORMAT(A7,1X,3I2,3X,24I4)
 1046 FORMAT(A7,1X,3I2,3X,24(I6,A1))
CWCH, 11/22/99. ADD OPTION FOR 4-DIGIT YEAR AES DATA.
 1047 FORMAT(A7,I4,2I2,3X,24(I6,A1))
 1048 FORMAT(A7,I4,2I2,3X,96(I6,A1))
 1050 FORMAT(72X,I4)
 1051 FORMAT(73X,I4)
C#### WCH, 8/1/95.  CHANGE IBUF FORMAT FROM I6 TO A6.
CWCH, 11/9/99. READ YEAR (3RD PARAMETER) AS 4 DIGITS, NOT 2.  
 1060 FORMAT(3X,A6,2X,A4,2X,I4,I2,I4,I3,100(I4,I6,2A1))
 1070 FORMAT(/,' ===> Error !! in your rainfall file input.',/)
 1080 FORMAT(50A1)
 1085 FORMAT(25X,I2,1X,I2,4X,4(F6.0,F7.0,F7.0,1X))
 1095 FORMAT(25X,9X,4(F6.0,F7.0,F7.0,1X))
 1090 FORMAT(/,' Changing double quotation marks to single quotation',
     +         ' marks in input file.',/)
 1150 FORMAT(66X,I2)
 1155 FORMAT(I2,1X,I2,24I5)
 9000 FORMAT(/,' Error ==> Limit of ',I6,
     +         ' Precipitation values exceeded for this year.')
C#### WCH, 8/93.
 9110 FORMAT(/,' ===> ERROR. Time interval for new data,',F7.1,
     1 ' sec. does not',/,'     agree with time interval,',F7.1,
     2 ' sec. of data on unit JOUT with which',/,
     3 '     new data are to be combined.  Run stopped.')
C#### WCH, 10/11/93.
 9400 FORMAT(/,' WARNING! END-OF-FILE REACHED ON UNIT',I3,' WHILE READIN
     1G PRECIPITATION DATA',/,' FOR IFORM = 4 OR 6.  SHOULD HAVE A NUMBE
     2R 10 IN FIRST TWO COLUMNS',/,' TO CONCLUDE THIS FILE.  POSSIBLE ER
     3ROR.  PROCESSING CONTINUES.')
C#### WCH, 8/4/95. CHANGE RMFORT TO LAHEY.
 9500 FORMAT(' ERROR WHILE READING PRECIP. DATA ON UNIT',I3,' FOR WHAT S
     1HOULD BE A',/,' LINE 8 INPUT STRING. CURRENT PARAMETER VALUES ARE:
     2',/,' LINE ID =',I3,/,' YEAR =',I5,/,' MONTH =',I3
     3,/,' DAY =',I3,/,' NO. LINES =',I3,/,' LAHEY ERROR NO. =',I5
     4,/,' RUN STOPPED FROM RAIN BLOCK.')
C#### WCH, 10/2/96.  CHANGE 'RMFORT' TO 'LAHEY'.
 9501 FORMAT(' ERROR WHILE READING PRECIP. DATA ON UNIT',I3,' FOR WHAT S
     1HOULD BE A',/,' LINE 8 INPUT STRING.  LAHEY ERROR NO. =',I5,/,' SE
     2E OUTPUT FILE FOR COMPLETE MESSAGE.',/,
     3' RUN STOPPED FROM RAIN BLOCK.')
C#### WCH, 8/4/95.  CHANGE RMFOR TO LAHEY.
 9600 FORMAT(' ERROR WHILE READING PRECIP. DATA ON UNIT',I3,' FOR WHAT S
     1HOULD BE A',/,' LINE 9 INPUT STRING. CURRENT PARAMETER VALUES ARE:
     2',/,' LINE ID =',I3,/,' YEAR =',I5,/,' MONTH =',I3
     3,/,' DAY =',I3,/,' NO. LINES =',I3,/,' LINE NO. =',I3
     4,/,' HOUR =',I4,/,' MIN =',I4,/,' RAIN =',I5
     5,/,' LAHEY ERROR NO. =',I5
     6,/,' RUN STOPPED FROM RAIN BLOCK.')
C#### WCH, 10/2/96.  CHANGE 'RMFORT' TO 'LAHEY'.
 9601 FORMAT(' ERROR WHILE READING PRECIP. DATA ON UNIT',I3,' FOR WHAT S
     1HOULD BE A',/,' LINE 9 INPUT STRING.  LAHEY ERROR NO. =',I5,/,' SE
     2E OUTPUT FILE FOR COMPLETE MESSAGE.',/,
     3' RUN STOPPED FROM RAIN BLOCK.')
 9650 FORMAT(50A1)
 9651 FORMAT(' OFFENDING LINE FROM UNIT',I3,' MAY BE:',/,1X,50A1)
C#### WCH, 8/1/95.  TWO NEW WARNING MESSAGES.
 9655 FORMAT (/, ' $$$$ CAUTION.  AT LEAST ONE OF YEAR/MONTH/DAY IS .LE.
     1 ZERO ON UNIT',I3,/,' LIKELY HAVE EXTRA CARRIAGE RETURN AT END OF
     2INPUT PRECIPITATION DATA FILE.',/,' ASSUME END-OF-FILE AND CEASE D
     3ATA INPUT.')
 9660 FORMAT (/,' ERROR!!.  INCORRECT DAY OF YEAR COMPUTED IN SUB. GTRAI
     1N: IDAY =',I8,/,' HAS TO BE CAUSED BY WRONG YEAR/MONTH/DAY = ',
     2 3I5,/,' CURRENT YEAR SHOULD BE: NEWYR =',I5,/,
     3' CEASE DATA INPUT.  PERFORM FINAL RAIN ANALYSIS.')
C#### WCH, 10/2/96.
Cwch, 7/28/04
 9700 FORMAT(/,' ERROR DURING INITIAL READ OF UNIT',I3,/,
     1 ' Visual Fortran IOSTAT =',I5,/,
     2 ' CURRENT STATION ID VALUE =',A10)
C=======================================================================
      END
``` 
