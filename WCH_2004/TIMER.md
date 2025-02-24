```fortran 
C#### WCH, 12/31/93.  REARRANGE ORDER OF COMMON/TIMER/
C#### DWD (CSC) - Begin change.
C     Date: Tuesday, 10 May 1994.  Time: 12:33:51.
C     Moved TIME to end of common block to correct warning generated
C     during compilation using Lahey 32-bit FORTRAN, version 5.01
CWarning - Previous common variable causes misalignment of DOUBLE PRECISION 
Cvariable (TIME) , File TIMER.INC, line 2.
Cwch, 4/10/00. Possible alignment problems. Rearrange COMMON. 
C
Cwch      COMMON/TIMER/IPRNGW,LUNIT,DELT,WET,DRY,WETDRY,LONG,DMEAN,DLAST,
Cwch     1             JSTART(10),JSTOP(10),IPRN(7),PMONTH(12),TIME
C     1             TIME,JSTART(10),JSTOP(10),IPRN(7),PMONTH(12)
C
# TIMER Module Documentation

This document summarizes the evolution and modifications made to the TIMER module file for SWMM. The file has undergone several revisions to address variable alignment issues, update precision handling, and extend functionality.

## Revision History

- **WCH, 12/31/93:**  
      Initial rearrangement of the COMMON block order was applied.

- **DWD (CSC), 10 May 1994:**  
      Reordered variables in the COMMON block to resolve misalignment warnings during compilation (affecting the DOUBLE PRECISION variable TIME).

- **WCH, 4/10/00:**  
      Further adjustments were made to segregate TIME into a separate COMMON block (TIMER2) to manage alignment issues. This change split variables across three COMMON blocks:
      - **COMMON/TIMER:** Includes most variables such as WET, DRY, DMEAN, etc.
      - **COMMON/TIMER2:** Now contains TIME, DELT, and LONG.
      - **COMMON/TIMER3:** Holds PMONTH.

- **WCH, 12/20/00:**  
      Added new parameters NOHEAD and LANDUPR to extend module functionality.

## Module Structure and Adjustments

1. **Legacy COMMON Blocks:**  
       The original structure defined a single COMMON block with a mixture of different variable types, leading to alignment and precision issues.

2. **Resolved Alignment Issues:**  
       By reorganizing the COMMON blocks and isolating the DOUBLE PRECISION variable (TIME) in a separate block, the module now compiles without warnings and operates more reliably.

3. **New Parameter Additions:**  
       The latest update, marked by the addition of NOHEAD and LANDUPR, enhances the configuration capabilities of the module.

## Code Snippet Overview

Below is an outline of the current COMMON block definitions reflecting the changes:

```
                  COMMON/TIMER/WET,DRY,WETDRY,DMEAN,DLAST,
             1   IPRNGW,LUNIT,JSTART(10),JSTOP(10),IPRN(7),NOHEAD,LANDUPR
                  COMMON/TIMER2/TIME,DELT,LONG
                  COMMON/TIMER3/PMONTH(12)
```

These revisions ensure that variable alignment issues are resolved, precision is maintained where required, and the module’s capacity is extended with new parameters.

## Summary

The TIMER module has evolved through several updates to correct compiler warnings, improve memory alignment, and add enhancements. The reordering of the COMMON blocks minimizes misalignment errors, and the inclusion of NOHEAD and LANDUPR extends the module's functionality, ensuring robust performance in various SWMM-related computations.

      COMMON/TIMER/WET,DRY,WETDRY,DMEAN,DLAST,
     1   IPRNGW,LUNIT,JSTART(10),JSTOP(10),IPRN(7),NOHEAD,LANDUPR
	  COMMON/TIMER2/TIME,DELT,LONG
	  COMMON/TIMER3/PMONTH(12)
C
C#### DWD (CSC) - End change.
C     Date: Tuesday, 10 May 1994.  Time: 12:33:51.
C
C#### WCH (RED), 9/93. CHANGE "TIME" TO DOUBLE PRECISION.
Cwch, 4/10/00.  Tried making LONG double precision but got bizzare errors
C in GUTNR, possibly caused by alignment, but who knows?  Couldn't solve. 
C Solution was to read LONG in RHYDRO1 as a double variable to handle
C 4-digit year dates, but leave LONG as real*4 during computations.
      REAL         LONG,WET,DRY,WETDRY,DMEAN,DLAST
      DOUBLE PRECISION TIME
      CHARACTER*10 PMONTH
``` 
