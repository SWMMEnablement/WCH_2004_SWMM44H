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
Cwch, 12/20/00. Add parameter NOHEAD and LANDUPR
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
