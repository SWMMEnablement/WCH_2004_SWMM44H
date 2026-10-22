      SUBROUTINE WSTAT
C	RUNOFF BLOCK
C	CALLED BY SMSTAT NEAR LINE 29
C=======================================================================
C     SUBROUTINE TO PRINT SUMMARY STATS FOR WATERSHEDS
C     WRITTEN BY CHUCK MOORE, CDM, 8/93.
C     EDITED FOR STYLE BY WCH, 8/93.
C     METRIC CORRECTION FOR RAINFALL PRINTOUT, WCH, 11/15/93.
Cred  Change NGTO(I) to NAMEG(J) in write statements - 1/25/94
Cred  Change KGTO(I) to KAMEG(J) in write statements - 1/25/94
Cred  Add loop 9000 to find channel number - 1/25/94
C     Eliminate unneeded AREAPER, WCH, 6/5/95.
C     Use separate summer for pervious area losses, WCH, 3/28/02.
C     Fix goto ID for subcat to subcat routing, WCH, 3/28/02.
C     Add field width to subarea low-flow print-outs.  WCH, 4/15/02. 
C=======================================================================
C    note : programmed only for ft/pound/sec system 
C           some work on convertion factors required 
C           if metrics used   
C=======================================================================
C     SPECIFICATION STATEMENTS
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'RUNSTAT.INC'
	INCLUDE 'OVERLAND.INC'
C=======================================================================
      REAL LOSPER,PEAKPER,PEAKIMP,PEAKTOT
Cwch 3/28/02
	CHARACTER KDOUT*10
C=======================================================================
C     TOTAL RAINFALL IN INCHES
C     Convert in./hr * sec to in. by dividing by 3600 s/hr. 
C=======================================================================
CIM INCREASE HYETOGRAPHS  ~~~~~~~~~~~~~
      DO 10 J=1,MAXRG
  10  RTOT(J)=RTOT(J)/3600.
cim   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
C=======================================================================
CJES SUBCATCHMENT SUMMARY TABLE
C=======================================================================
      ICOUNT   = 0 
      DO 500 I = 1, NOW
      IF (WAREA(I).NE.0.0) THEN 
c.....
Cred  Change NGTO(I) to NAMEG(J) in write statements - 1/25/94
Cred  Add loop 9000 to find channel number - 1/25/94
c.....
Cwch 3/28/02. Must check for subcat to subcat routing for goto ID.
C     Since NGTO and KGTO are used for connectivity printouts in RHYDRO1,
C     have to search for correct ID. 
           IF(ISC2SC.EQ.1.AND.IFLOWP(I).GE.3) THEN
	          DO 8000 JJ = 1,NOW
	          DO 8000 KK = 1,NCP
	          IF(NWTOW(KK,JJ).EQ.I) J = JJ
 8000           CONTINUE
                IF(JCE.EQ.0) IDOUT = NAMEW(J)
	          IF(JCE.EQ.1) KDOUT = KAMEW(J)
		      ELSE 
                nogg                = nog + nsave
                if(nog.eq.999) nogg = nsave
                DO 9000 JJ = 1,nogg
                DO 9000 KK = 1,NCP
                IF(NWTOG(KK,JJ).EQ.I) J = JJ
 9000           CONTINUE
                IF(JCE.EQ.0) IDOUT = NAMEG(J)
	          IF(JCE.EQ.1) KDOUT = KAMEG(J)
	          ENDIF
C=======================================================================
C     RUNOFF IN INCHES
C=======================================================================
C     WRITE(N6,*) I,WAREA(I)
C     WRITE(N6,*) (WAR(J,I),J=1,4)
C     WRITE(N6,*) (SUBQPEAK(J,I),J=1,6)
C     WRITE(N6,*) (SUBDEP(J,I),J=1,4)
C     WRITE(N6,*) (RTOT(J),J=1,10)
C
           AREATOT = 0.
           AREAIMP = 0.
C#### WCH, 6/5/95.  ELIMINATE, NOT USED.           AREAPER = 0.
           FRACIMP = 0.
           RUNPER  = 0.
           LOSPER  = 0.
           PEAKPER = 0.
           RUNIMP  = 0.
           PEAKIMP = 0.
           RUNTOT  = 0.
           PEAKTOT = 0.
           UNITRF  = 0.
           ICOUNT  = ICOUNT + 1 
           IF(ICOUNT.GE.2)  GO TO 460
           IF(ICOUNT.EQ.42) ICOUNT = 0 
C=======================================================================
CJES   SUBCATCHMENT SUMMARY TABLE HEADER 
C=======================================================================
           WRITE(N6,1000)
           IF(METRIC.EQ.1) WRITE(N6,1001)
           IF(METRIC.EQ.2) WRITE(N6,1002)
 460       CONTINUE
C=======================================================================
CJES    FINDING STATS ON PERVIOUS FRACTION 
C=======================================================================
           IF(WAR(2,I).LE.0.0) GO TO 465
           RUNPER  =  SUBDEP(2,I)/WAR(2,I)*12.0
C#### WCH, 11/15/93
C           LOSPER  =  RTOT(NHYET(I))/CMET(5,METRIC) - RUNPER
Cwch 3/28/02.
           LOSPER  =  SUBQLOSS(I)/WAR(2,I)*12.0
           PEAKPER =  SUBQPEAK(2,I)
C=======================================================================
CJES    FINDING STATS ON IMPERVIOUS FRACTION 
CJES    COMBINING TWO IMPERVIOUS SHEET PLANES (K=1 AND 3)
C=======================================================================
  465      AREAIMP = WAR(1,I) + WAR(3,I) + WAR(4,I)
           IF(AREAIMP.LE.0.0) GO TO 470
           RUNIMP = (SUBDEP(1,I) + SUBDEP(3,I) +  SUBDEP(4,I))
     1        /AREAIMP*12.0
           AREAIMP = AREAIMP / 43560.
           PEAKIMP = SUBQPEAK(5,I)
C=======================================================================
CJES  FINDING STATS ON SUBCATCHMENT 
Cwch 4/11/02. Must check for redirected flow from imperv to perv etc.
C     Then only runoff is from subareas 1,3,4 (imperv) or 
c     subarea 2 (perv).
C=======================================================================
  470      AREATOT = WAR(1,I)+WAR(2,I)+WAR(3,I)+WAR(4,I)
           RUNTOT  = (SUBDEP(1,I)+SUBDEP(2,I)+SUBDEP(3,I)+SUBDEP(4,I))
     1        /AREATOT*12.0
           IF(IREDIR.EQ.1.AND.(IFLOWP(I).EQ.1.OR.IFLOWP(I).EQ.4)) 
     1          RUNTOT = SUBDEP(2,I)/AREATOT*12.0
           IF(IREDIR.EQ.1.AND.(IFLOWP(I).EQ.2.OR.IFLOWP(I).EQ.5)) 
     1          RUNTOT = (SUBDEP(1,I)+SUBDEP(3,I)+SUBDEP(4,I))
     2             /AREATOT*12.0
           AREATOT = AREATOT/43560.0
           FRACIMP = AREAIMP / AREATOT * 100.0
           UNITRF  = SUBQPEAK(6,I) /  AREATOT
           PEAKTOT = SUBQPEAK(6,I)
C
           NGAG = NHYET(I)
C#### WCH, 11/15/93.  NOTE, RAIN NEVER CONVERTED FROM METRIC.
           RTOTNG = RTOT(NGAG)
           IF(METRIC.EQ.2) THEN
                AREATOT = AREATOT/2.471
C####                RTOTNG  = RTOTNG*25.4
                RUNPER  = RUNPER*25.4
                LOSPER  = LOSPER*25.4
                PEAKPER = PEAKPER/35.315
                RUNIMP  = RUNIMP*25.4
                PEAKIMP = PEAKIMP/35.315
                RUNTOT  = RUNTOT*25.4
                PEAKTOT = PEAKTOT/35.315
                UNITRF  = UNITRF*25.4
                ENDIF
Cwch 3/28/02. Here, replace NAMEG(J) and KAMEG(J) with IDOUT and KDOUT.
           IF(JCE.EQ.0) WRITE(N6,1100) NAMEW(I),IDOUT,AREATOT,
     1                  FRACIMP,RTOTNG,RUNPER,LOSPER,PEAKPER,
     2                  RUNIMP,PEAKIMP,RUNTOT,PEAKTOT,UNITRF
           IF(JCE.EQ.1) WRITE(N6,1101) KAMEW(I),KDOUT,AREATOT,
     1                  FRACIMP,RTOTNG,RUNPER,LOSPER,PEAKPER,
     2                  RUNIMP,PEAKIMP,RUNTOT,PEAKTOT,UNITRF
           ELSE
           IF(JCE.EQ.0) WRITE(N6,1100) NAMEW(I),IDOUT,WAREA(I)
           IF(JCE.EQ.1) WRITE(N6,1101) KAMEW(I),KDOUT,WAREA(I)
           ENDIF
 500  CONTINUE 
      WRITE(N6,1200)
      RETURN
 1000 FORMAT('1'//35X,'SUMMARY STATISTICS FOR SUBCATCHMENTS',/35X,
     !36('=')//54X,'PERVIOUS AREA     IMPERVIOUS AREA    ',
     !' TOTAL SUBCATCHMENT AREA ',/
     !51X,'-------------------  -----------------  ',
     !'-------------------------',/
     !33X,'        TOTAL     TOTAL',
     !9X,'PEAK              PEAK               PEAK     PEAK',/,3X,
     !'             GUTTER                 SIMULATED   RUNOFF TOTAL ',
     !'RUNOFF   RUNOFF   RUNOFF      RUNOFF  RUNOFF   UNIT ',/,3X,
     !' SUBCATCH-  OR INLET  AREA  PERCENT  RAINFALL   DEPTH LOSSES ',
     !' RATE     DEPTH    RATE        DEPTH   RATE    RUNOFF')
 1001 FORMAT (3X,
     !' MENT NO.     NO.     (AC)   IMPER.   (IN)      (IN)   ',      
     !'(IN)   (CFS)     (IN)   (CFS)        (IN)   (CFS)   (IN/HR)',/,
     ! 3X,116('-'))
 1002 FORMAT (3X,
     !' MENT NO.     NO.     (HA)   IMPER.   (MM)      (MM)   ',      
     !'(MM)   (CMS)     (MM)   (CMS)        (MM)   (CMS)   (MM/HR)',/,
     ! 3X,116('-'))
Cwch, 4/15/02. Change subarea flow rates to F9.3 from F9.2.
 1100  FORMAT(1X,I10,1X,I10,F8.2,F8.1,F8.2,F10.3,F8.3,F8.3,F9.3,F9.3,
     !  F10.3,F9.3,F9.3)
 1101  FORMAT(1X,A10,1X,A10,F8.2,F8.1,F8.2,F10.3,F8.3,F8.3,F9.3,F9.3,
     !  F10.3,F9.3,F9.3)
 1200 FORMAT(/10X,' *** NOTE *** IMPERVIOUS AREA STATISTICS AGGREGATE',
     ! ' IMPERVIOUS AREAS WITH AND WITHOUT DEPRESSION STORAGE',/)
      END
