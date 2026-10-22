      SUBROUTINE PRFLOW(ISKIP)
C     RUNOFF BLOCK
C     CALLED BY RUNOFF
C=======================================================================
C     ISKIP = 0: Print flows for inlets and channel/pipes (M3 locations).
C     ISKIP = 1: Print stages for channel/pipes (M4 locations) and 
C                groundwater flow output (indicated on H2 line).
C     ISKIP = 2: Print flows for subcatchments (M5 locations).
C=======================================================================
C     PRFLOW was updated for SWMM release 4.0 by R.DICKINSON.
C     The most recent update: JANUARY, 1990.
C     Correct subscript for FLOW and add IF-STMT for FLEW = 0, WCH (RED),
C       9/23/93.
C     Ensure zero std.dev. if zero flows, WCH, 11/15/93.
C     Make time step averaging consistent with other blocks for volume
C       calculations, WCH, 11/30/93.
C     Fix subscript and index for print-out of titles for depth 
C       print-outs, WCH, 4/22/94.
C     Include negative sign in print ID, WCH, 12/5/94.
C     Add missing header print statement, WCH, 9/6/00.
C     Change ISKIP = 0 code to allow printing of subcatchment flows,
C       using ISKIP = 2.  WCH, 12/20/00.
C     Minor output format changes, WCH, 1/18/01.
C     Major increase in printout for groundwater, WCH, 7/1/03. 
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'GRWTR.INC'
      INCLUDE 'NEW88.INC'
Cwch, 12/20/00
      INCLUDE 'OVERLAND.INC.'
      DIMENSION FLOW(NW),FMEAN(3,10),DEV(3,10),TOTFLW(3,10)
      DIMENSION FMAX(3,10),FMIN(3,10)
      DIMENSION STGA(NGW),TH1A(NGW),GWFLWA(NGW),TOTDEV(3,10)
      CHARACTER*3 BMJ
      DATA BMJ/'   '/
C=======================================================================
      NSCRT2 = NSCRAT(2)
      NSCRT5 = NSCRAT(5)
      NSCRT7 = NSCRAT(7)
      IF(ISKIP.EQ.1) GO TO 2900
Cwch, 12/20/00
      IF(ISKIP.EQ.0.AND.NPRNT.EQ.0) GO TO 2900
	IF(ISKIP.EQ.1)                GO TO 2900
	IF(ISKIP.EQ.2.AND.MSUBC.EQ.0) RETURN
C=======================================================================
Cwch, 12/20/00.  In the code the follows, add IF statements to read
C     subcatchment outflows from NSCRAT(9) if ISKIP = 2.
C=======================================================================
C     Read the header information on the scratch file NNPRNT times.
C     NNPRNT = NPRNT = number of inlets requested on group M1, or
C     NNPRNT = MSUBC = number of subcatchments requested on group M5. 
C=======================================================================
	IF(ISKIP.EQ.0) THEN
		NNPRNT = NPRNT
	    NSCRTCH = NSCRT2
	    ELSE
          NNPRNT = MSUBC
          NSCRTCH = NSCRAT(9)
	    ENDIF
      DO 2000 J  = 1,NNPRNT,9
      REWIND NSCRTCH
      KOT        = J + 8
      TIME       = 0.0
      IF(KOT.GT.NNPRNT) KOT = NNPRNT
      IEND        = KOT - J + 1
      DO 2100 K   = 1,IEND
      FMAX(1,K)   = 0.0
      FMIN(1,K)   = 1.0E30
      DEV(1,K)    = 0.0
      TOTFLW(1,K) = 0.0
 2100 TOTDEV(1,K) = 0.0
      IF(METRIC.EQ.1) THEN
           IF(ISKIP.EQ.0) THEN
			WRITE(N6,1030) TITLE(1),TITLE(2)
              WRITE(N6,1040) (BMJ,K=J,KOT)
C#### WCH, 12/5/94.  INCLUDE NEGATIVE SIGN WHEN PRINTING OUTFLOWS.
	        IF(JCE.EQ.0) WRITE(N6,1041)
     +         (ISIGN(1,IPRNT(K))*NAMEG(IABS(IPRNT(K))),K=J,KOT)
              IF(JCE.EQ.1) WRITE(N6,1043) (KAMEG(IPRNT(K)),K=J,KOT) 
	        ENDIF
	     IF(ISKIP.EQ.2) THEN
              WRITE(N6,1031) TITLE(1),TITLE(2)
              WRITE(N6,1140) (BMJ,K=J,KOT)
	        IF(JCE.EQ.0) WRITE(N6,1041) (NAMEW(ISUBC(K)),K=J,KOT)
	        IF(JCE.EQ.1) WRITE(N6,1043) (KAMEW(ISUBC(K)),K=J,KOT)
	        ENDIF
           WRITE(N6,1045) (BMJ,K=J,KOT)
           WRITE(N6,1050) (BMJ,K=J,KOT)
           WRITE(N6,1055) (BMJ,K=J,KOT)
          ELSE
           IF(ISKIP.EQ.0) THEN
              WRITE(N6,1035) TITLE(1),TITLE(2)
              WRITE(N6,1040) (BMJ,K=J,KOT)
C#### WCH, 12/5/94.  INCLUDE NEGATIVE SIGN WHEN PRINTING OUTFLOWS.
              IF(JCE.EQ.0) WRITE(N6,1041)
     +         (ISIGN(1,IPRNT(K))*NAMEG(IABS(IPRNT(K))),K=J,KOT)
              IF(JCE.EQ.1) WRITE(N6,1043) (KAMEG(IPRNT(K)),K=J,KOT)
	        ENDIF
	     IF(ISKIP.EQ.2) THEN
              WRITE(N6,1036) TITLE(1),TITLE(2)
              WRITE(N6,1140) (BMJ,K=J,KOT)
	        IF(JCE.EQ.0) WRITE(N6,1041) (NAMEW(ISUBC(K)),K=J,KOT)
	        IF(JCE.EQ.1) WRITE(N6,1043) (KAMEW(ISUBC(K)),K=J,KOT)
	        ENDIF
Cwch, 9/6/00, Add this write statement.  
           WRITE(N6,1045) (BMJ,K=J,KOT)
           WRITE(N6,1060) (BMJ,K=J,KOT)
           WRITE(N6,1065) (BMJ,K=J,KOT)
          ENDIF
      KLINE     = 0
      INTCNT    = 0
      DLAST     = 0.0
C=======================================================================
C#### WCH, 11/30/93.  MAKE CONSISTENT SUMMATION WITH RUNOFF, COMBINE
C                     AND STATS.  IF PREVIOUS FLOW WAS ZERO, THEN USE
C                     NEW DELT FOR DMEAN.
C=======================================================================
      ITEST = 0
      DO 2800 M = 1, 1000000
      READ(NSCRTCH,END=2810) JULDAY,TIMDAY,DELT,(FLOW(N),N=1,NNPRNT)
      TIME      = TIME + DELT
      DMEAN     = 0.5 * (DELT + DLAST)
C#### WCH, 11/30/93.
      IF(ITEST.EQ.0) DMEAN = DELT
      DLAST     = DELT
      IF(KLINE.EQ.151) KLINE = 0
      INTCNT                 = INTCNT + 1
      CALL DATED
C=======================================================================
C     Caution! from here on, flows are in cubic met/sec if METRIC = 2.
C=======================================================================
      KK        = 0
      FLEW      = 0.0
C#### WCH, 11/30/93.
      ITEST = 0
      DO 2850 K = J,KOT
      IF(METRIC.EQ.2) FLOW(K) = FLOW(K) * 0.028317
C#### WCH, 11/30/93
      IF(FLOW(K).GT.0.0) ITEST = 1
      KK = KK + 1
      IF(FLOW(K).LT.FMIN(1,KK)) FMIN(1,KK) = FLOW(K)
C#### WCH (RED), 9/93.  CHANGE SUBSCRIPT J TO K FOR FLOW().
C#### WCH, 11/30/93        IF(FLOW(K).EQ.0.0) DLAST = 0.0
      IF(FLOW(K).GT.0.0) THEN
                   FLEW = FLEW + FLOW(K)
                   IF(FLOW(K).GT.FMAX(1,KK)) FMAX(1,KK) = FLOW(K)
                   TOTFLW(1,KK) = TOTFLW(1,KK) + DMEAN*FLOW(K)
                   TOTDEV(1,KK) = TOTDEV(1,KK) + DMEAN*FLOW(K)**2
                   ENDIF
 2850 CONTINUE
C#### WCH (RED), 9/93.  ADD IF-STMT FOR FLEW = 0.
C#### WCH, 11/30/93      IF(FLEW.EQ.0.0) DLAST = 0.0
      IF(FLEW.GT.0.0.AND.INTCNT.EQ.INTERV) KLINE = KLINE + 1
      IF(INTCNT.EQ.INTERV.AND.FLEW.GT.0.0) WRITE(N6,1610)
     +             MONTH,NDAY,NYEAR,JHR,MINUTE,(FLOW(K),K=J,KOT)
C=======================================================================
C     Write the title lines if 50 lines per page have been written.
Cwch, 12/20/00. Do not print intermediate headers if NOHEAD = 1.
C=======================================================================
      IF(INTCNT.LT.INTERV) GO TO 2800
      IF((KLINE.EQ.51.OR.KLINE.EQ.101.OR.KLINE.EQ.151).
     +  AND.NOHEAD.EQ.0) THEN
         IF(METRIC.EQ.1) THEN
           IF(ISKIP.EQ.0) THEN
			WRITE(N6,1030) TITLE(1),TITLE(2)
              WRITE(N6,1040) (BMJ,K=J,KOT)
C#### WCH, 12/5/94.  INCLUDE NEGATIVE SIGN WHEN PRINTING OUTFLOWS.
	        IF(JCE.EQ.0) WRITE(N6,1041)
     +         (ISIGN(1,IPRNT(K))*NAMEG(IABS(IPRNT(K))),K=J,KOT)
              IF(JCE.EQ.1) WRITE(N6,1043) (KAMEG(IPRNT(K)),K=J,KOT) 
	        ENDIF
	     IF(ISKIP.EQ.2) THEN
              WRITE(N6,1031) TITLE(1),TITLE(2)
              WRITE(N6,1140) (BMJ,K=J,KOT)
	        IF(JCE.EQ.0) WRITE(N6,1041) (NAMEW(ISUBC(K)),K=J,KOT)
	        IF(JCE.EQ.1) WRITE(N6,1043) (KAMEW(ISUBC(K)),K=J,KOT)
	        ENDIF
           WRITE(N6,1045) (BMJ,K=J,KOT)
           WRITE(N6,1050) (BMJ,K=J,KOT)
           WRITE(N6,1055) (BMJ,K=J,KOT)
          ELSE
           IF(ISKIP.EQ.0) THEN
              WRITE(N6,1035) TITLE(1),TITLE(2)
              WRITE(N6,1040) (BMJ,K=J,KOT)
C#### WCH, 12/5/94.  INCLUDE NEGATIVE SIGN WHEN PRINTING OUTFLOWS.
              IF(JCE.EQ.0) WRITE(N6,1041)
     +         (ISIGN(1,IPRNT(K))*NAMEG(IABS(IPRNT(K))),K=J,KOT)
              IF(JCE.EQ.1) WRITE(N6,1043) (KAMEG(IPRNT(K)),K=J,KOT)
	        ENDIF
	     IF(ISKIP.EQ.2) THEN
              WRITE(N6,1036) TITLE(1),TITLE(2)
              WRITE(N6,1140) (BMJ,K=J,KOT)
	        IF(JCE.EQ.0) WRITE(N6,1041) (NAMEW(ISUBC(K)),K=J,KOT)
	        IF(JCE.EQ.1) WRITE(N6,1043) (KAMEW(ISUBC(K)),K=J,KOT)
	        ENDIF
           WRITE(N6,1045) (BMJ,K=1,IEND)
           WRITE(N6,1060) (BMJ,K=1,IEND)
           WRITE(N6,1065) (BMJ,K=1,IEND)
          ENDIF
         ENDIF
      IF(INTCNT.EQ.INTERV) INTCNT = 0
 2800 CONTINUE
C=======================================================================
C     Calculate the event statistics for SWMM.
C=======================================================================
 2810 DO 2825 K   = 1,IEND
      FMEAN (1,K) = TOTFLW(1,K) / TIME
C#### WCH, 11/15/93
      DEV(1,K) = 0.0
      IF(TOTFLW(1,K).GT.0.0) THEN
                        ARG  = TOTDEV(1,K)/TIME - FMEAN(1,K)**2
                        IF(ARG.GT.0.0) DEV(1,K) = SQRT(ARG)
                        ENDIF
 2825 CONTINUE
      WRITE(N6,1960) (FMEAN(1,K),K=1,IEND)
      WRITE(N6,1970) (DEV(1,K),K=1,IEND)
      WRITE(N6,1975) (FMAX(1,K),K=1,IEND)
      WRITE(N6,1976) (FMIN(1,K),K=1,IEND)
      WRITE(N6,1980) (TOTFLW(1,K),K=1,IEND)
      IF(METRIC.EQ.1) WRITE(N6,1985) (BMJ,K=1,IEND)
      IF(METRIC.EQ.2) WRITE(N6,1990) (BMJ,K=1,IEND)
 2000 CONTINUE
Cwch, 12/20/00.
	IF(ISKIP.EQ.2) RETURN
C=======================================================================
C     Print out subsurface water information.
Cwch, 7/1/03. Make several changes related to printing five additional
C     variables, including printing table for just one subcat at a time. 
C=======================================================================
 2900 IF(NGWGF.EQ.0) GO TO 3900
C      DO 3000 IG = 1,NOGWSC,3
      DO 3000 IG = 1,NGWGF
C      KOT        = IG + 2
C      IF(KOT.GT.NOGWSC) KOT = NOGWSC
      REWIND NSCRT5
C      IEND        = KOT - IG + 1
C      DO 3100 J   = 1,3
      DO 3100 K   = 1,8
      FMAX(1,K)   = 0.0
      FMIN(1,K)   = 1.0E30
      TOTFLW(1,K) = 0.0
 3100 TOTDEV(1,K) = 0.0
      IF(JCE.EQ.0) WRITE(N6,2530) NAMWPR(IG),NGWTPR(IG)
      IF(JCE.EQ.1) WRITE(N6,2531) KMSUPR(IG),KGWTPR(IG)
      WRITE(N6,2540) 
      IF(METRIC.EQ.1) WRITE(N6,2550) 
      IF(METRIC.EQ.2) WRITE(N6,2555) 
      WRITE(N6,2560) 
      NTCOUN     = 0
      JLINE      = 0
      DO 3500 MA = 1,1000000
      READ(NSCRT5,END=3501) JULDAY,TIMDAY,DELT,(STGA(J8),
     +                       J8=1,NGWGF),(GWFLWA(J9),J9=1,NGWGF),
     +                       (TH1A(J7),J7=1,NGWGF),
     +             (ETUPR(J10),J10=1,NGWGF),(ETDPR(J11),J11=1,NGWGF),
     +             (DEPPR(J12),J12=1,NGWGF),(ENFPR(J13),J13=1,NGWGF),
     +             (EVGWPR(J14),J14=1,NGWGF)
      CALL DATED
      NTCOUN = NTCOUN + 1
      IF(NTCOUN.EQ.INTERV) JLINE = JLINE+1
Cwch, 7/1/03============================================================
C     For statistics, second subscript:
C     1 = stage (STGA)
C     2 = soil moisture (TH1A)
C     3 = groundwater outflow to channel (GWFLWA)
C     4 = groundwater outflow to deep perc (DEPPR)
C     5 = inflow to groundwater from infiltration (ENFPR)
C     6 = upper, unsaturated zone ET (ETUPR)
C     7 = lower, saturated zone ET (ETDPR)
C     8 = total subcatchment ET (EVGWPR)
C=======================================================================
C      KK        = 0
C      DO 3300 K = IG,KOT
      IF(METRIC.EQ.2) THEN
                      STGA(IG)   = STGA(IG)/CMET(1,METRIC)
                      GWFLWA(IG) = GWFLWA(IG)/CMET(8,METRIC)
	                DEPPR(IG)  = DEPPR(IG)/CMET(8,METRIC)
	                ENFPR(IG)  = ENFPR(IG)/CMET(8,METRIC)
	                ETUPR(IG)  = ETUPR(IG)/CMET(8,METRIC)
	                ETDPR(IG)  = ETDPR(IG)/CMET(8,METRIC)
	                EVGWPR(IG) = EVGWPR(IG)/CMET(8,METRIC)
                      ENDIF
C      KK = KK + 1
      TOTFLW(1,1) = TOTFLW(1,1) + DELT*STGA(IG)
      TOTDEV(1,1) = TOTDEV(1,1) + DELT*STGA(IG)**2.0
      IF(STGA(IG).GT.FMAX(1,1))   FMAX(1,1) = STGA(IG)
      IF(STGA(IG).LT.FMIN(1,1))   FMIN(1,1) = STGA(IG)
      TOTFLW(1,2) = TOTFLW(1,2) + DELT*TH1A(IG)
      TOTDEV(1,2) = TOTDEV(1,2) + DELT*TH1A(IG)**2
      IF(TH1A(IG).GT.FMAX(1,2))   FMAX(1,2) = TH1A(IG)
      IF(TH1A(IG).LT.FMIN(1,2))   FMIN(1,2) = TH1A(IG)
      TOTFLW(1,3) = TOTFLW(1,3) + DELT*GWFLWA(IG)
      TOTDEV(1,3) = TOTDEV(1,3) + DELT*GWFLWA(IG)**2
      IF(GWFLWA(IG).GT.FMAX(1,3)) FMAX(1,3) = GWFLWA(IG)
      IF(GWFLWA(IG).LT.FMIN(1,3)) FMIN(1,3) = GWFLWA(IG)
      TOTFLW(1,4) = TOTFLW(1,4) + DELT*DEPPR(IG)
      TOTDEV(1,4) = TOTDEV(1,4) + DELT*DEPPR(IG)**2
      IF(DEPPR(IG).GT.FMAX(1,4)) FMAX(1,4) = DEPPR(IG)
      IF(DEPPR(IG).LT.FMIN(1,4)) FMIN(1,4) = DEPPR(IG)
      TOTFLW(1,5) = TOTFLW(1,5) + DELT*ENFPR(IG)
      TOTDEV(1,5) = TOTDEV(1,5) + DELT*ENFPR(IG)**2
      IF(ENFPR(IG).GT.FMAX(1,5)) FMAX(1,5) = ENFPR(IG)
      IF(ENFPR(IG).LT.FMIN(1,5)) FMIN(1,5) = ENFPR(IG)
      TOTFLW(1,6) = TOTFLW(1,6) + DELT*ETUPR(IG)
      TOTDEV(1,6) = TOTDEV(1,6) + DELT*ETUPR(IG)**2
      IF(ETUPR(IG).GT.FMAX(1,6)) FMAX(1,6) = ETUPR(IG)
      IF(ETUPR(IG).LT.FMIN(1,6)) FMIN(1,6) = ETUPR(IG)
      TOTFLW(1,7) = TOTFLW(1,7) + DELT*ETDPR(IG)
      TOTDEV(1,7) = TOTDEV(1,7) + DELT*ETDPR(IG)**2
      IF(ETDPR(IG).GT.FMAX(1,7)) FMAX(1,7) = ETDPR(IG)
      IF(ETDPR(IG).LT.FMIN(1,7)) FMIN(1,7) = ETDPR(IG)
      TOTFLW(1,8) = TOTFLW(1,8) + DELT*EVGWPR(IG)
      TOTDEV(1,8) = TOTDEV(1,8) + DELT*EVGWPR(IG)**2
      IF(EVGWPR(IG).GT.FMAX(1,8)) FMAX(1,8) = EVGWPR(IG)
      IF(EVGWPR(IG).LT.FMIN(1,8)) FMIN(1,8) = EVGWPR(IG)
C=======================================================================
Cwch, 12/20/00.  No intermediate headers if NOHEAD = 1.
C=======================================================================
      IF(JLINE.EQ.51.AND.NOHEAD.EQ.0) THEN
                      IF(JCE.EQ.0) WRITE(N6,2530) NAMWPR(IG),NGWTPR(IG)
                      IF(JCE.EQ.1) WRITE(N6,2531) KMSUPR(IG),KGWTPR(IG)
                      WRITE(N6,2540) 
                      IF(METRIC.EQ.1) WRITE(N6,2550) 
                      IF(METRIC.EQ.2) WRITE(N6,2555) 
                      WRITE(N6,2560) 
                      JLINE = 1
                      ENDIF
      IF(NTCOUN.EQ.INTERV)  WRITE(N6,2570) MONTH,NDAY,NYEAR,JHR,MINUTE,
     +      STGA(IG),TH1A(IG),GWFLWA(IG),DEPPR(IG),ENFPR(IG), 
     +      ETUPR(IG),ETDPR(IG),EVGWPR(IG)
      IF(NTCOUN.EQ.INTERV)  NTCOUN = 0
 3500 CONTINUE
C=======================================================================
C     End of loop for time-step file reads.  End of file exit to 3501. 
C=======================================================================
C 3501 DO 3400 J  = 1,3
 3501 DO 3400 K  = 1,8
      FMEAN(1,K) = TOTFLW(1,K)/TIME
      DEV(1,K)   = 0.0
      GARG       = TOTDEV(1,K)/TIME - FMEAN(1,K)**2.0
 3400 IF(GARG.GT.0.0) DEV(1,K) = SQRT(GARG)
                      WRITE(N6,2575) (FMEAN(1,K),K=1,8)
                      WRITE(N6,2580) (DEV(1,K),K=1,8)
                      WRITE(N6,2581) (FMAX(1,K),K=1,8)
                      WRITE(N6,2582) (FMIN(1,K),K=1,8)
                      WRITE(N6,2585) (TOTFLW(1,K),K=3,8)
      IF(METRIC.EQ.1) WRITE(N6,2586) (BMJ,J=1,IEND)
      IF(METRIC.EQ.2) WRITE(N6,2587) (BMJ,J=1,IEND)
 3000 CONTINUE
 3900 CONTINUE
C=======================================================================
C     Write the channel depth information.
C=======================================================================
      IF(MDEEP.GT.0) THEN
      DO 4000 J  = 1,MDEEP,9
      REWIND NSCRT7
      KOT        = J + 8
      TIME       = 0.0
C#### WCH, 4/22/94.  CHANGE NPRNT TO MDEEP.
C####      IF(KOT.GT.NPRNT) KOT = MDEEP
      IF(KOT.GT.MDEEP) KOT = MDEEP
      IEND        = KOT - J + 1
      DO 4100 K   = 1,IEND
      FMAX(1,K)   = 0.0
      FMIN(1,K)   = 1.0E30
      DEV(1,K)    = 0.0
      TOTFLW(1,K) = 0.0
 4100 TOTDEV(1,K) = 0.0
      IF(METRIC.EQ.1) THEN
                      WRITE(N6,4030) TITLE(1),TITLE(2)
                      WRITE(N6,4040) (BMJ,K=J,KOT)
                      IF(JCE.EQ.0) WRITE(N6,4041)
     +                             (NAMEG(IDEEP(K)),K=J,KOT)
                      IF(JCE.EQ.1) WRITE(N6,4043)
     +                             (KAMEG(IDEEP(K)),K=J,KOT)
                      WRITE(N6,4045) (BMJ,K=J,KOT)
                      WRITE(N6,4050) (BMJ,K=J,KOT)
                      WRITE(N6,4055) (BMJ,K=J,KOT)
                      ELSE
                      WRITE(N6,4035) TITLE(1),TITLE(2)
                      WRITE(N6,4040) (BMJ,K=J,KOT)
                      IF(JCE.EQ.0) WRITE(N6,4041)
     +                             (NAMEG(IDEEP(K)),K=J,KOT)
                      IF(JCE.EQ.1) WRITE(N6,4043)
     +                             (KAMEG(IDEEP(K)),K=J,KOT)
                      WRITE(N6,4060) (BMJ,K=J,KOT)
                      WRITE(N6,4065) (BMJ,K=J,KOT)
                      ENDIF
      KLINE     = 0
      INTCNT    = 0
      DO 4800 M = 1, 1000000
      READ(NSCRT7,END=4810) JULDAY,TIMDAY,DELT,(GDEPTH(N),N=1,MDEEP)
      TIME      = TIME + DELT
      IF(KLINE.EQ.151) KLINE = 0
      INTCNT                 = INTCNT + 1
      CALL DATED
      KK        = 0
      FLEW      = 0.0
      DO 4850 K = J,KOT
      JJ        = IDEEP(K)
      IF(NPG(JJ).EQ.2) THEN
                       THETA     = COS(GDEPTH(K))*GWIDTH(JJ)/2.0
                       GDEPTH(K) = GWIDTH(JJ)/2.0 - THETA
                       IF(GDEPTH(K).LT.0.0) THEN
                            THETA     = COS(3.1415927-GDEPTH(K)) *
     +                                               GWIDTH(JJ)/2.0
                            GDEPTH(K) = THETA + GWIDTH(JJ)/2.0
                            ENDIF
                       ENDIF
      GDEPTH(K) = GDEPTH(K)/CMET(1,METRIC)
      KK = KK + 1
      IF(GDEPTH(K).LT.FMIN(1,KK)) FMIN(1,KK) = GDEPTH(K)
      IF(GDEPTH(K).GT.0.0) THEN
                   FLEW = FLEW + GDEPTH(K)
                   IF(GDEPTH(K).GT.FMAX(1,KK)) FMAX(1,KK) = GDEPTH(K)
                   TOTFLW(1,KK) = TOTFLW(1,KK) + DELT*GDEPTH(K)
                   TOTDEV(1,KK) = TOTDEV(1,KK) + DELT*GDEPTH(K)**2
                   ENDIF
 4850 CONTINUE
      IF(INTCNT.EQ.INTERV) KLINE = KLINE + 1
      IF(INTCNT.EQ.INTERV) WRITE(N6,1610)
     +             MONTH,NDAY,NYEAR,JHR,MINUTE,(GDEPTH(K),K=J,KOT)
C=======================================================================
C     WRITE THE TITLE LINES IF 50 LINES PER PAGE HAVE BEEN WRITTEN.
Cwch, 12/20/00.  Do this only if NOHEAD = 0.
C=======================================================================
      IF(INTCNT.LT.INTERV) GO TO 4800
      IF((KLINE.EQ.51.OR.KLINE.EQ.101.OR.KLINE.EQ.151).
     +    AND.NOHEAD.EQ.0) THEN
                   IF(METRIC.EQ.1) THEN
                         WRITE(N6,4030) TITLE(1),TITLE(2)
                         WRITE(N6,4040) (BMJ,K=1,IEND)
                         IF(JCE.EQ.0) WRITE(N6,4041)
C#### WCH, 4/22/94.  CHANGE SUBCRIPT IPRNT TO IDEEP IN THESE TWO LINES.
     +                             (NAMEG(IABS(IDEEP(K))),K=J,KOT)
                         IF(JCE.EQ.1) WRITE(N6,4043)
     +                             (KAMEG(IDEEP(K)),K=J,KOT)
                         WRITE(N6,4045) (BMJ,K=1,IEND)
                         WRITE(N6,4050) (BMJ,K=1,IEND)
                         WRITE(N6,4055) (BMJ,K=1,IEND)
                         ELSE
                         WRITE(N6,4035) TITLE(1),TITLE(2)
                         WRITE(N6,4040) (BMJ,K=1,IEND)
                         IF(JCE.EQ.0) WRITE(N6,4041)
     +                             (NAMEG(IABS(IPRNT(K))),K=J,KOT)
                         IF(JCE.EQ.1) WRITE(N6,4043)
     +                             (KAMEG(IPRNT(K)),K=J,KOT)
                         WRITE(N6,4045) (BMJ,K=1,IEND)
                         WRITE(N6,4060) (BMJ,K=1,IEND)
                         WRITE(N6,4065) (BMJ,K=1,IEND)
                         ENDIF
                   ENDIF
      IF(INTCNT.EQ.INTERV) INTCNT = 0
 4800 CONTINUE
C=======================================================================
C     Calculate the event statistics for SWMM.
C=======================================================================
 4810 DO 4825 K   = 1,IEND
      FMEAN (1,K) = TOTFLW(1,K) / TIME
C#### WCH, 11/15/93
      DEV(1,K) = 0.0
      IF(TOTFLW(1,K).GT.0.0) THEN
                        ARG  = TOTDEV(1,K)/TIME - FMEAN(1,K)**2.0
                        IF(ARG.GT.0.0) DEV(1,K) = SQRT(ARG)
                        ENDIF
 4825 CONTINUE
      WRITE(N6,1960) (FMEAN(1,K),K=1,IEND)
      WRITE(N6,1970) (DEV(1,K),K=1,IEND)
      WRITE(N6,1975) (FMAX(1,K),K=1,IEND)
      WRITE(N6,1976) (FMIN(1,K),K=1,IEND)
 4000 CONTINUE
C=======================================================================
C     ENDIF for IF-BLOCK for depth print-out.
C=======================================================================
      ENDIF
C=======================================================================
 1030 FORMAT(1H1,/,
     1' **********************************************',/,
     1' *  Summary of quantity results (flow in cfs) *',/,
     1' **********************************************',//,
     14X,A80,/,4X,A80,/)
 1031 FORMAT(1H1,/,
     1' ***********************************************************',/,
     1' *  Summary of SUBCATCHMENT quantity results (flow in cfs) *',/,
     1' ***********************************************************',//,
     14X,A80,/,4X,A80,/)
 1035 FORMAT(1H1,/,
     1' **********************************************',/,
     1' *  Summary of quantity results (flow in cms) *',/,
     1' **********************************************',//,
     14X,A80,/,4X,A80,/)
 1036 FORMAT(1H1,/,
     1' ***********************************************************',/,
     1' *  Summary of SUBCATCHMENT quantity results (flow in cms) *',/,
     1' ***********************************************************',//,
     14X,A80,/,4X,A80,/)
 1040 FORMAT(/,21X,10(A3,'Chan/Inlt'))
 1140 FORMAT(/,21X,10(A3,'Subcatmt.'))
C#### WCH, 12/5/94.  CHANGE FROM 5X,I7 TO 3X,I9.
 1041 FORMAT(21X,10(2X,I10))
 1043 FORMAT(22X,10(2X,A10))
 1045 FORMAT(6X,'Date',5X,'Time',3X,10(A3,2X,'  Flow '))
 1050 FORMAT(2X,'Mo/Da/Year',2X,'Hr:Min ',10(A3,6X,'Cfs'))
 1055 FORMAT(2X,'----------',2X,'------ ',10(A3,5X,'----'))
 1060 FORMAT(2X,'Mo/Da/Year',2X,'Hr:Min ',10(A3,'Cubic m/s'))
 1065 FORMAT(2X,'----------',2X,'------ ',10(A3,'---------'))
 1560 FORMAT(1X,I2,'/',I2,'/',I4,I5)
 1610 FORMAT(2X,I2,'/',I2,'/',I4,2X,I2,I4,1X,10F12.3)
Cwch, 1/18/01.  Minor spacing changes.
 1960 FORMAT(/,'Flow wt''d means......',10(F12.4))
 1970 FORMAT(  'Flow wt''d std-devs...',10(F12.4))
 1975 FORMAT(   'Maximum value........',10(F12.4))
 1976 FORMAT(   'Minimum value........',10(F12.4))
 1980 FORMAT(   'Total loads..........',10(1PE12.4))
 1985 FORMAT(20X,10(A3,' Cubic-ft'))
 1990 FORMAT(20X,10(A3,'  Cubic-m'))
 2530 FORMAT(1H1,/,
     1' *************************************************',/,
     1' *  Subsurface Summary for Stage, Soil Moisture, *',/,
     1' *   Outflows, Inflow, and ET                    *',/,
     1' *************************************************',/,
     2' *   Flow from Subcatchment # to Channel/Pipe #  *',/,
     1' *************************************************',//,
     3 21X,3(4X,I10,' to ',I10,3X))
 2531 FORMAT(1H1,/,
     1' *************************************************',/,
     1' *  Subsurface Summary for Stage, Soil Moisture, *',/,
     1' *                     and Groundwater Flow      *',/,
     1' *************************************************',/,
     2' *   Flow from Subcatchment # to Channel/Pipe #  *',/,
     1' *************************************************',//,
     3 21X,3(4X,A10,' to ',A10,3X))
Cwch, 7/1/03. Changes to formats as add five new columns. 
 2540 FORMAT(/,26X,'Stage      Soil  Ch. Outflow  ',
     1'Deep Perc. Outflow   Infil. Inflow    ET unsat.     ET sat. ',
     2' ET Tot. Subcat.')
 2550 FORMAT(2X,'Mo/Da/Year   Hr:Min',T27,
     1             '   ft  Moisture          cfs  ',
     2'               cfs             cfs         cfs         cfs  ',
	3'            cfs')
 2555 FORMAT(2X,'Mo/Da/Year   Hr:Min',T27,
     1             '    m  Moisture        cum/s  ',
     2'             cum/s           cum/s        cum/s      cum/s  ',
	3'          cum/s')
 2560 FORMAT(2X,'----------   ------',T26,
     1             '------   -------   ----------   ',
	2' ----------------     -----------   ---------  ---------- ',
	3'   -------------')
 2570 FORMAT(2X,2(I2,'/'),I4,3X,I2,I4,2F10.3,F13.4,F20.5,F16.5,1PE12.4,
     11PE12.4,1PE17.4)
 2575 FORMAT(/,'Flow wt''d means......',2F10.3,F13.4,F20.5,F16.5,
     1 1PE12.4,1PE12.4,1PE17.4)
 2580 FORMAT  ('Flow wt''d std-devs...',2F10.3,F13.4,F20.5,F16.5,
     1 1PE12.4,1PE12.4,1PE17.4)
 2581 FORMAT   ('Maximum value........',2F10.3,F13.4,F20.5,F16.5,
     1 1PE12.4,1PE12.4,1PE17.4)
 2582 FORMAT   ('Mininum value........',2F10.3,F13.4,F20.5,F16.5,
     1 1PE12.4,1PE12.4,1PE17.4)
 2585 FORMAT   ('Total loads..........',20X,1PE13.4,1PE20.5,1PE16.5,
     1 1PE12.4,1PE12.4,1PE17.4)
 2586 FORMAT(42X,'  Cubic feet...')
 2587 FORMAT(42X,'Cubic meters...')
 4030 FORMAT(1H1,/,
     1' ################################################',/,
     1' #  Summary of quantity results (Depth in feet) #',/,
     1' ################################################',//,
     14X,A80,/,4X,A80,/)
 4035 FORMAT(1H1,/,
     1' ##################################################',/,
     1' #  Summary of quantity results (Depth in meters) #',/,
     1' ##################################################',//,
     14X,A80,/,4X,A80,/)
 4040 FORMAT(/,19X,10(A3,' Channel '))
 4041 FORMAT(19X,10(2X,I10))
 4043 FORMAT(20X,10(2X,A10))
 4045 FORMAT(4X,'Date',5X,'Time',3X,10(A3,2X,' Depth '))
 4050 FORMAT(2X,'Mo/Da/Year',2X,'HR:MIN ',10(A3,5X,'Feet'))
 4055 FORMAT(2X,'--------',2X,'------ ',10(A3,5X,'----'))
 4060 FORMAT(2X,'Mo/Da/Year',2X,'Hr:Min ',10(A3,'  Meters '))
 4065 FORMAT(2X,'--------',2X,'------ ',10(A3,'---------'))
C=======================================================================
      RETURN
      END
