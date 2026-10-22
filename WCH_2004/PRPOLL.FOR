      SUBROUTINE PRPOLL(ISKIP)
C     RUNOFF BLOCK
C	CALLED BY SUBROUTINE RUNOFF NEAR LINE 272
C=======================================================================
C     PRPOLL created for SWMM release 4.0 by R.DICKINSON.
C     WCH, 8/6/93.  ADD MESSAGE ABOUT QUALITY CONTINUITY ERROR.
C     WCH, 9/3/93.  ADD I/I QUALITY TO CONTINUITY.
C     WCH, 11/15/93.  CHECK FOR NON-ZERO MINIMA WHEN ALL FLOW = 0.
C     WCH, 11/30/93.  MAKE TIME STEP AVERAGING CONSISTENT WITH OTHER
C       BLOCKS FOR VOLUME/LOAD CALCULATIONS.
C     WCH, 4/7/94. ADD MESSAGE ABOUT BUILDUP TOTALS.
C     WCH, 1/23/97. ALTER HEADER FOR QUALITY SUMMARY.
C     CIM, 4/5/99   Change to increase maximum number of parameter 
C       to MQUAL.
C     WCH, 12/20/00. Add ISKIP = 1 option to use same code to print
C       subcatchment flows and quality.  Also, add additional quality
C       sumations.  Add summations for overland flow removal. 
C     WCH, 2/10.01.  Include output of channel/pipe losses.  
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'NEW88.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'QUALTY.INC'
C#### WCH, 9/93
      INCLUDE 'RDII.INC'
Cwch, 12/20/00
      INCLUDE 'OVERLAND.INC'
      CHARACTER BMJ*3,UNTN(4)*8,EFMT*8,FOROUT(MQUAL+9)*35,
     1          CLOSER*1,UTOT(MQUAL)*8,LINOUT(2)*4,IDASH*4,KOCGUT*10
      DIMENSION QUAL(MQUAL,NW),FLOW(NW),FMAX(MQUAL+1),FMIN(MQUAL+1)
C#### WCH, 9/93, 12/20/00
Cwch, 2/10/01
      DIMENSION PCENT(MQUAL,14),PSUM(MQUAL,9),PCRDIS(MQUAL),
     1PCRDIT(MQUAL),SPR(MQUAL),PC1CHP(MQUAL),PC2CHP(MQUAL)
      DIMENSION TQUALC(MQUAL),TQUALS(MQUAL),CMEAN(MQUAL),
     1STNDEV(MQUAL),TQUALF(MQUAL)
Cwch, 12/20/00
	CHARACTER PRNAME(2)*9,PRNAMP*9
C=======================================================================
      DATA LINOUT/' IN','OUT'/
      DATA EFMT/',1PE10.3'/,CLOSER/')'/
      DATA UNTN/'  POUNDS','TOT QUAN',' Q*C*DT ','KILOGRAM'/
      DATA BMJ/'   '/,IDASH/'----'/
	DATA PRNAME/'Location ','Subcatmt '/
C=======================================================================
C####      FOROUT(13) = '(''Flow wtd means.....'',F11.4'
C####      FOROUT(14) = '(''Flow wtd std devs..'',F11.4'
C#### WCH, 12/2/93. MAKE PRINTOUTS CONSISTENT AMONG SUBROUTINES.
C#### DWD (CSC) - Begin change.
C     Date: Tuesday, 10 May 1994.  Time: 12:43:23.
C     Modified hollerith value to correct fatal error generated
C     during compilation using Lahey 32-bit FORTRAN, version 5.01
C     Fatal - Context requires INTEGER Hollerith constant to be: 4Htext.
C      FOROUT(13) = 29H('Flow wt''d means....',F11.4
C      FOROUT(14) = 29H('Flow wt''d std devs.',F11.4
      FOROUT(MQUAL+3) = '(''Flow wtd means.......'',F11.4'
      FOROUT(MQUAL+4) = '(''Flow wtd std devs....'',F11.4'
C#### DWD (CSC) - End change.
C     Date: Tuesday, 10 May 1994.  Time: 12:43:23.
      FOROUT(MQUAL+5) = '(''Maximum value........'',F11.3'
      FOROUT(MQUAL+6) = '(''Minimum value........'',F11.3'
      FOROUT(MQUAL+7) = '(''Total loads..........'',1PE11.3'
      FOROUT(MQUAL+8) = '(2X,I2,''/'',I2,''/'',I4,2X,I2,I5,F11.3'
      FOROUT(MQUAL+9) = '                      '
      DO 50 J    = 2,12
  50  FOROUT(J)  = '                        '
      NSCRT2     = NSCRAT(2)
Cwch, 12/20/00
	IF(ISKIP.EQ.1) GO TO 600
      LUP = 0
	IF(JLAND.GT.1.AND.LANDUPR.EQ.1) LUP = 1
C=======================================================================
C********** CHANGE THE UNITS OF ARRAY SUM FROM MG AND MG/SEC
C**********                  TO KG AND KG/SEC
C********** IF METRIC EQUALS 1, CHANGE THE UNITS TO LBS AND LBS/SEC
C********** IF NDIM(J) GT 0 THEN DO NOT DIVIDE BY 1.0E06
C=======================================================================
      DO 280 J = 1,NQS
      KDIM     = NDIM(J) + 1
      GWQ(J)   = GWQ(J)  * FACT3(KDIM)
      IF(KDIM.EQ.1)   GWQ(J)    = GWQ(J)    / 1.0E06
C#### WCH, 9/93
      IF(KDIM.EQ.1)   SUMRDII(J) = SUMRDII(J)/1.0E6
      IF(METRIC.EQ.1) THEN
                      GWQ(J)    = GWQ(J)    * 2.2046
                      REFF(1,J) = REFF(1,J) * 2.2046
C#### WCH, 9/93.
                      SUMRDII(J) = SUMRDII(J)*2.2046
                      ENDIF
 280  CONTINUE
      DO 400 K = 1,10
      DO 399 I = 1,NQS
      KDIM     = NDIM(I) + 1
      IF(KDIM.EQ.1) THEN
                    SUM(I,K)    = SUM(I,K)    / 1.0E06
Cwch, 2/10/01
                    IF(K.LE.2) SUMREMG(I,K) = SUMREMG(I,K) / 1.0E06
	              IF(K.EQ.1) SUMWAT(I)    = SUMWAT(I)    /1.0E6
                    IF(LUP.EQ.1) THEN
	                 DO J = 1,JLAND
	                 SUMNL(J,I,K)  = SUMNL(J,I,K) / 1.0E6
	                 IF(K.EQ.1) SUMNL(J,I,11) = SUMNL(J,I,11) / 1.0E6
	                 ENDDO
	                 ENDIF
                    IF(METRIC.EQ.1) THEN
                       SUM(I,K)     = SUM(I,K)     * 2.2046
                       PSHED(1,I,K) = PSHED(1,I,K) * 2.2046
                       IF(K.EQ.1) SUMWAT(I) = SUMWAT(I)*2.2046
                       PBASIN(I,K)  = PBASIN(I,K)  * 2.2046
Cwch, 2/10/01
                       IF(K.LE.2) SUMREMG(I,K) = SUMREMG(I,K)*2.2046
                       IF(LUP.EQ.1) THEN
	                    DO J = 1,JLAND
	                    SUMNL(J,I,K) = SUMNL(J,I,K) * 2.2046
	                 IF(K.EQ.1) SUMNL(J,I,11) = SUMNL(J,I,11) * 2.2046
	                    ENDDO
	                    ENDIF
                       ENDIF
                    ENDIF
C=======================================================================
C****** SUM(I,1)  STORES THE INITIAL BUILDUP FROM QSHED
C****** SUM(I,3)  STORES THE INITIAL CATCHBASIN LOADS FROM QSHED
C******           NOW THEY MUST BE ADDED TO SUM(I,2) AND SUM(I,4)
C=======================================================================
      IF(K.EQ.2) THEN
	    SUM(I,K) = SUM(I,K) + SUM(I,1)
	    IF(LUP.EQ.1) THEN
	       DO J = 1,JLAND
	       SUMNL(J,I,K) = SUMNL(J,I,K) + SUMNL(J,I,1)
	       ENDDO
	       ENDIF
	    ENDIF
      IF(K.EQ.4) THEN
	    SUM(I,K) = SUM(I,K) + SUM(I,3)
	    IF(LUP.EQ.1) THEN
	       DO J = 1,JLAND
	       SUMNL(J,I,K) = SUMNL(J,I,K) + SUMNL(J,I,3)
	       ENDDO
	       ENDIF
	    ENDIF
C=======================================================================
C********** CONVERT SUM(I,10) FROM UNITS OF QUANTITY * FLOW TO
C********** UNITS OF QUANTITY, I.E., KG, MPN, OR STANDARD UNITS
C     SUM(I,10) = load into inlets, summed in Sub. GUTTER. 
C=======================================================================
      IF(K.EQ.10) SUM(I,K)  = SUM(I,K)  * FACT3(KDIM) * CMET(8,METRIC)
 399  CONTINUE
 400  CONTINUE
C=======================================================================
C********* CALCULATE PSUM BASED ON ARRAY SUM FROM QSHED
C*********
C*********       PSUM(K,1) = TOTAL SURFACE AND CATCHBASIN BUILDUP
C*********       PSUM(K,2) = NET SURFACE BUILDUP
C*********       PSUM(K,3) = TOTAL WASHOFF
C*********       PSUM(K,4) = TOTAL SUBCATCHMENT LOAD
C*********       PSUM(K,5) = FLOW WEIGHTED INLET CONCENTRATION
C*********                   INLET LOAD / TOTAL PIPE/CHANNEL FLOW
C*********       PSUM(K,6) = PSUM(K,4) - GROUNDWATER AND RDII LOADS.
C*********       PSUM(K,7) = REMOVAL BY INFILTRATION
C*********       PSUM(K,8) = REMOVAL BY BMP FRACTION
C*********       PSUM(K,9) = REMOVAL BY FIRST ORDER DECAY
C=======================================================================
Cwch, 12/20/00.  Need some new statement numbers, so change:
C                Old		New
C                500		420
C                550		440
C                600		460
C=======================================================================
      DO 420 K  = 1,NQS
      PSUM(K,1) = SUM(K,2) + SUM(K,4)
      PSUM(K,2) = SUM(K,2) - SUM(K,5)
      PSUM(K,3) = SUM(K,6) + SUM(K,7)
C#### WCH, 9/93
C     SUM(K,6) = surface washoff
C     SUM(K,7) = catchbasin washoff
C     SUM(K,8) = fractions from other constituents
C     SUM(K,9) = precipitation load
      PSUM(K,4) = SUM(K,8) + SUM(K,9) + PSUM(K,3) + GWQ(K) + SUMRDII(K)
	PSUM(K,6) = SUM(K,8) + SUM(K,9) + PSUM(K,3)
      KDIM      = NDIM(K)  + 1
      IF ((CNT(5) * FACT3(KDIM)).NE.0.0) THEN
          PSUM(K,5) = SUM(K,10) / (CNT(5) * FACT3(KDIM))
          ELSE
          PSUM(K,5) = 0.0
          END IF
      IF(METRIC.EQ.2) PSUM(K,5) = PSUM(K,5) * 0.02831605
C=======================================================================
C     IF KDIM EQUALS ZERO RECONVERT THE CONCENTRATION BACK TO
C        MG/L FROM POUNDS OR KILOGRAMS.
C=======================================================================
      IF(KDIM.EQ.1) THEN
                                    PSUM(K,5) = PSUM(K,5) * 1.0E06
                    IF(METRIC.EQ.1) PSUM(K,5) = PSUM(K,5) / 2.2046
                    ENDIF
Cwch, 12/20/00
      IF(ISC2SC.EQ.1) THEN
	       DO II = 1,3
	       I = II + 6
	       PSUM(K,I) = 0.0
          	   DO J = 1,JLAND
	           PSUM(K,I)     = PSUM(K,I) + SUMREM(J,K,II)
	           SUMREM(J,K,II) = SUMREM(J,K,II)/1.0E6
	           IF(METRIC.EQ.1) SUMREM(J,K,II) = SUMREM(J,K,II)*2.2046
	           ENDDO
             PSUM(K,I) = PSUM(K,I)/1.0E6
	       IF(METRIC.EQ.1) PSUM(K,I) = PSUM(K,I)*2.2046
	       PSUM(K,4) = PSUM(K,4) - PSUM(K,I)
	       ENDDO
	       ENDIF
 420  CONTINUE
C=======================================================================
C******** CALCULATE PCENT BASED ON ARRAY SUM AND ARRAY PSUM
C********
C********     PCENT(K,1) = STREET SWEEPING / TOTAL SURFACE BUILDUP
C********     PCENT(K,2) = SURFACE WASHOFF / TOTAL SURFACE BUILDUP
C********     PCENT(K,3) = SURFACE WASHOFF / NET SURFACE BUILDUP
C********     PCENT(K,4) = SURFACE WASHOFF / TOTAL SUBCATCHMENT LOAD
C********     PCENT(K,5) = SURFACE WASHOFF / TOTAL LOAD TO INLETS
C********     PCENT(K,6) = CATCHBASIN WASHOFF / INSOLUBLE FRACTION
C********     PCENT(K,7) = CATCHBASIN WASHOFF / TOTAL LOAD TO INLETS
C********     PCENT(K,8) = INSOLUBLE FRACTION / TOTAL SUBCATCHMENT LOAD
C********     PCENT(K,9) = INSOLUBLE FRACTION / TOTAL LOAD TO INLETS
C********     PCENT(K,10)= PRECIPITATION / TOTAL SUBCATCHMENT LOAD
C********     PCENT(K,11)= PRECIPITATION / TOTAL LOAD TO INLETS
C********     PCENT(K,12)= GROUNDWATER LOAD / TOTAL SUBCATCHMENT LOAD
C********     PCENT(K,13)= GROUNDWATER LOAD / TOTAL LOAD TO INLETS
C********     PCENT(K,14)= PERCENT ERROR IN QUALITY CONTINUITY
C
C#### WCH, 9/93
C             PCRDIS(K)  = I/I LOAD / TOTAL SUBCATCHMENT LOAD
C             PCRDIT(K)  = I/I LOAD / TOTAL LOAD TO INLETS
Cwch, 2/10/01
C             PC1CHP(K)  = CH/PIPE REMOVAL / SUBCATCHMENT LOAD
C             PC2CHP(K)  = CH/PIPE REMOVAL / SUBCATCHMENT LOAD
C
C********                  ERROR--> INLET LOAD + MASS REM - SUBCAT LOAD
C********                           -----------------------------------
C********                                      SUBCAT LOAD
C=======================================================================
      DO 440 K   = 1,NQS
C#### WCH, 9/93
      PCRDIS(K)  = 0.0
      PCRDIT(K)  = 0.0
Cwch, 2/10/01
      PC1CHP(K)  = 0.0
	PC2CHP(K)  = 0.0
      DO 440 J   = 1,14
      PCENT(K,J) = 0.0
 440  CONTINUE
      DO 460 K   = 1,NQS
C=======================================================================
C********** TEST THE DENOMINATORS FOR ZERO VALUES
C=======================================================================
      IF(SUM(K,2).GT.0.0)  THEN
                           PCENT(K,1) = 100.0 *  SUM(K,5) / SUM(K,2)
                           PCENT(K,2) = 100.0 *  SUM(K,6) / SUM(K,2)
                           ENDIF
      IF(PSUM(K,2).GT.0.0) PCENT(K,3) = 100.0 *  SUM(K,6) / PSUM(K,2)
      IF(PSUM(K,4).GT.0.0) THEN
                           PCENT(K,4) = 100.0 *  SUM(K,6) / PSUM(K,4)
                           PCENT(K,10)= 100.0 *  SUM(K,9) / PSUM(K,4)
C#### WCH, 9/93
                           PCRDIS(K)  = 100.0 *  SUMRDII(K)/PSUM(K,4)
                           PCENT(K,8) = 100.0 *  SUM(K,8) / PSUM(K,4)
                           PCENT(K,6) = 100.0 *  SUM(K,7) / PSUM(K,4)
                           PCENT(K,12)= 100.0 *  GWQ(K)   / PSUM(K,4)
Cwch, 2/10/01
                           PC1CHP(K)  = 100.0 *  SUMREMG(K,1)/PSUM(K,4)
                           PC2CHP(K)  = 100.0 *  SUMREMG(K,2)/PSUM(K,4)
Cwch, 12/20/00
                           PCENT(K,14)= 100.0 * (SUM(K,10) - PSUM(K,4)
     1                           + SUMWAT(K) + SUMREMG(K,1) 
     2                           + SUMREMG(K,2) + REFF(1,K))/PSUM(K,4)
                           ENDIF
      IF(SUM(K,10).GT.0.0) THEN
                           PCENT(K,5) = 100.0 *  SUM(K,6) / SUM(K,10)
                           PCENT(K,7) = 100.0 *  SUM(K,7) / SUM(K,10)
                           PCENT(K,9) = 100.0 *  SUM(K,8) / SUM(K,10)
                           PCENT(K,11)= 100.0 *  SUM(K,9) / SUM(K,10)
C#### WCH, 9/93
                           PCRDIT(K)  = 100.0 *  SUMRDII(K)/SUM(K,10)
                           PCENT(K,13)= 100.0 *  GWQ(K)   / SUM(K,10)
                           ENDIF
 460  CONTINUE
C=======================================================================
Cwch, 12/20/00
C     Include optional summary of pollutant generation and washoff by 
C     land use.  Use similar format to above quality summary printout.
C     Array SUMNL has saved identical info as SUM, but for each land use.
C     Do this only if there is more than one land use.
C=======================================================================
      WRITE(N6,650)
      WRITE(N6,660) (PNAME(J),J=1,NQS)
      WRITE(N6,660) (PUNIT(J),J=1,NQS)
      WRITE(N6,665) (IDASH,J=1,NQS)
      WRITE(N6,666)
C     Initial surface load (line 1).
      WRITE(N6,670) (SUM(K,1),K=1,NQS)
	IF(LUP.EQ.1) THEN
	    DO L = 1,JLAND
			DO K = 1,NQS
	        SPR(K) = 0.0
	        IF(SUM(K,1).GT.0.0) SPR(K) = SUMNL(L,K,1)/SUM(K,1)*100.0
			ENDDO
		WRITE(N6,9670) L,LNAME(L),(SPR(J),J=1,NQS)
	    ENDDO
	    ENDIF
C     Total surface buildup (line 2).
      WRITE(N6,680) (SUM(K,2),K=1,NQS)
      IF(LUP.EQ.1) THEN
	    DO L = 1,JLAND
			DO K = 1,NQS
	        SPR(K) = 0.0
	        IF(SUM(K,2).GT.0.0) SPR(K) = SUMNL(L,K,2)/SUM(K,2)*100.0
	        ENDDO
		WRITE(N6,9670) L,LNAME(L),(SPR(J),J=1,NQS)
	    ENDDO
	    ENDIF
C     Initial catchbasin load (line 3).
      WRITE(N6,690) (SUM(K,3),K=1,NQS)
      IF(LUP.EQ.1) THEN
	    DO L = 1,JLAND
			DO K = 1,NQS
	        SPR(K) = 0.0
	        IF(SUM(K,3).GT.0.0) SPR(K) = SUMNL(L,K,3)/SUM(K,3)*100.0
	        ENDDO
		WRITE(N6,9670) L,LNAME(L),(SPR(J),J=1,NQS)
	    ENDDO
	    ENDIF
C     Total catchbasin load (line 4).
      WRITE(N6,700) (SUM(K,4),K=1,NQS)
      IF(LUP.EQ.1) THEN
	    DO L = 1,JLAND
			DO K = 1,NQS
	        SPR(K) = 0.0
	        IF(SUM(K,4).GT.0.0) SPR(K) = SUMNL(L,K,4)/SUM(K,4)*100.0
	        ENDDO
		WRITE(N6,9670) L,LNAME(L),(SPR(J),J=1,NQS)
	    ENDDO
	    ENDIF
C     Total catchbasin and surface buildup (line 5).
      WRITE(N6,710) (PSUM(K,1),K=1,NQS)
      IF(LUP.EQ.1) THEN
	    DO L = 1,JLAND
			DO K = 1,NQS
	        SPR(K) = 0.0
	        IF(PSUM(K,1).GT.0.0) SPR(K) = 
     1       			(SUMNL(L,K,2)+SUMNL(L,K,4))/PSUM(K,1)*100.0
	        ENDDO
		WRITE(N6,9670) L,LNAME(L),(SPR(J),J=1,NQS)
	    ENDDO
	    ENDIF
      WRITE(N6,667)
C	Load remaining on surface (line 6)
      WRITE(N6,720) (PSHED(1,K,1),K=1,NQS)
      IF(LUP.EQ.1) THEN
	    DO L = 1,JLAND
			DO K = 1,NQS
	        SPR(K) = 0.0
	        IF(PSHED(1,K,1).GT.0.0) SPR(K) = 
     1    			SUMNL(L,K,10)/PSHED(1,K,1)*100.0
	        ENDDO
		WRITE(N6,9670) L,LNAME(L),(SPR(J),J=1,NQS)
	    ENDDO
	    ENDIF
C	Load remaining in surface water (line 6a).
      IF(ISC2SC.EQ.1) THEN
	  WRITE(N6,725) (SUMWAT(K),K=1,NQS)
        IF(LUP.EQ.1) THEN
	    DO L = 1,JLAND
			DO K = 1,NQS
	        SPR(K) = 0.0
	        IF(SUMWAT(K).GT.0.0) SPR(K) = 
     1    			SUMNL(L,K,11)/SUMWAT(K)*100.0
	        ENDDO
		WRITE(N6,9670) L,LNAME(L),(SPR(J),J=1,NQS)
	    ENDDO
	    ENDIF
	  ENDIF
C     Load remaining in catchbasins (line 7).
      WRITE(N6,730) (PBASIN(K,1),K=1,NQS)
C     Load remaining in channel/pipes (line 8).
      WRITE(N6,740) (REFF(1,K),K=1,NQS)
      WRITE(N6,668)
C	Street sweeping removal (line 9).
      WRITE(N6,750) (SUM(K,5),K=1,NQS)
      IF(LUP.EQ.1) THEN
	    DO L = 1,JLAND
			DO K = 1,NQS
	        SPR(K) = 0.0
	        IF(SUM(K,5).GT.0.0) SPR(K) = SUMNL(L,K,5)/SUM(K,5)*100.0
	        ENDDO
		WRITE(N6,9670) L,LNAME(L),(SPR(J),J=1,NQS)
	    ENDDO
	    ENDIF
C     Net surface buildup (line 10). 
      WRITE(N6,760) (PSUM(K,2),K=1,NQS)
C	Surface washoff (line 11).
      WRITE(N6,770) (SUM(K,6),K=1,NQS)
      IF(LUP.EQ.1) THEN
	    DO L = 1,JLAND
			DO K = 1,NQS
	        SPR(K) = 0.0
	        IF(SUM(K,6).GT.0.0) SPR(K) = SUMNL(L,K,6)/SUM(K,6)*100.0
	        ENDDO
		WRITE(N6,9670) L,LNAME(L),(SPR(J),J=1,NQS)
	    ENDDO
	    ENDIF
C	Catchbasin washoff (line 12).
      WRITE(N6,780) (SUM(K,7),K=1,NQS)
      IF(LUP.EQ.1) THEN
	    DO L = 1,JLAND
			DO K = 1,NQS
	        SPR(K) = 0.0
	        IF(SUM(K,7).GT.0.0) SPR(K) = SUMNL(L,K,7)/SUM(K,7)*100.0
	        ENDDO
		WRITE(N6,9670) L,LNAME(L),(SPR(J),J=1,NQS)
	    ENDDO
	    ENDIF
C	Total washoff (surface plus catchbasin) (line 13).
      WRITE(N6,790) (PSUM(K,3),K=1,NQS)
      IF(LUP.EQ.1) THEN
	    DO L = 1,JLAND
			DO K = 1,NQS
	        SPR(K) = 0.0
	        IF(PSUM(K,3).GT.0.0) SPR(K) = 
     1       			(SUMNL(L,K,6)+SUMNL(L,K,7))/PSUM(K,3)*100.0
	        ENDDO
		WRITE(N6,9670) L,LNAME(L),(SPR(J),J=1,NQS)
	    ENDDO
	    ENDIF
C	Insoluble washoff load (fraction from other constituents) (line 14). 
      WRITE(N6,800) (SUM(K,8),K=1,NQS)
      IF(LUP.EQ.1) THEN
	    DO L = 1,JLAND
			DO K = 1,NQS
	        SPR(K) = 0.0
	        IF(SUM(K,8).GT.0.0) SPR(K) = SUMNL(L,K,8)/SUM(K,8)*100.0
	        ENDDO
		WRITE(N6,9670) L,LNAME(L),(SPR(J),J=1,NQS)
	    ENDDO
	    ENDIF
C	Precipitation load (.
      WRITE(N6,810) (SUM(K,9),K=1,NQS)
      IF(LUP.EQ.1) THEN
	    DO L = 1,JLAND
			DO K = 1,NQS
	        SPR(K) = 0.0
	        IF(SUM(K,9).GT.0.0) SPR(K) = SUMNL(L,K,9)/SUM(K,9)*100.0
	        ENDDO
		WRITE(N6,9670) L,LNAME(L),(SPR(J),J=1,NQS)
	    ENDDO
	    ENDIF
C	Sum surface, catchbasin, other constituents and precip 
C       loads (line 15a).
      WRITE(N6,812) (PSUM(K,6),K=1,NQS)
      IF(LUP.EQ.1) THEN
	    DO L = 1,JLAND
			DO K = 1,NQS
	        SPR(K) = 0.0
	        IF(PSUM(K,6).GT.0.0) SPR(K) = (SUMNL(L,K,6) + SUMNL(L,K,7)
     1 + SUMNL(L,K,8) + SUMNL(L,K,9))/PSUM(K,6)*100.0
	        ENDDO
		WRITE(N6,9670) L,LNAME(L),(SPR(J),J=1,NQS)
	    ENDDO
	    ENDIF
C=======================================================================
Cwch, 12/20/00
C	Include overland flow removals, if simulated.
C     Caution!  Here, reuse SUM(K,1), SUM(K,2) and SUM(K,3).
C     Convert mg to kg as before and convert to pounds if necessary.
C=======================================================================
      IF(ISC2SC.EQ.1) THEN
	    WRITE(N6,9684)
C	Removal by infiltration (line 15b).
	    WRITE(N6,9685) (PSUM(K,7),K=1,NQS)
          IF(LUP.EQ.1) THEN
	      DO L = 1,JLAND
			DO K = 1,NQS
	        SPR(K) = 0.0
	       IF(PSUM(K,7).GT.0.0) SPR(K) = SUMREM(L,K,1)/PSUM(K,7)*100.0
	        ENDDO
		  WRITE(N6,9670) L,LNAME(L),(SPR(J),J=1,NQS)
	      ENDDO
	      ENDIF
C	Removal by removal fraction (line 15c).
	    WRITE(N6,9690) (PSUM(K,8),K=1,NQS)
          IF(LUP.EQ.1) THEN
     	      DO L = 1,JLAND
			DO K = 1,NQS
	        SPR(K) = 0.0
	       IF(PSUM(K,8).GT.0.0) SPR(K) = SUMREM(L,K,2)/PSUM(K,8)*100.0
	        ENDDO
		  WRITE(N6,9670) L,LNAME(L),(SPR(J),J=1,NQS)
	      ENDDO
	      ENDIF
C     Removal by first-order decay (line 15d).
	    WRITE(N6,9695) (PSUM(K,9),K=1,NQS)
          IF(LUP.EQ.1) THEN
	      DO L = 1,JLAND
			DO K = 1,NQS
	        SPR(K) = 0.0
	       IF(PSUM(K,9).GT.0.0) SPR(K) = SUMREM(L,K,3)/PSUM(K,9)*100.0
	        ENDDO
		  WRITE(N6,9670) L,LNAME(L),(SPR(J),J=1,NQS)
	      ENDDO
	      ENDIF
	    ENDIF
C     Loads from groundwater inflow (line 16). 
      WRITE(N6,815) (GWQ(K),  K=1,NQS)
C#### WCH, 9/93
C     RDII loads (line 16a).
      WRITE(N6,816) (SUMRDII(K),K=1,NQS)
C     Net subcatchment load (line 17).
      WRITE(N6,820) (PSUM(K,4),K=1,NQS)
Cwch, 2/10/01.
C     Removal by BMP channel/pipe BMP fraction (line 17a).
      WRITE(N6,821) (SUMREMG(K,1),K=1,NQS)
C     Removal by 1st order decay in channel/pipes (line 17b).
	WRITE(N6,822) (SUMREMG(K,2),K=1,NQS)
C     Total load to inlets (line 18).
      WRITE(N6,830) (SUM(K,10),K=1,NQS)
      WRITE(N6,835) (PSUM(K,5),K=1,NQS)
      WRITE(N6,669)
      WRITE(N6,840) (PCENT(K,1),K=1,NQS)
      WRITE(N6,850) (PCENT(K,2),K=1,NQS)
      WRITE(N6,860) (PCENT(K,3),K=1,NQS)
      WRITE(N6,870) (PCENT(K,4),K=1,NQS)
      WRITE(N6,880) (PCENT(K,5),K=1,NQS)
      WRITE(N6,890) (PCENT(K,6),K=1,NQS)
      WRITE(N6,900) (PCENT(K,7),K=1,NQS)
      WRITE(N6,910) (PCENT(K,8),K=1,NQS)
      WRITE(N6,920) (PCENT(K,9),K=1,NQS)
      WRITE(N6,930) (PCENT(K,10),K=1,NQS)
      WRITE(N6,940) (PCENT(K,11),K=1,NQS)
Cwch, 12/20/00. Percentages for overland flow removal (line 30a).
      IF(ISC2SC.EQ.1) THEN
	    DO K = 1,NQS
	    SPR(K) = 0.0
	    IF(PSUM(K,6).GT.0.0) SPR(K) = PSUM(K,7)/PSUM(K,6)*100.0
	    ENDDO
	    WRITE(N6,9700) (SPR(K),K=1,NQS)
C     Percentages for BMP fraction removal (line 30b).
	    DO K = 1,NQS
	    SPR(K) = 0.0
	    IF(PSUM(K,6).GT.0.0) SPR(K) = PSUM(K,8)/PSUM(K,6)*100.0
	    ENDDO
	    WRITE(N6,9705) (SPR(K),K=1,NQS)
C     Percentages for first-order decay removal (line 30c).
	    DO K = 1,NQS
	    SPR(K) = 0.0
	    IF(PSUM(K,6).GT.0.0) SPR(K) = PSUM(K,9)/PSUM(K,6)*100.0
	    ENDDO
	    WRITE(N6,9710) (SPR(K),K=1,NQS)
	    ENDIF
C
      WRITE(N6,945) (PCENT(K,12),K=1,NQS)
      WRITE(N6,946) (PCENT(K,13),K=1,NQS)
C#### WCH, 9/93
      WRITE(N6,947) (PCRDIS(K),K=1,NQS)
      WRITE(N6,948) (PCRDIT(K),K=1,NQS)
Cwch, 2/10/01.
C     Percentages for channel/pipe BMP fraction removal (line 32c).
      WRITE(N6,9715) (PC1CHP(K),K=1,NQS)
C     Percentages for channel/pipe 1st order decay removal (line 32d).
	WRITE(N6,9720) (PC2CHP(K),K=1,NQS)
C     Final continuity check (line 33).
      WRITE(N6,950) (PCENT(K,14),K=1,NQS)
C#### WCH, 8/6/93.  PRINT WARNING ABOUT QUALITY CONTINUITY ERROR.
      WRITE(N6,955)
C=======================================================================
Cwch, 12/20/00. Add option to use same code to print subcatchment output.
C     ISKIP = 0 means print M3-line values.
C     ISKIP = 1 means print M5-line values.
C=======================================================================
  600 IF(NPRNT.EQ.0.AND.ISKIP.EQ.0) RETURN
	IF(MSUBC.EQ.0.AND.ISKIP.EQ.1) RETURN
C=======================================================================
C     PRINT OUT QUALITY INFORMATION
C
C     CALCULATE THE REQUIRED CONVERSION FACTORS.
C                                       FOR METRIC = 1:
C     CFACT1 CONVERTS MG/L * CU FT  TO  POUNDS.
C     CFACT2 CONVERTS QUANTITY/L * CU FT  TO  QUANTITY.
C                                      FOR METRIC = 2:
C     CFACT1 CONVERTS MG/L * CU M  TO  KG.
C     CFACT2 CONVERTS QUANTITY/L * CU M  TO  QUANTITY.
C=======================================================================
      CFACT1 = 28.31605 / 453592.
      CFACT2 = 28.31605
      IF(METRIC.EQ.2) THEN
                      CFACT1 = 1000.0 / 1000000.0
                      CFACT2 = 1000.0
                      ENDIF
C=======================================================================
Cwch, 12/20/00. In the code that follows, several changes have been
C     made to print subcatchment output if ISKIP = 1.
C=======================================================================
C     Read the header information on the scratch file NPRNT or
C       MSUBC times.
C     NPRNT = Number of inlets requested on group M1.
C     MSUBC = Number of subcatchments requested on group M5.
C=======================================================================
      IF(ISKIP.EQ.0) THEN
		NNPRNT  = NPRNT
	    NSCRTCH = NSCRT2
	    PRNAMP  = PRNAME(1)
	    ELSE
	    NNPRNT  = MSUBC
	    NSCRTCH = NSCRAT(9)
	    PRNAMP  = PRNAME(2)
	    ENDIF
      DO 2000 J = 1,NNPRNT
      REWIND NSCRTCH
      TIME      = 0.0
      FOROUT(1) = FOROUT(MQUAL+8)
      IF(ISKIP.EQ.0) THEN
          K         = IABS(IPRNT(J))
C#### WCH, 12/5/94.  INCLUDE NEGATIVE SIGN WHEN PRINTING OUTFLOWS.
		LOCGUT    = ISIGN(1,IPRNT(J))*NAMEG(K)
C####      LOCGUT    = NAMEG(K)
		KOCGUT    = KAMEG(K)
	    ELSE
          K         = J
	    LOCGUT    = NAMEW(ISUBC(K))
	    KOCGUT    = KAMEW(ISUBC(K))
	    ENDIF
      TOTFLO    = 0.0
      TFLOSQ    = 0.0
      FMAX(1)   = 0.0
      FMIN(1)   = 1.0E30
      DO 1010 N = 1,NQS
      FMAX(N+1) = 0.0
      FMIN(N+1) = 1.0E30
      TQUALC(N) = 0.0
      TQUALS(N) = 0.0
      CMEAN(N)  = 0.0
      STNDEV(N) = 0.0
      TQUALF(N) = 0.0
 1010 CONTINUE
C=======================================================================
C     The variable LX specifies whether inflows or outflows are printed.
C=======================================================================
      IF(ISKIP.EQ.0) THEN      
                            LX = 1
          IF(IPRNT(J).LT.0) LX = 2
	    ELSE
	                      LX = 2
	    ENDIF
C=======================================================================
C     WRITE THE TITLE CARD FOR QUANTITY AND QUALITY.
C     THERE ARE TWO POSSIBLE TITLE CARDS DEPENDING ON THE VALUE OF METRIC.
C=======================================================================
         IF(METRIC.EQ.1) THEN
                         IF(JCE.EQ.0) WRITE(N6,1025) PRNAMP,LOCGUT,
     1                                LINOUT(LX),TITLE(1),TITLE(2)
                         IF(JCE.EQ.1) WRITE(N6,1026) PRNAMP,KOCGUT,
     1                                     TITLE(1),TITLE(2)
                         WRITE(N6,1040) (PNAME(KK),KK=1,NQS)
                         WRITE(N6,1050) (PUNIT(KK),KK=1,NQS)
                         WRITE(N6,1055) (BMJ,KK=1,NQS)
                         ELSE
                         IF(JCE.EQ.0) WRITE(N6,1020) PRNAMP,LOCGUT,
     1                                LINOUT(LX),TITLE(1),TITLE(2)
                         IF(JCE.EQ.1) WRITE(N6,1021) PRNAMP,KOCGUT,
     1                                     TITLE(1),TITLE(2)
                         WRITE(N6,1040) (PNAME(KK),KK=1,NQS)
                         WRITE(N6,1060) (PUNIT(KK),KK=1,NQS)
                         WRITE(N6,1055) (BMJ,KK=1,NQS)
                         ENDIF
      DO 1500 I     = 1,NQS
      FOROUT(I+1)   = EFMT
 1500 CONTINUE
      FOROUT(NQS+2) = CLOSER
C=======================================================================
      DO 1535 K = 1,NQS
      IF(NDIM(K).EQ.0) THEN
                       UTOT(K)   = UNTN(1)
                       IF(METRIC.EQ.2) UTOT(K) = UNTN(4)
                       ENDIF
      IF(NDIM(K).EQ.1) UTOT(K) = UNTN(2)
      IF(NDIM(K).EQ.2) UTOT(K) = UNTN(3)
 1535 CONTINUE
C=======================================================================
C     KLINE --> IS A COUNTER THAT SUMS THE NUMBER OF LINES PRINTED ON A PAGE.
C=======================================================================
      KLINE     = 0
      INTCNT    = 0
      DLAST     = 0.0
C=======================================================================
C#### WCH, 11/30/93.  MAKE CONSISTENT SUMMATION WITH RUNOFF, COMBINE
C                     AND STATS.  IF PREVIOUS FLOW WAS ZERO, THEN USE
C                     NEW DELT FOR DMEAN.
C=======================================================================
      ITEST     = 0
      DO 1800 M = 1, 1000000
      READ(NSCRTCH,END=1810,ERR=1810) JULDAY,TIMDAY,
     1                               DELT,(FLOW(N),N=1,NNPRNT)
      DMEAN     = 0.5 * (DELT + DLAST)
C#### WCH, 11/30/93
      IF(ITEST.EQ.0) DMEAN = DELT
      ITEST     = 0
      IF(FLOW(J).GT.0.0) ITEST = 1
C
      DLAST     = DELT
      TIME      = TIME     + DELT
      IF(KLINE.EQ.151) KLINE = 0
      INTCNT                 = INTCNT + 1
      IF(FLOW(J).GT.0.0.AND.INTCNT.EQ.INTERV) KLINE = KLINE + 1
C=======================================================================
C     Caution! from here on, flows are in cu m/sec if METRIC = 2.
C=======================================================================
      IF(METRIC.EQ.2) FLOW(J) = FLOW(J) * 0.028317
      CALL DATED
C=======================================================================
      READ(NSCRTCH,END=1810,ERR=1810) ((QUAL(L,N),L=1,NQS),N=1,NNPRNT)
      IF(FLOW(J).EQ.0.0) DLAST = 0.0
      IF(FLOW(J).GT.0.0) THEN
         IF(INTCNT.EQ.INTERV) WRITE(N6,FOROUT)
     1     MONTH,NDAY,NYEAR,JHR,MINUTE,FLOW(J),(QUAL(N,J),N=1,NQS)
         TOTFLO   = TOTFLO + DMEAN*FLOW(J)
         TFLOSQ   = TFLOSQ + DMEAN*FLOW(J)**2
         IF(FLOW(J).GT.FMAX(1)) FMAX(1) = FLOW(J)
         IF(FLOW(J).LT.FMIN(1)) FMIN(1) = FLOW(J)
         DO 135 N   = 1,NQS
         TQUALF(N)  = TQUALF(N)  + DMEAN*QUAL(N,J)*FLOW(J)
         TQUALS(N)  = TQUALS(N)  + DMEAN*QUAL(N,J)*QUAL(N,J)*FLOW(J)
         IF(QUAL(N,J).GT.FMAX(N+1)) FMAX(N+1) = QUAL(N,J)
         IF(QUAL(N,J).LT.FMIN(N+1)) FMIN(N+1) = QUAL(N,J)
  135    CONTINUE
C=======================================================================
C     Write the title lines if 50 lines per page have been written.
Cwch, 12/20/00. Do this only if NOHEAD = 0.
C=======================================================================
         IF(NOHEAD.EQ.0) THEN
          IF(KLINE.EQ.51.OR.KLINE.EQ.101.OR.KLINE.EQ.151) THEN
            IF(INTCNT.LT.INTERV) GO TO 1800
            IF(METRIC.EQ.1) THEN
                      IF(JCE.EQ.0) WRITE(N6,1025) PRNAMP,LOCGUT,
     1                             LINOUT(LX),TITLE(1),TITLE(2)
                      IF(JCE.EQ.1) WRITE(N6,1026) PRNAMP,KOCGUT,
     1                                  TITLE(1),TITLE(2)
                      WRITE(N6,1040) (PNAME(KK),KK=1,NQS)
                      WRITE(N6,1050) (PUNIT(KK),KK=1,NQS)
                      WRITE(N6,1055) (BMJ,KK=1,NQS)
                      ELSE
                      IF(JCE.EQ.0) WRITE(N6,1020) PRNAMP,LOCGUT,
     1                             LINOUT(LX),TITLE(1),TITLE(2)
                      IF(JCE.EQ.1) WRITE(N6,1021) PRNAMP,KOCGUT,
     1                                  TITLE(1),TITLE(2)
                      WRITE(N6,1040) (PNAME(KK),KK=1,NQS)
                      WRITE(N6,1060) (PUNIT(KK),KK=1,NQS)
                      WRITE(N6,1055) (BMJ,KK=1,NQS)
                      ENDIF
            ENDIF
          ENDIF
	   ENDIF
      IF(INTCNT.EQ.INTERV) INTCNT = 0
 1800 CONTINUE
C=======================================================================
C     Calculate the event statistics for SWMM.
C=======================================================================
 1810 DO 1850 K = 1,NQS
      IF (NDIM(K).EQ.0) TQUALC(K) = TQUALF(K) * CFACT1
      IF (NDIM(K).EQ.1) TQUALC(K) = TQUALF(K) * CFACT2
      IF (NDIM(K).EQ.2) TQUALC(K) = TQUALF(K)
 1850 CONTINUE
C=======================================================================
C     TOTFLO may equal zero. This really should not occur.
C=======================================================================
      TMEAN = TOTFLO / TIME
C#### WCH, 11/15/93
      FLODEV = 0.0
      IF(TOTFLO.GT.0.0) THEN
                        ARG    = TFLOSQ/TIME - TMEAN*TMEAN
                        IF(ARG.LT.1.0E-35) ARG = 0.0
                        IF(ARG.GT.0.0) FLODEV  = SQRT(ARG)
                        DO 1950 I = 1,NQS
                        CMEAN(I)  = TQUALF(I) / TOTFLO
                        STNDEV(I) = 0.0
                        ARG       = TQUALS(I)/TOTFLO-CMEAN(I)*CMEAN(I)
                        IF(ARG.LT.1.0E-35) ARG   = 0.0
                        IF(ARG.GT.0.0) STNDEV(I) = SQRT(ARG)
 1950                   CONTINUE
                        ENDIF
      FOROUT(1) = FOROUT(MQUAL+3)
      WRITE(N6,1055) (BMJ,KK=1,NQS)
      WRITE(N6,FOROUT) TMEAN,(CMEAN(I),I=1,NQS)
      FOROUT(1) = FOROUT(MQUAL+4)
      WRITE(N6,FOROUT) FLODEV,(STNDEV(I),I=1,NQS)
      FOROUT(1) = FOROUT(MQUAL+5)
      WRITE(N6,FOROUT) FMAX(1),(FMAX(I+1),I=1,NQS)
      FOROUT(1) = FOROUT(MQUAL+6)
C#### WCH, 11/15/93
      WRITE(N6,FOROUT) AMIN1(0.0,FMIN(1)),(AMIN1(0.0,FMIN(I+1)),I=1,NQS)
      FOROUT(1) = FOROUT(MQUAL+7)
      DO 1900 I   = 1,NQS
 1900 FOROUT(I+1) = EFMT
      WRITE(N6,FOROUT) TOTFLO,(TQUALC(I),I=1,NQS)
      IF(METRIC.EQ.1) WRITE(N6,1985) (UTOT(K),K=1,NQS)
      IF(METRIC.EQ.2) WRITE(N6,1990) (UTOT(K),K=1,NQS)
 2000 CONTINUE
C=======================================================================
C#### WCH, 1/23/97.
  650 FORMAT(/,1H1,/,16X,
     1' #####################################################',/,16X,
     2' #             Runoff Quality Summary Page           #',/,16X,
     3' # If NDIM = 0 Units for:   loads    mass rates      #',/,16X,
     4' #             METRIC = 1    lb        lb/sec        #',/,16X,
     5' #             METRIC = 2    kg        kg/sec        #',/,16X,
     6' # If NDIM = 1 Loads are in units of quantity        #',/,16X,
     7' #             and mass rates are quantity/sec       #',/,16X,
     8' # If NDIM = 2 loads are in units of concentration   #',/,16X,
     9' #             times volume and mass rates have units#',/,16X,
     9' #             of concentration times volume/second  #',/,16X,
     1' #####################################################',/)
CIM note in following statements I changed 10( to 99(
  660 FORMAT(33X,99(2X,A8))
  665 FORMAT(33X,99(2X,A4,'----'))
  666 FORMAT(1X,'Inputs',/,
     1         1X,'------')
  667 FORMAT(/,1X,'Remaining Loads',/,
     1         1X,'---------------')
  668 FORMAT(/,1X,'Removals',/,
     1         1X,'--------')
  669 FORMAT(/,1X,'Percentages',
     1         /,1X,'-----------')
Cwch, 2/10/01. Try changing 1PE10.2 to 1PE10.3
  670 FORMAT(1X,' 1. INITIAL SURFACE LOAD........',99(1PE10.3))
  680 FORMAT(1X,' 2. TOTAL SURFACE BUILDUP.......',99(1PE10.3))
  690 FORMAT(1X,' 3. INITIAL CATCHBASIN LOAD.....',99(1PE10.3))
  700 FORMAT(1X,' 4. TOTAL CATCHBASIN LOAD.......',99(1PE10.3))
  710 FORMAT(1X,' 5. TOTAL CATCHBASIN AND       ',/,
     1          '     SURFACE BUILDUP (2+4).......',99(1PE10.3))
  720 FORMAT(1X,' 6. LOAD REMAINING ON SURFACE...',99(1PE10.3))
Cwch, 12/20/00
  725 FORMAT(1X,' 6a.LOAD REMAINING IN SURF WATER',99(1PE10.3))
  730 FORMAT(1X,' 7. REMAINING IN CATCHBASINS....',99(1PE10.3))
  740 FORMAT(1X,' 8. REMAINING IN CHANNEL/PIPES..',99(1PE10.3))
  750 FORMAT(1X,' 9. STREET SWEEPING REMOVAL.....',99(1PE10.3))
  760 FORMAT(1X,'10. NET SURFACE BUILDUP (2-9)...',99(1PE10.3))
  770 FORMAT(1X,'11. SURFACE WASHOFF.............',99(1PE10.3))
  780 FORMAT(1X,'12. CATCHBASIN WASHOFF..........',99(1PE10.3))
  790 FORMAT(1X,'13. TOTAL WASHOFF (11+12).......',99(1PE10.3))
  800 FORMAT(1X,'14. LOAD FROM OTHER CONSTITUENTS',99(1PE10.3))
  810 FORMAT(1X,'15. PRECIPITATION LOAD..........',99(1PE10.3))
Cwch, 12/20/00
  812 FORMAT(1X,'15a.SUM SURFACE LOAD (13+14+15).',99(1PE10.3))
  815 FORMAT(1X,'16. TOTAL GROUNDWATER LOAD......',99(1PE10.3))
C#### WCH, 9/93
  816 FORMAT(1X,'16a.TOTAL I/I LOAD..............',99(1PE10.3))
Cwch, 12/20/00
  820 FORMAT(1X,'17. NET SUBCATCHMENT LOAD',/,
     1         '     (15a-15b-15c-15d+16+16a)....',99(1PE10.3))
Cwch, 2/10/01.
  821 FORMAT(' >>Removal in channel/pipes (17a, 17b):',/,
     1       1X,'17a.REMOVE BY BMP FRACTION......',99(1PE10.3))
  822 FORMAT(1X,'17b.REMOVE BY 1st ORDER DECAY...',99(1PE10.3))
  830 FORMAT(1X,'18. TOTAL LOAD TO INLETS........',99(1PE10.3))
  835 FORMAT(1X,'19. FLOW WT''D AVE.CONCENTRATION',
     1      /,1X, '    (INLET LOAD/TOTAL FLOW).....',99(1PE10.3))
  840 FORMAT(1X,'20. STREET SWEEPING (9/2).......',99(F10.3))
  850 FORMAT(1X,'21. SURFACE WASHOFF (11/2)......',99(F10.3))
  860 FORMAT(1X,'22. NET SURFACE WASHOFF(11/10)..',99(F10.3))
  870 FORMAT(1X,'23. WASHOFF/SUBCAT LOAD(11/17)..',99(F10.3))
  880 FORMAT(1X,'24. SURFACE WASHOFF/INLET LOAD',
     1       /,1X,'    (11/18).....................',99(F10.3))
  890 FORMAT(1X,'25. CATCHBASIN WASHOFF/         ',
     1       /,1X,'    SUBCATCHMENT LOAD (12/17)...',99(F10.3))
  900 FORMAT(1X,'26. CATCHBASIN WASHOFF/         ',
     1       /,1X,'    INLET LOAD (12/18)..........',99(F10.3))
  910 FORMAT(1X,'27. OTHER CONSTITUENT LOAD/     ',
     1       /,1X,'    SUBCATCHMENT LOAD (14/17)...',99(F10.3))
  920 FORMAT(1X,'28. INSOLUBLE FRACTION/         ',
     1       /,1X,'    INLET LOAD (14/18)..........',99(F10.3))
  930 FORMAT(1X,'29. PRECIPITATION/              ',
     1       /,1X,'    SUBCATCHMENT LOAD (15/17)...',99(F10.3))
  940 FORMAT(1X,'30. PRECIPITATION/              ',
     1       /,1X,'    INLET LOAD (15/18)..........',99(F10.3))
  945 FORMAT(1X,'31. GROUNDWATER LOAD/           ',
     1       /,1X,'    SUBCATCHMENT LOAD (16/17)...',99(F10.3))
  946 FORMAT(1X,'32. GROUNDWATER LOAD/           ',
     1       /,1X,'    INLET LOAD (16/18)..........',99(F10.3))
  947 FORMAT(1X,'32a.INFILTRATION/INFLOW LOAD/   ',
     1       /,1X,'    SUBCATCHMENT LOAD (16a/17)..',99(F10.3))
  948 FORMAT(1X,'32b.INFILTRATION/INFLOW LOAD/   '
     1       /,1X,'    INLET LOAD (16a/18).........',99(F10.3))
  950 FORMAT(1X,'33. INLET LOAD SUMMATION ERROR',
     1       /,1X,'    (18+8+6a+17a+17b-17)/17.....',99(F10.3))
C#### WCH, 8/6/93.
C#### WCH, 4/7/94.
  955 FORMAT(/,' CAUTION. Due to method of quality routing (Users Manual
     1, Appendix IX)',/,' quality routing through channel/pipes is sensi
     2tive to the time step.',/,' Large "Inlet Load Summation Errors" ma
     3y result.',/,' These can be reduced by adjusting the time step(s).
     4',/,' Note: surface accumulation during dry time steps at end of s
     5imulation is',/,' not included in totals.  Buildup is only perform
     6ed at beginning of',/,' wet steps or for street cleaning.')
C#### WCH, 12/5/94.  CHANGE FROM I8,2X TO I9,1X.
 1020 FORMAT(/,1H1,/,
     1' ****************************************************',/,
     2' *  Summary of Quantity and Quality Results at      *',/,
     3' *  ',A9,I10,1X,A3,'Flow in cms.             *',/,
     4' *  Values are instantaneous at indicated time step *',/,
     5' ****************************************************',//,
     63X,A80,/,2X,A80,/)
 1021 FORMAT(/,1H1,/,
     1' ****************************************************',/,
     2' *  Summary of Quantity and Quality Results at      *',/,
     3' *  ',A9,A10,3X,'Flow in cms.              *',/,
     4' *  Values are instantaneous at indicated time step *',/,
     5' ****************************************************',//,
     63X,A80,/,2X,A80,/)
C#### WCH, 12/5/94.  CHANGE FROM I8,2X TO I9,1X.
 1025 FORMAT(/,1H1,/,
     1' ****************************************************',/,
     2' *  Summary of Quantity and Quality Results at      *',/,
     3' *  ',A9,I10,1X,A3,'Flow in cfs.             *',/,
     4' *  Values are instantaneous at indicated time step *',/,
     5' ****************************************************',//,
     63X,A80,/,3X,A80,/)
 1026 FORMAT(/,1H1,/,
     1' ****************************************************',/,
     2' *  Summary of Quantity and Quality Results at      *',/,
     3' *  ',A9,A10,3X,'Flow in cfs.              *',/,
     4' *  Values are instantaneous at indicated time step *',/,
     5' ****************************************************',//,
     63X,A80,/,3X,A80,/)
 1040 FORMAT(6X,'Date',5X,'Time',8X,'Flow ',99(2X,A8))
 1050 FORMAT(2X,'Mo/Da/Year',2X,'Hr:Min',8X,'cfs  ',99(2X,A8))
 1055 FORMAT(2X,'----------',2X,'-------',4X,'-------',
     199(A2,'--------'))
 1060 FORMAT(2X,'Mo/Da/Year',2X,'Hr:Min',8X,'cum/s',99(2X,A8))
 1985 FORMAT(25X,'Cub-Ft ',99(2X,A8))
 1990 FORMAT(25X,'Cub-Met',99(2X,A8))
Cwch, 12/20/00
 9670 FORMAT(1X,'  %Land Use',I3,': ',A8,'........',99(F10.4))
 9684 FORMAT(' >>Removals during overland flow (15b, 15c, 15d):')
 9685 FORMAT(1X,'15b.REMOVE BY INFILTRATION......',99(1PE10.3))
 9690 FORMAT(1X,'15c.REMOVE BY BMP FRACTION......',99(1PE10.3))
 9695 FORMAT(1X,'15d.REMOVE BY 1st ORDER DECAY...',99(1PE10.3))
 9700 FORMAT(1X,'30a.INFILTRATION REMOVAL/       ',
     1       /,1X,'    SURFACE LOAD (15b/15a)......',99(F10.3))
 9705 FORMAT(1X,'30b.SUBCAT BMP FRACTION REMOVAL/       ',
     1       /,1X,'    SURFACE LOAD (15c/15a)......',99(F10.3))
 9710 FORMAT(1X,'30c.SUBCAT 1st ORDER DECAY REMOVAL/    ',
     1       /,1X,'    SURFACE LOAD (15d/15a)......',99(F10.3))
 9715 FORMAT(1X,'32c.CH/PIPE BMP FRACTION REMOVAL/       ',
     1       /,1X,'    SUBCATCHMENT LOAD (17a/17)..',99(F10.3))
 9720 FORMAT(1X,'32d.CH/PIPE 1st ORDER DECAY REMOVAL/    ',
     1       /,1X,'    SUBCATCHMENT LOAD (17b/17)..',99(F10.3))
C=======================================================================
      RETURN
      END
