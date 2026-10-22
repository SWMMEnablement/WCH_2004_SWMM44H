      SUBROUTINE RUNOFF
C=======================================================================
C     The runoff subroutine was updated December, 1990,
C     by Robert E. Dickinson (RED) and Wayne C. Huber (WCH).
C
C     The RUNOFF Block uses up to eight (8/93) scratch sets and one 
C       output data-set.
C     Updated by Chuck Moore (CIM), CDM, 8/93 to initialize statistics 
C       parameters for subcatchment and channel/pipe statistics.
C     Subscript error in initialization.  WCH, 3/27/00 and 4/5/00.
C	Include and initialize new overland flow variables. WCH, 12/20/00.
C     Close scratch files upon exiting Runoff.  RED, 5/21/01. 
C     Initialization for subcat loss summer. WCH, 3/28/02.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'QUALTY.INC'
      INCLUDE 'GRWTR.INC'
      INCLUDE 'NEW88.INC'
      INCLUDE 'NEW89.INC'
C#### C. MOORE, CDM, 8/93.
      INCLUDE 'RDII.INC'
      INCLUDE 'RUNSTAT.INC'
Cwch, 12/20/00. 
	INCLUDE 'OVERLAND.INC'
C=======================================================================
      IF(NOQ.EQ.0) THEN
                   WRITE(N6,11)
                   WRITE(*,11)
                   ELSE
                   WRITE(N6,12)
                   WRITE(*,12)
                   ENDIF
      INCNT  = INCNT  + 1
      IOUTCT = IOUTCT + 1
C=======================================================================
C     Limits for dimension statements
C=======================================================================
C     NW     = Number of subcatchments and number of inlets.
C     NG     = Number of channels.
C     NCP    = Number of subcatchments to a channel and number of
C              channels connected to another channel.  Also it is the
C              number of channels connected to an inlet.
C     MQUAL  = Number of water quality constituents.
C     NLU    = Number of possible land uses.
C     NING   = Number of inlets.
C     NRANVL = Number of rainfall and temperature values.
C#######################################################################
C=========        CNT(K)    are used for the continuity check at the
C=========                  end of SUBROUTINE HYDRO.
C=========        CNT(1)  = sum of RAIN AND SNOW
C=========        CNT(2)  = sum of RAIN ONLY
C=========        CNT(3)  = sum of INITIAL SNOW COVER
C=========        CNT(4)  = sum of INFILTRATION
C=========        CNT(5)  = sum of FLOW OUT OF PIPE/GUTTER SYSTEM
C=========        CNT(6)  = sum of EVAPORATION
C=========        CNT(7)  = sum of FLOW REMAINING IN PIPE/GUTTER
C=========        CNT(8)  = sum of SNOW REMANING IN SNOW COVER
C=========        CNT(9)  = sum of SNOW REMOVED FROM THE BASIN
C=========        CNT(10) = sum of FLOW REMINING IN SURFACE STORAGE
C=========        CNT(11) = sum of UPPER ZONE ET
C=========        CNT(12) = sum of LOWER ZONE ET
C=========        CNT(13) = sum of GROUNDWATER FLOW
C=========        CNT(14) = sum of DEEP PERCOLATION
C=========        CNT(15) = sum of INITIAL SUBSURFACE STORAGE
C=========        CNT(16) = sum of FINAL SUBSURFACE STORAGE
C=========        CNT(17) = sum of INFILTRATION TO SUBSURFACE STORAGE
C=========        CNT(20) = sum of INITIAL CHANNEL STORAGE
C=========        CNT(21) = sum of FLOW OFF SUBCATCHMENTS
C#######################################################################
      DO 10 J       = 1,30
  10  CNT(J)        = 0.0
      DO 20 J       = 1,NW
C#### WCH (CIM), 12/9/94.  MOVE QSUR, QIN, GFLOW, OUTFLOW TO NG LOOP.
      WFLOW(J)      = 0.0
      WAREA(J)      = 0.0
      WLMIN(J)      = 0.0
      WLMAX(J)      = 0.0
      NHYET(J)      = 0
      WDEPTH(4,J)   = 0.0
      LND(J)        = 0
      KLND(J)       = 0
C#######################################################################
C     Chuck Moore, CDM, 8/93.
C     Initialize peak flow and total volume statistics for subcatchment
C     statistics.
C     Also initialize some infiltration/inflow parameters.
C#######################################################################
      ICURVE(J)     = 0
      SEWAREA(J)    = 0.0
      FLOWII(J)     = 0.0
Cwch, 12/20/00. Initialize redirected overland flow variables.
	IFLOWP(J)	  = 0
      RFLOW(J)      = 0.0
	ENFILW(J)     = 0.0
	FLOW2SUB(J)   = 0.0
	WFLOW0(J)     = 0.0
	WVOL0(J)      = 0.0
C
      DO 14 JJ=1,12
      RDIIR(J,1,JJ)    = 0.0
      RDIIR(J,2,JJ)    = 0.0
 14   RDIIR(J,3,JJ)    = 0.0
      SUBQPEAK(1,J) = 0.0
      SUBQPEAK(2,J) = 0.0
      SUBQPEAK(3,J) = 0.0
      SUBQPEAK(4,J) = 0.0
      SUBQPEAK(5,J) = 0.0
      SUBQPEAK(6,J) = 0.0
Cwch 3/28/02.
      SUBQLOSS(J)   = 0.0
      SUBDEP(1,J)   = 0.0  
      SUBDEP(2,J)   = 0.0  
      SUBDEP(3,J)   = 0.0  
      SUBDEP(4,J)   = 0.0      
C==========================================================================
      DO 15 K       = 1,MQUAL
Cwch, 12/20/00
	POFF2C0(K,J)  = 0.0
	POFF2C1(K,J)  = 0.0
      PBASIN(K,J)   = 0.0
      DO 15 I       = 1,NLU
      POFF(I,K,J)   = 0.0
   15 PSHED(I,K,J)  = 0.0
      DO 20 K       = 1,3
      WSNOW(K,J)    = 0.0
      FW(K,J)       = 0.0
   20 WDEPTH(K,J)   = 0.0
C#######################################################################
C     Chuck Moore, CDM, 8/93.  Initialize rainfall totals by gage.
C=======================================================================
CIM INCREASE HYETOGRAPHS ~~~~~~~~~
      DO 25 I = 1,MAXRG
cim   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
   25 RTOT(I) = 0.0
C=======================================================================
C     Initialize more I/I parameters.
C=======================================================================
      DO 27 J = 1,NTK
      DO 27 I = 1,3
      DO 27 JJ=1,12
      DSTORE(J,I,JJ)  = 0.0
      DREC(J,I,JJ)    = 0.0
   27 STORAGE(J,I,JJ) = 0.0
C#######################################################################
      DO 30 I       = 1,NG
C#### WCH (CIM), 12/9/94.  NEXT 4 VARIABLES MOVED INTO THIS NG LOOP.
Cwch, 3/27/00. Change subscripts for next 4 variables to I, not J.
      QSUR(I)       = 0.0
      QIN(I)        = 0.0
      GFLOW(I)      = 0.0
      OUTFLW(I)     = 0.0
C
      LCHAN(I)      = 0
      KCHAN(I)      = 0
      NPG(I)        = 0
      NGUT(I)       = 0.0
      GDEPTH(I)     = 0.0
      GLEN(I)       = 0.0
      WTYPE(I)      = -1
C#######################################################################
C     Chuck I. Moore, CDM, 8/93.
C     Initialize maximum values for channel/pipe statistics.
C=======================================================================
      MAXJUL(I) = 0
      MAXTIM(I) = 0.0
      MAXDEP(I) = 0.0
      SURLEN(I) = 0.0
      MAXSUR(I) = 0.0
      MAXQIN(I) = 0.0
C####
      DO 30 J       = 1,MQUAL
C#### WCH, 9/93
      SUMRDII(J)    = 0.0
      CONCII(J)     = 0.0
      GWQ(J)        = 0.0
      CGWQ(J)       = 0.0
Cwch, 2/10/01
      GREMOVE(J,I)  = 0.0
      CDOT(J,I)     = 0.0
      CVSUR(J,I)    = 0.0
   30 C(J,I)        = 0.0
C
      DO 40 J       = 1,NW
   40 NGTOI(J)      = 0
      DO 50 J       = 1,NCP
C#### WCH (CIM), 23/9/94.  CHANGE DO 50 LOOP TO NG, NOT NW.
      DO 50 I       = 1,NG
   50 NGTOG(J,I)    = 0
      DO 60 J       = 1,NCP
Cwch 4/5/00, Should be loop to NW, not NG.
Cwch, 12/20/00.  Add NWTOW(NCP,NW) for rerouting overland flow. 
      DO 60 I       = 1,NW
	NWTOW(J,I)	  = 0
   60 NWTOG(J,I)    = 0
Cwch, 12/20/00.  Additional quality summations.
C     Note, primary quality summers, SUM(MQUAL,1-10), 
C     initialized in QINT. 
	DO 65 J = 1,MQUAL
	QDECAY(J)      = 0.0
Cwch, 2/10/01.
      SUMREMG(J,1)   = 0.0
	SUMREMG(J,2)   = 0.0
	DO 65 L = 1,NLU
	REMOVE(L,J)   = 0.0
	DO I = 1,3
		SUMREM(L,J,I) = 0.0
		ENDDO
	DO 65 I = 1,11
   65 SUMNL(L,J,I)  = 0.0
Cwch, 12/20/00
      SUMWAT = 0.0
C=======================================================================
C     Open all scratch files for the Runoff Block.
C=======================================================================
      IF(JOUT(IOUTCT).GT.0.AND.FFNAME(25+IOUTCT).EQ.'JOT.UF')
     +      OPEN(JOUT(IOUTCT),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JOUT(IOUTCT).GT.0.AND.FFNAME(25+IOUTCT).NE.'JOT.UF')
     +      OPEN(JOUT(IOUTCT),FILE=FFNAME(25+IOUTCT),FORM='UNFORMATTED',
     +      STATUS='UNKNOWN')
      IF(JKP(51).NE.2.AND.NSCRAT(1).GT.0.AND.FFNAME(51).NE.'SCRT1.UF') O
     +PEN(NSCRAT(1),FILE=FFNAME(51),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(52).NE.2.AND.NSCRAT(2).GT.0.AND.FFNAME(52).NE.'SCRT2.UF') O
     +PEN(NSCRAT(2),FILE=FFNAME(52),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(53).NE.2.AND.NSCRAT(3).GT.0.AND.FFNAME(53).NE.'SCRT3.UF') O
     +PEN(NSCRAT(3),FILE=FFNAME(53),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(54).NE.2.AND.NSCRAT(4).GT.0.AND.FFNAME(54).NE.'SCRT4.UF') O
     +PEN(NSCRAT(4),FILE=FFNAME(54),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(55).NE.2.AND.NSCRAT(5).GT.0.AND.FFNAME(55).NE.'SCRT5.UF') O
     +PEN(NSCRAT(5),FILE=FFNAME(55),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(56).NE.2.AND.NSCRAT(6).GT.0.AND.FFNAME(56).NE.'SCRT6.UF') O
     +PEN(NSCRAT(6),FILE=FFNAME(56),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(57).NE.2.AND.NSCRAT(7).GT.0.AND.FFNAME(57).NE.'SCRT7.UF') O
     +PEN(NSCRAT(7),FILE=FFNAME(57),FORM='UNFORMATTED',STATUS='UNKNOWN')
C#### C. MOORE, 8/93
      IF(JKP(58).NE.2.AND.NSCRAT(8).GT.0.AND.FFNAME(58).NE.'SCRT8.UF') O
     +PEN(NSCRAT(8),FILE=FFNAME(58),FORM='UNFORMATTED',STATUS='UNKNOWN')
Cwch, 12/20/00
      IF(JKP(59).NE.2.AND.NSCRAT(9).GT.0.AND.FFNAME(59).NE.'SCRT9.UF') O
     +PEN(NSCRAT(9),FILE=FFNAME(59),FORM='UNFORMATTED',STATUS='UNKNOWN')
      IF(JKP(51).NE.2.AND.NSCRAT(1).GT.0.AND.FFNAME(51).EQ.'SCRT1.UF')
     +             OPEN(NSCRAT(1),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JKP(52).NE.2.AND.NSCRAT(2).GT.0.AND.FFNAME(52).EQ.'SCRT2.UF')
     +             OPEN(NSCRAT(2),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JKP(53).NE.2.AND.NSCRAT(3).GT.0.AND.FFNAME(53).EQ.'SCRT3.UF')
     +             OPEN(NSCRAT(3),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JKP(54).NE.2.AND.NSCRAT(4).GT.0.AND.FFNAME(54).EQ.'SCRT4.UF')
     +             OPEN(NSCRAT(4),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JKP(55).NE.2.AND.NSCRAT(5).GT.0.AND.FFNAME(55).EQ.'SCRT5.UF')
     +             OPEN(NSCRAT(5),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JKP(56).NE.2.AND.NSCRAT(6).GT.0.AND.FFNAME(56).EQ.'SCRT6.UF')
     +             OPEN(NSCRAT(6),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JKP(57).NE.2.AND.NSCRAT(7).GT.0.AND.FFNAME(57).EQ.'SCRT7.UF')
     +             OPEN(NSCRAT(7),FORM='UNFORMATTED',STATUS='SCRATCH')
C#### C. MOORE, CDM, 8/93.
      IF(JKP(58).NE.2.AND.NSCRAT(8).GT.0.AND.FFNAME(58).EQ.'SCRT8.UF')
     +             OPEN(NSCRAT(8),FORM='UNFORMATTED',STATUS='SCRATCH')
Cwch, 12/20/00
      IF(JKP(59).NE.2.AND.NSCRAT(9).GT.0.AND.FFNAME(59).EQ.'SCRT9.UF')
     +             OPEN(NSCRAT(9),FORM='UNFORMATTED',STATUS='SCRATCH')
C=======================================================================
C     Call input subroutines RHYDRO1, RHYDRO2 and maybe QHYDRO.
C=======================================================================
      WRITE(*,1001)
      CALL RHYDRO1
C=======================================================================
C     Read in quality inputs by calling QHYDRO if KWALTY = 1.
C=======================================================================
      NQS = 0
      IF(KWALTY.EQ.1) THEN
                      WRITE(*,9020)
                      CALL QHYDRO
                      ELSE
                      WRITE(N6,1550)
                      ENDIF
C=======================================================================
      CALL RHYDRO2
C=======================================================================
C     Set up ordering array.
C=======================================================================
      NING     = NSAVE
      DO 100 I = 1,NING
      IF(NGTOI(I).EQ.0) GO TO 110
      NSPOT       = NG+1-I
  100 NGUT(NSPOT) = NGTOI(I)
C=======================================================================
C     Build tree structure.
C=======================================================================
  110 DO 130 I = 1,NG
      KSPOT    = NG+1-I
      ISUB     = NGUT(KSPOT)
      IF(ISUB.LE.0) GO TO 130
      DO 120 J = 1,NCP
      IF(NGTOG(J,ISUB).EQ.0) GO TO 130
      NSPOT       = NSPOT-1
  120 NGUT(NSPOT) = NGTOG(J,ISUB)
  130 CONTINUE
C=======================================================================
C     Shift to start of array.
C=======================================================================
      NSPOT    = 0
      DO 140 I = 1,NG
      IF(NGUT(I).EQ.0) GO TO 140
      NSPOT       = NSPOT+1
      NGUT(NSPOT) = NGUT(I)
  140 CONTINUE
C=======================================================================
C     Call watershed simulation.
C=======================================================================
      CALL HYDRO
      WRITE(*,1002)
      IF(IPRN(2).EQ.0) THEN
                       CALL HCURVE(1)
                       CALL HCURVE(2)
                       CALL HCURVE(3)
                       ENDIF
      IF(NSVGW.GT.0)   CALL HCURVE(4)
      WRITE(*,1003)
C=======================================================================
Cwch, 12/20/00.
C     PRPOLL(0) means call for print of M3 channel/inlet locations.
C     PRPOLL(1) means call for print of M5 subcat locations.
C     PRFLOW(0) means call for print of M3 channel/inlet locations.
C     PRFLOW(1) means call for print of M4 depth locations.
C     PRFLOW(2) means call for print of M5 subcat locations.
C=======================================================================
      IF(NQS.GT.0) THEN
                   CALL PRPOLL(0)
	             IF(MSUBC.GT.0) CALL PRPOLL(1)
                   IF(MDEEP.GT.0.OR.NGWGF.GT.0) CALL PRFLOW(1)
                   ELSE
                   CALL PRFLOW(0)
				 IF(MSUBC.GT.0) CALL PRFLOW(2)
                   ENDIF
      WRITE(N6,21)
      WRITE(*,21)
C=======================================================================
cred  close the scratch files when exiting - 5/21/01
cred  this has been a problem if the next block is transport or extran
      closing_scratch_files:   do i = 1,8
                               if (nscrat(i).gt.0) close (nscrat(i))
                               enddo       closing_scratch_files
cred  5/21/01  get rid of the binary rainfall file name for subsequent 
cred  blocks
cred  if tranpsort or extran was used then the temp or rain file would
cred  disappear - noted by many CDM users of SWMM
      if(FFNAME(51).NE.'SCRT1.UF') FFNAME(51) = 'SCRT1.UF'
cred  5/21/01  get rid of the binary temperature file name for 
cred  subsequent blocks
      if(FFNAME(53).NE.'SCRT3.UF') FFNAME(51) = 'SCRT3.UF'
C=======================================================================
      RETURN
C=======================================================================
   11 FORMAT(/,
     1' ###################################################',/,
     2' # Entry made to the Runoff Block, last updated by #',/,
     3' # Oregon State University, and Camp, Dresser and  #',/,
     3' # McKee, Inc., July 2003.                         #',/,
     4' ###################################################',/,
     5' # "And wherever water goes, amoebae go along for  #',/,
     6' #  the ride"                      Tom Robbins     #',/,
     7' ###################################################',/)
   12 FORMAT(/,
     1' ###################################################',/,
     2' # Entry made to the Runoff Block, last updated by #',/,
     3' # Oregon State University, and Camp, Dresser and  #',/,
     3' # McKee, Inc., July 2003.                         #',/,
     4' ###################################################',/)
   21 FORMAT(/,' ===> Runoff simulation ended normally.')
 1001 FORMAT(/,' Entering input subroutine.')
 1002 FORMAT(/,' Entering plot output subroutine.')
 1003 FORMAT(/,' Entering print output subroutine.')
 1550 FORMAT(//,
     +' ************************************************',/,
     +' *  Quality simulation not included in this run *',/,
     +' ************************************************',//)
 9020 FORMAT(/,' Reading water quality information.')
C=======================================================================
      END
