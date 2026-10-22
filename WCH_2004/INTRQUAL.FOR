	SUBROUTINE INTRQUAL
C=======================================================================
C     Subroutine to enter Transport Block quality input variables.
C     Created by Wayne Huber, July 2001. 
C     Called from Sub. INTRAN
C     Uses part of code formerly in Sub. INTRAN
C     Main reason for new subroutine is to input parameters for 
C       simulation of DO cycle using modified Streeter-Phelps.  Follow
C       WASP6 options. 
C     Add new F2, F3, F4 lines, 7/6/01. 
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'DRWF.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'NAMES.INC'
      INCLUDE 'TST.INC'
      INCLUDE 'NEW81.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'FLODAT.INC'
      INCLUDE 'NEWTR.INC'
      INCLUDE 'FSPLIT.INC'
CIM  CHANGE FOR VARIABLE BASE FLOW IN TRANSPORT   11/97
      INCLUDE 'MOBFF.INC'
	INCLUDE 'TRANWQ.INC'
C=======================================================================
CIMT  CHANGE DIMENSIONS FROM 4 TO MQUAL  (NDUM, PNDUM, PUDUM)
      DIMENSION QI(NET),QO(NET),QO1(NET),QO2(NET),NDUM(MQUAL),VR(MQUAL)
Cwch, 4/18/02.      
	DIMENSION CTCOEF(8),SETLUNIT(2)
      CHARACTER PNONE*8,PNDUM(MQUAL)*8,PUDUM(MQUAL)*8,BLANK*8,KNEED*10,
     +          BMJ*10,KSURF(NTOA)*8,DASH*10,KOEQ*10,SETLUNIT*8
      EQUIVALENCE (QO(1),Q(1,2,2)),(QI(1),Q(1,1,2))
      EQUIVALENCE (QO1(1),QMAX(1)),(QO2(1),QFULL(1))
      DATA BLANK/'        '/
      DATA PNONE/'  NONE  '/
	DATA DASH/'  --------'/
Cwch, 7/6/01. Coefficients for computation of DO saturation. 
	DATA CTCOEF/-139.34411,1.575701E5,-6.642308E7,1.2438E10,
     +-8.621949E11,-0.017674,10.754,2140.7/
Cwch, 4/18/02.
      DATA SETLUNIT/'(FT/DAY)',' (M/DAY)'/
C=======================================================================
C     INPUT INFORMATION FOR QUALITY PARAMETERS.
C     XLparms = new quality summers. 
Cwch, 7/6/01. New initialization.
C=======================================================================
      XLREAER  = 0.0
	XLBOD2DO = 0.0
	XLNOD2DO = 0.0
	XLSEDOD  = 0.0
	XLNO3SRC = 0.0
	DO J = 1,MQUAL
	   XLREMOV(J) = 0.0
	   XLSETL(J)  = 0.0
	   VSETL(J)   = 0.0
	   FDIS(J)    = 1.0
	   DO K = 1,NE
	     TREMOVE(K,J) = 0.0
	     ENDDO
	   ENDDO
	FBOD5 =  1.0
	KWQPAR = 0
	KWIND  = 0
	KOVAR  = 0
      KSPG   = 0
	WRITE (*,*) ' '
	WRITE (*,*) ' Reading water quality parameters.'
      WRITE (N6,4090)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP F1 <<<<<<<<<<<<
C=======================================================================
      DO 4140 K = 1,NPOLL
      READ(N5,*,ERR=888) CC,KPOL,PNDUM(K),PUDUM(K),NDUM(K),DECAY(K),
     1                   SPG(K),(PSIZE(J,K),PGR(J,K),J=2,5),PSDWF(K),
	2                   VSETL(K),FDIS(K)
C=======================================================================
C     HERE, USER INPUT DEFINES POLLUTANT.
C=======================================================================
      IF(KPOL.EQ.0) THEN
           IPOLX(K)    = 0	
Cwch, 8/29/00. Check for KPOL.ne.0 when no interface file input.
           ELSEIF((NWQ.EQ.0.OR.K.GE.5).AND.KPOL.EQ.0) THEN
		      IF(NCNTRL.EQ.1) WRITE(N6,4087) K
	     ELSE
           IF(NWQ.EQ.1) THEN
	        IF(K.EQ.1) THEN
	          IF(KPOL.LT.0) THEN
	             FBOD5 = 1.0/(1.0-EXP(-DECAY(K)*5.0))
	             KPOL  = -KPOL
	             ENDIF
	          PNDUM(1) = '   CBOD '
	          PUDUM(1) = '   MG/L '
	          ENDIF
	        IF(K.EQ.2) THEN
	          PNDUM(2) = '    NOD '
	          PUDUM(2) = '   MG/L '
	          ENDIF
	        IF(K.EQ.3) THEN
	          PNDUM(3) = '  NO3-N '
	          PUDUM(3) = '   MG/L '
	          ENDIF
	        IF(K.EQ.4) THEN
	          PNDUM(4) = '   DO   '
	          PUDUM(4) = '   MG/L '
	          ENDIF
	        IF(NDUM(K).NE.0) THEN
	          WRITE(N6,9200) K,NDUM(K)
	          NDUM(K) = 0
	          ENDIF
	        ENDIF
           IPOLX(K)    = KPOL
	     IF(NWQ.EQ.0.OR.K.GE.5) THEN
              PNDUM(K)    = PNAME(KPOL)
              PUDUM(K)    = PUNIT(KPOL)
              NDUM(K)     = NDIM(KPOL)
	        ENDIF
           ENDIF
      PSIZE(1,K) = 0.0
      PGR(1,K)   = 100.0
      WRITE (N6,4130)  K,PNDUM(K),PUDUM(K),NDUM(K),KPOL,DECAY(K),SPG(K)
	IF(NWQ.EQ.1.AND.K.EQ.1) WRITE (N6,9230) FBOD5
      IF(SPG(K).GT.1.0) KSPG = KSPG+1
      DECAY(K)               = DECAY(K)/86400.0
 4140 CONTINUE
Cwch, 4/18/02.
      WRITE(N6,4135) SETLUNIT(METRIC)
      DO 4141 K = 1,NPOLL
      PNAME(K)  = PNDUM(K)
      PUNIT(K)  = PUDUM(K)
      NDIM(K)   = NDUM(K)
	IF(NWQ.EQ.1.AND.K.EQ.4) VSETL(4) = 0.0
	IF(NWQ.EQ.1.AND.K.EQ.4) FDIS(4)  = 1.0
	WRITE(N6,4137) PNAME(K),VSETL(K),FDIS(K)
C     Convert from ft/day or m/day to ft/sec.
      VSETL(K) = VSETL(K)*CMET(1,METRIC)/86400.
 4141 CONTINUE
C
      IF(KSPG.EQ.0) WRITE (N6,4145)
      IF(KSPG.EQ.0) GO TO 4170
      WRITE (N6,4150)
C
      DO 4160 K = 1,NPOLL
      IF(SPG(K).LE.1.0) GO TO 4160
      WRITE (N6,4155) K,PSDWF(K),(PSIZE(L,K),PGR(L,K),L=1,5)
C=======================================================================
C#### WCH, 1/13/95.  MODIFY THIS ERROR CHECKING ROUTINE.  DO NOT ALLOW
C     PSIZE(J) TO BE > PSIZE(J+1).
C=======================================================================
C####      DO 4156 J = 2,4
C####      IF(PSIZE(J,K).LE.PSIZE(5,K)) GO TO 4156
C####      PSIZE(5,K) = PSIZE(J,K)
      DO 4156 J = 1,4
      IF(PSIZE(J,K).LE.PSIZE(J+1,K)) GO TO 4156
         WRITE(N6,4151) K,J,J+1,J,J+1,PSIZE(J+1,K)
         PSIZE(J,K) = PSIZE(J+1,K)
 4156 CONTINUE
C=======================================================================
C#### WCH, 1/13/95.  LAST PERCENTAGE, PGR(5) SHOULD = 0.  WARN IF NOT.
C=======================================================================
      IF(PGR(5,K).GT.0.0) WRITE(N6,4152) K,PGR(5,K)
C=======================================================================
C     ONLY PERFORM SCOUR/DEPOSITION FOR POLLUTANTS WITH UNITS OF MG/L.
C=======================================================================
      IF(NDIM(K).GT.0) THEN
                       WRITE (N6,4157) K,NDIM(K),K
                       SPG(K) = 1.0
                       KSPG = KSPG-1
                       ENDIF
 4160 CONTINUE
 4170 CONTINUE
C=======================================================================
C     Print constant concentrations in manhole inflows.  Move here from
C     former printout with rest of element information because now
C     input pollutant parameters *after* element info printout.
C     Also print out delivery ratios for Type 27 element. 
C=======================================================================
      IPPP = 0
	DO I = 1,NE
	  IF((NTYPE(I).EQ.19.AND.DIST(I).NE.0.0).OR.NTYPE(I).EQ.27)IPPP=1
	  ENDDO
	IF(IPPP.EQ.1) THEN
	     WRITE(N6,9100)
	     WRITE(N6,9110) (PNAME(J),J=1,NPOLL)
	     IF(METRIC.EQ.1) WRITE(N6,9111) (PUNIT(J),J=1,NPOLL)
	     IF(METRIC.EQ.2) WRITE(N6,9112) (PUNIT(J),J=1,NPOLL)
	     WRITE(N6,9115) (DASH,J=1,NPOLL)
	     DO 5000 I = 1,NE
	     IF(NTYPE(I).EQ.19.AND.DIST(I).NE.0.0) THEN
		     IF(JCE.EQ.0) WRITE(N6,9151) NOE(I),NTYPE(I),
     1          DIST(I)/CMET(8,METRIC),(PMANN(I,J),J=1,NPOLL)
		     IF(JCE.EQ.1) WRITE(N6,9152) KOE(I),NTYPE(I),
     1          DIST(I)/CMET(8,METRIC),(PMANN(I,J),J=1,NPOLL)
C     CONVERT CONST. INFLOW AND CONCENTRATIONS TO LOADS,
C     IN UNITS OF CONCENTRATION * CFS.
                DO  J=1,NPOLL
                   PMANN(I,J)=PMANN(I,J)*DIST(I)
	             ENDDO
	       ELSEIF(NTYPE(I).EQ.27) THEN
		     IF(JCE.EQ.0) WRITE(N6,9151) NOE(I),NTYPE(I),
     1          DIST(I)/CMET(8,METRIC),(DRATIO(I,J),J=1,NPOLL)
		     IF(JCE.EQ.1) WRITE(N6,9152) KOE(I),NTYPE(I),
     1           DIST(I)/CMET(8,METRIC),(DRATIO(I,J),J=1,NPOLL)
	         ENDIF
 5000      CONTINUE
           ENDIF
C=======================================================================
C     Optional entry of BMP removal fractions.
C    >>>>>>>>>>>>>>>> READ DATA LINE F2 <<<<<<<<<<<<<<<<<<
C=======================================================================
      DO 5080 N = 1,NE
      READ(N5,*,ERR=888) CC
	IF(CC.NE.'F2') THEN
	   BACKSPACE N5
	   GOTO 5090
	   ELSE
	   BACKSPACE N5
	   IF(JCE.EQ.0) READ(N5,*,ERR=888) NOEQ,(VR(J),J=1,NPOLL)
	   IF(JCE.EQ.1) READ(N5,*,ERR=888) KOEQ,(VR(J),J=1,NPOLL)
	   IF(N.EQ.1) THEN
	      WRITE(N6,9005) (PNAME(J),J=1,NPOLL)
	      WRITE(N6,9006) (PUNIT(J),J=1,NPOLL)
	      WRITE(N6,9007) (DASH,J=1,NPOLL)
	      ENDIF
C=======================================================================
C     Find internal number.
C=======================================================================
         DO 5020 K = 1,NE
	   IF(JCE.EQ.0.AND.NOE(K).EQ.NOEQ) GOTO 5050
         IF(JCE.EQ.1.AND.KOE(K).EQ.KOEQ) GOTO 5050
 5020    CONTINUE
	   IF(JCE.EQ.0) WRITE(N6,9013) NOEQ,N
         IF(JCE.EQ.1) WRITE(N6,9014) KOEQ,N
	   GOTO 5080
 5050 IF(.NOT.(NTYPE(K).LE.18.OR.NTYPE(K).EQ.20.OR.NTYPE(K).EQ.22)) THEN
            IF(JCE.EQ.0) WRITE(N6,9016) NOEQ,N,NTYPE(K)
            IF(JCE.EQ.1) WRITE(N6,9017) KOEQ,N,NTYPE(K)
            GOTO 5080
	      ENDIF
         IF(JCE.EQ.0) WRITE(N6,9010) NOEQ,K,NTYPE(K),(VR(J),J=1,NPOLL)
         IF(JCE.EQ.1) WRITE(N6,9011) KOEQ,K,NTYPE(K),(VR(J),J=1,NPOLL)
	   DO J = 1,NPOLL
	      TREMOVE(K,J) = VR(J)
	      ENDDO
         ENDIF
 5080 CONTINUE
C=======================================================================
 5090 IF(NWQ.EQ.0) RETURN
C=======================================================================
C     Additional input routines when simulating DO-BOD (NWQ = 1).
Cwch, 7/6/01
C=======================================================================
C    >>>>>>>>>>>>>>>> READ DATA LINE F3 <<<<<<<<<<<<<<<<<<
C               General and Default Parameters
C=======================================================================
      READ (N5,*,ERR=888) CC
	IF(CC.NE.'F3') THEN
	     WRITE(N6,9240) CC
	     STOP ' ERROR. MUST HAVE F3 LINE WHEN NWQ = 1. STOP IN TRANS.'
	     ELSE
	     BACKSPACE N5
           READ (N5,*,ERR=888) CC,KWQPAR,THETA,PCSAT,
     1        SALINITY,TADEF,TWDEF,WINDDEF,SODDEF
	     ENDIF
	WRITE(N6,9250)
	IF(KWQPAR.EQ.0) WRITE(N6,9255)
	IF(KWQPAR.EQ.1) WRITE(N6,9256)
	IF(KWQPAR.EQ.2) WRITE(N6,9257)
	WRITE(N6,9260) THETA,FDIS(1),FDIS(2),PCSAT,SALINITY
C=======================================================================
C     Maintain following units internally:
C     Settling velocity: ft/s
C     Air temperature: deg C
C     Water temperature: deg C
C     Wind speed: m/s
C     Sediment O2 demand: mg/ft^2-s
C=======================================================================
	IF(METRIC.EQ.1) THEN
	     WRITE(N6,9261) VSETL(1)*86400.,VSETL(2)*86400.,TADEF,
     1         TWDEF,WINDDEF,SODDEF
	     TADEF = (TADEF-32.0)*5.0/9.0
	     TWDEF = (TWDEF-32.0)*5.0/9.0
	     WIND  = WIND/CMET(1,2)
	     SODDEF = SODDEF/86400.*1000.
	     ENDIF
	IF(METRIC.EQ.2) THEN
	     WRITE(N6,9262) VSETL(1)/CMET(1,2)*86400.,
     1         VSETL(2)/CMET(1,2)*86400.,TADEF,TWDEF,WINDDEF,SODDEF
	     SODDEF   = SODDEF/86400./CMET(13,2)*1000.
	     ENDIF
C=======================================================================
C	Compute DO saturation at default temperature and salinity.
C     See Sub. QUALPARM for references. 
C=======================================================================
     	TKELV = 273.15 + TWDEF
      ARG = CTCOEF(1) + CTCOEF(2)/TKELV
	DO I = 2,4
	  ARG = ARG + CTCOEF(I+1)/TKELV**I
	  ENDDO
	IF(SALINITY.GT.0.0) ARG = ARG + SALINITY*(CTCOEF(6) +
     1  CTCOEF(7)/TKELV + CTCOEF(8)/TKELV**2)
	CS = EXP(ARG)
	WRITE(N6,9263) CS
C=======================================================================
C	Set channel parameters = defaults.
C=======================================================================
      DO 5100 N = 1,NE
	IF(NTYPE(N).LE.18.OR.NTYPE(N).EQ.20.OR.NTYPE(N).EQ.22) THEN
	     DEKD(N)   = DECAY(1)
	     DEKN(N)   = DECAY(2)
           TAIR(N)   = TADEF
	     TWATER(N) = TWDEF
	     WIND(N)   = WINDDEF
	     SOD(N)    = SODDEF
	     ENDIF
 5100 CONTINUE
C=======================================================================
C    >>>>>>>>>>>>>>>> READ DATA LINE F4 <<<<<<<<<<<<<<<<<< 
C                   Reaeration Parameters
C=======================================================================
      READ(N5,*,ERR=888) CC,KWIND,KOVAR,KWPARM
	IF(KOVAR.EQ.1) THEN
	     BACKSPACE N5
	     READ(N5,*,ERR=888) CC,KWIND,KOVAR,KWPARM,REA
	     ENDIF
	WRITE(N6,9269)
      IF(KWIND.EQ.0) WRITE(N6,9270) 
	IF(KWIND.EQ.1) WRITE(N6,9271)
	IF(KOVAR.EQ.0) WRITE(N6,9280)
	IF(KOVAR.EQ.1) WRITE(N6,9281)
	IF(KOVAR.EQ.1.AND.METRIC.EQ.1) WRITE(N6,9291) REA
	IF(KOVAR.EQ.1.AND.METRIC.EQ.2) WRITE(N6,9292) REA
C=======================================================================
C    >>>>>>>>>>>>>>>> READ F5 DATA LINES <<<<<<<<<<<<<<<<<<
C            User-input of O'Connor reaeration parameters.
C     CAPPA = Von Karman's constant, Kappa.
C=======================================================================
      CAPPA  = 0.4
	CAPPA3 = 0.4**0.3333333
      IF(KWPARM.EQ.1) THEN
	     READ(N5,*,ERR=888) CC,XLAMBDA,GAMMAZ,UTSTAR,UCSTAR,
     1        PRESURE,ZEE,ZAA
	     ZAA = ZAA* 100.
	     ELSE
	     XLAMBDA = 3.0   ! dimensionless inverse roughness Reynolds No.
	     GAMMAZ  = 6.5   ! dimensionless boundary Reynolds No.
	     UTSTAR  = 11.0  ! cm/s, reference shear velocity in transition zone
	     UCSTAR  = 10.0  ! cm/s, critical (smooth - transition) shear velocity
	     PRESURE = 1000. ! mb, atmospheric presure
	     ZEE     = 0.25  ! cm, rough regime roughness
	     ZAA     = 1000. ! cm, wind measurement height
	     ENDIF
	WRITE(N6,9295) XLAMBDA,GAMMAZ,UTSTAR,UCSTAR,PRESURE,ZEE,ZAA
C=======================================================================
C    >>>>>>>>>>>>>>>> READ F6 DATA LINES <<<<<<<<<<<<<<<<<<
C                  Parameters for Each Channel
C=======================================================================
	IF(KWQPAR.EQ.1) THEN
         IPPP = 0
         DO 5200 N = 1,NE
	   READ(N5,*,ERR=888) CC
	   IF(CC.NE.'F4') THEN
	        BACKSPACE N5
	        GOTO 5210
	        ENDIF
	   IPPP = IPPP + 1
	   IF(IPPP.EQ.1) THEN
	        WRITE(N6,9300)
	        IF(METRIC.EQ.1) WRITE(N6,9301)
	        IF(METRIC.EQ.2) WRITE(N6,9302)
	        WRITE(N6,9303)
	        ENDIF
	   BACKSPACE N5
	   IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,NOEQ,(VR(I),I=1,6)
	   IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,KOEQ,(VR(I),I=1,6)
C=======================================================================
C     Find internal number.
C=======================================================================
         DO 5120 K = 1,NE
	   IF(JCE.EQ.0.AND.NOE(K).EQ.NOEQ) GOTO 5150
         IF(JCE.EQ.1.AND.KOE(K).EQ.KOEQ) GOTO 5150
 5120    CONTINUE
	   IF(JCE.EQ.0) WRITE(N6,9305) NOEQ,N
         IF(JCE.EQ.1) WRITE(N6,9306) KOEQ,N
	   GOTO 5200
 5150 IF(.NOT.(NTYPE(K).LE.18.OR.NTYPE(K).EQ.20.OR.NTYPE(K).EQ.22)) THEN
            IF(JCE.EQ.0) WRITE(N6,9316) NOEQ,N,NTYPE(K)
            IF(JCE.EQ.1) WRITE(N6,9317) KOEQ,N,NTYPE(K)
            GOTO 5200
	      ENDIF
         IF(JCE.EQ.0) WRITE(N6,9310) NOEQ,K,NTYPE(K),VR
         IF(JCE.EQ.1) WRITE(N6,9311) KOEQ,K,NTYPE(K),VR
	   DEKD(K)   = VR(1)/86400.
	   DEKN(K)   = VR(2)/86400.
         IF(METRIC.EQ.1) THEN
            TAIR(K)   = (VR(3)-32.0)*5.0/9.0
	      TWATER(K) = (VR(4)-32.0)*5.0/9.0
	      WIND(K)   = VR(5)/CMET(1,2)
	      SOD(K)    = VR(6)/86400.*1000.
	      ENDIF
	   IF(METRIC.EQ.2) SOD(K) = VR(6)/86400./CMET(13,2)*1000.
 5200    CONTINUE
         ENDIF
 5210 CONTINUE
      IF(KWQPAR.EQ.1.AND.IPPP.EQ.0) WRITE(N6,9320)
C=======================================================================
      RETURN
  888 CALL IERROR
Cwch, 8/29/00
 4087 FORMAT (' CAUTION!! POLLUTANT NAME/UNITS ON F1 LINE NUMBER ',I3,
     1' LINE WILL NOT BE USED WHEN KPOL.NE.0',/,
	2' NEED TO DEFINE POLLUTANT WHEN NCNTRL=1 (NO INTERFACE FILE INPUT)
	3.')
Cwch, 4/18/02
 4090 FORMAT(///,
     110X,' *****************************************************',/,
     210X,' *   QUALITY PARAMETERS TO BE SIMULATED IN TRANSPORT *',/,
     310X,' *****************************************************',//,
     1T27,'TYPE OF',T37,'INTERFACE FILE    DECAY COEF',T66,'SPECIFIC',/,
     3 ' NO.  NAME      UNITS      UNITS   POSITION (IF ANY)   (1/DAY)
     4  GRAVITY',/,' ---  ----      -----      -----   -----------------
     5   -------    -------')
 4130 FORMAT (1X,I2,3X,A8,2X,A8,I6,I13,F18.3,3F12.3)
 4135 FORMAT(///,
     110X,' ******************************',/,
     210X,' *   SETTLING VELOCITY DATA   *',/,
     310X,' ******************************',//,
	4' POLLUTANT  SETTLING VELOCITY  DISSOLVED',/,
	5'     NAME       ',A8,  '        FRACTION',/,
     6' --------    ---------------    --------')
 4137 FORMAT(1X,A8,F19.5,F12.4)        
 4145 FORMAT (//,
     110X,' *******************************************',/,
     210X,' *     ALL SPECIFIC GRAVITIES ARE LE 1.0.  *',/,
     310X,' *     NO SIMULATION OF SCOUR-DEPOSITION.  *',/,
     410X,' *******************************************',//)
 4150 FORMAT (//,
     15X,' **********************************************',/,
     25X,' * PARTICLE SIZE DISTRIBUTIONS FOR QUALITY    *',/,
     35X,' * CONSTITUENTS WITH SPECIFIC GRAVITIES > 1.0 *',/,
     45X,' **********************************************',//,
     25X,' POLL.    MAX DWF     SIZE     PERCENT',/,
     35X,'  NO.    SIZE (MM)    (MM)     GREATER',/,
     45X,' ----    ---------    ----     -------')
C#### WCH, 1/13/95.
 4151 FORMAT(' WARNING! FOR POLLUTANT NO.',I3,', PSIZE(',I1,') > PSIZE('
     1,I1,').  NOT ALLOWED.',/,' SET PSIZE(',I1,') = PSIZE(',I1,') = ',
     2 F7.4,' MM.')
 4152 FORMAT(' WARNING! FOR POLL. NO.',I3,' PGR(5) NOT = 0.  CALCULATION
     1 ERRORS MAY RESULT.')
C
 4155 FORMAT(5X,I4,2F12.5,F10.3,/,4(21X,F12.5,F10.3,/))
 4157 FORMAT(/,' ===> WARNING !! FROM SUB TRANS.  NDIM(',I1,') = ',I2,
     1 ', GT 0.  CONCENTRATION UNITS MUST BE MG/L FOR USE OF SCOUR/DEPOS
     2ITION ROUTINES.',/,' SPECIFIC GRAVITY, SPG, HAS BEEN SET = 1.0 FOR
     3 POLLUTANT NO.',I2,' TO PERMIT CONTINUATION OF RUN.')
 9005 FORMAT(/,
     1' **************************************************************'
     1,/,
	3' * OPTIONAL BMP REMOVAL FRACTIONS, BASED ON LOAD, ON F2 LINES *'
	4,/,
	5' **************************************************************'
	6,//,   '    ELEMENT  INTERNAL  TYPE',99(2X,A8))
 9006 FORMAT('     NUMBER    NUMBER      ',99(2X,A8))
 9007 FORMAT(' ----------    ------  ----',99(A10))
 9010 FORMAT(1X,I10,I10,I6,99(F8.3))
 9011 FORMAT(1X,A10,I10,I6,99(F8.3))
 9013 FORMAT(/,' $$$$ WARNING! NO MATCH FOR ELEMENT ',I10,', ROW '
     1,I4,', OF F2 LINES.  WILL USE DEFAULT CHANNEL PARAMETERS.')
 9014 FORMAT(/,' $$$$ WARNING! NO MATCH FOR ELEMENT ',A10,', ROW '
     1,I4,', OF F2 LINES.  WILL USE DEFAULT CHANNEL PARAMETERS.')
 9016 FORMAT(/,' $$$$ WARNING. ELEMENT NO. ',I10, ', ROW ',I3,' OF F2 LI
     1NES MUST BE CHANNEL OR HAVE STORAGE.',/,
	2' BUT NTYPE =',I3,'. INPUT DATA FROM THIS LINE IGNORED.')
 9017 FORMAT(/,' $$$$ WARNING. ELEMENT NO. ',A10, ', ROW ',I3,' OF F2 LI
     1NES MUST BE CHANNEL OR HAVE STORAGE.',/,
	2' BUT NTYPE =',I3,'. INPUT DATA FROM THIS LINE IGNORED.')
 9100 FORMAT(/,
     1' ***********************************************************',/,
     2' * MANHOLES (NTYPE = 19) WITH NONZERO INFLOWS AND POSSIBLE *',/,
     3' * CONSTANT INFLOW CONCENTRATIONS *OR* DELIVERY RATIOS FOR *',/,
     4' * TYPE 27 QUALITY DIVIDER ELEMENT.                        *',/,
     5' ***********************************************************')
 9110 FORMAT(/,'   ELEMENT   TYPE  INFLOW',99(2X,A8))
 9111 FORMAT  ('    NUMBER          CFS ',99(2X,A8))
 9112 FORMAT  ('    NUMBER          CMS ',99(2X,A8))
 9115 FORMAT  (' ----------  ---- -------',99A10)
 9151 FORMAT  (1X,I10,I6,F8.3,99F10.3)
 9152 FORMAT  (1X,A10,I6,F8.3,99F10.3)
 9200 FORMAT(/,' $$$WARNING. WHEN SIMULATING DO-BOD (NWQ = 1), *MUST* HA
     1VE UNITS FOR FIRST FOUR CONSTITUENTS = MG/L (NDUM = 0).',/,
	2' INPUT ERROR FOR CONSTITUENT',I3,'. NDUM =',I2,'. NDUM SET = 0.')
 9230 FORMAT(' *** FOR DO SIMULATION (NWQ = 1), TO GET ULTIMATE BOD ***'
     1,/,' MULTIPLY INCOMING BOD5 LOADS (IF PRESENT) BY:',8X,F6.2)
 9240 FORMAT(/,' #### ERROR. MUST HAVE F3 LINE FOLLOWING F1 LINES WHEN S
     1IMULATING LINKED DO (NWQ = 1).',/,
	2' RUN STOPPED FROM SUBROUTINE INTRQUAL.')
 9250 FORMAT(/,
     1' *****************************************************',/,
     2' * INPUT PARAMETERS FOR LINKED DO-BOD-NOD SIMULATION *',/,
	3' *****************************************************')
 9255 FORMAT(/,' KWQPAR = 0: CONSTANT TEMP, WIND, SOD FROM F2 LINE.')
 9256 FORMAT(/,' KWQPAR = 1: TEMP, WIND, SOD VARIABLE BY CHANNEL FROM F4
     1 LINES.')
 9257 FORMAT(/,' KWQPAR = 2: TEMP, WIND, SOD VARIABLE BY CHANNEL AND IN 
     1TIME. ENTER USING NSCRAT(9) FILE.')
 9260 FORMAT(/,
     1' GLOBAL AND DEFAULT PARAMETERS:',/,
	1' ------------------------------',/,
     2' TEMPERATURE COEF. FOR CBOD............',F6.4,/,
	3' TEMPERATURE COEF. FOR NOD.............',F6.4,/,
	4' TEMPERATURE COEF. FOR DO..............',F6.4,/,
     5' DISSOLVED FRACTION OF CBOD............',F6.4,/,
	6' DISSOLVED FRACTION OF NOD.............',F6.4,/,
	7' PERCENT DO SATURATION IN INFLOWS......',F6.2,/,
     8' SALINITY IN CHANNELS, PPT.............',F6.2)
 9261 FORMAT(
     1' CBOD SETTLING VELOCITY...............',F7.4,' FT/DAY',/,
	2'  NOD SETTLING VELOCITY...............',F7.4,' FT/DAY',/,
     3' DEFAULT AIR TEMPERATURE...............',F6.2,' deg F',/,
	4' DEFAULT WATER TEMPERATURE.............',F6.2,' deg F',/,
	5' DEFAULT WIND SPEED....................',F6.2,' FT/S',/,
	6' DEFAULT SEDIMENT OXYGEN DEMAND........',F6.3,' G/FT^2-DAY')
 9262 FORMAT(
     1' CBOD SETTLING VELOCITY...............',F7.4,' M/DAY',/,
	2'  NOD SETTLING VELOCITY...............',F7.4,' M/DAY',/,
     3' DEFAULT AIR TEMPERATURE...............',F6.2,' deg C',/,
	4' DEFAULT WATER TEMPERATURE.............',F6.2,' deg C',/,
	5' DEFAULT WIND SPEED....................',F6.2,' M/S',/,
	6' DEFAULT SEDIMENT OXYGEN DEMAND........',F6.3,' G/M^2-DAY')
 9263 FORMAT(/,
     1' SAT. DO AT DEFAULT TEMP AND SALINITY..',F6.2,' MG/L')
 9269 FORMAT(/,
     1' *********************************************',/,
     2' * INPUT PARAMETERS FOR REAERATION (LINE F4) *',/,
	3' *********************************************')
 9270 FORMAT(/,' KWIND = 0: INCLUDE WIND-DRIVEN REAERATION, USING OCONNE
     1R (1983).')
 9271 FORMAT(/,' KWIND = 1: DO NOT INCLUDE WIND-DRIVEN REAERATION.')
 9280 FORMAT(' KOVAR = 0: USE COVAR METHOD FOR FLOW-DRIVEN REAERATION.')
 9281 FORMAT(' KOVAR = 1: INPUT OWN REAERATION COEFS FOR FLOW-DRIVEN REA
     1ERATION.')
 9291 FORMAT(' FOR Ka (= K2) IN 1/DAY, VELOCITY IN FT/S, AND DEPTH IN FT
     1:',/,' EQUATION CONSTANT = ',F7.3,/,
	2' VELOCITY EXPONENT = ',F7.3,/,
	3' DEPTH EXPONENT =    ',F7.3)
 9292 FORMAT(' FOR Ka (= K2) IN 1/DAY, VELOCITY IN M/S, AND DEPTH IN M:'
     1,/,' EQUATION CONSTANT = ',F7.3,/,
	2' VELOCITY EXPONENT = ',F7.3,/,
	3' DEPTH EXPONENT =    ',F7.3)
 9295 FORMAT(/,' PARAMETERS FOR USE IN O''CONNOR (1983) WIND-DRIVEN REAE
     1RATION EQUATIONS.',/,' SEE HIS PAPER FOR EXPLANATION.',//,
     2' XLAMBDA = ',F7.2,' Inverse roughness Reynolds No.',/,
	3' GAMMAZ  = ',F7.2,' Boundary layer Reynolds No.',/,
	4' UTSTAR  = ',F7.2,' cm/s. Ref. shear velocity in transition zone.
     5',/,' UCSTAR  = ',F7.2,' cm/s. Critical shear velocity for smooth 
	6- transition zone boundary.',/,
	7' PRESURE = ',F7.1,' mb. Atmospheric presure.',/,
	8' ZEE     = ',F7.3,' cm. Surface roughness in rough regime.',/,
	9' ZEE     = ',F7.1,' cm. Elevation of wind measurement.')
 9300 FORMAT(/,
     1' ********************************************************',/,
     2' * VARIABLE CHANNEL PARAMETERS FOR LINKED DO (F6 LINES) *',/,
	3' ********************************************************',//,
     4'    ELEMENT  INTERNAL  TYPE    CBOD     NOD    AIR  WATER   WIND
     5       SOD',/,
	6'     NUMBER    NUMBER         DECAY   DECAY   TEMP   TEMP  SPEED'
     7)
 9301 FORMAT(
     1'                             (1/DAY) (1/DAY) (degF) (degF) (FT/S)
     2 (G/FT^2-DAY)')
 9302 FORMAT(
     1'                             (1/DAY) (1/DAY) (degC) (degC)  (M/S)
     2  (G/M^2-DAY)')
 9303 FORMAT(
     1' ----------  --------  ----  ------- ------  ------ ------ ------
	2 ------------')
 9305 FORMAT(/,' $$$$ WARNING! NO MATCH FOR ELEMENT ',I10,', ROW '
     1,I4,', OF F6 LINES.  WILL USE DEFAULT CHANNEL PARAMETERS.')
 9306 FORMAT(/,' $$$$ WARNING! NO MATCH FOR ELEMENT ',A10,', ROW '
     1,I4,', OF F6 LINES.  WILL USE DEFAULT CHANNEL PARAMETERS.')
 9310 FORMAT(1X,I10,I10,I6,2F8.3,3F7.2,F10.4)
 9311 FORMAT(1X,A10,I10,I6,2F8.3,3F7.2,F10.4)
 9316 FORMAT(/,' $$$$ WARNING. ELEMENT NO. ',I10, ', ROW ',I3,' OF F6 LI
     1NES MUST BE CHANNEL OR HAVE STORAGE.',/,
	2' BUT NTYPE =',I3,'. INPUT DATA FROM THIS LINE IGNORED.')
 9317 FORMAT(/,' $$$$ WARNING. ELEMENT NO. ',A10, ', ROW ',I3,' OF F6 LI
     1NES MUST BE CHANNEL OR HAVE STORAGE.',/,
	2' BUT NTYPE =',I3,'. INPUT DATA FROM THIS LINE IGNORED.')
 9320 FORMAT(/,' #### ERROR! DATA REQUESTED ON F6 LINES, BUT NO F6 LINES
     1 PRESENT. RUN CONTINUES WITH DEFAULT CHANNEL PARAMETERS.')
	END