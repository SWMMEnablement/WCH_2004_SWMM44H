      PROGRAM TESTK2
C=======================================================================
C     Test reaeration routines.
C     Wayne Huber, July 6, 2001. 
C     Reason: O'Connor routines are complex.  Let user see effect 
C      of parameters, if desired. 
C     Do for different combinations of WIND, XLAMBDA, air and water 
C      temperatures.
C=======================================================================
      DIMENSION REA(3)
	CHARACTER IRUN*1,IKEY*1,FILENAME*20,CC*2,CCARD*2,TITLE*80,
     1 MMETRIC*1
C=======================================================================
	N5     = 5
      N6     = 6
	N55    = 55
	METRIC = 1
      CAPPA  = 0.40     ! Von Karman's kappa. 
	CAPPA3 = 0.73681  ! Cube root of CAPPA
C=======================================================================
C     Determine whether user will input own data, in text file.
C=======================================================================
      WRITE(*,9000)
	WRITE(*,9075)
	WRITE(*,9002)
	READ *, IRUN
	IF(IRUN.EQ.'N'.OR.IRUN.EQ.'n') THEN
	     WRITE(*,9005) 
	     READ *, IKEY
	     IF(IKEY.EQ.'F'.OR.IKEY.EQ.'f') THEN
C=======================================================================
C     Here, input name of input file.
C=======================================================================
                WRITE(*,9010)
   20	          READ *, FILENAME
C=======================================================================
C     Use Subroutine STRIP, from main SWMM, to strip comments and
C     prepare text file.
   	          OPEN(UNIT=N55,FILE=FILENAME,STATUS='OLD',ERR=25)
	          GOTO 30
C=======================================================================
C     Assume error is when input file does not exist. 
C=======================================================================
   25                WRITE(*,9400) FILENAME
                     GOTO 20
C=======================================================================
   30           OPEN(UNIT=N5,STATUS='SCRATCH',CARRIAGECONTROL='FORTRAN')
	          WRITE(*,9015)
	          REWIND N55
	          CALL STRIPX(N5,N6,N55)
C=======================================================================
C     Read a title. >>>>>>>>>>>>> A1 line.
C=======================================================================
	          CCARD = 'A1'
		      READ(N5,*,ERR=3000,END=4000,IOSTAT=IOOSTAT) CC,TITLE
	          IF(CC.NE.CCARD) GOTO 2000
C=======================================================================
C	>>>>>>>>>> B1 line <<<<<<<<<<<<<<
C=======================================================================
                CCARD = 'B1'
              READ(N5,*,ERR=3000,END=4000,IOSTAT=IOOSTAT) CC,KOVAR,KWIND
	          IF(CC.NE.CCARD) GOTO 2000
			  IF(KWIND.EQ.0) THEN
	               BACKSPACE N5
	               READ(N5,*,ERR=3000,END=4000,IOSTAT=IOOSTAT) CC,
     1                 KOVAR,KWIND,XLAMBDA,GAMMAZ,PRESURE,SALINITY,
     2                 ZEE,ZAA,UTSTAR,UCSTAR,THETA3,TAIR,TWATER,WINDU
	               ZAA = ZAA*100.0
	               ENDIF
C=======================================================================
C     >>>>>>>>> C1 line <<<<<<<<<<<<<
C=======================================================================
                IF(KOVAR.LT.2) THEN
	               CCARD = 'C1'
	               READ(N5,*,ERR=3000,END=4000,IOSTAT=IOOSTAT) CC,
     1                   METRIC,DEPTHU,VELLU
	               IF(CC.NE.CCARD) GOTO 2000
	               IF(KOVAR.EQ.1) THEN
	                   BACKSPACE N5
	                   READ(N5,*,ERR=3000,END=4000,IOSTAT=IOOSTAT) 
     1                     CC,METRIC,DEPTHU,VELLU,REA
                         ENDIF
	               ENDIF
C=======================================================================
C     Here, enter values from keyboard.  Tedious. 
C=======================================================================
                ELSE
	          WRITE(*,9020)
	          READ *, TITLE
	          IER1 = 0
   50           WRITE(*,9022)
	          READ *, KOVAR
	          WRITE(*,9024)
			  READ *, KWIND
	          IF(KOVAR.EQ.2.AND.KWIND.EQ.1.AND.IER1.EQ.0) THEN
	              IF(IER1.EQ.0) WRITE(*) ' ERROR. Cannot have KOVAR=2 
     1and KWIND = 1. Please change KOVAR or KWIND.'
	              IER1 = 1
				  GOTO 50
	              ELSEIF(KOVAR.EQ.2.AND.KWIND.EQ.1) THEN
                         WRITE(*) ' ERROR. CANNOT HAVE KOVAR=2 AND KWIND
     1 = 1.'
	                   STOP ' SORRY, RUN STOPPED.'
	              ENDIF
C=======================================================================
C     Enter values for flow-reaeration.
C=======================================================================
	              IF(KOVAR.LE.1) THEN
	                   WRITE(*,9026)
	                   READ *, MMETRIC
	                   IF(MMETRIC.EQ.'M'.OR.MMETRIC.EQ.'m') METRIC = 2
	                   WRITE(*,9028)
				       READ *, DEPTHU
	                   WRITE(*,9030)
	                   READ *, VELLU
	                   IF(KOVAR.EQ.1) THEN
	                        WRITE(*,9032)
	                        READ *, REA(1)
	                        WRITE(*,9034)
	                        READ *, REA(2)
	                        WRITE(*,9036)
	                        READ *, REA(3)
	                        ENDIF
	                   ENDIF
C=======================================================================
C     Enter values for wind-reaeration.
C=======================================================================
	              IF(KWIND.EQ.0) THEN
                         WRITE(*,9038)
	                   READ *, XLAMBDA
	                   WRITE(*,9040) 
					   READ *, GAMMAZ
	                   WRITE(*,9042) 
					   READ *, PRESURE
	                   WRITE(*,9044)
	                   READ *, SALINITY
	                   WRITE(*,9046) 
	                   READ *, ZEE
	                   WRITE(*,9048) 
					   READ *, ZAA
					   ZAA = ZAA*100. 
	                   WRITE(*,9050)
					   READ *, UTSTAR
	                   WRITE(*,9052) 
	                   READ *, UCSTAR
	                   WRITE(*,9054)
	                   READ *, THETA3
	                   WRITE(*,9056) 
					   READ *, TAIR
	                   WRITE(*,9058)
	                   READ *, TWATER
	                   WRITE(*,9060)
	                   READ *, WINDU
	                   ENDIF
	          ENDIF
C=======================================================================
C     End of keyboard input!
C=======================================================================
C     Here, use default values for test. 
C=======================================================================
           ELSE
	     WRITE(*,9070)
	     TITLE = ' TEST OF REAERATION EQUATIONS WITH DEFAULT VALUES'
C=======================================================================
C     Test with default values.
C=======================================================================
           KOVAR    = 0
	     KWIND    = 0
	     XLAMBDA  = 3.0
	     GAMMAZ   = 6.5
	     PRESURE  = 1000.  ! mb
	     SALINITY = 0.0    ! ppt
	     ZEE      = 0.25   ! cm
	     ZAA      = 1000.  ! cm
	     UTSTAR   = 10.0
	     UCSTAR   = 11.0
	     THETA3   = 1.024
	     TAIR     = 20.  ! degC
	     TWATER   = 20.  ! degC
	     REA(1)   = 1.0
	     REA(2)   = 1.0
	     REA(3)   = 1.0
	     DEPTHU   = 10.  ! ft
	     VELLU    = 10.  ! ft/s
	     WINDU    = 40.  ! m/s
	     ENDIF
C=======================================================================
	OPEN(UNIT=N6,FILE='TESTK2.OUT',STATUS='UNKNOWN')
	WRITE(N6,9000)
	WRITE(N6,9080) TITLE
	WRITE(N6,9085) KOVAR,KWIND
	IF(KOVAR.LE.1.AND.METRIC.EQ.1) WRITE(N6,9090) DEPTHU,VELLU
	IF(KOVAR.LE.1.AND.METRIC.EQ.2) WRITE(N6,9092) DEPTHU,VELLU
	IF(KOVAR.EQ.1.AND.METRIC.EQ.1) WRITE(N6,9094)
	IF(KOVAR.EQ.1.AND.METRIC.EQ.2) WRITE(N6,9096)
	IF(KOVAR.EQ.1) WRITE(N6,9098) REA
      IF(KWIND.EQ.0) WRITE(N6,9100) CAPPA,XLAMBDA,GAMMAZ,PRESURE,
     1 SALINITY,ZEE,ZAA,UTSTAR,UCSTAR,THETA3,TAIR,TWATER,WINDU
	IF(KOVAR.LE.1) THEN
C=======================================================================
C     Test flow-driven reaeration values.  Units of depth and velocity
C      are ft and ft/s internally in program.
C=======================================================================
           CONV = 1.0
	     IF(METRIC.EQ.2) CONV = 3.2808
		 DEPTHU = DEPTHU/CONV
	     VELLU  = VELLU/CONV 
	     IF(METRIC.EQ.1) WRITE(N6,9105)
	     IF(METRIC.EQ.2) WRITE(N6,9106)
	     WRITE(N6,9108)
	     DDEPTH = DEPTHU/5.0
	     DVELLU = VELLU/5.0
	     DO I = 1,5
	          DEPTH = FLOAT(I)*DDEPTH
	          DO J = 1,5
	              VELLU = FLOAT(J)*DVELLU
      CALL REAERATEX(DECAY2,DEPTH,VELLU,WIND,TAIR,TWATER,REA,
     1 THETA3,KWIND,KOVAR,GNUW,GNUA,RHOW,RHOA,DMOLEC,XLAMBDA,ZEE,CAPPA,
     2 GAMMAZ,UTSTAR,PRESURE,UCSTAR,CAPPA3,CDRAG,II,DELG1,ZZERO,USTAR,
     3 CAYEL,DECAY2W,SALINITY,G1,ZAA,N6,DECAY2F)
	              WRITE(N6,9110) DEPTH*CONV,VELLU*CONV,DECAY2F*84600.
                    ENDDO
	          ENDDO
	     ENDIF
C=======================================================================
C     Test wind-driven reaeration values.  Metric units are chosen to
C      match O'Connor paper. Wind must be in ft/s to feed to 
C      subroutine. Wind is converted to cm/s therein.  Likewise, depth
C      needs to be in feet. 
C=======================================================================
      IF(KWIND.EQ.0) THEN
	     CONV = 3.2808
	     IF(METRIC.EQ.2) CONV = 1.0
	     DWIND = WINDU/40.0*CONV
	     DO I = 1,40
	           WIND = FLOAT(I)*DWIND
      CALL REAERATEX(DECAY2,3.2808,VELLU,WIND,TAIR,TWATER,REA,
     1 THETA3,KWIND,KOVAR,GNUW,GNUA,RHOW,RHOA,DMOLEC,XLAMBDA,ZEE,CAPPA,
     2 GAMMAZ,UTSTAR,PRESURE,UCSTAR,CAPPA3,CDRAG,II,DELG1,ZZERO,USTAR,
     3 CAYEL,DECAY2W,SALINITY,G1,ZAA,N6,DECAY2F)
	           IF(I.EQ.1) THEN
	                WRITE(N6,9115) DMOLEC,GNUW,GNUA,RHOW,RHOA
	                WRITE(N6,9117)
	                WRITE(N6,9120)
	                ENDIF        
	           WRITE(N6,9150) WIND/CONV,DECAY2W*86400.,CAYEL*864.,
	1                   CDRAG,II,G1,DELG1,USTAR,ZZERO
	           ENDDO
           ENDIF
	CLOSE (N6)
	PRINT *, ' '
	PRINT *, ' Program ended normally.  Look for output in file TESTK2
     1.OUT.'
	CALL EXIT
C=======================================================================
C     Error routine.
C=======================================================================
 2000 WRITE(*,9500) CCARD,CC
	STOP ' Inspect input data file.'
 3000 WRITE(*,9600) CCARD
      print *,' INPUT ERR LOCATION. CVF ERROR NUMBER = ', IOOSTAT
	STOP ' Inspect input data file.'
 4000 WRITE(*,9700) CCARD
      print *,' END-OF-FILE LOCATION. CVF ERROR NUMBER = ',IOOSTAT
      STOP ' Inspect input data file.'
C=======================================================================
 9000 FORMAT(
     1' ***********************************************************',/,
	2' * Welcome to test of reaeration routines, Program TESTK2. *',/,
	2' *   A component of the EPA Storm Water Management Model.  *',/,
	3' *         Oregon State University, March 2002.            *',/,
	4' ***********************************************************')
 9002 FORMAT(
     1' You may make a default run with variable, wind, air and water'
	2,/,' temperatures, and O''Connor lambda1, or enter your own',/,
	3' test data. Units for wind-driven reaeration data are in',/,
     4' metric, to correspond with O''Connor''s (1983) paper.',/,
     5' Units for flow-driven data are U.S. customary (default), or',/,
     6' optionally metric with user-input.',//,
	5' ==> Do you wish to make a default run (enter Y or N)?')
 9005 FORMAT(/,' You wish to provide your own data.  Do you want to ente
     1r as many as 18 values',/,' from keyboard (K), or instead from a t
     2ext file (F), with values',/,
     3' entered according to program documentation in file TESTK2.DAT?'
     3,//,' ==> Enter K or F.')
 9010 FORMAT(/,' Enter name of input data text file.  Maximum 20 charact
     1ers.',/,' May include path name. May not include blanks.')
 9015 FORMAT(' Run will proceed with data from indicated input file.')
 9020 FORMAT(' ENTER a title, up to 80 characters. ENCLOSE IN SINGLE QUO
     1TES!')
 9022 FORMAT(' Enter parameter KOVAR:',/,
     1' Enter 0 to test flow-reaeration using Covar (1983) reaeration eq
     2uations, or',/,
	3' Enter 1 to test flow-reaeration using user-supplied reaeration e
	4quation parameters, or',/,
	5' Enter 2 to skip test of flow-reaeration equations.')
 9024 FORMAT(' Enter parameter KWIND:',/,
     1' Enter 0 to test O''Connor (1983) wind-reaeration equations, or'
     2,/,' Enter 1 to skip test of O''Connor (1983) equations.')
 9026 FORMAT(' Enter "M" for metric or "U" for U.S. customary units for 
     1test of flow-reaeration.')
 9028 FORMAT(' Enter upper bound for 5 flow depths (ft or m).')
 9030 FORMAT(' Enter upper bound for 5 water velocities (ft/s or m/s).')
 9032 FORMAT(' Enter three parameters for reaeration equation of form K2
     1 (or Ka) = REA1*velocity^REA2/depth^REA3.',/,
	2' Units for REA1 must correspond to metric or U.S. customary units
	3 for depth and velocity.',/,
	4' Enter REA1.')
 9034 FORMAT(' Enter REA2.')
 9036 FORMAT(' Enter REA3.')
 9038 FORMAT(' Enter parameters corresponding to O''Connor''s wind-drive 
     1n reaeration equations.',/,
	2' SEE HIS PAPER FOR EXPLANATION: D.J. O''Connor, Wind Effects on 
	3Gas-Liquid Transfer Coefficients,',/,
	4' J. Environmental Engineering, ASCE, Vol. 109, No. 3, June 1983, 
	5pp. 731-752.',/,
	6' NOTE. Only indicated metric units will be used.',/,
	7' Enter XLAMBDA1 (reciprocal roughness Reynolds No.)')
 9040 FORMAT(' Enter GAMMAZ (boundary thickness Reynolds No.')
 9042 FORMAT(' Enter atmospheric pressure, mb.')
 9044 FORMAT(' Enter water salinity, ppt.')
 9046 FORMAT(' Enter assymptotic water surface roughness, for fully roug
     1h conditions, cm.')
 9048 FORMAT(' Enter elevation of wind measurement, m.')
 9050 FORMAT(' Enter transitional shear velocity UTSTAR, used in transit
     1ional equation for KsubL, cm.')
 9052 FORMAT(' Enter transition shear velocity UCSTAR, smooth-transition
     1 zone, cm.')
 9054 FORMAT(' Enter reaeration rate temperature coefficient, THETA3.')
 9056 FORMAT(' Enter air temperature, deg-C.')
 9058 FORMAT(' Enter water temperature, deg-C.')
 9060 FORMAT(' Enter upper bound of wind speed WINDU, m/s.  Values will 
     1be computed for 40 equal increments, 0 to WINDU.')
C
 9070 FORMAT(' You have indicated a run using default variation of param
     1eters.')
 9075 FORMAT(/,' Look for output in text file: TEST2K.OUT in default dir
     2ectory.',/,' Caution! Prior output file will be overwritten. Change
     3e name prior',/,' to this run if you wish to save it.',/,
     4' To abort run, hit <contrl>-C.',/)
 9080 FORMAT(/,1X,A80)
 9085 FORMAT (/,' TEST OF SWMM REAERATION ROUTINE, SUBROUTINE REAERATE.'
     1,/,' For flow-driven reaeration, see: A.P. Covar, "Selecting the P
	1roper',/,' Reaeration Coefficient for Use in Water Quality Models,
	2" Proc. EPA',/,' Conference on Environmental Modeling and Simulati
     2on, W.R. Ott, ed.,',/,' Cincinnati, OH, April 1976, EPA-600/9-76-0
     316, July, 1976.',/,
     4' For wind-driven reaeration, see: D.J. O''Connor, "Wind Effects o
     4n',/,' Gas-Liquid Transfer Coefficients," J. Environmental Enginee
     4ring,',/,' ASCE, Vol. 109, No. 3, June 1983, pp. 731-752.',/,
     5' See also TESTK2 documentation file.',//,
     6' PARAMETER VALUES:',/,
     7' KOVAR   =',I4,/,
     8' KWIND   =',I4)
 9090 FORMAT(/,' FOR FLOW-DRIVEN REAERATION:',/,
     1' UPPER BOUND FOR 5 DEPTHS         ',1X,F7.2' ft',/,
	2' UPPER BOUND FOR 5 VELOCITIES     ',1X,F7.2,' ft/s')
 9092 FORMAT(/,' FOR FLOW-DRIVEN REAERATION:',/,
     1' UPPER BOUND FOR 5 DEPTHS         ',1X,F7.2' m',/,
	2' UPPER BOUND FOR 5 VELOCITIES     ',1X,F7.2,' m/s')
 9094 FORMAT(/,' FOR DEPTH IN ft AND VELOCITY IN ft/s:')
 9096 FORMAT(/,' FOR DEPTH IN m AND VELOCITY IN m/s:')
 9098 FORMAT(' USER REAERATION COEF REA1 ',F8.3,/,
     1       ' USER REAERATION COEF REA2 ',F8.3,/,
     2       ' USER REAERATION COEF REA3 ',F8.3)
 9100 FORMAT(/,' FOR WIND-DRIVEN REAERATION:',/,
     1' KAPPA      =',F7.3,' Von Karman''s KAPPA'/,
	1' XLAMBDA1   =',F7.3,' Inverse roughness Reynolds No. (O''Connor E
	1qn. 12)',/,
     1' GAMMAZ     =',F7.3,' Boundary layer thickness Reynolds No. (O''C
     2onnor Eqn. 14)',/,
	2' PRESURE    =',F7.1,' mb',/,
	2' SALINITY   =',F7.2,' ppt',/,
	3' ZEE        =',F7.3,' cm, Fully rough surface roughness(O''Connor
     3 Eqn. 15a)',/,
	3' ZAA        =',F7.1,' cm, Elevation of wind measurement',/,
	4' UTSTAR     =',F7.2,' cm/s, (O''Connor Eqn. 15a)',/,
	4' UCSTAR     =',F7.2,' cm/s, (O''Connor Eqn. 16)',/,
     5' THETA(3)   =',F7.3,' Temperature coefficient',/,
	5' AIR TEMP   =',F7.2,' deg-C',/,
	6' WATER TEMP =',F7.2,' deg-C',/,
	6' WINDU      =',F7.2,' m/s, = upper bound for wind.')
 9105 FORMAT(/,' OUTPUT FROM FLOW-DRIVEN REAERATION TESTS.',/,
     1' Units of depth and velocity are ft and ft/s.')
 9106 FORMAT(/,' OUTPUT FROM FLOW-DRIVEN REAERATION TESTS.',/,
     1' Units of depth and velocity are m and m/s.')
 9108 FORMAT(' Units of K2 (or Ka) are 1/day.',//,
     3'   DEPTH  VELOCITY      K2',/,
	4' -------  -------- -------')
 9110 FORMAT(F8.3,F10.3,F8.3)

 9115 FORMAT(//,' OUTPUT FROM WIND-DRIVEN REAERATION TESTS.',//,
     1' ON BASIS OF WATER TEMPERATURE, SALINITY, AIR TEMPERATURE AND PRE
     1SSURE:'
	2,/,' DMOLEC = ',F9.7,' cm2/s, molecular diffusivity of oxygen in w
     3ater',/,
     4' GNUW   = ',F9.7,' cm2/s, kinematic viscosity of water'
	5,/,' GNUA   = ',F9.7,' cm2/s, kinematic viscosity of air',/,
	6' RHOW   = ',F6.1,' kg/m3, density of water',/,
	7' RHOA   = ',F6.3,' kg/m3, density of air')
 9117 FORMAT(/,' IN OUTPUT THAT FOLLOWS:',/,
     1' WIND   = wind speed, measured at elevation ZAA, m/s',/,
     1' DECAY2 = K2 or Ka, 1/day, for depth of 1 m = KsubL/1',/,
	2' KsubL  = liquid film coefficient, m/day',/,
	3' CDRAG  = drag coefficient, Cd',/,
	4' II     = number of iterations required to solve for CDRAG',/,
	5' G1     = SQRT(CDRAG)',/,
	6' DELG1  = relative error in G1 at end of iterations',/,
	7' USTAR  = shear velocity in air = G1*WIND, cm/s',/,
	8' ZZERO  = roughness height at water surface, cm')
 9120 FORMAT(//,
     1'   WIND  DECAY2   KsubL   CDRAG II       G1          DELG1   USTA
     2R   ZZERO',/,
	3'    m/s   1/day   m/day                                       cm/
     5s      cm')
 9150 FORMAT(F7.2,2F8.3,F8.5,I3,1X,F8.5,E15.7,F8.3,F8.5)
 9400 FORMAT(/,' ERROR! Input file name ',A20,' not found.',/,
     1' Please enter complete file name (and optional path) again.')
 9500 FORMAT(/,' ERROR! Expecting line ',A2,' but found line ',A2,/,
     1' in input file. Run stopped.')
 9600 FORMAT(/,' ERROR! Error while attempting to read line ',A2)
 9700 FORMAT(/,' ERROR! End of file encountered in input data file while
     1 attempting to read line ',A2)
	END
