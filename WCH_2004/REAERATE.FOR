      SUBROUTINE REAERATE(DECAY2,DBAR,VELL,WIND,TAIR,TWATER,REA,
     1 THETA3,KWIND,KOVAR,GNUW,GNUA,RHOW,RHOA,DMOLEC,XLAMBDA,ZEE,CAPPA,
     2 GAMMAZ,UTSTAR,PRESURE,UCSTAR,CAPPA3,CDRAG,I,DELG1,ZZERO,USTAR,
     3 CAYEL,DECAY2W,SALINITY,G1,ZAA,N6,DECAY2F)
C=======================================================================
C     Called from Sub. QUALSOLN and from a separate MAIN-program,
C       just for testing. 
C     Created July 2001 by Wayne Huber in order to compute
C       reaeration coefficient when simulating linked DO.
C=======================================================================
	DIMENSION CDENS(5),REA(3)
C=======================================================================
C     Set values here based on O'Connor (1983) and WASP documentation.
C     CDENS values are for water density as function of temperature 
C      and salinity.
C     CAPPA3 = cube root(CAPPA).
C=======================================================================
C      DATA ZEE/0.25/,CAPPA/0.40/,XLAMBDA/3.0/,GAMMAZ/6.5/,UTSTAR/10.0/,
C     1PRESURE/1000.0/,UCSTAR/11.0/,CAPPA3/0.73681/
      DATA CDENS/28.14,-0.0735,-0.00469,0.802,-0.002/
C=======================================================================
C     Reaeration coefficient = DECAY2 = K2 = Ka, 1/sec.
C     KOVAR = 0, Use Covar (1983) combination method for reaeration
C             coefficient: Owens et al. for DBAR <= 2.0 ft, and
C             combination of Churchill et al. and O'Connor and Dobbins
C             for DBAR > 2 ft.
C           = 1, Use user-supplied coefficients.
C     KWIND = 0, Use O'Connor (1983) wind-driven reaeration.  
C           = 1, Do not include wind-driven reaeration.
C     If wind-driven reaeration is included, K2 = max(flow-driven, 
C     wind-driven).  
C     DBAR  = Average depth, ft.
C     VELL  = Average velocity, ft/s.
C     TWATER= Water temperature, deg C. 
C=======================================================================
	DECAY2  = 0.0
	DECAY2F = 0.0
	DECAY2W = 0.0
	IF(DBAR.LE.0.0) RETURN
	IF(KOVAR.EQ.2) GOTO 500
      IF(KOVAR.EQ.0) THEN
	     IF(DBAR.LE.2.0) THEN
C=======================================================================
C     Here for Owens, Edwards, Gibbs (1964)
C=======================================================================
              DECAY2 = 21.7*VELL**0.67/DBAR**1.85
	        DECAY2 = DECAY2*THETA3**(TWATER-20.0)/86400.0
	        GOTO 500
	        ELSE
C=======================================================================
C     Dividing line between O'C&D and CEB, from Covar Fig. 2
C=======================================================================
	        IF(DBAR.GE.1.085*VELL**2.727) THEN
C=======================================================================
C     Here for O'Connor and Dobbins (1958)
C     DMOLEC = molecular diffusivity, a function of temperature,ft2/s.
C     Do not use other temperature correction. 
C=======================================================================
                 DMOLEC = 2.1926E-8*1.037**(TWATER-20.0)
                 DECAY2 = SQRT(DMOLEC*VELL)/DBAR**1.5/86400.
	           GOTO 500
                 ELSE
C=======================================================================
C     Here for Churchill, Elmore and Buckingham (1962)
C=======================================================================
                 DECAY2 = 11.57*VELL**0.969/DBAR**1.673
	           DECAY2 = DECAY2 *THETA3**(TWATER-20.0)/86400.0
                 GOTO 500
	           ENDIF
	         ENDIF
           ELSE
C=======================================================================
C     Here for user-supplied reaeration coefficient equation.
C=======================================================================
           DECAY2 = REA(1)*VELL**REA(2)/DBAR**REA(3)
	     DECAY2 = DECAY2*THETA3**(TWATER-20.0)/86400.
	     ENDIF
C=======================================================================
  500 DECAY2F = DECAY2
      IF(KWIND.EQ.1) RETURN
C=======================================================================
C     Here, program D.J. O'Connor, J. Env. Eng., June 1983, pp. 731-752.
C     There are several physical constants included in the messy eqns.
C     Kinematic viscosity of air (cm2/s) = 0.15929*1.0064642^(T-20),
C      from Table 113, p. 395, R.J.List, Smithsonian Meteorological
C      Tables, 1966.  Regression fit to data.
C     Kinematic viscosity of water (cm2/s)=0.01003*0.97498^(T-20).
C      Regression fit to data. 
C     Based on Table C.1, p. 650, in Bedient and Huber, 2nd edition.
C     Air density: Use gas law: rho = p/RT, where p = pressure in mb,
C      T = absolute virtual temp, but just assume dry air here, degK,
C      and R = gas constant = 2.8704 mb-degK-m3/kg.  rho in kg/m3
C     Water density: Use Eqn. 3.4, P. 102, of Thomann and Mueller (1987), 
C      For rho (g/cm3) as function of T and salinity.  See coefficients 
C      in equation and DATA statement.
C     Molecular diffusity of O2 in water: see O'Connor&Dobbins, above.
C     GNUW(Tw) = kinematic viscosity of water, cm2/s
C     GNUA(Ta) = kinematic viscosity of air, cm2/s.
C     RHOW(Tw) = density of water, kg/m3.
C     RHOA(Ta) = density of air, kg/m3.
C     DMOLEC(Tw) = molecular diffusivity of O2 in water, cm2/s.
C     CAPPA = Von Karman's constant = 0.40.
C     ZEE = equilibrium roughness for high wind, cm.
C     Set ZEE = 0.25 cm for intermediate scale (rivers) application.
C     XLAMBDA = O'Connor inverse roughness Reynolds number =
C               GNU/(U* Zo)
C     Set XLAMBDA = 3 for intermediate scale application.
C     GAMMAZ  = O'Connor boundary layer Reynoles No. = Delta U*/GNU
C     Set GAMMAZ = 6.5 for intermediate scale application.
C     UCSTAR = critical shear velocity, cm/s. Set = 11 cm/s for
C              intermediate application.
C     UTSTAR = transition shear velocity for roughness. Set = 10 cm/s.
C     PRESURE = atmospheric pressure, mb.  Assume 1000 mb.
C     CDRAG = drag coefficient in air, for wind.  Must solve iteratively
C             using combination of O'Connor eqns. 6b, 11 and 15a.
C=======================================================================
      CAYEL = 0.0
	IF(WIND.LE.0.0) RETURN
C=======================================================================
C     Solve for physical constants first.
C=======================================================================
      GNUW = 0.01003*0.97498**(TWATER-20.0)
      GNUA = 0.15929*1.0064642**(TAIR-20.0)
C     Multiply by 1000 to get units of kg/m3.
      RHOW = 1000.0*(1.0+0.001*((CDENS(1)+CDENS(2)*TWATER+
     1       CDENS(3)*TWATER**2) + (CDENS(4) + CDENS(5)*TWATER)*
	2       (SALINITY - 35.0)))
	RHOA   = PRESURE/2.8704/(TAIR+273.16)
	DMOLEC = 2.037E-5*1.037**(TWATER-20.0)
C=======================================================================
C     Have messy, iterative solution for drag coefficient for all cases.
C     For Newton-Raphson, TOP = function, DENOM = derivative w.r.t. G1.
C     Convert wind from ft/s to cm/s.
C     B1 = combination of coefficients.
C     G1 = sqrt(CDRAG)
C     Set starting guesses on basis of O'Connor Fig. 2.
C     Units of wind and shear velocity are cm/s. 
C     Units of roughness are cm. 
C     Wind supposedly measured at 10 m = 1000 cm = ZAA. Can be reset
C      by user. 
C=======================================================================
      WINDY = WIND*30.48
      B1 = ZAA*WINDY*XLAMBDA/GNUA
C=======================================================================
C     Have good polynomial fit for Cd for WIND <= 15 m/s for T = 20 C.
C     For first guess, use assymptotic value for WIND > 20 m/s.
C     Use polynomial fit to exact (numerical) estimate, for WIND < 15.
C     Fit is for XLAMBDA = 3, ZEE = 0.25 cm , and U*t = 10 cm/s, 
C      and T = 20 C. Solution is sensitive to air temp (and thus GNUA). 
C     Could program other typical values from O'Connor, but instead,
C      try to make solution robust.
C=======================================================================
C     Newton-Raphson works OK for higher winds, e.g., >= about 10 m/s, 
C      but can get hung up on poor first guess in range of minimum Cd.
C     Use bisection if Newton-Raphson fails. 
C=======================================================================
      IBYE    = 0
  550 CONTINUE
      IF(IBYE.EQ.0) THEN
	     IF(WINDY.GE.1950.0) THEN
	        G1 = CAPPA/LOG(ZAA/ZEE)
	        CDRAG1 = G1**2
              ELSEIF(WINDY.GE.100.0) THEN
			   WY     = WINDY/100.
	     CDRAG1 = 0.001432866 - 0.0001023954*WY + 0.000018627307*WY**2 
     1              - 5.675395E-07*WY**3
	           G1     = SQRT(CDRAG1)
	        ELSE  ! These statements never used.  Leave here for archive. 
			WY     = WINDY/100.
	     CDRAG1 = 0.00230649 - 0.006362736*WY + 0.01336092*WY**2 
     1              - 0.007948304*WY**3
	           G1     = SQRT(CDRAG1)
	        ENDIF
C=======================================================================
  	     I = 0
  600	     I = I + 1
  	     EXXP = EXP(-G1*WINDY/UTSTAR)
	     ARG = ZAA/ZEE+B1*G1*EXXP
	     TOP = 1.0/G1 - 1.0/CAPPA*LOG(ARG)
	     TOP2 = 1.0/CAPPA*B1*EXXP*(1.0 - G1*WINDY/UTSTAR)
	     DENOM = -1.0/G1**2 - TOP2/ARG
	     DELG1 = TOP/DENOM
	     IF(ABS(DELG1/G1).LT.0.0001) THEN
              G1 = G1 - DELG1
	        delg1 = abs(delg1/g1)
		    GOTO 750
	        ENDIF
	     IF(-DELG1.GT.G1) THEN
	        DELG1 = -G1
	        GOTO 650
	        ENDIF
	     IF(DELG1.GT.G1) DELG1 = 0.8*G1
  650	     G1 = G1 - DELG1
	     IF(G1.LE.0.0) G1 = 0.025
	     IF(G1.GE.1.0) G1 = 0.30 
           IF(I.LE.10) GOTO 600
C=======================================================================
C     For no convergence, try bisection routine.
C=======================================================================
           IBYE  = 1
	     GOTO 550
  750      CDRAG = G1**2
           ELSE
C=======================================================================
C     Here, use bisection as fallback, when IBYE = 1.
C=======================================================================
           I     = 1
	     IER1  = 0
           G1MAX = CAPPA/LOG(ZAA/ZEE)
		 G1MIN = SQRT(0.0005)
		 FMAX  =  1.0/G1MAX - 1.0/CAPPA*LOG(ZAA/ZEE+B1*G1MAX*
     1               EXP(-G1MAX*WINDY/UTSTAR))
  780		 FMIN  =  1.0/G1MIN - 1.0/CAPPA*LOG(ZAA/ZEE+B1*G1MIN*
     1               EXP(-G1MIN*WINDY/UTSTAR))
	     IF(SIGN(1.0,FMAX).EQ.SIGN(1.0,FMIN)) THEN
C     TROUBLE
             IF(IER1.EQ.0) THEN
		      WRITE(N6,9500)  I,WINDY,G1MAX,G1MIN,FMAX,FMIN
                G1MIN = 0.0
	          IER1  = 1
	          GOTO 780
	          ELSE
	          G1 = SQRT(0.0012)
	          GOTO 900
	          ENDIF
	       ENDIF
  	     G1    = (G1MIN + G1MAX)/2.0
  800      G1OLD = G1
	     FF = 1.0/G1 - 1.0/CAPPA*LOG(ZAA/ZEE+B1*G1*
     1               EXP(-G1*WINDY/UTSTAR))
	     IF(SIGN(1.0,FF).EQ.SIGN(1.0,FMAX)) THEN
	        FMAX  = FF
	        G1MAX = G1
              ELSE
	        FMIN  = FF
	        G1MIN = G1
	        ENDIF
	     G1 = (G1MIN + G1MAX)/2.0
	     delg1 = abs((g1 - g1old)/g1)
           I  = I + 1
	     IF(ABS((G1-G1OLD)/G1).LE.0.001) GOTO 900
	     IF(I.LE.15) GOTO 800
C=======================================================================
C     Harder to define default.  Just use CDRAG = 0.0012
C=======================================================================
	     G1    = SQRT(0.0012)
  900      CDRAG = G1**2
           ENDIF
C=======================================================================
C     Define transitions as follows:
C     Knowing Cd, compute Zo (O'Connor Eqn. 11) and U* (Eqn. 6b).
C     Set lower transition (smooth to transitional) with roughness 
C      Reynolds number = 1.  Then U*c = 1 * nu/Zo.  (nu = kinematic
C      viscosity of air = GNUA.)  But use fixed value given by
C      O'Connor, e.g., UCSTAR = 10 cm/s, typically.
C     Actual U* = USTAR = sqrt(Cd)*WIND.  If Actual < critical, use 
C      smooth (O'Connor Eqn. 23b).
C     Set upper bound of transitional region at Zo > 0.95 Ze.
C     In transitional region, use O'Connor Eqn. 26a.  In rough region,
C      use O'Connor Eqn. 25b.  
C     CAYEL = K-subL = liquid film transfer coef., cm/s. 
C=======================================================================
      ZZERO  = ZAA*EXP(-CAPPA/G1)
	USTAR  = G1*WINDY
	IF(USTAR.LE.UCSTAR) THEN
C=======================================================================
C     Here for smooth region.  Use O'Connor Eqn. 23b.
C=======================================================================
           CAYEL = (DMOLEC/GNUW)**0.6666667*SQRT(CDRAG*RHOA/RHOW)*
     1              CAPPA3/GAMMAZ*WINDY
	     ELSEIF(ZZERO.LE.0.95*ZEE) THEN
C=======================================================================
C     Here for transitional region.  Use O'Connor Eqn. 26a.
C=======================================================================
           GAMMAU = GAMMAZ*USTAR/UCSTAR*EXP(-USTAR/UCSTAR+1)
		 TERM1  = (DMOLEC/GNUW)**0.6666667*CAPPA3/GAMMAU*
     1               SQRT(RHOA/RHOW)*USTAR
           TERM2  = SQRT(DMOLEC/CAPPA*USTAR/ZZERO*RHOA/RHOW*GNUA/GNUW)
		 CAYEL  = 1.0/(1.0/TERM1 + 1.0/TERM2)   
	     ELSEIF(ZZERO.GT.0.95*ZEE) THEN
C=======================================================================
C     Here for rough region.  Use O'Connor Eqn. 25b.
C=======================================================================
           CAYEL = SQRT(DMOLEC/CAPPA/ZEE*RHOA/RHOW*GNUA/GNUW*G1*WINDY)
           ENDIF
C=======================================================================
C     K2 = Ka = KsubL/avg depth = CAYEL/DBAR = DECAY2W
C	Convert cm/s to ft/s.
C	Use max of K2-flow, K2-wind.
C=======================================================================
      DECAY2W = CAYEL/DBAR/30.48
	DECAY2  = AMAX1(DECAY2,DECAY2W)
	RETURN
C=======================================================================
 9500 FORMAT(' ERROR. HUNG UP IN SUB. REAERATE BISECTION. Set lower boun
     1d Cd = 0.',/,' NEXT ERROR, JUST USE DEFAULT Cd = 0.012.',/,
     2' I,WINDY,G1MAX,G1MIN,FMAX,FMIN: ',I3,5E15.7)
	END
