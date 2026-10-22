	SUBROUTINE QUALPARM(AS,ASSETL,DBAR,DBARSETL,VELL,CS,WAVG)
C=======================================================================
C     Transport Block routine to compute surface area, average depth, 
C       velocity, and saturated DO concentration for channels involved 
C       in linked DO quality simulation.
C     Call from Sub. QUALSOLN and from INITAL. 
C     Created by Wayne Huber, 7/6/01.
C=======================================================================
      INCLUDE 'TAPES.INC'
	INCLUDE 'HUGO.INC'
	INCLUDE 'TST.INC'
	INCLUDE 'TRANWQ.INC'
C=======================================================================
C     Note, many flow and other Transport parameters are in Common in
C       HUGO.INC, including area and element "M."
C=======================================================================
      DIMENSION WXY(2,2),DXY(2,2),CTCOEF(8),SXX(17),SYY(17)
C=======================================================================
C     Coefficients for calculation of DO saturation concentration.
C=======================================================================
	DATA CTCOEF/-139.34411,1.575701E5,-6.642308E7,1.2438E10,
     +-8.621949E11,-0.017674,10.754,2140.7/
C=======================================================================
C	AS       = surface area for reaeration, ft2
C     ASSETL   = surface area for settling, ft2
C     DBAR     = average depth for reaeration, ft
C     DBARSETL = average depth for settling, ft
C     VELL     = average velocity, ft/s
C     CS       = saturated DO concentration, mg/L
C=======================================================================
C     Average depth is used to compute reaeration and settling.
C     Average depth = cross-sectional area/surface width.
C     Width is computed in Function RADH.
C     Use average over two ends and starting and ending time steps. 
C     Must deal with lift stations (type 20) and storage units 
C     (type 22) separately.
C=======================================================================
      IF(NTYPE(M).EQ.20) GOTO 200
	IF(NTYPE(M).EQ.22) GOTO 300
      SUMXY2 = 0.0
	VELL   = 0.0
      DO 50 I = 1,2
	DO 50 J = 1,2
C     Call RADH only to have width computed.
      RRR      = RADH(A(M,I,J))
      SUMXY2   = SUMXY2 + DEPTH(A(M,I,J)/AFULL(M))
	SUMXY1   = SUMXY1 + WIDTHM
	VELL     = VELL   + VEL(Q(M,I,J),A(M,I,J))
   50	WXY(I,J) = WIDTHV
      VELL     = VELL/4.0
	WIDTHM   = SUMXY1/4.0
C     Function DEPTH() returns normalized depth, not actual depth.
C     Hence, DMAX ranges from zero to 1.0.
	DMAX     = SUMXY2/4.0
	DBAR     = 0.0
	SUMXY1   = 0.0
     	SUMXY2   = 0.0
	DO 60 I = 1,2
	DO 60 J = 1,2
	IF(WXY(I,J).GT.0.0) SUMXY1 = SUMXY1 + A(M,I,J)/WXY(I,J)
   60	SUMXY2 = SUMXY2 + WXY(I,J)
      DBAR = SUMXY1/4.0
	WAVG = SUMXY2/4.0
	AS   = 0.0
	IF(DMAX.GE.0.001) AS = DIST(M)*WAVG
C     For settling, use max width in the event of a closed conduit 
C     and depth is above depth where closing begins. 
      ASSETL = DIST(M)*WIDTHM
	DBARSETL = 0.0
	IF(WIDTHM.GT.0.0) DBARSETL = (A(M,1,1) + A(M,1,2) + 
     1        A(M,2,1) + A(M,2,2))/4.0/WIDTHM
	GOTO 500
C=======================================================================
C     Lift station.  Assume no reaeration or settling. 
C=======================================================================
  200 VELL     = 0.0
	DBAR     = 10000.
	DBARSETL = 10000.
	AS       = 0.0
	ASSETL   = 0.0
	GOTO 500
C=======================================================================
C     Storage unit.  Need to extract area from storage arrays.
C=======================================================================
  300 IS     = KSTORE(M)
      MINT   = MINTS(IS)
C=======================================================================
C     Store current depth and area arrays into SXX and SYY for  
C     linear interpolation.  
C=======================================================================
      DO I = 1,MINT
          SXX(I)   = TSDEP(IS,I)
          SYY(I)   = TSAREA(IS,I)
	    ENDDO
      CALL TINTRP(SXX,SYY,MINT,DEPTHLL(IS),A1)
	CALL TINTRP(SXX,SYY,MINT,DEPTHL(IS),A2)
	AS     = (A1 + A2)/2.0
	ASSETL = AS
	DBAR   = 0.0
	IF(A1.GT.0.0) DBAR = STORL(IS)/A1/2.0
	IF(A2.GT.0.0) DBAR = DBAR + STORE(IS)/A2/2.0
	DBARSETL = DBAR
C=======================================================================
C     Estimate horizontal velocity by horizontal path length / residence
C     time.  Path length taken as ratio of sqrt(area to max. area) times
C     max flow length.
C=======================================================================
      PATH = SQRT(A1/SYY(MINT))*STORLEN(IS)
	RES  = 0.0
	IF(Q(M,2,1).GT.0.0) RES  = STORL(IS)/Q(M,2,1)
	VELL = 0.0
	IF(RES.GT.0.0) VELL = PATH/RES/2.0
	PATH = SQRT(A2/SYY(MINT))*STORLEN(IS)
	RES  = 0.0
	IF(Q(M,2,2).GT.0.0) RES = STORE(IS)/Q(M,2,2)
	IF(RES.GT.0.0) VELL = VELL + PATH/RES/2.0
C=======================================================================
C     Use standard saturation concentration vs. temperature relation
C     for fresh water (no chlorinity).
C     From Standard Methods (APHA) and from Bensen and Krause, 
C       Limnology and Oceanograpy, Vol. 29, No. 3, pp. 620-632, 1984.
C     Values are for sea level.  
C=======================================================================
  500	TKELV = 273.15 + TWATER(M)
      ARG = CTCOEF(1) + CTCOEF(2)/TKELV
	DO I = 2,4
	  ARG = ARG + CTCOEF(I+1)/TKELV**I
	  ENDDO
	IF(SALINITY.GT.0.0) ARG = ARG + SALINITY*(CTCOEF(6) +
     1  CTCOEF(7)/TKELV + CTCOEF(8)/TKELV**2)
	CS = EXP(ARG)
	RETURN
	END





