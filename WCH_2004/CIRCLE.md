```fortran 
      SUBROUTINE CIRCLE(ALPHA,PS,DN,ID)
C     TRANSPORT BLOCK
C     CALLED BY FINDA NEAR LINE 41
C=======================================================================
C     ROUTINE TO COMPUTE HYDRAULIC ELEMENTS OF CIRCULAR PIPE.
C                USES NEWTON-RAPHSON ITERATION TO FIND THETA.
# Extensive Summary of the CIRCLE Subroutine

This Fortran subroutine, CIRCLE, computes the hydraulic elements of a circular pipe by determining the subtended angle, THETA, using a Newton-Raphson iteration. It is designed to work with normalized parameters and supports three modes of operation determined by the input variable ID:

## Modes of Operation

1. **ID = 1 or 2:**  
      - Input: ALPHA is given (where ALPHA = A/AFULL, the ratio of the current area to full area).  
      - Process:  
        - **First Step:** Compute an initial guess for THETA using a parabolic approximation that is valid only for THETA < 1.3 radians (corresponding to ALPHA < 0.04 or PSI < 0.015).  
        - **Newton-Raphson Iteration:** Apply iterative refinement to solve the equation relating THETA, ALPHA, and the circular geometry. If the iteration fails to converge within a certain tolerance, the subroutine reverts to the first guess and prints a convergence warning.
        - **Outcome:**  
             - For ID = 1, after obtaining THETA, calculate PSI (non-dimensional flow parameter).  
             - For ID = 2, calculate DN (normalized depth) instead.

2. **ID = 3:**  
      - Input: PSI is given (where PSI = Q/QFULL, a flow ratio).  
      - Process:  
        - **Initial Guess:** Estimate THETA with a parabolic approximation based on PSI.  
        - **Newton-Raphson Iteration:** Adjust THETA iteratively until convergence is reached. If unsuccessful, the subroutine resorts to the initial guess and records a warning.
        - **Outcome:** Once THETA is determined, compute ALPHA as the ratio of the computed section’s area to the full area.

## Handling Special Cases

- **Out of Range Checks:**  
  - For ALPHA values outside the (0,1) interval, the subroutine assigns boundary values for PSI and DN.
  - For PSI values outside the (0,1) interval, ALPHA is similarly set to boundary conditions.

- **Series Expansions for Small Values:**  
  - When ALPHA or PSI are very small, a series expansion is used. This simplification leverages expansions for sine and cosine for small angles.
  
- **Convergence Warning:**  
  - If the Newton-Raphson iteration does not meet the tolerance criteria (ABS(D) ≤ 0.0001), the subroutine prints a warning message indicating the lack of convergence and continues with the first guess.

## Additional Details

- **External References:**  
  - The file includes an external include file (TAPES.INC) which may contain common definitions or routines required by the subroutine.
  
- **Modification History:**  
  - Comments in the code note updates and modifications by various authors (notably W.C.H. and A.H. Elliot) aimed at improving convergence for larger angles or refining the starting guesses.

- **Formatting Note:**  
  - The parabolic starting guess used to compute THETA is valid only when THETA is less than 1.3 radians. This directly corresponds to conditions where ALPHA is less than 0.04 or PSI is less than 0.015.

This summary provides an extensive overview of the structure, purpose, and main computational strategies of the CIRCLE subroutine, offering insights into how it manages different hydraulic scenarios in circular pipes.
C     WEAKER STARTING GUESSES FOR HIGHER VALUES COULD CONCEIVABLY
C                                         CAUSE CONVERGENCE PROBLEMS.
C     UPDATED (NEW COMMON) BY W.C.H., SEPTEMBER 1981.
C     Modified by A.H.Elliot, Nat. Inst. Water Atm. Res, NZ, 29/6/00 to 
C     improve convergence for large theta.  Added to SWMM44gu by 
C     WCH, 8/10/00.
C=======================================================================
      INCLUDE 'TAPES.INC'
C=======================================================================
C     ALPHA = A/AFULL
C     PS    = PSI = Q/QFULL
C     DN    = DEPTH/DIAMETER
C     THETA = SUBTENDED ANGLE
C
C     ID = 1, GIVEN ALPHA, COMPUTE THETA THEN PSI.
C     ID = 2, GIVEN ALPHA, COMPUTE THETA THEN DN.
C     ID = 3, GIVEN PSI,   COMPUTE THETA THEN ALPHA.
C=======================================================================
      IF(ID.LE.2) THEN
         IF(ALPHA.LE.0.0.OR.ALPHA.GE.1.0) GO TO 200
         IF(ALPHA.LE.1.0E-5)              GO TO 400
C=======================================================================
C     FIRST CALCULATE THETA GIVEN ALPHA.
C     USE PARABOLIC APPROXIMATION FOR FIRST GUESS FOR ALPHA < 0.04
C=======================================================================
         THETA = 0.031715 - 12.79384 * ALPHA + 8.28479 * SQRT(ALPHA)
         IF(ALPHA.GT.0.04) THETA = 1.2 + 5.08 * (ALPHA - 0.04) / 0.96
         TH1 = THETA
         AP  = 6.283185 * ALPHA
C=======================================================================
         DO 20 K = 1,40
         D       = - (AP - THETA + SIN(THETA)) / (1 - COS(THETA))
C     Modification by AHE, NIWA 29/6/00 to improve convergence for 
C     large theta
	   IF(D>1) D=SIGN(1.,D)
         THETA   = THETA - D
         IF(ABS(D).LE.0.0001) GO TO 40
   20    CONTINUE
C=======================================================================
C     IF NO CONVERGENCE, USE FIRST GUESS.
C=======================================================================
         THETA = TH1
         WRITE(N6,35) ID,ALPHA,THETA
C=======================================================================
C     CALCULATE PSI.
C=======================================================================
   40    IF(ID.NE.2) THEN
                  PS = (THETA - SIN(THETA)) ** 1.666667 /
     +                 6.283185 / THETA ** .66667
C=======================================================================
C                 CALCULATE NORMALIZED DEPTH.
C=======================================================================
                  ELSE
                  DN = (1.0 - COS(THETA / 2.0)) / 2.0
                  ENDIF
C=======================================================================
C     END OF ID LE 2
C=======================================================================
         RETURN
         ENDIF
C=======================================================================
C     START OF ID EQ 3
C=======================================================================
      IF(PS.LE.0.0.OR.PS.GE.1.0) GO TO 300
      IF(PS.LE.1.0E-6)           GO TO 500
C=======================================================================
C        FIRST COMPUTE THETA GIVEN PSI.
C        USE PARABOLIC APPROXIMATION FOR FIRST GUESS FOR PS < 0.015
C=======================================================================
      THETA = 0.12103 - 55.5075 * PS + 15.62254 * SQRT(PS)
      IF(PS.GT.0.015) THETA = 1.2 + 1.94 * (PS - 0.015) / 0.485
      IF(PS.GT.0.5)   THETA = 3.14 + 1.03 * (PS - 0.5) / 0.4
      IF(PS.GT.0.90)  THETA = 4.17 + 1.12 * (PS - 0.90) / 0.176
      TH1   = THETA
      AP    = 6.283185 * PS
C=======================================================================
      DO 120 K = 1,40
      THETA    = ABS(THETA)
      TT       = THETA - SIN(THETA)
      TT23     = TT ** 0.666667
      T3       = THETA ** 0.333333
      D        = AP * THETA / T3 - TT * TT23
      D        = D/(AP*.666667/T3-1.666667*TT23*(1.0-COS(THETA)))
      THETA    = THETA - D
      IF(ABS(D).LE.0.0001) GO TO 140
 120  CONTINUE
C=======================================================================
C     IF NO CONVERGENCE, USE FIRST GUESS.
C=======================================================================
      THETA = TH1
      WRITE(N6,35) ID,PS,THETA
C=======================================================================
C     CALCULATE ALPHA
C=======================================================================
  140 ALPHA = (THETA - SIN(THETA)) / 6.283185
      RETURN
C=======================================================================
  200 IF(ALPHA.GE.1.0) THEN
                       PS    = 1.0
                       DN    = 1.0
                       ELSE
                       PS    = 0.0
                       DN    = 0.0
                       ENDIF
      RETURN
C=======================================================================
  300 IF(PS.GE.1.0) THEN
                    ALPHA = 1.0
                    ELSE
                    ALPHA = 0.0
                    ENDIF
      RETURN
C=======================================================================
C     FOR SMALL ALPHA (HENCE THETA), USE EXPANSION FOR SIN & COSINE.
C=======================================================================
  400 THETA          = (37.69911 * ALPHA) ** 0.33333
      IF(ID.EQ.1) PS = THETA ** 4.33333 / 124.4797
      IF(ID.EQ.2) DN = THETA ** 2 / 16.0
      RETURN
C=======================================================================
C     FOR SMALL PS (HENCE THETA), USE EXPANSION FOR SIN & COSINE.
C=======================================================================
  500 THETA = (124.4797 * PS) ** 0.2307692
      ALPHA = THETA ** 3 / 37.69911
      RETURN
C=======================================================================
   35 FORMAT(/,' ===>  WARNING !! NO CONVERGENCE IN SUBROUTINE CIRCLE.
     1. ID =',I8,'   ALPHA OR PSI =',1PE15.7,'   THETA1 =',1PE15.7,/)
C=======================================================================
      END
``` 
