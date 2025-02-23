```fortran 
      FUNCTION DEPTH(ALPHA)
C	TRANSPORT BLOCK
C
C=======================================================================
C     COMPUTES NORMALIZED DEPTH IN CONDUIT GIVEN
C     NORMALIZED AREA, ALPHA.   LAST UPDATED BY RED IN NOVEMBER, 1988.
C     RED, 12/31/93.  CHECK FOR RANGE OF SUBSCRIPT.
C     WCH, 12/5/94.  CHECK FOR RANGE OF SUBSCRIPT FOR MOD. BASKET HAND.
C     CIM, 9/8/00.  Add option for detailed calculation of irregular
C                   sections.
C=======================================================================
      INCLUDE 'TAPES.INC'
      # Fortran Function DEPTH Summary

      This markdown provides a complete summary of the Fortran code in the file. The function `DEPTH(ALPHA)` computes the normalized depth in various conduit configurations based on the normalized area (`ALPHA`). Below is an extensive overview of the code structure and its functionality.

      ---

      ## 1. Overview

      - **Purpose:**  
            Computes the normalized depth for a conduit system based on the normalized area, with specific handling for different conduit types and configurations.

      - **Usage Context:**  
            Part of a broader hydraulic model/material, the function is used to relate the cross-sectional area properties to the depth of flow.

      ---

      ## 2. File Structure and Includes

      The file makes use of several include files to import common parameters and subroutines:

      - `TAPES.INC`
      - `HUGO.INC`
      - `NEW81.INC`
      - `FLODAT.INC`
      - `NEWTR.INC`
      - `TABLES.INC` *(as provided in the selection placeholder)*

      These include files likely contain variable declarations, additional subroutines (for example, a routine called `CIRCLE`), and definitions for arrays (like `DNORM`, `ANORM`, and `QCURVE`) that are referenced throughout the function.

      ---

      ## 3. Function Description

      ### 3.1 Input Parameters

      - **ALPHA:**  
            The normalized cross-sectional area used to determine the flow depth.

      - **Global variables/constants:**  
            Variables like `NTYPE`, `KDEPTH`, and several geometry and flow parameters (`GEOM1`, `GEOM2`, `GEOM3`, `P2`, `P5`, `P6`, `DIST`, etc.) are used. Arrays such as `DNORM` and `ANORM` store tabulated functions for interpolation.

      ### 3.2 Algorithm Breakdown

      #### A. Early Returns
      - **Rectangular Conduit:**  
            When the conduit type indicates a rectangular channel (or when `ALPHA` equals 0.0), the function returns `ALPHA` directly as the depth.
            
      - **Tabular D-A Curve & Branching by NTPE Type:**  
            - The function distinguishes between several conduit types using `NTPE` (element type) and `KDEPTH` (depth computation method flag).  
            - If `KDEPTH` is 2, then the code applies different logic for:
                  - **Parabolic/Power/Irregular Corridors (NTPE 16):**  
                        Uses interpolation over tabulated values for `QCURVE` or, in detailed cases, `QCURV2`. The interpolation index `I` is computed using `ALPHA` increments (0.04 or 0.0016).
                  - **Non-Circular or Circular with Large Depths:**  
                        Interpolates using normalized depth arrays (`DNORM` and `ANORM`).  
                        For low flows (`ALPHA < 0.04`), a parabolic correction improves the depth estimate with a second-order interpolation term.

      #### B. Special Cases for High Accuracy and Functional Forms

      - **CIRCULAR Sections (High Accuracy at Low Flows):**  
            Uses a dedicated routine (`CIRCLE`) to compute the depth for low flow conditions.

      - **Modified Basket-Handle Conduit (NTPE EQ 10):**  
            Converts normalized area to actual area and computes the depth using a different functional form, with checks to avoid exceeding geometry limits.

      - **Rectangular, Triangular Bottom (NTPE EQ 11):**  
            Implements the functional form taking into account area surplus relative to a triangular section.

      - **Rectangular, Round Bottom (NTPE EQ 12):**  
            Uses two different methods depending on area thresholds.  
            For lower areas, it calls the `CIRCLE` routine; for larger areas, it applies an interpolation with adjustments based on geometric scaling.

      - **Trapezoid Section (NTPE EQ 13):**  
            Computes depth through an inverted quadratic relation considering the trapezoidal geometry.

      ---

      ## 4. Interpolation Details

      - **Linear and Parabolic Interpolation:**  
            The code uses simple linear interpolation between tabulated values and enhances the estimate via a parabolic correction in some cases.
            
      - **Index Handling:**  
            Special care is taken for the index calculations to avoid out-of-bound errors (e.g., ensuring `I` remains within valid limits).

      - **Floating Point Adjustments:**  
            Many divisions and multiplications aim to convert normalized values into actual flow-depth measurements using the geometric properties of the conduit (variables like `AFULL`, `GEOM1`, `GEOM2`, `GEOM3`, etc.).

      ---

      ## 5. Summary

      - The function is organized into multiple conditional branches to handle different types of conduits and flow regimes.
      - Early exits simplify cases where the normalized area directly defines the depth, whereas more complex conduit shapes require detailed interpolation and geometric adjustments.
      - Code readability is enhanced with extensive comments and inline modifications (noted by identifiers like `CIM###` and `C####`), tracing the evolution of the algorithm over time.

      ---

      ## 6. Additional Notes

      - **Maintainability:**  
            The use of include files (`TABLES.INC` among others) aids modularity. Any changes to the geometric or flow parameters can be maintained separately.
            
      - **Historical Context:**  
            The comments indicate revisions and enhancements over many years, with contributions by several authors (e.g., RED, WCH, CIM).

      - **Extensibility:**  
            The structure allows the addition of more conduit types by extending the conditional branches.

      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      INCLUDE 'FLODAT.INC'
C#### CIM 9/8/00  Add NEWTR common block
      INCLUDE 'NEWTR.INC'
      DIMENSION QI(NET),QO(NET)
      EQUIVALENCE (QO(1),Q(1,2,2)),(QI(1),Q(1,1,2))
C=======================================================================
C     KDEPTH(NTPE) = 1 FOR CONDUIT WITH A FUNCTIONAL D-A RELATIONSHIP.
C     KDEPTH(NTPE) = 2 FOR CONDUIT WITH A TABULAR D-A RELATIONSHIP.
C     KDEPTH(NTPE) = 3 FOR ELEMENT OTHER THAN CONDUIT.
C=======================================================================
      NTPE = NTYPE(M)
C=======================================================================
C     IN RECTANGULAR CONDUIT, NORMALIZED DEPTH EQUALS NORMALIZED AREA.
C=======================================================================
      IF(NTPE.EQ.2.OR.ALPHA.EQ.0.0) THEN
                                  DEPTH = ALPHA
                                  RETURN
                                  ENDIF
C=======================================================================
C     ROUTINE FOR TABULAR D-A CURVE.
C     LINEAR  INTERPOLATION BETWEEN TABULAR POINTS IS USED.
C=======================================================================
      IF(KDEPTH(NTPE).EQ.2) THEN
C=======================================================================
C     PARABOLIC, POWER FUNCTION OR NATURAL CHANNELS
C     Note that NTPE = 16 for all three channel types. 
C=======================================================================
           IF(NTPE.EQ.16) THEN
                   I     = 1 + IFIX(ALPHA/0.04)
                   IF(I.GT.25) I = 25
C#### RED (WCH), 12/31/93.  ADD ANOTHER CHECK FOR VALUE OF I.
                   IF(I.LE.0)  I = 1
CIM###  9/8/00  CIMDETAIL   add if here
             IF(IDETAIL.EQ.0.OR.I.GE.2) THEN
                   DELTA = (ALPHA - 0.04*FLOAT(I-1))/0.04
                   M1    = NQC(M)
                   DEPTH = (QCURVE(M1,2,I) +
     +                    (QCURVE(M1,2,I+1)-QCURVE(M1,2,I))*DELTA)
CIM###  9/8/00  CIMDETAIL   here is code for detailed section
             ELSE    ! HERE IDETAIL = 1 and I LT 2
                   I  =  1 + IFIX(ALPHA/0.0016)
                   IF (I.LE.0) I = 1
                   DELTA = (ALPHA - 0.0016*FLOAT(I-1))/0.0016
                   M1    = NQC(M)
                   DEPTH = (QCURV2(M1,2,I) +
     +                    (QCURV2(M1,2,I+1)-QCURV2(M1,2,I))*DELTA)
cim      WRITE(N6,*) 'DEPTH2',M,M1,ALPHA,I,DELTA,DEPTH,QCURV2(M1,2,I),
cim     +QCURV2(M1,2,I+1)
             ENDIF
CIM###  9/8/00 end of change
                   RETURN
                   ENDIF
C=======================================================================
C     NON-CIRCULAR CONDUITS OR CIRCULAR CONDUITS WITH LARGE DEPTHS
C=======================================================================
           IF(NTPE.NE.1.OR.ALPHA.GE.0.04) THEN
                          DALPHA = 1.0 / (FLOAT(NN(NTPE)) - 1.0)
                          I      = IFIX(ALPHA/DALPHA) + 1
                          IF(I.GE.NN(NTPE)) THEN
                              DEPTH = DNORM(NTPE,I)
                              ELSE
                          DEPTH = DNORM(NTPE,I)+(ALPHA - ANORM(NTPE,I))/
     +                            DALPHA*(DNORM(NTPE,I+1)-DNORM(NTPE,I))
                              ENDIF
C=======================================================================
C                        IMPROVE ESTIMATE WITH PARABOLIC INTERPOLATION
*                                FOR NON-CIRCULAR CONDUITS.
C=======================================================================
                         IF(ALPHA.LT.0.04) DEPTH = DEPTH + (ALPHA -
     +                  ANORM(NTPE,I)) * (DNORM(NTPE,I) - 2.0 *
     +                  DNORM(NTPE,I+1)+DNORM(NTPE,I+2))/(2.0*DALPHA**2)
                         RETURN
                         ENDIF
C=======================================================================
C     SPECIAL ROUTINE FOR HIGH ACCURACY AT LOW FLOWS.
C=======================================================================
           ALF = ALPHA
           CALL CIRCLE(ALF,PS,DN,2)
                        DEPTH = DN
           IF(NTPE.EQ.12) DEPTH = DEPTH*2.0*GEOM3(M)/(P2(M)*DIST(M))
           RETURN
           ENDIF
C=======================================================================
C     END OF TABULAR COMPUTATIONS.
C     BEGIN FUNCTIONAL-FORM COMPUTATIONS (KEPTH = 1).
C=======================================================================
C     FUNCTIONAL FORM FOR MODIFIED BASKET-HANDLE.
C=======================================================================
      IF (NTPE.EQ.10) THEN
                    AA = ALPHA*AFULL(M)
                    IF(AA.LE.GEOM3(M)) THEN
                          DEPTH = AA/GEOM2(M)/(GEOM1(M)+GEOM2(M)/2.0)
                          RETURN
                          ENDIF
                    ALF   = (AA-GEOM3(M)+P5(M)/2.0)/P5(M)
                    I     = IFIX(ALF/0.02) + 1
C#### WCH, 12/5/94.  CHECK FOR POSSIBLE I GT 51.  CHANGE EQ TO GE.
                    IF(I.GE.51) THEN
                            DD = DNORM(1,I)
                            ELSE
                            DD    = DNORM(1,I)+(ALF-ANORM(1,I))/0.02 *
     +                              (DNORM(1,I+1)-DNORM(1,I))
                            DEPTH = ((DD-0.5)*GEOM2(M)+GEOM1(M))/
     +                              (GEOM1(M) + GEOM2(M)/2.0)
                            ENDIF
                    RETURN
                    ENDIF
C=======================================================================
C     FUNCTIONAL FORM FOR RECTANGULAR, TRIANGULAR BOTTOM.
C=======================================================================
      IF (NTPE.EQ.11) THEN
                    AA = ALPHA*AFULL(M)
                    AB = GEOM3(M)*GEOM2(M)/2.0
                    IF(AA-AB.LT.0.0) DEPTH = GEOM3(M)/GEOM1(M) *
     +                                       SQRT(AA/AB)
                    IF(AA-AB.EQ.0.0) DEPTH = GEOM3(M)/GEOM1(M)
                    IF(AA-AB.GT.0.0) DEPTH = (GEOM3(M) +
     +                                       (AA-AB)/GEOM2(M))/GEOM1(M)
                    RETURN
                    ENDIF
C=======================================================================
C     FUNCTIONAL FORM FOR RECTANGULAR, ROUND BOTTOM.
C=======================================================================
      IF (NTPE.EQ.12) THEN
                    AA = ALPHA*AFULL(M)
                    IF (AA.GT.P6(M)) THEN
                                     DD    = P2(M)*DIST(M)-GEOM1(M)
                                     DEPTH = (DD+(AA-P6(M))/GEOM2(M))/
     +                                       (P2(M)*DIST(M))
                                     RETURN
                                     ENDIF
                    ALF = ALPHA*AFULL(M)/(3.1415965*GEOM3(M)*GEOM3(M))
                    IF(ALF.LT.0.04) THEN
                           CALL CIRCLE(ALF,PS,DN,2)
                           DEPTH = DN
                           IF(NTPE.EQ.12) DEPTH = DEPTH*2.0*GEOM3(M)/
     +                                          (P2(M)*DIST(M))
                           RETURN
                           ENDIF
                    I     = IFIX(ALF/0.02) + 1
                    IF(I.EQ.51) THEN
                            DEPTH = DNORM(1,I)
                            ELSE
                            DEPTH = DNORM(1,I) + (DNORM(1,I+1) -
     +                              DNORM(1,I))/0.02*(ALF-ANORM(1,I))
                            ENDIF
                    DEPTH = DEPTH*2.0*GEOM3(M)/(P2(M)*DIST(M))
                    RETURN
                    ENDIF
C=======================================================================
C    FUNCTIONAL FORM FOR TRAPEZOID
C=======================================================================
      IF(NTPE.EQ.13) THEN
                   AA = ALPHA * AFULL(M)
                   DEPTH = (-GEOM2(M) + SQRT(GEOM2(M)**2 +
     +                     4.0 * AA/GEOM3(M))) * GEOM3(M)/2.0/GEOM1(M)
                   RETURN
                   ENDIF
      END
``` 
