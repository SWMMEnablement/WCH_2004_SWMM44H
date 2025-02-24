```fortran 
      FUNCTION PSI(ALPHA)
C     TRANSPORT BLOCK
C=======================================================================
C     FINDS Q/QFULL (PSI) GIVEN A/AFULL (ALPHA) FOR FUNCTIONAL Q-A CURVE
C
C     UPDATED (NEW COMMON) BY W.C.H., SEPTEMBER 1981.
# Comprehensive Summary of the PSI Function

This document summarizes the Fortran PSI function used for computing the flow ratio (PSI), defined as Q/QFULL, based on the area ratio (ALPHA = A/AFULL). Over several revisions, the code has been tuned to handle different channel geometries and ensure high numerical accuracy.

## Key Features

- **Input Validation:**  
      The function immediately returns if ALPHA ≤ 0, ensuring no invalid computations proceed.

- **Channel Type Determination (NTPE):**  
      Based on the variable NTPE (derived from an external array `NTYPE`), the function handles multiple cases:
      
      1. **Low Flow Handling (NTPE = 1):**  
             - Uses the external subroutine `CIRCLE` to obtain high-accuracy PSI values at very low flows.

      2. **Rectangular Conduits (NTPE = 2):**  
             - Checks if ALPHA exceeds a threshold (`ALFMAX`).  
             - Computes PSI using a modified formulation involving a scaling factor calculated from parameter R and a power expression.

      3. **Modified Basket-Handle (NTPE = 10):**  
             - Determines the effective area (`AA`) and compares it to a geometry threshold.  
             - Either computes PSI using a roughness-controlled approach or proceeds with a normalized calculation based on modified variables.

      4. **Rectangular with Triangular Bottom (NTPE = 11):**  
             - Divides the computation based on whether the computed area is below a critical value (AB).  
             - Uses power functions for low values and interpolation when ALPHA exceeds a set maximum (`ALFMAX`).

      5. **Rectangular with Round Bottom (NTPE = 12):**  
             - Offers dual computation paths:  
                   - For higher effective areas, either applies a similar threshold-based adjustment (using ALFMAX) or performs a two-step calculation involving derivatives of geometric parameters.  
                   - For very low effective areas, leverages the `CIRCLE` routine, followed by scaling with a dedicated parameter.
             - In some cases, it also defines interpolated values (using the QNORM array) based on discretized intervals.

      6. **Trapezoid (NTPE = 13):**  
             - Involves solving for an intermediate variable via a quadratic-like equation (using a square root) before applying a power function to get PSI.

      7. **Tabular PSI Calculation (NTPE = 16):**  
             - Primarily used for power functions and natural channels.  
             - Branches based on whether PSI is calculated directly from the QCURVE data (using the fourth column, per revision note “USE QCURVE(4), NOT (3) FOR FLOW.  WCH, 7/6/01”) or by interpolating using the QNORM and ANORM arrays.
             - Contains boundary checks (e.g., limiting interpolation indices) to guard against out-of-range errors.

## Included External Resources

The function relies on several INC files that supply necessary parameters, coefficients, and arrays:
- `TAPES.INC`
- `PSIDPS.INC`
- `TABLES.INC`
- `HUGO.INC`
- `NEW81.INC`
- `FLODAT.INC`

These files provide the definitions for parameters such as P4, P5, P7, GEOM1-3, AFULL, and many others that are essential for the various computations.

## Revision Highlights

- **Updated Flow Calculation:**  
      A crucial update noted in the comments specifies the usage of `QCURVE(4)` instead of `QCURVE(3)` for flow computations. This change, highlighted in the comment:
      
      "C     USE QCURVE(4), NOT (3) FOR FLOW.  WCH, 7/6/01."
      
      ensures that the correct dataset column is used, thereby improving the accuracy of PSI values determined via tabular interpolation.

- **Error Handling:**  
      Protective measures, such as immediate returns and index bounds checking (for interpolation cases), are in place to handle unlikely but possible input errors.

## Conclusion

The PSI function is a robust tool for computing flow ratios across a variety of channel profiles using both analytical formulas and tabular interpolation. Its case-specific logic allows for targeted computations that account for low-flow special cases, different geometric configurations, and updated computational routines to enhance accuracy. This detailed documentation serves as both a technical reference and a guide for further modifications or integrations.
 
C     FIX MAX NUMBER OF TABULAR POINTS FOR UNLIKELY BUT POSSIBLE ERROR.
C       WCH, 5/24/02.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'PSIDPS.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      INCLUDE 'FLODAT.INC'
C=======================================================================
      PSI = 0.0
      IF(ALPHA.LE.0.0) RETURN
      NTPE  = NTYPE(M)
# PSI Function: Functional Q-A Curve Calculation in Fortran

This document provides a complete technical description and conversion of the original Fortran PSI function code into structured Markdown. It includes a table of contents, hierarchical sections, syntax-highlighted code blocks, reference-style links, tables, image alt text, collapsible sections, and navigation cues.

---

## Table of Contents

1. [Introduction](#introduction)
2. [Code Workflow and Technical Overview](#code-workflow-and-technical-overview)
3. [Channel Types and Cases](#channel-types-and-cases)
      - [NTPE = 1: Low Flow Handling](#ntpe--1-low-flow-handling)
      - [NTPE = 2: Rectangular Conduits](#ntpe--2-rectangular-conduits)
      - [NTPE = 10: Modified Basket-Handle](#ntpe--10-modified-basket-handle)
      - [NTPE = 11: Rectangular, Triangular Bottom](#ntpe--11-rectangular-triangular-bottom)
      - [NTPE = 12: Rectangular, Round Bottom](#ntpe--12-rectangular-round-bottom)
      - [NTPE = 13: Trapezoid](#ntpe--13-trapezoid)
      - [NTPE = 16: Tabular PSI Calculation](#ntpe--16-tabular-psi-calculation)
4. [References and Resources](#references-and-resources)
5. [Appendices](#appendices)
      - [Channel Type Summary Table](#channel-type-summary-table)
      - [Diagram](#diagram)

---

## Introduction

The `PSI` function computes the ratio Q/QFULL (named PSI) corresponding to the ratio A/AFULL (named ALPHA) using a functional Q-A curve. Originally written in Fortran, this function handles different channel types based on the variable `NTPE` and addresses numerical precision considerations at low flows. The code applies various computational procedures (e.g., `CIRCLE` subroutine call) and includes error protection measures for certain conditions.

---

## Code Workflow and Technical Overview

The function `PSI` is structured to:
- Check for valid input (ALPHA > 0).
- Determine the type of channel or geometric configuration using `NTPE`.
- Use dedicated procedures, such as `CALL CIRCLE`, for special low-flow treatments.
- Provide separate logic branches for various channel types (e.g., rectangular, trapezoid, round-bottom channels) via conditional statements.
- Incorporate tabular interpolation when special channel types are detected.

Each branch uses mathematical constructs and parameters (like `P4(M)`, `P5(M)`, etc.) drawn from included `INC` files to compute the correct PSI value.

---

## Channel Types and Cases

### NTPE = 1: Low Flow Handling

This branch calls a special routine for high accuracy at low flows:
- The input ALPHA is directly processed using the `CIRCLE` function.
- The computed PSI is returned once processed.

<details>
  <summary>Technical Details</summary>
  
  - Direct assignment of `ALF = ALPHA`
  - Call: `CALL CIRCLE(ALF, PS, DN, 1)`
  - PSI is set from the subroutine output.
  
</details>

---

### NTPE = 2: Rectangular Conduits

For rectangular conduit flows:
- PSI computation adapts if ALPHA exceeds a threshold (ALFMAX).
- Otherwise, PSI is computed using a modified formula involving a computed variable `CATH`.

<details>
  <summary>Technical Details</summary>
  
  - Calculation of `AAA = 2.0 * R * ALPHA + 1.0`
  - CATH recalculated through a power expression.
  
</details>

---

### NTPE = 10: Modified Basket-Handle

Handles non-linear flow characteristics:
- Uses comparisons with geometric thresholds (`GEOM3`).
- Decides on either a roughness-controlled formula or a normalized calculation.

<details>
  <summary>Technical Details</summary>
  
  - Calculation involves `AA = ALPHA * AFULL(M)` and `RH = RADH(AA)`
  - Two separate branches clarify differing computational methods.
  
</details>

---

### NTPE = 11: Rectangular, Triangular Bottom

The PSI calculation is divided depending on a comparison with variable `AB`:
- Uses power functions when ALPHA is low.
- Switches to interpolation when ALPHA is high.

<details>
  <summary>Technical Details</summary>
  
  - Two conditional branches manage the computation: one for low and one for high ALPHA values.
  - Interpolation uses similar thresholds as NTPE = 2.
  
</details>

---

### NTPE = 12: Rectangular, Round Bottom

Handles a different geometric configuration:
- Uses two distinct formulas based on the computed `AA`.
- Implements both a direct call to `CIRCLE` and a table interpolation for different ALPHA ranges.

<details>
  <summary>Technical Details</summary>
  
  - For ALPHA where `ALF < 0.04`, the `CIRCLE` function is used.
  - Otherwise, table-based interpolation with precomputed arrays (`QNORM`, `ANORM`) is applied.
  
</details>

---

### NTPE = 13: Trapezoid

The algorithm computes:
- An intermediate variable `AAA` derived from a quadratic-like formulation.
- A subsequent power function to yield PSI.

<details>
  <summary>Technical Details</summary>
  
  - `AAA` is computed with a square root and scaling factors.
  - PSI is determined via a power operation on the ratio of `AA` and adjusted geometry.
  
</details>

---

### NTPE = 16: Tabular PSI Calculation

Used primarily for power functions and natural channels:
- Computes PSI by interpolating values from the QCURVE table.
- Contains two major branches to handle different table configurations.

<details>
  <summary>Technical Details</summary>
  
  - The first branch uses QCURVE(4) for consolidation.
  - The second branch interpolates values using the `QNORM` and `ANORM` arrays.
  
</details>

---

## Code Listing

Below is the fully annotated and formatted code block extracted from the original file:

```fortran
            FUNCTION PSI(ALPHA)
C     TRANSPORT BLOCK
C=======================================================================
C     FINDS Q/QFULL (PSI) GIVEN A/AFULL (ALPHA) FOR FUNCTIONAL Q-A CURVE
C
C     UPDATED (NEW COMMON) BY W.C.H., SEPTEMBER 1981.
C     USE QCURVE(4), NOT (3) FOR FLOW.  WCH, 7/6/01. 
C     FIX MAX NUMBER OF TABULAR POINTS FOR UNLIKELY BUT POSSIBLE ERROR.
C       WCH, 5/24/02.
C=======================================================================
            INCLUDE 'TAPES.INC'
            INCLUDE 'PSIDPS.INC'
            INCLUDE 'TABLES.INC'
            INCLUDE 'HUGO.INC'
            INCLUDE 'NEW81.INC'
            INCLUDE 'FLODAT.INC'
C=======================================================================
            PSI = 0.0
            IF(ALPHA.LE.0.0) RETURN
            NTPE  = NTYPE(M)
C     CALL SPECIAL ROUTINE TO GET HIGH ACCURACY AT LOW FLOWS
C=======================================================================
            IF(NTPE.EQ.1) THEN
                                    ALF = ALPHA
                                    CALL CIRCLE(ALF, PS, DN, 1)
                                    PSI = PS
                                    RETURN
                                    ENDIF
C=======================================================================
C     SPECIAL FUNCTIONAL FORM FOR RECTANGULAR CONDUITS.
C=======================================================================
            IF(NTPE.EQ.2) THEN
                                    R = P5(M)
                                    IF(ALPHA.GT.ALFMAX(NTPE)) THEN
                                                      PSI = P4(M)+(ALPHA-ALFMAX(NTPE))*
        1                           (1.0-P4(M))/(1.0-ALFMAX(NTPE))
                                                      RETURN
                                                      ENDIF
                                    AAA = 2.0*R*ALPHA+1.0
                                    CATH = (ALPHA*P7(M)/AAA)**0.6666667
                                    PSI  = ALPHA*CATH
                                    RETURN
                                    ENDIF
C=======================================================================
C     FUNCTIONAL FORM FOR MODIFIED BASKET-HANDLE.
C=======================================================================
            IF(NTPE.EQ.10) THEN
                                     AA = ALPHA*AFULL(M)
                                     IF (AA.GT.GEOM3(M)) THEN
                                                  RH = RADH(AA)
                                                  PSI = 1.49/ROUGH(M)*AA*RH**0.6666667/P1(M)
                                                  RETURN
                                                  ENDIF
                                     ALF  = AA/GEOM3(M)
                                     R    = GEOM1(M)/GEOM2(M)
                                     AAA  = 2.0*ALF*R+1.0
                                     CATH = (ALF*P7(M)/AAA)**0.6666667*P6(M)
                                     PSI  = ALF*CATH
                                     RETURN
                                     ENDIF
C=======================================================================
C     FUNCTIONAL FORM FOR RECTANGULAR, TRIANGULAR BOTTOM.
C=======================================================================
            IF(NTPE.EQ.11) THEN
                                     AB = GEOM3(M)*GEOM2(M)/2.0
                                     AA = ALPHA*AFULL(M)
                                     IF (AA.LE.AB) THEN
                                                                  PSI = P7(M)*ALPHA**1.333333
                                                                  RETURN
                                                                  ENDIF
                                     IF (ALPHA.GT.ALFMAX(NTPE)) THEN
                                                      PSI = P4(M)+(ALPHA-ALFMAX(NTPE))*
        1                           (1.0-P4(M))/(1.0-ALFMAX(NTPE))
                                                      RETURN
                                                      ENDIF
                                     AAA = GEOM3(M)/P5(M)-GEOM3(M)+(2.0*GEOM1(M) -
        1                   GEOM3(M))*ALPHA
                                     CATH = (ALPHA*P6(M)/AAA)**0.6666667
                                     PSI = ALPHA*CATH
                                     RETURN
                                     ENDIF
C=======================================================================
C     FUNCTIONAL FORM FOR RECTANGULAR, ROUND BOTTOM.
C=======================================================================
            IF(NTPE.EQ.12) THEN
                                     AA = ALPHA*AFULL(M)
                                     IF(AA.GT.P6(M)) THEN
                                                       IF (ALPHA.GT.ALFMAX(NTPE)) THEN
                                                              PSI = P4(M)+(ALPHA-ALFMAX(NTPE))*
        1                                (1.0-P4(M))/(1.0-ALFMAX(NTPE))
                                                              RETURN
                                                              ENDIF
                                                       D1 = GEOM3(M)*P5(M)+2.0*GEOM1(M)+GEOM2(M)
                                                       D2 = GEOM3(M)*P5(M)+2.0/GEOM2(M)*
        1                           (AFULL(M)*ALPHA-P6(M))
                                                       CATH = (ALPHA*D1/D2)**0.6666667
                                                       PSI  = ALPHA*CATH
                                                       RETURN
                                                       ENDIF
                                     ALF = ALPHA*AFULL(M)/(3.1415965*GEOM3(M)*GEOM3(M))
                                     IF(ALF.LT.0.04) THEN
                                                                    CALL CIRCLE (ALF, PS, DN, 1)
                                                                    PSI = PS
                                                                    PSI = PSI*P7(M)
                                                                    RETURN
                                                                    ENDIF
                                     I = IFIX(ALF/0.02) + 1
                                     PSI = QNORM(1,I)+(QNORM(1,I+1)-QNORM(1,I))/0.02*
        1                              (ALF-ANORM(1,I))
                                     PSI = PSI*P7(M)
                                     RETURN
                                     ENDIF
C=======================================================================
C    FUNCTIONAL FORM FOR TRAPEZOID
C=======================================================================
            IF(NTPE.EQ.13) THEN
                                     AA   = ALPHA * AFULL(M)
                                     AAA  = (-GEOM2(M) + SQRT(GEOM2(M)**2 +
        1                     4.0 * AA/GEOM3(M))) * 0.5 * GEOM3(M)
                                     CATH =  AA/(GEOM2(M) + AAA * P5(M))/P6(M)
                                     PSI  = ALPHA * CATH**0.6666667
                                     RETURN
                                     ENDIF
C=======================================================================
C     INCLUDE TABULAR PSI CALC. IN CASE PSI IS CALLED BY KLASS=2 CONDUIT
Cwch, 3/14/02. This same code for power function and natural channels.
C     NTPE also = 16 for these two channels (changed from 14 and 15 in 
C     Sub. INTRAN).  
C=======================================================================
            IF(NTPE.EQ.16) THEN
                                     KK = NQC(M)
                                     DALPHA = QCURVE(KK,2,2) - QCURVE(KK,2,1)
                                     I      = IFIX(ALPHA/DALPHA + 1.0)
                                     IF(I.GE.26) I = 25
Cwch, 7/6/01. Use QCURVE(4), not (3) for flow. 
                                     PSI    = QCURVE(KK,4,I) + (QCURVE(KK,4,I+1) -
        1                      QCURVE(KK,4,I)) / DALPHA *
        2                     (ALPHA - QCURVE(KK,2,I))
                                     ELSE
                                     DALPHA = ANORM(NTPE,2) - ANORM(NTPE,1)
                                     I      = IFIX(ALPHA/DALPHA + 1.0)
Cwch, 5/24/02. Should not be in this ELSE-location, but in case we are,
C     should use MM(MTPE) as max, not 26. 
C                   IF(I.GE.26) I = 25
                                     IF(I.GE.MM(NTPE)) I = MM(NTPE) - 1
                                     PSI    = QNORM(NTPE,I) + (QNORM(NTPE,I+1) -
        1                      QNORM(NTPE,I))/DALPHA*(ALPHA-ANORM(NTPE,I))
                                     ENDIF
            RETURN
            END
```

---

## References and Resources

- [Fortran Standards and Guidelines][fortran-guidelines]
- [Numerical Methods in Engineering][numerical-methods]
- [Technical Documentation Best Practices][tech-docs]

---

## Appendices

### Channel Type Summary Table

| NTPE | Channel Type Description                                  |
|------|-----------------------------------------------------------|
| 1    | Low Flow Handling (using CIRCLE routine)                  |
| 2    | Rectangular Conduits                                      |
| 10   | Modified Basket-Handle                                    |
| 11   | Rectangular with Triangular Bottom                        |
| 12   | Rectangular with Round Bottom                             |
| 13   | Trapezoid                                                 |
| 16   | Tabular PSI Calculation for Additional Natural/Power Channels  |

### Diagram

![Flow Diagram](https://via.placeholder.com/500x300 "PSI Function Flow Diagram")

---

[fortran-guidelines]: https://www.fortran90.org "Fortran 90/95 Standards"
[numerical-methods]: https://en.wikipedia.org/wiki/Numerical_analysis "Numerical Analysis"
[tech-docs]: https://www.microsoft.com "Technical Documentation Best Practices"

---

This comprehensive Markdown document encapsulates the technical details of the original Fortran PSI function while adding modern documentation structure.
C     CALL SPECIAL ROUTINE TO GET HIGH ACCURACY AT LOW FLOWS
C=======================================================================
      IF(NTPE.EQ.1) THEN
                  ALF = ALPHA
                  CALL CIRCLE(ALF,PS,DN,1)
                  PSI = PS
                  RETURN
                  ENDIF
C=======================================================================
C     SPECIAL FUNCTIONAL FORM FOR RECTANGULAR CONDUITS.
C=======================================================================
      IF(NTPE.EQ.2) THEN
                  R = P5(M)
                  IF(ALPHA.GT.ALFMAX(NTPE)) THEN
                           PSI = P4(M)+(ALPHA-ALFMAX(NTPE))*
     1                           (1.0-P4(M))/(1.0-ALFMAX(NTPE))
                           RETURN
                           ENDIF
                  AAA = 2.0*R*ALPHA+1.0
                  CATH = (ALPHA*P7(M)/AAA)**0.6666667
                  PSI  = ALPHA*CATH
                  RETURN
                  ENDIF
C=======================================================================
C     FUNCTIONAL FORM FOR MODIFIED BASKET-HANDLE.
C=======================================================================
      IF(NTPE.EQ.10) THEN
                   AA = ALPHA*AFULL(M)
                   IF (AA.GT.GEOM3(M)) THEN
                          RH = RADH(AA)
                          PSI = 1.49/ROUGH(M)*AA*RH**0.6666667/P1(M)
                          RETURN
                          ENDIF
                   ALF  = AA/GEOM3(M)
                   R    = GEOM1(M)/GEOM2(M)
                   AAA  = 2.0*ALF*R+1.0
                   CATH = (ALF*P7(M)/AAA)**0.6666667*P6(M)
                   PSI  = ALF*CATH
                   RETURN
                   ENDIF
C=======================================================================
C     FUNCTIONAL FORM FOR RECTANGULAR, TRIANGULAR BOTTOM.
C=======================================================================
      IF(NTPE.EQ.11) THEN
                   AB = GEOM3(M)*GEOM2(M)/2.0
                   AA = ALPHA*AFULL(M)
                   IF (AA.LE.AB) THEN
                                 PSI = P7(M)*ALPHA**1.333333
                                 RETURN
                                 ENDIF
                   IF (ALPHA.GT.ALFMAX(NTPE)) THEN
                           PSI = P4(M)+(ALPHA-ALFMAX(NTPE))*
     1                           (1.0-P4(M))/(1.0-ALFMAX(NTPE))
                           RETURN
                           ENDIF
                   AAA = GEOM3(M)/P5(M)-GEOM3(M)+(2.0*GEOM1(M) -
     1                   GEOM3(M))*ALPHA
                   CATH = (ALPHA*P6(M)/AAA)**0.6666667
                   PSI = ALPHA*CATH
                   RETURN
                   ENDIF
C=======================================================================
C     FUNCTIONAL FORM FOR RECTANGULAR, ROUND BOTTOM.
C=======================================================================
      IF(NTPE.EQ.12) THEN
                   AA = ALPHA*AFULL(M)
                   IF(AA.GT.P6(M)) THEN
                            IF (ALPHA.GT.ALFMAX(NTPE)) THEN
                                PSI = P4(M)+(ALPHA-ALFMAX(NTPE))*
     1                                (1.0-P4(M))/(1.0-ALFMAX(NTPE))
                                RETURN
                                ENDIF
                            D1 = GEOM3(M)*P5(M)+2.0*GEOM1(M)+GEOM2(M)
                            D2 = GEOM3(M)*P5(M)+2.0/GEOM2(M)*
     1                           (AFULL(M)*ALPHA-P6(M))
                            CATH = (ALPHA*D1/D2)**0.6666667
                            PSI  = ALPHA*CATH
                            RETURN
                            ENDIF
                   ALF = ALPHA*AFULL(M)/(3.1415965*GEOM3(M)*GEOM3(M))
                   IF(ALF.LT.0.04) THEN
                                   CALL CIRCLE (ALF,PS,DN,1)
                                   PSI = PS
                                   PSI = PSI*P7(M)
                                   RETURN
                                   ENDIF
                   I = IFIX(ALF/0.02) + 1
                   PSI = QNORM(1,I)+(QNORM(1,I+1)-QNORM(1,I))/0.02*
     1                              (ALF-ANORM(1,I))
                   PSI = PSI*P7(M)
                   RETURN
                   ENDIF
C=======================================================================
C    FUNCTIONAL FORM FOR TRAPEZOID
C=======================================================================
      IF(NTPE.EQ.13) THEN
                   AA   = ALPHA * AFULL(M)
                   AAA  = (-GEOM2(M) + SQRT(GEOM2(M)**2 +
     1                     4.0 * AA/GEOM3(M))) * 0.5 * GEOM3(M)
                   CATH =  AA/(GEOM2(M) + AAA * P5(M))/P6(M)
                   PSI  = ALPHA * CATH**0.6666667
                   RETURN
                   ENDIF
C=======================================================================
C     INCLUDE TABULAR PSI CALC. IN CASE PSI IS CALLED BY KLASS=2 CONDUIT
Cwch, 3/14/02. This same code for power function and natural channels.
C     NTPE also = 16 for these two channels (changed from 14 and 15 in 
C     Sub. INTRAN).  
C=======================================================================
      IF(NTPE.EQ.16) THEN
                   KK = NQC(M)
                   DALPHA = QCURVE(KK,2,2) - QCURVE(KK,2,1)
                   I      = IFIX(ALPHA/DALPHA + 1.0)
                   IF(I.GE.26) I = 25
Cwch, 7/6/01. Use QCURVE(4), not (3) for flow. 
                   PSI    = QCURVE(KK,4,I) + (QCURVE(KK,4,I+1) -
     1                      QCURVE(KK,4,I)) / DALPHA *
     2                     (ALPHA - QCURVE(KK,2,I))
                   ELSE
                   DALPHA = ANORM(NTPE,2) - ANORM(NTPE,1)
                   I      = IFIX(ALPHA/DALPHA + 1.0)
Cwch, 5/24/02. Should not be in this ELSE-location, but in case we are,
C     should use MM(MTPE) as max, not 26. 
C                   IF(I.GE.26) I = 25
                   IF(I.GE.MM(NTPE)) I = MM(NTPE) - 1
                   PSI    = QNORM(NTPE,I) + (QNORM(NTPE,I+1) -
     1                      QNORM(NTPE,I))/DALPHA*(ALPHA-ANORM(NTPE,I))
                   ENDIF
      RETURN
      END
``` 
