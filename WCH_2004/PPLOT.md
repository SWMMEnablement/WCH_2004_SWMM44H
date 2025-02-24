```fortran 
      SUBROUTINE PPLOT(IX,IY,K,NPLOT,ALPLOT)
C=======================================================================
C     THIS PART OF GRAPHING ROUTINES PRINTS THE 51 BY 101 ARRAY, A,
C               OF POINTS.
C     IT ALSO ASSIGNS SYMBOLS TO THE ARRAY AND PRINTS TITLES, LABELS.
C     THIS SUBROUTINE LAST MODIFIED MARCH, 1988 BY RED.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'PLTARY.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'LAB.INC'
# PPLOT Subroutine Overview

This document provides an extensive summary of the Fortran subroutine PPLOT, which is responsible for plotting a graphical array with dimensions 51 × 101. The subroutine handles both symbol assignment within the array and formatted output for display, including titles, labels, and plot outlines.

---

## File Information

- **File Name:** PPLOT.md
- **Location:** `/C:/Users/dickinro/OneDrive - Autodesk/Documents/WCH_2004_SWMM44H/WCH_2004/PPLOT.md`

---

## Key Components and Structure

### 1. Header Comments
- **Purpose:** Describes the subroutine’s role in printing and symbol assignment for a 2D array of points.
- **Modification Note:** Last modified in March 1988 by Red.

### 2. Included Files
The subroutine includes several external files:
- **TAPES.INC:** Likely contains tape or file handling definitions.
- **PLTARY.INC:** Contains definitions for the plotting array and related variables.
- **CONTR.INC:** Might include variable or constant definitions.
- **LAB.INC:** Provides label definitions for the output.

### 3. Variable Declarations and Data Initialization
- **ALPLOT** and **SYM:** 
    - `ALPLOT` is a character variable (length 10).
    - `SYM` is an array holding symbols for various plot points, e.g., plotting characters such as `*`, `+`, `X`, etc.
- **DATA Statement:** Initializes the `SYM` array which assigns specific symbols to index positions.

### 4. Main Execution Flow

#### Symbol Placement (Labels: 10 and 240)
- **Conditional SELECT:** 
    - The subroutine checks the value of `K` (likely from external input) using a computed branch (`IF(K-99) 10,20,240`), leading to three code paths.
    - **Case at Label 10:** 
        - Assigns a specific symbol (`SYM(K)`) to a designated array position.
        - Returns immediately thereafter.
- **Label 240 (Outline Initialization):**
    - Iterates over the array dimensions to assign default symbols along the borders.
    - Sets left/right boundaries and top/bottom edges with specific symbols from the `SYM` array.

#### Array Printing and Labeling (Labels: 20 to 190)
- **Loop Over Array Rows:**
    - Uses a main DO loop from 1 to 6 for printing each row with corresponding labels.
    - Depending on the value `YLAB(1)`, a specific formatted write statement (`FORMAT 90, 95, or 100`) is chosen.
- **Intermediate Loops:**
    - Additional nested loops (using labels 170 and 180) process further rows between main label printing.
    - Special conditions check rows `24`, `26`, and `28` to print vertical markers (`VERT1`, `VERT2`, `VERT3`).
- **Final Print Statements:**
    - After processing the array, additional formatted output is generated:
        - Axis Labels (`XLAB`)
        - Plot location or configuration information (`NPLOT`, `ALPLOT`, and horizontal labels `HORIZ`).
        - Plot title (`HTITLE`).

### 5. Formatting Definitions
A series of FORMAT statements define how to print the array and associated labels:
- **General Title Formats:** Used for printing headers and footers.
- **Numeric Formats:** Formats for printing numeric labels with precision (e.g., `F14.3`, `1PE14.2`).
- **Specialized Formats:** Specific layouts for vertical markers and plot outlines.

---

## Subroutine Summary
- **Primary Function:**  
    - Assigns symbols to elements in a 2D plotting array.
    - Prints the complete array along with titles and labels using formatted output.
- **Control Flow:**  
    - Uses conditional branching (`IF` and computed `SELECT`) to handle different formatting and output scenarios.
- **Loop Constructs:**  
    - Iterates over array dimensions using nested DO loops for both assigning values and printing.
- **Output Preparation:**  
    - Involves initializing a plot outline and ensuring that boundaries are visually distinct with special symbols.
- **Importance:**  
    - Provides an early example of Fortran routines for graphical output and formatted printing, which can be used in legacy scientific and engineering applications.

---

This comprehensive summary converts the original Fortran source code into a clear markdown format, explaining the structure and purpose of each section. Any insertion of additional content for plotting operations should follow these summarized guidelines.

      CHARACTER ALPLOT*10,SYM(9)*1
      DATA SYM/'*','+','''','X','.','2',' ','I','-'/
C=======================================================================
C     PLACE PROPER SYMBOL IN ARRAY LOCATION.
C=======================================================================
      IF(K-99) 10,20,240
   10 A(51-IY,IX+1) = SYM(K)
      RETURN
C=======================================================================
C     PRINT TOTAL ARRAY, A, PLUS LABELS AND TITLES.
C=======================================================================
   20 CONTINUE
      I  = 0
      J2 = 1
      WRITE(N6,31)
      DO 180 II = 1,6
      I         = I+1
      IF(YLAB(1).LE.100000.0) THEN
                              WRITE(N6,90) YLAB(II),(A(I,J),J=1,101)
                              ELSE IF(YLAB(1).GT.100000.0) THEN
                              WRITE(N6,100) YLAB(II),(A(I,J),J=1,101)
                              ELSE
                              WRITE(N6,95) YLAB(II),(A(I,J),J=1,101)
                              ENDIF
      IF(II.EQ.6) GO TO 190
C=======================================================================
      DO 170 JJ = J2,9
      I         = I+1
      IF(I.EQ.24) THEN
                  WRITE(N6,120) VERT1,(A(I,J),J=1,101)
                  ENDIF
      IF(I.EQ.26) THEN
                  WRITE(N6,120) VERT2,(A(I,J),J=1,101)
                  ENDIF
      IF(I.EQ.28) THEN
                  WRITE(N6,120) VERT3,(A(I,J),J=1,101)
                  ENDIF
      IF(I.NE.24.AND.I.NE.26.AND.I.NE.28) WRITE(N6,160) (A(I,J),J=1,101)
  170 CONTINUE
      J2 = 1
  180 CONTINUE
  190 CONTINUE
      WRITE(N6,200) XLAB
      IF(JCE.EQ.0) WRITE(N6,210) NPLOT,HORIZ(1),HORIZ(2)
      IF(JCE.EQ.1) WRITE(N6,211) ALPLOT,HORIZ(1),HORIZ(2)
      WRITE(N6,30) HTITLE
      RETURN
C=======================================================================
C     INITIALIZE PLOT OUTLINE.
C=======================================================================
  240 DO 260 I = 1,50
      DO 250 J = 1,101
  250 A(I,J)   = SYM(7)
      A(I,1)   = SYM(8)
      A(I,101) = SYM(8)
  260 CONTINUE
      DO 270 J = 1,101
      A(1,J)   = SYM(9)
  270 A(51,J)  = SYM(9)
      DO 280 I = 1,101,10
      A(1,I)   = SYM(8)
  280 A(51,I)  = SYM(8)
      DO 290 I = 11,41,10
      A(I,1)   = SYM(9)
      A(I,101) = SYM(9)
  290 CONTINUE
C=======================================================================
   30 FORMAT(/,18X,A30,1X,A30)
   31 FORMAT(/,'1')
   90 FORMAT(' ',F14.3,1X,101A1)
   95 FORMAT(' ',F14.6,1X,101A1)
  100 FORMAT(' ',1PE14.2,1X,101A1)
  120 FORMAT(1X,A10,5X,101A1)
  160 FORMAT(16X,101A1)
  200 FORMAT(F18.1,10F10.1)
  210 FORMAT(/,18X,'LOCATION NO. : ',I9,1X,A30,2X,A30)
  211 FORMAT(/,17X,'LOCATION NO. : ',A10,1X,A30,2X,A30)
C=======================================================================
      RETURN
      END
``` 
