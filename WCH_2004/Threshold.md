```fortran 
C   Common block for printout of threshold elevation values for junctions.
C   Added 10/14/99  by C. Moore  CDM
C
      Common /ThreshIN/ NTHRESH,NJUNTHR,THRESH(MTHRESH,NEE),
     A                JUNTHR(NEE),THRESHID(MTHRESH)
      Common /ThreshOUT/TFIRST(MTHRESH,NEE),TDUR(MTHRESH,NEE),
     a                ITCOUNT(MTHRESH,NEE),TLASTTIME(MTHRESH,NEE)
      Character*32 THRESHID
      LOGICAL TLASTTIME
# File Overview

This document defines two Fortran common blocks used for printing threshold elevation values for junctions. The code includes both input and output parameters, as well as supporting variables such as arrays and logical flags.

## File Location

The file is located at:  
`/C:/Users/dickinro/OneDrive - Autodesk/Documents/WCH_2004_SWMM44H/WCH_2004/Threshold.md`

## Contents

### Common Block: ThreshIN

- **Purpose:**  
      Holds the input data for threshold calculations.
      
- **Components:**  
      - `NTHRESH` – Count of thresholds.
      - `NJUNTHR` – Number of junction thresholds.
      - `THRESH(MTHRESH, NEE)` – Array containing threshold elevation values.
      - `JUNTHR(NEE)` – Array containing junction threshold information.
      - `THRESHID(MTHRESH)` – Character array (each 32 characters long) representing threshold identifiers.

### Common Block: ThreshOUT

- **Purpose:**  
      Contains the output data produced after processing the thresholds.
      
- **Components:**  
      - `TFIRST(MTHRESH, NEE)` – Array for storing the first time a threshold event occurs.
      - `TDUR(MTHRESH, NEE)` – Array representing the duration of threshold events.
      - `ITCOUNT(MTHRESH, NEE)` – Array that counts the iterations or events recorded.
      - `TLASTTIME(MTHRESH, NEE)` – Logical array indicating if the last timestamp should be considered.

## Additional Details

- **Comments:**  
      The file begins with a comment block that outlines the purpose of the common blocks and notes that the code was added on 10/14/99 by C. Moore (CDM).  
- **Fortran Specifics:**  
      - The file uses Fortran’s common block functionality to share variables across different program units.
      - The use of `Character*32` and `LOGICAL` types is standard in Fortran for defining fixed-length strings and boolean values, respectively.

This summary provides a clear explanation of the content and structure of the Fortran code within the markdown file.

