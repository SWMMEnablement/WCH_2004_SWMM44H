```fortran 
C=======================================================================
C     NVORF  ==> Number of variable orifices.
C     NVOTIM ==> Number of data points for variable orifices.
C=======================================================================
C#### WCH, 7/30/97.  Move PARAMETER statement to TAPES.INC.
C####      PARAMETER(NVORF=20,NVOTIM=25)
      COMMON/ORF/LORIF(NEO),AORIF(NEO),CORIF(NEO),
     +           VORIF(NVORF,NVOTIM,3),NVOR,NORIF
Cwch, 7/22/04.
cred  add a place holder for F2 type orifice - 4/15/2002
      # WCH 2004 SWMM Data Documentation

      ## Table of Contents
      1. [Introduction](#introduction)
      2. [Variable Orifices Documentation](#variable-orifices-documentation)
            - [Original Code Details](#original-code-details)
            - [F2 Orifice Details](#f2-orifice-details)
      3. [References](#references)
      4. [Appendices](#appendices)

      ## Introduction
      This document provides a complete, technical description of the Fortran code used in the SWMM module for variable orifices. It integrates the original code with updates for F2 type orifice modifications.

      ## Variable Orifices Documentation

      ### Original Code Details
      The following Fortran code snippet defines the original data structures for variable orifices as well as data for gates:
      ```fortran
      C=======================================================================
      C     NVORF  ==> Number of variable orifices.
      C     NVOTIM ==> Number of data points for variable orifices.
      C=======================================================================
      C#### WCH, 7/30/97.  Move PARAMETER statement to TAPES.INC.
      C####      PARAMETER(NVORF=20,NVOTIM=25)
                  COMMON/ORF/LORIF(NEO),AORIF(NEO),CORIF(NEO),
              +           VORIF(NVORF,NVOTIM,3),NVOR,NORIF
      Cwch, 7/22/04.
      cred  add a place holder for F2 type orifice - 4/15/2002
      CIM=======================================================================
      CIM DATA FOR GATES 
                  COMMON/ORF2/NOGATES,ICNODE(NEO),OOPEN(NEO),OCLOSE(NEO),
              .OCAREA(NEO),ORATE(NEO),IDIR(NEO),IOPRNT(NEO),IOINV(NEO)
      CIM END     
      ```

      ### F2 Orifice Details
      The following code replaces the placeholder ($SELECTION_PLACEHOLDER$) to introduce F2 type orifice variables. This ensures complete data handling for the additional orifice type:
      ```fortran
      integer f2_line, f2_limit
                  common/f2_orf/f2_line(neo), f2_limit(neo)
      CIM START
      ```

      <details>
        <summary>Additional Technical Details</summary>

      #### Code Comments
      - The file is maintained at `/C:/Users/dickinro/OneDrive - Autodesk/Documents/WCH_2004_SWMM44H/WCH_2004/ORF.md`.
      - The variable `NEO` defines the number of elements.
      - Consistent indentation and organization have been preserved for ease of maintenance.
      - The `COMMON` blocks are used to share variables across different parts of the program.

      #### Section Navigation
      - **Original Code Details:** Covers the principal definitions and common blocks.
      - **F2 Orifice Details:** Integrates new F2 orifice definitions into the shared code base.

      </details>

      ## References
      - [Fortran Documentation][fortrandocs]
      - [SWMM Technical Overview][swmmdocs]

      ## Appendices

      ### Appendix A: Code Summary Table
      | Section              | Description                                   | Code Reference                             |
      |----------------------|-----------------------------------------------|--------------------------------------------|
      | Original Code        | Defines orifice and gate variables            | Section "Original Code Details"            |
      | F2 Orifice Details   | Provides additional F2 type orifice parameters  | Section "F2 Orifice Details"                 |

      ### Appendix B: Diagram
      ![Flow Diagram](https://via.placeholder.com/150 "Workflow Diagram")

      [fortrandocs]: https://en.wikibooks.org/wiki/Fortran
      [swmmdocs]: https://www.epa.gov/water-research/swm     
CIM=======================================================================
CIM DATA FOR GATES 
      COMMON/ORF2/NOGATES,ICNODE(NEO),OOPEN(NEO),OCLOSE(NEO),
     .OCAREA(NEO),ORATE(NEO),IDIR(NEO),IOPRNT(NEO),IOINV(NEO)
CIM END     
``` 
