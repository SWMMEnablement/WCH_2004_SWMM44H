      CHARACTER SCF*4,KINNUM*10,BORDER*10,KYN*10,KPE*10,KJN*10,
     +                                  KUE*10,KOE*10,KGEOM*10
CIMT
CIMT  Changes made by C. Moore to increase number of constituents in
CIMT  Transport
cIMT   dimensions changed from 4 to MQUAL for appropriate variables
cIMT       4/99
C    Note, value of NQP = NET in Tapes.inc. 
CIM  NOTE XNT HAS DIMENSIONS OF 10+6*MQUAL
      DOUBLE PRECISION XNT
      COMMON/HUGO1/A(NET,2,2),Q(NET,2,2)
      COMMON/HUGO2/CPOL1(NQP,2,MQUAL),CPOL2(NQP,2,MQUAL)
      COMMON/HUGO3/TIME,QFULL(NET),AFULL(NET),DXDT(NET),C1(NET),
     1       SLOPE(NET),DIST(NET),GEOM1(NET),ROUGH(NET),DWDAYS,EPSIL,DT
Cwch, 2/15/01.  Make TIME double precision
      REAL*8 TIME
      COMMON/HUGO4/QDWF(NET),P1(NET),RNOFF(NET),QINFIL(NET),
     1       WDWF(NET,MQUAL),PLUTO(MQUAL,NET),P2(NET),P5(NET),P6(NET),
     2       P7(NET),BARREL(NET),GEOM2(NET),QMAX(NET),GEOM3(NET),
     3       P4(NET),SCOUR(NET,MQUAL),XNT(10+6*MQUAL)
      COMMON/HUGO5/KSTORE(NET),NPE(NTOA),NYN(NTOA),NORDER(NET),
     1       JSURF(NTOA),
     1       NINNUM(NET),INUE(NET,3),NTYPE(NET),JR(NET),
     2       IOLD(NET),NOE(NET),NUE(NET,3),NNYN,NNPE,NSURF,NPLOT,JPRINT,
     3       NOUTS,INTPRT,NFILTH,NDESN,NCNTRL,NINFIL,KSPG,NINPUT,NKLASS,
# Technical Documentation for HUGO.md Fortran Code

This document provides a complete technical conversion of the original Fortran source code into Markdown. It preserves the structure, technical details, and comments from the original file.

## Table of Contents

1. [Introduction](#introduction)
2. [Common Blocks Overview](#common-blocks-overview)
      - [Detailed Code Listing](#detailed-code-listing)
3. [Reference Links](#reference-links)
4. [Appendices](#appendices)

---

## Introduction

This document presents the converted Fortran source from the HUGO.md file used in SWMM-related projects. The content includes technical details such as COMMON block declarations, variable descriptions, and historical comments. The structure below adheres to a clear header hierarchy with code blocks, tables, and collapsible sections to enhance navigation and readability.

---

## Common Blocks Overview

The following sections outline various COMMON blocks defined in the original code. A summary of each block and its key variables is provided in the table below.

### Summary Table of COMMON Blocks

| COMMON Block | Variables / Details |
| ------------ | ------------------- |
| HUGO1        | A(NET,2,2), Q(NET,2,2) |
| HUGO2        | CPOL1(NQP,2,MQUAL), CPOL2(NQP,2,MQUAL) |
| HUGO3        | TIME, QFULL(NET), AFULL(NET), DXDT(NET), C1(NET), SLOPE(NET), DIST(NET), GEOM1(NET), ROUGH(NET), DWDAYS, EPSIL, DT |
| HUGO4        | QDWF(NET), P1(NET), RNOFF(NET), QINFIL(NET), WDWF(NET,MQUAL), PLUTO(MQUAL,NET), P2(NET), P5(NET), P6(NET), P7(NET), BARREL(NET), GEOM2(NET), QMAX(NET), GEOM3(NET), P4(NET), SCOUR(NET,MQUAL), XNT(10+6*MQUAL) |
| HUGO5        | KSTORE(NET), NPE(NTOA), NYN(NTOA), NORDER(NET), JSURF(NTOA), NINNUM(NET), INUE(NET,3), NTYPE(NET), JR(NET), IOLD(NET), NOE(NET), NUE(NET,3), NNYN, NNPE, NSURF, NPLOT, JPRINT, NOUTS, INTPRT, NFILTH, NDESN, NCNTRL, NINFIL, KSPG, NINPUT, NKLASS, NE, NDT, KFULL, NOS, NPOLL, NPRINT, ITER, NITER, M, N, *[Additional block components]* |
| HUGO6        | KOE(NET), KUE(NET,3), KYN(NTOA), KINNUM(NET) *(inserted content)* |
| *(Continuation)* | KPE(NTOA), KJN(NTHO), BORDER(NTHR), KGEOM(NET), SCF(NET) |
| WASP         | ISUMRY |
| CIMHV        | NPOLL2 |
| HUGO7        | NOHEAD |

> Note: The `HUGO6` block is inserted at the placeholder and is critical to the maintenance of technical accuracy.

---

### Detailed Code Listing

Below is the complete Fortran code with the technical details preserved. Each section is wrapped in a code block with Fortran syntax highlighting.

<details open>
  <summary>Click to expand the full code</summary>

```fortran
        CHARACTER SCF*4,KINNUM*10,BORDER*10,KYN*10,KPE*10,KJN*10,
       +                                  KUE*10,KOE*10,KGEOM*10
CIMT
CIMT  Changes made by C. Moore to increase number of constituents in
CIMT  Transport
cIMT   dimensions changed from 4 to MQUAL for appropriate variables
cIMT       4/99
C    Note, value of NQP = NET in Tapes.inc. 
CIM  NOTE XNT HAS DIMENSIONS OF 10+6*MQUAL
        DOUBLE PRECISION XNT
        COMMON/HUGO1/A(NET,2,2),Q(NET,2,2)
        COMMON/HUGO2/CPOL1(NQP,2,MQUAL),CPOL2(NQP,2,MQUAL)
        COMMON/HUGO3/TIME,QFULL(NET),AFULL(NET),DXDT(NET),C1(NET),
       1       SLOPE(NET),DIST(NET),GEOM1(NET),ROUGH(NET),DWDAYS,EPSIL,DT
Cwch, 2/15/01.  Make TIME double precision
        REAL*8 TIME
        COMMON/HUGO4/QDWF(NET),P1(NET),RNOFF(NET),QINFIL(NET),
       1       WDWF(NET,MQUAL),PLUTO(MQUAL,NET),P2(NET),P5(NET),P6(NET),
       2       P7(NET),BARREL(NET),GEOM2(NET),QMAX(NET),GEOM3(NET),
       3       P4(NET),SCOUR(NET,MQUAL),XNT(10+6*MQUAL)
        COMMON/HUGO5/KSTORE(NET),NPE(NTOA),NYN(NTOA),NORDER(NET),
       1       JSURF(NTOA),
       1       NINNUM(NET),INUE(NET,3),NTYPE(NET),JR(NET),
       2       IOLD(NET),NOE(NET),NUE(NET,3),NNYN,NNPE,NSURF,NPLOT,JPRINT,
       3       NOUTS,INTPRT,NFILTH,NDESN,NCNTRL,NINFIL,KSPG,NINPUT,NKLASS,
       4       NE,NDT,KFULL,NOS,NPOLL,NPRINT,ITER,NITER,M,N
        COMMON/HUGO6/KOE(NET),KUE(NET,3),KYN(NTOA),KINNUM(NET),
       1       KPE(NTOA),KJN(NTHO),BORDER(NTHR),KGEOM(NET),SCF(NET)
C#### JLM, 10/93.  ADD FOR WASP LINKAGE.
        COMMON/WASP/ ISUMRY

CIMHV## 9/8/00 change to have depth and velocity output to interface.
        COMMON/CIMHV/ NPOLL2
CIMHV
Cwch, 10/3/01
        COMMON/HUGO7/ NOHEAD
```

</details>

---

## Reference Links

For further reading and context, consider the following references:

- [SWMM Project Overview][swmm]
- [Fortran Documentation][fortran-docs]

[swmm]: https://www.epa.gov/water-research/storm-water-management-model-swmm "SWMM Project"
[fortran-docs]: https://en.wikipedia.org/wiki/Fortran "Fortran on Wikipedia"

---

## Appendices

### Appendix A: Image Illustration

Below is an example image with descriptive alt text (for illustration purposes):

![Flow Diagram of SWMM Modules](https://via.placeholder.com/400 "Flow Diagram showing the interaction between various SWMM modules.")

### Appendix B: Section Navigation

- [Return to Top](#technical-documentation-for-hugomd-fortran-code)
- [Back to Table of Contents](#table-of-contents)

---

*End of Document*

     1       KPE(NTOA),KJN(NTHO),BORDER(NTHR),KGEOM(NET),SCF(NET)
C#### JLM, 10/93.  ADD FOR WASP LINKAGE.
      COMMON/WASP/ ISUMRY

CIMHV## 9/8/00 change to have depth and velocity output to interface.
      COMMON/CIMHV/ NPOLL2
CIMHV
Cwch, 10/3/01
      COMMON/HUGO7/ NOHEAD
 
