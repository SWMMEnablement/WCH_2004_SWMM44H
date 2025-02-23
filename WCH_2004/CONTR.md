```fortran 
C     EXTRAN BLOCK
C#### WCH, 4/11/94.  ADD IDATZ AND REARRANGE ORDER.
      COMMON/CONTR/XTIME,DELT,DELT2,TIME,TIME2,RDELT,IDATZ,MJSW,JSLOT,
     1             NJ,NC,NTC,NTL,ICYC,NJSW,METRIC,
     2             NTCYC,ISOL,MSUR,NSTOP,MTIME,MURGE,KSUPER,NEQUAL,
     3             JDOWN,GRVT,FUDGE,ALPHA1,ALPHA2
      CHARACTER*80 ALPHA1,ALPHA2
      REAL         DELT,TIME,RDELT,DELT2,TIME2
Cwch, 7/20/04. Add double precision XTIME for Extran time keeping.
      REAL*8 XTIME
C ### RHF 12/20/96
      # CONTR.md

      This document contains Fortran common block definitions used for simulation configuration and time/routing management in the legacy SWMM software.

      ## Complete Markdown Documentation and Extensive Summary

      This document provides a comprehensive markdown transformation of the original Fortran source file used in legacy SWMM software. It includes detailed descriptions of each common block, variable definitions, and purpose within simulation configuration and execution.

      ### Table of Contents
      - [Overview](#overview)
      - [Common Block Definitions](#common-block-definitions)
            - [EXTRAN Block](#extran-block)
            - [Routing Blocks: ROUTE4 and ROUTE4A](#routing-blocks)
            - [Additional Routing and Drawdown Blocks: ROUTE5 and ROUTE6](#additional-routing-and-drawdown-blocks)
      - [Complete Fortran Code](#complete-fortran-code)
      - [Summary](#summary)

      ### Overview

      This file contains Fortran code defining a set of common blocks that enable shared variable access across various modules in the SWMM simulation software. The common blocks provide critical parameters for:
      - Maintaining simulation time and scheduling.
      - Managing simulation control flags and time increments.
      - Controlling advanced routing, including drawdown, flow tolerances, and optional weights.

      ### Common Block Definitions

      #### EXTRAN Block

      - **Purpose:**  
            The EXTRAN block sets up variables for general simulation control, including timekeeping and configuration flags.
            
      - **Key Variables:**
            - `XTIME`: A double-precision variable for high-accuracy time tracking.
            - `DELT`, `DELT2`, `TIME`, `TIME2`, `RDELT`: Real variables managing time increments.
            - Control variables such as `IDATZ`, `MJSW`, `JSLOT`, `NJ`, `NC`, `NTC`, etc., that structure the simulation flow.
            - `ALPHA1` and `ALPHA2`: Character arrays (80 characters) that may store parameter labels or additional configuration details.

      #### Routing Blocks

      - **COMMON /ROUTE4/ Block:**
            - **Purpose:**  
                  Handles routing tolerance settings which are crucial for the simulation's flow calculations.
            - **Key Variables:**  
                  `TOLCS1`, `TOLCS2` (tolerances), `QLOWCS` (flow threshold), and `KREDO` (control flag).

      - **COMMON /ROUTE4A/ Block:**
            - **Purpose:**  
                  Extends routing setups by managing new input parameters for advanced routing configurations.
            - **Key Variables:**  
                  Includes `NOROUT`, `NEWQIN`, `NEWQTAPE`, `NEWQCARD`, and `ISOLSKIP` for adjusting routing conditions.

      #### Additional Routing and Drawdown Blocks

      - **COMMON /ROUTE5/ Block:**
            - **Purpose:**  
                  Specifically handles variables related to drawdown conditions, particularly for M1 scenarios.
            - **Key Variables:**  
                  Generally dynamically defined variables such as `IM2` related to drawdown correction.

      - **COMMON /ROUTE6/ Block:**
            - **Purpose:**  
                  Introduces optional weights in simulation for parameters such as head and additional flow considerations.
            - **Key Variables:**
                  - Weight arrays: `WUP(3)`, `WMD(3)`, `WDN(3)`
                  - Additional parameters: `NAVER` and `NFASNH` for weight averaging and other optional settings.

      ### Complete Fortran Code

      Below is the complete Fortran code as embedded in the markdown document:

      ```fortran
      C     EXTRAN BLOCK
      C#### WCH, 4/11/94.  ADD IDATZ AND REARRANGE ORDER.
                                     COMMON/CONTR/XTIME,DELT,DELT2,TIME,TIME2,RDELT,IDATZ,MJSW,JSLOT,
                        1             NJ,NC,NTC,NTL,ICYC,NJSW,METRIC,
                        2             NTCYC,ISOL,MSUR,NSTOP,MTIME,MURGE,KSUPER,NEQUAL,
                        3             JDOWN,GRVT,FUDGE,ALPHA1,ALPHA2
                                     CHARACTER*80 ALPHA1,ALPHA2
                                     REAL         DELT,TIME,RDELT,DELT2,TIME2
      Cwch, 7/20/04. Add double precision XTIME for Extran time keeping.
                                     REAL*8 XTIME
      C ### RHF 12/20/96

      ----------------------------------------------
      COMMON /ROUTE4/ TOLCS1,TOLCS2,QLOWCS,KREDO
                                     COMMON /ROUTE4A/ NOROUT,NEWQIN,NEWQTAPE,NEWQCARD,ISOLSKIP
      ----------------------------------------------
      c  additional variables for handling of M1 drawdown condition
                                     COMMON /ROUTE5/ IM2
      Cwch, 7/23/04. Add variables for optional weights in NHEAD.
                                     COMMON /ROUTE6/ WUP(3),WMD(3),WDN(3),NAVER,NFASNH
      ```

      ### Summary

      This extensive summary and transformation into markdown format provides both a structured explanation and the complete source code listing. Key aspects covered include:

      - A detailed breakdown of how each common block is leveraged:
            - EXTRAN for time control and simulation settings.
            - Route blocks (ROUTE4 / ROUTE4A) for advanced flow and tolerance management.
            - ROUTE5/ROUTE6 for specialized handling of drawdown and weighting configurations.
            
      - Clear demarcation of code sections, allowing easier navigation and maintenance of legacy Fortran code within the SWMM simulation framework.

      This markdown serves as both a reference and documentation for developers working on maintaining or updating the simulation software.

      The file defines multiple common blocks used for:
      - **Time Keeping and Control Parameters:** The `/CONTR/` block includes variables for simulation time, time increments, and various control parameters.
      - **Routing and Drawdown Handling:** Blocks such as `/ROUTE5/` and `/ROUTE6/` include variables for routing, drawdown conditions, and optional weight factors.
      - **Extended Routing Settings:** The additional routing common blocks `/ROUTE4/` and `/ROUTE4A/` help manage refined routing conditions, including threshold limits and new input configurations.

      ## Detailed Analysis

      ### 1. The EXTRAN Block

      - **Purpose:**  
            Defines shared variables used for simulation control — including time keeping, control flags, and cycle settings.

      - **Key Variables:**  
            - `XTIME` is defined with double precision for higher accuracy in time tracking.
            - `IDATZ`, `MJSW`, `JSLOT`, and several cycle and routing flags manage program flow.

      ### 2. Additional Routing Blocks

      The additional routing configuration is introduced later and includes the following two sections:

      - **COMMON /ROUTE4/ Block:**  
            Declares variables such as `TOLCS1`, `TOLCS2`, `QLOWCS`, and `KREDO` for handling specific routing tolerances and flow controls.

      - **COMMON /ROUTE4A/ Block:**  
            Declares variables such as `NOROUT`, `NEWQIN`, `NEWQTAPE`, `NEWQCARD`, and `ISOLSKIP` for advanced routing input management.

      ### 3. Other Common Blocks

      - **COMMON /ROUTE5/ Block:**  
            Manages variables for scenarios such as handling M1 drawdown conditions.

      - **COMMON /ROUTE6/ Block:**  
            Contains optional weight variables (`WUP`, `WMD`, `WDN`) and additional parameters (`NAVER`, `NFASNH`).

      ## Complete Fortran Code in Markdown

      ```fortran
      C     EXTRAN BLOCK
      C#### WCH, 4/11/94.  ADD IDATZ AND REARRANGE ORDER.
                        COMMON/CONTR/XTIME,DELT,DELT2,TIME,TIME2,RDELT,IDATZ,MJSW,JSLOT,
                   1             NJ,NC,NTC,NTL,ICYC,NJSW,METRIC,
                   2             NTCYC,ISOL,MSUR,NSTOP,MTIME,MURGE,KSUPER,NEQUAL,
                   3             JDOWN,GRVT,FUDGE,ALPHA1,ALPHA2
                        CHARACTER*80 ALPHA1,ALPHA2
                        REAL         DELT,TIME,RDELT,DELT2,TIME2
      Cwch, 7/20/04. Add double precision XTIME for Extran time keeping.
                        REAL*8 XTIME
      C ### RHF 12/20/96
      ----------------------------------------------
      COMMON /ROUTE4/ TOLCS1,TOLCS2,QLOWCS,KREDO
                        COMMON /ROUTE4A/ NOROUT,NEWQIN,NEWQTAPE,NEWQCARD,ISOLSKIP
      ----------------------------------------------
      c  additional variables for handling of M1 drawdown condition
                        COMMON /ROUTE5/ IM2
      Cwch, 7/23/04. Add variables for optional weights in NHEAD.
                        COMMON /ROUTE6/ WUP(3),WMD(3),WDN(3),NAVER,NFASNH
      ```

      ## Summary

      This rewritten markdown version transforms the original Fortran file into a structured document with an extensive summary. Key updates include:
      - Detailed explanation of common block purposes.
      - Clear differentiation between the original EXTRAN block and additional routing blocks.
      - Inclusion of extended routing configurations (`/ROUTE4/` and `/ROUTE4A/`) that manage additional simulation inputs and tolerances.

      # End of Document
c  additional variables for handling of M1 drawdown condition
      COMMON /ROUTE5/ IM2
Cwch, 7/23/04. Add variables for optional weights in NHEAD.
      COMMON /ROUTE6/ WUP(3),WMD(3),WDN(3),NAVER,NFASNH


``` 
