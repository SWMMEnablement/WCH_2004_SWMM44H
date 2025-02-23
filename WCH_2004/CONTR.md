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

      ## Overview

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
