# WCH_2004_SWMM44H
 swmm 4.4.H code 

# This file documents changes made to swmm44gu by Wayne Huber or others 
# since its first release on September 28, 1999.

WCH = Wayne Huber
RED = Robert Dickinson
CIM = Chuck Moore
MCH = Mitch Heineman
LR  = Lew Rossman

# SWMM44GU Changelog Document

This document details the evolution of the SWMM44GU code since its initial release on September 28, 1999. Over the years, numerous modifications have been made by various contributors to improve functionality, correctness, computational precision, and compatibility with additional features (including metric conversions, interface flexibility, and error handling).

## Authors and Contributors

- **WCH**: Wayne Huber
- **RED**: Robert Dickinson
- **CIM**: Chuck Moore
- **MCH**: Mitch Heineman
- **LR  = Lew Rossman

## Summary of Changes

1. **Initial Corrections and Adjustments**  
    - Early modifications rectified formatting issues, adjusted statements, and fixed errors in subroutines (e.g., Yroute corrections and format changes) to enhance code operation.

2. **Subroutine Enhancements**  
    - Revisions to subroutines such as `CATCH`, `GETCUR`, `GROUND`, and `GUTNR` were implemented to refine error checking, improve loop logic, and ensure proper handling of numerical edge cases. 
    - Added checks (or removed them when unnecessary) to handle operations like raising zero to a power, protecting against runtime errors.

3. **Improved Input/Output and User Interaction**  
    - Updates were made to input routines (e.g., INDAT1, INDAT2) to handle optional parameters and support new kinds of data (like pump on/off options).
    - Enhancements in output routines (as seen in OUTPUT.FOR) address issues like NAN results, layout correction, and inclusion of extra summary details for clarity in printouts.

4. **Metric and Unit Conversions**  
    - As the tool evolved, metric conversion became a key focus. Several changes ensure correct unit conversions—especially in subroutines involving rainfall, channel geometry, and groundwater computations.
    - Changes include corrections in the calculation of flow and adjustments using updated conversion factors.

5. **Interface and Formatting Improvements**  
    - Error messages were augmented and adjusted (e.g., concerning incompatible alphanumeric options) to help users identify input mistakes.
    - Uniform default starting dates and other formatting tweaks were introduced to maintain consistency across different simulation blocks.

6. **Additional Functionalities**  
    - New options were added to support overland flow quantity and quality routing between channels and landscapes.
    - Further modifications allow for detailed spatial weighting and the inclusion of new features like statistical output for transport subroutines.

7. **Bug Fixes and Stability Enhancements**  
    - Numerous bugs—from subscript errors to logical flaws in summations and indexing—were fixed throughout the document’s change items.
    - Corrections ensure proper handling of multiple rain gages, consistent variable usage, and stable execution even for complex model configurations.

8. **Backward Compatibility and Legacy Updates**  
    - Many changes were made while retaining backwards compatibility with earlier versions.
    - Adjustments include reinstating certain options and reordering code segments to support legacy input formats while introducing new capabilities.

## Detailed Change History

The file contains a numbered list of 112 change items detailing:

- Specific corrections in subroutine operations (e.g., in GETCUR, INDAT1, RDIIRES).
- Format updates for improved data presentation (e.g., revised field widths, adjusted output formats).
- Extended capabilities including:
  - Enhanced error reporting (with detailed messages for users).
  - Additional functionalities in overland and channel routing.
  - New subroutines and updates to existing ones to address evolving simulation requirements.
  
Highlights include early fixes addressing basic errors, mid-term refinements enhancing the user interface and metric conversions, and later additions that improve simulation accuracy, performance, and stability for both hydraulic and quality routing.

## Modified Selection Placeholder

The modified selection placeholder now reads as follows:


1. 10/11/99, C. Moore.  Yroute: Correct C. Moore correction at line 118 
to make akon = -1.0000001

The next changes, 2-13, were made about 10/15/99 by Chuck Moore:

2.  Subroutine CATCH around line 90.  Change loop from 900 to NW.

3. In GETCUR, you put in a check for raising zero to a power around line 443.  This check is not needed if you do not turn the Fortran runtime power operations project setting on.  This gives error if you raise a zero to a zero or a negative real number to a real power (e.g.:  -2.3**2.0 is not valid but -2.3**2 is)  I think that there is no need to use this switch.

4. In GROUND near line 54 you put in checks that are not needed if the
power operations project setting run time error is not used.

5. In GUTNR at two locations near line 171 and 179 you put in two checks 
that are not need if power operations check is not used.

6. I made some minor changes to INDAT1 that you should probably include..  
A comment near line 1756, tests for zu less than z at line 1934 and 1963.  
Change to format number 5270 and 5271 and added formats 8050 and 8055 to 
provide error statement for negative zp.  

7. INDAT2 near lines 1127 and 1140.  I added option for reading pon and 
poff for type 4 pumps but have commented them out in my version due to
incompatibility issues with MIKE.  These have been tested and I think 
that these should be in your version.  See documentation in 
the EXTRAN44.DOC file.

8. In OUTPUT.FOR near line 48.  We found that models that had both input 
from unformatted sequential and from K_ line produced a NAN in the 
continuity summary.  NAN stands for Not-a-number.  From the online 
reference "Not a Number (NaN) results from an operation involving 
one or more invalid operands. For instance 0/0 and SQRT ( - 1) result 
in NaN. In general, an operation involving a NaN produces another NaN."  
The check in OUTPUT is a patch that seems to work.  I didn't have time to 
go back an trace where exactly the NAN is occurring in the computations.  
Also have a check for case where node has a positive QQI and negative QOU.  
This occurs when you are loading a storage junction that is also 
overflowing and results in large continuity errors.  Again this is a 
patch for some other problem somewhere in the bowels of the code.  Best 
way to prevent this is to not load storage nodes that surcharge to land 
surface elevations.

9. QSHED near line 197 you put in check for power operation that is not
needed.

10.  RDIIRES.  I changed format on statements 7010 and 7011 to make things 
line up. (28X to 32X). This was needed because I changed width of 
subcatchment ID's in first line of statement.

11. RHYDRO1 near line 332.  Changed loop from 900 to NG. Yes people are
running models with more than 9000 catchments and conduits!

12. STRIP near 72.  Added zeros to end of H1 lines in EXTRAN.  Used for
optional input of JON and JOFF for pump type 4.  (see comments 7 above).

13. wrtflow.for.  Changed format statement 7000 to prevent wrapping.

---------

14. HCURVE, change for correct printing of rainfall hyetograph over
long time periods.  WCH, 11/10/99

15. GTRAIN. Correct Format statements for IFORM = 0 and 1 to read 4-digit
dates and ensure Y2K compatibility.  WCH, 11/10/99

16. Increase field with for DT printout in INTRAN.  WCH, 11/11/99.

17. Increase field with for 9200 Format in STRT.  WCH, 11/11/99.

18. Allow negative IDATZ on Extran B1 line (INDAT1 and INDAT3) to force 
this date to be used even with interface file input.  WCH, 11/12/99.

19. Changes in RAIN and GTRAIN to provide for current Canadian 
precipitation formats.  WCH, 11/22/99.

20. Add additional summary information from printouts of rainfall data 
in Rain Block.  WCH, 11/23/99.

21. Minor format change in RAIN.FOR, and delete extraneous line.  
WCH, 3/27/00.

22. In BOUND, define KW at Stmt 320.  WCH (CIM), 3/27/00.

23. In INDAT2, correct weir data print-out.  WCH (CIM), 3/27/00.

24. In RUNOFF.FOR, fix subscript error during initialization.  WCH, 3/27/00.

24. Create uniform default starting date of 19410802 (YearMoDay) in all 
blocks.  Will be used if user inputs zeros for any date.  WCH, 3/27/00.

25. Metric corrections in BRIDGES print-outs, WCH, 3/28/00.

26. Fix metric conversion and other typo involving gravity in INDAT1, 
for computation of equivalent roughness.  WCH, 3/28/00.  (This typo found 
by swmm-user Junshan Su.)  

27. Fix metric rainfall conversion problem with Runoff Block RDII routine 
(Sub. RDIIRES).  WCH, 3/28/00.

28. Let Temp Block read 4-digit years from NWS data (Sub. GTCOLD).
WCH, 3/28/00.

29. Declare TIME as double precision in GAMP, for consistency.  WCH, 4/5/00.

30. Fix subscript initialization error in RUNOFF.  WCH, 4/5/00.

31. Fix format range overflows in ASCRESA.FOR.  CIM, 4/10/00.

32. Use updated INDAT2.FOR from Chuck Moore with pump-on, pump-off 
option and check for model with no boundaries.  CIM, 4/10/00.

33. Fix variable LONG on Runoff line B3 to read last digit of 
year-month-day format.  WCH, 4/11/00.

34. Fix metric conversion for type 23 weir element in Transport 
(Sub INTRAN).  But use of weir element is discouraged.  Use general 
Type 26 rating curve flow divider instead.  WCH, 8/10/00.  

35. Minor corrections in Transport Sub. CIRCLE to improve convergence
(courtesy Sandy Elliot, NIWAR, New Zealand) and typo in Runoff Sub. GUTTER
(courtesy Andy Rowen, Rutgers Univ.).  WCH, 8/10/00.

36. Fix minor typos in SLOP and WRTFLOW that apparently have not affected
execution.  (Courtesy Prof. Jehng-Jung Kao, National Chiao Tung Univ., 
Taiwan)  WCH, 8/15/00

37. Input error checks in Trans Block Sub. INTRAN in case of missing
constant pollutant concentrations in E1 lines and missing pollutant
info in F1 lines.  WCH, 8/29/00.

38. Changes to output format of TRANS.FOR to enhance readability of output
from hydraulic design routine.  WCH, 8/29/00.

39. Fix output error for headings in Runoff Sub. PRFLOW.  WCH, 9/7/00.

40. Correct array initialization in Extran Sub. TRANSX to get correct
continuity check when using hotstart.  WCH from RED, 9/8/00.

41. Metricate header and add column headers for Extran ASCII output
file for channel flows.  WCH, 9/8/00.

**The following four changes (42-45) were supplied by Chuck Moore, 
CDM, 9/8/00.  Additional documentation is in the Word file 
CDM9-8-99.DOC, included in CDM-DOC.ZIP.  .  

42. TRANSPORT IRREGULAR SECTIONS :   By default, TRANSPORT subdivides 
sections into 25 equal-area segments. For irregular sections that 
include large overbank areas, the lowermost equal area-segment can 
include the entire incised stream channel.  An option was added to 
break the lowermost equal-area segment into 25 additional segments.  
This option is invoked by IDETAIL on B0 input line.  This is 
documented in TRANS.DOC.  Most useful if you are driving WASP 
from TRANSPORT or are using TRANSPORT velocities to evaluate 
stream velocities and scour potential.  

43. TRANSPORT DEPTH AND VELOCITY:  Add option to have TRANSPORT 
output head and velocity to the interface file for statistical 
analysis.  Controlled by negative NPOLL on B1 line.   Documented 
in TRANS.DOC.

44.	EXTRAN CRITICAL ELEVATION COMPARISON:  Add optional D2 and D3 
line to read elevations against which junction elevations will be 
compared and reported.  Documented in EXTRAN.DOC

45.	RDII METRICATION:  Changes to make RDII calculations and 
continuity check correct when working in metric units.  I notice 
that Dr. Huber did some changes also.  In this version area remains 
in hectares and rainfall in mm.  Flows are computed in CFS.  
I'm pretty sure that my changes work.  Continuity error check is 
correct also.

------

46. Change Sub. STRIP to leave numbers that begin in first column.  
Necessary for some Graph Block input, and useful for wrap-around data for 
all blocks.  WCH, 9/14/00.

47. Change Sub. GRAPH to correct metric error and add error message.
WCH, 9/14/00

48. Fix Sub. GRAPH to read input from text file MFILE, to read data
in free-format form, and to read lines as indicated in .DOC file.
RED and WCH, 9/16/00.

49. Add overland flow quantity and quality routing from one plane to
another.  Several Runoff subroutines, plus new Sub SHEDQUAL.  
WCH, 12/20/00.

50. Summarize Runoff quality loads by land use as option.  Mainly 
Sub. PRPOLL.  WCH, 12/20/00.

51. Add Runoff option not to print intermediate headers every 50 lines,
Subs. PRFLOW and PRPOLL.  WCH, 12/20/00.

52. Minor formating and logic fixes in Runoff Sub. QHYDRO.  WCH,
1/4/01 and 1/18/01.

53. Correct imperviou subarea statistics in Runoff Sub. WSHED.  
WCH, 1/17/01.

54. Minor spacing changes in Runoff Subs. RHYDRO1 and GUTTER.  
WCH, 1/18/01.

55. Correct Extran Sub. INDAT1 for initial date.  WCH (RED), 2/4/01.

56. Add error message for too-high initial depth in Runoff channel/pipes, 
Sub. RHYDRO1.  WCH, 2/7/01.

57. Initialize some variables in Subs. RAIN and GTRAIN.  WCH 
(Nerkez Gavranovic), 2/9/01.

58. Correct Extran Sub. OUTPUT to plot upstream/downstream heads
and water surface slope.  WCH (Nerkez Gavranovic), 2/9/01.

59. Add decay and BMP fraction removal to Runoff channel/pipes.  Subs.
GQUAL and QHYDRO.  WCH, 2/10,01.

60. Metric correction in Runoff for groundwater continuity check, Sub.
HYDRO.  WCH (Sandy Elliot, NIWA, New Zealand), 2/10/01.

61. Check for same NAMEG and NGTO on Runoff G1 line, Sub. RHYDRO1.
WCH, 2/12/01. 

62. Metric conversion from standard U.S. sizes for Extran conduit types
9, 10 and 11.  Sub. INDAT1 and additional conversions in array CMET, 
initialized in BLKMN.  WCH and RED, 2/12/01. 

63. Make Transport time variables double precision in attempt to avoid
errors for long simulations, e.g., in reading R1 lines.  WCH, 2/15/01.

63. Add metric headers to Extran bridge output, Sub. BRIDGES.  WCH, 2/15/01.

64. Change version name to SWMM44H.EXE.  WCH and Chuck Moore, 2/21/01.

65. Add error message to EXTRAN Sub. INDAT3, related to NTIDE = 4.
WCH, 3/26/01.  

66. Fix limit on number of L1-L2 lines in QHYDRO.  RED, 5/6/01.

67. Fix time step problem when combining files with variable time step
in COMBIN.  RED, 5/6/01.

68. Fix error message in TIDEFILE.FOR that caused gargantuan output
files.  RED, 5/6/01. 

69. In Extran Sub INDAT3, also use user-supplied initial starting time
on B1 line if IDATZ < 0.  RED, 5/7/01.

70. Correct Runoff Subs. QSHED and WSHED to use correct rainfall and 
melt for subcatchment with erosion, in event of multiple rain gages. 
Error noted by Ching L. Chen.  WCH, 5/8/01. 

71. Raise limit on number of subcatchment numbers for snowmelt input
in Runoff Sub. SNOWIN.  RED, 5/20/01.

72. Close scratch files when exiting Runoff, in Sub. RUNOFF.  This
avoids wiping out RAIN and TEMP interface files if Transport or
Extran runs follow.  RED, 5/21/01.

73. Check for zero subarea areas when rerouting overland flow, in 
Runoff Sub. WSHED.  WCH, 6/7/01. 

74. Error in hydraulic radius calcuation for trapezoidal channels in
Transport, Sub. RADH.  Old R-values likely too high.  WCH, 7/11/01. 

75. Increase field widths in Transport output to accomodate output of 
more than 1,000,000 time steps.  Sub PRINTF.  WCH, 7/26/01

76. Provide for no intermediate headers in tabular Transport output of 
hydrographs.  Subs PRINTF and INTRAN.  WCH, 10/3/01

77. Increase field width to 10 for integer IDs for printout of connecting
conduits for table of junction data in Extran.  Sub INDAT1.  WCH, 10/30/01

78. Added error message to Extran Sub. SEDEPTH re. need for ZP at both
ends of pipe when using sediment depth.  Chuck Moore, 11/1/01. 

79. Fix metric conversion for groundwater output, Sub. WSHED.  Sandy 
Elliot and Bob Dickinson, 3/1/02. 

80. Fix metric conversion for groundwater input, Sub. Catch.  Parameters
A1 and A2 were incorrectly converted and were too low by a factor of 
3.28^2B  = ~10^B when metric input was used.  Sandy Elliot, 3/13/02. 

81. Fix metric conversion in Sub GETCUR affecting metric input of natural
channels from Transport.  WCH (Sandy Elliot motivation), 3/14/02.

82. Linked DO-BOD-NOD quality routing in Transport, begun 7/6/01 but
not tested until March 2002.  WCH.  Testing still incomplete.  
Documentation is in new TRANSWQ.DOC.  New subroutines QUALSOLN, QUALPARM, 
INTRQUAL, and REAERATE.  Also, accompanying program TESTK2.EXE to test 
various reaeration options.  Documentation is in file TESTK2.DAT, included
with TRANSWQ.ZIP. 

83. Include depth in print of initial channel conditions in Transport.
Modify Sub. INITAL.  WCH, 3/21/02. 

84. Fix continuity check for subcatchment to subcatchment overland flow
rerouting.  Make subcatchment summary statistics correct.  Subs.
WSHED, WSTAT and maybe elsewhere.  WCH, 3/28/02.

85. Fix error message re. L2 lines and slightly alter constituent field
widths for printout in QHYDRO.  WCH, 3/29/02.

86. Fix Green-Ampt subroutine GAMP for overland flow rerouting.  Formerly
would not allow infiltration greater than the cumulative rainfall. Also 
minor related corrections to Sub. WSHED.  WCH, 4/11/02.

87. Changed header for ascii (text) file of rainfall interface file. Prints
headers correctly for multiple rain gages.  Sub. COMBIN1.  WCH, 4/11/02.

88. Fix subcatchmentment stats so rainfall printed for each subcatchment
does not include rainfall prior to start of storm.  Sub HYDRO, plus minor
format change to Sub WSTAT.  WCH, 4/15/02.

89. Add Transport error message (Sub INTRAN) re. missing value of KGEOM
at end of E1 lines.  WCH, 4/15/02.

90. Change Sub GETCUR to use default natural channel slope = 0.01 if zero 
entered. An error message is printed so user can change to desired slope 
if need be.  Affects Extran and Transport.  WCH, 4/16/02.

91. Add error message for incompatible alphanumeric option ($ANUM) between
blocks.  Subs. INTRAN, INDAT3, STRT.  WCH, 4/18/02.

92. Allow override of interface file starting date/time (use values from
B1/B2 lines) in Transport. Sub INTRANS.  WCH, 4/18/02. 

93. Reinstate correction number 33, re. date entry in Runoff.  WCH,
5/1/02.

94. Add error check for missing G2 lines for Transport storage units. Sub. 
TSTRDT.  WCH, 5/2/02.

95. Fix unlikely but maybe possible errors in searching through tabular
Q-A data in Transport.  Subs. PSI and FINDA.  WCH (Lew Rossman), 5/24/02. 

96. Correct groundwater input errors (related to sequencing of GW H2-H4 
lines among the subcatchment input stream) and add additional GW time
series output (inflow, outflow, ET) to time-step output.  WCH, 7/1/03.

97. Fix minimum equivalent orifice length in Extran (Sub. INFLOW) to be
200 ft or 200/3.28 m.  WCH, 7/2/03.

98. Fix S/T interpolation routines off interface file for too many
metric conversions for pollutant loads.  WCH, 11/24/03.

99. Fix STATS computation of average flow for units of depth/time.  
WCH, 11/24/03.  Otherwise need to multiply average flows by 3600 sec/hr 
when requesting output in depth/time. 

100. Fix index in Sub ROUTE related to using correct QCURVE2 array for
low-flow calcs in power and natural channels in Transport.  
CIM, 7/20/04.

101. Fix format for Transport output for natural and power channels in
Sub GETCUR. CIM, 7/20/04. 

102. Increase loop size in Sub COMBIN to allow ten million time steps 
to be read from files instead of one million.  WCH, 7/20/04.

103. Fix Sub STRIP so that its logic doesn't fail in padding some lines
with zeros when running Runoff and Transport Blocks in one, single run. 
CIM and MCH, 7/20/04. 

104. Add option to not print status changes between steady and 
non-steady flow routing in Extran.  Subs. TRANSX, INFLOW, INDAT1.
MCH, 7/20/04. 

105. Fix concrete arch pipe area calc for Extran, Sub INDAT1, in
accordance with Mike Gregory spreadsheet sent to swmm-users, 
10/10/03. Area ~ 0.7879*DEEP*WIDE.  WCH (CIM, RED), 7/20/04.

106. Fix inverted definition of DEEP and WIDE for Extran vertical
elliptical pipes read from table, Sub INDAT1. WCH (CIM, RED), 7/20/04. 

107. Fix second DELQ4 calculation in Extran Sub. YROUTE (ISOL =
1 & 4). May have effect of requiring smaller time step than before.  
WCH (RED), 7/20/04. 

108. For Extran case when downstream junction is critical or ,
supercritical add all surface area to upstream junction, not 
just half. Sub. NHEAD. WCH (RED), 7/20/04.  

109. For Extran subcritical flow with downstream junction near
critical, if not all surface area of conduit is added to
downstream junction (variable FASNH<1), add remaining area to
upstream node. Sub. NHEAD.  User option on new AA line.  WCH, 7/20/04.

110. Add double precision time variable (XTIME) for Extran time
keeping in Sub. TRANSX and CONTR.INC.  WCH from RED, 7/20/04. 

111. Fix Extran Subs. INDAT2 and INFLOW to allow for proper sequencing
of orifice F2 lines.  Can now be placed whenever variable area orifice
occurs, not just with first F1 lines.  RED (WCH), 7/22/04. 

112. Add Extran option for spatial weighting of channel/conduit
geometric parameters on new Line AA (Subs. INDAT1 and NHEAD), for
backwards and forwards compatibility.  WCH (RED), 7/23/04. 





