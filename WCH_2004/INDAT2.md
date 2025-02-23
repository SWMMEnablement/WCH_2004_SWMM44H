GitHub Copilot
Below is an extensive Markdown summary for the entire INDAT2.md file:

## Summary of INDAT2.md
This file contains the Fortran subroutine INDAT2 that is part of the extended transport (EXTRAN) module. Its primary purpose is to read and process input data groups (from E1 through I2) that describe hydraulic, storage, orifice, pump, weir, and outfall parameters. In addition, the subroutine performs necessary initializations, sets up node–conduit linkages (converted to an internal numbering system), and carries out extensive error checking. The file also embeds numerous format statements for output and error reporting. Below is a detailed breakdown of its content and structure.

1. General Purpose and Documentation
Purpose:

Reads data groups E1 to I2 (excluding hydrograph lines in "INFLOW").
Initializes variables and sets up internal representations for storage junctions, orifices, pumps, weirs, and outfalls.
Converts user input to an internal numbering system and carries out unit conversions and error checks.
Documentation and Revision History:

The file’s header comments explain updates made over the years (by WCH, RED, CIM, etc.), including improved error checking, adjustments to the power function volume calculation, and options for detailed input data printing.
2. Included Files and Common Block Setup
The subroutine begins by including several common-incidence files such as:
TAPES.INC
BD.INC
CONTR.INC
JUNC.INC
PIPE.INC
ORF.INC
WEIR.INC
BND.INC
OUT.INC
TIDE.INC
HYFLOW.INC
VOLDAT.INC
BE.INC (which contains JELEV among other parameters)
These inclusions supply standard constants, parameter values, and variable dimensions used throughout the subroutine.
3. Declarations and Variable Initialization
Character and Dimension Declarations:

Arrays such as ONAME, WNAME, PNAME, OUTF, and OUTG are dimensioned according to parameters defined in TAPES.INC.
Other variables include JTYPE (to classify junction types), logical flags (e.g., CIRCULAR), and additional temporary string variables.
Initialization of Name Arrays:

Loops assign default names to elements:
ONAME: Names for orifice storage junctions (formatted as "ORF #").
WNAME: Names for weir junctions.
PNAME: Names for pump nodes.
OUTF/OUTG: Default labels for free outfalls and gated outfalls.
A temporary string (TMPSTR) is used to convert integer indices to formatted names based on the number of digits.
4. Reading Optional Output Options
The subroutine first reads an optional E0 data line that, if present, sets a flag (NVSPR) to control whether detailed storage junction input data should be echoed to the output.
5. Reading Storage Junction Data (Data Group E1)
Data Reading and Error Checks:

The routine reads storage junction parameters including:
Junction identification (JSTORE or KSTORE).
Top elevations (ZTOP), stored area (ASTORE), and the number of stage–area data points (NUMST).
Volume Curve Construction:

The stage–area relationship is read into a 3D array (VCURVE) where:
The first index represents the junction.
Subsequent indices capture the stage, corresponding area (adjusted by a conversion coefficient QCOEF), and cumulative volume.
Integration is performed (using the trapezoidal rule) to build the volume curve from the area/stage data.
For cases where the junction is defined by a power function, a separate branch reads the coefficient and exponent for the power law relationship.
Data Correction and Warning:

The subroutine checks that the minimum stage area is valid (i.e. minimum depth is zero and area is positive) and corrects them if not.
If the data in the storage curve does not meet the criteria, warning messages are printed and error conditions are flagged.
6. Conversion to Internal Numbering System
After reading the vertex data for storage junctions, the subroutine converts junction identifiers from the external (user-specified) naming system to an internal numeric system.
It also sets additional parameters (like ZCROWN and GRELEV) based on the highest pipe crowns entering the junction.
Additional error checks ensure that every junction is properly connected to a pipe, pump, weir, or orifice.
7. Reading Orifice Data (Data Group F1 and Related)
The file handles reading of orifice data which may have:
Time History (F1):
Reads linking information between junctions.
Reads parameters like orifice area (AORIF), discharge coefficient (CORIF), and energy losses.
Orifices with Timed Closure and Gate Control (F3 and F4):
Special handling for gated orifices includes reading additional timing data, open/close elevations, and converting these elevation values relative to upstream junction invert elevation.
It makes adjustments if the orifice is rectangular (reading extra dimensions such as DEEPO and WIDEO) and checks if the computed area (or adjusted area) meets minimum criteria.
Negative control parameters are corrected (by taking absolute values) and appropriate flags (such as IOINV) are set for gate behavior.
Finally, the orifice data is written out for echo printing and further verified against consistency checks.
8. Reading Pump Data (Data Group H1)
Types of Pumps:

Pump types (IPTYP values) range from off-line pumps (with wet wells) to in-line lift pumps, head-dependent pumps, variable-speed pumps, and constant-speed lift stations.
The routine reads data specific to the pump type including:
Connected junctions,
Pump rates (PRATE),
Vertical discharge curves (VRATE),
Well volumes (VWELL),
For timed or head-based pumps, additional parameters like pump on/off depths (PON, POFF) and time delays (PONDELAY).
Unit and Elevation Conversions:

For systems where elevations are expressed relative to junction invert (JELEV = 4), the pump data (rates, and depth thresholds) are adjusted by subtracting the junction’s vertical offset.
Status Initialization:

The routine initializes the pump status arrays (e.g., IPOPR and TIMEON) and ensures that the number of pumps does not exceed program limits.
9. Reading Outfall Data
Free Outfalls (Data Group I1):

The subroutine reads “free” outfall data which do not have tide gates.
For each free outfall, junction data and boundary condition labels are read and the outfall is converted to an internal node.
Tide Gate Outfalls (Data Group I2):

Separately, the routine reads data for outfalls with tide gates.
It applies similar conversion procedures (from external identifiers to internal numbers) and checks for consistency.
Connectivity Checks:

After processing the outfall data, the subroutine verifies that all junctions that are supposed to have boundary conditions are correctly linked.
Additional error messages are produced if any junction is found without an associated conduit, pump, weir, or orifice.
10. Final Internal Connectivity and Error Reporting
Internal Number Conversion:

Nodes and structures are numbered sequentially to form the internal connectivity layout.
New conduit (or boundary) numbers are set (e.g., adding 90,000 for conduits) and connectivity arrays (NJUNC, KJUNC, NCHAN) are updated.
Error Conditions and Warnings:

The subroutine performs many checks:
Ensures that every junction is connected at least once.
Verifies the ordering of the discharge curves for pumps and the consistency of weir and orifice data.
Checks for negative parameter values (e.g., negative open/close depths) that might cause runtime errors.
Any errors are printed to the output unit (commonly N6) using formatted messages before the subroutine stops execution if needed.
11. Format Statements
The bottom of the file is dedicated to numerous FORMAT statements used for printing:
Detailed printouts of storage junction data.
Echo statements for orifice and pump data.
Error and warning messages are formatted with specific codes so users can identify the source of an error.
The format definitions provide clear labels on junction numbers, elevations, pump rates, conduit information, etc.
12. Overall Structure and Style
Modular Structure:

The subroutine is organized into several labeled blocks and loops that first read, then backspace (for lookahead or verification), and finally convert the data.
Extensive inline comments (with markers like “CIM”, “WCH”, “RED”) document the rationale behind changes and corrections.
Error Handling:

An error routine (invoked via CALL IERROR) is referenced to handle any unexpected issues during input processing.
The routine stops execution if critical errors (such as too many pumps or disconnected nodes) are detected.
Conversion and Consistency Checks:

Unit conversions (based on METRIC flags) are applied generously to ensure congruence between areas (acres vs. hectares), elevations (feet vs. meters), and flow rates.
Each section that reads a data group includes detailed checks so that the numerical inputs from the user are consistent with the simulation’s internal requirements.
Conclusion
The INDAT2 subroutine is a comprehensive and complex module that:

Reads a wide range of user input covering storage nodes, orifices, pumps, and outfalls.
Properly initializes and converts external data into an internal, consistent numbering system.
Implements thorough error checking and unit conversions.
Provides extensive formatted output for both debugging and verification purposes.
This extensive processing is critical for the subsequent simulation steps in the extended transport model, ensuring that all hydraulic components are properly interconnected and numerically consistent before the simulation proceeds.