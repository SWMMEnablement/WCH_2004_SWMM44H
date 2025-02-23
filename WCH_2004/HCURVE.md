```fortran 
      SUBROUTINE HCURVE(NTYPE)
C     GRAPH RUNOFF BLOCK
C     CALLED FROM RUNOFF NEAR LINES 265 through 269
C=======================================================================
C     HCURVE WAS LAST UPDATED BY THE UNIVERSITY OF FLORIDA JANUARY, 1989
C     DIMENSION MODIFICATION, DELETED LOOP, AND NEW PRINT, 12/92 BY WCH
# Fortran Subroutine HCURVE Overview

This file contains the Fortran subroutine `HCURVE`, which is responsible for plotting various hydrological curves. The subroutine is designed to handle different types of hydrographs such as rainfall hyetographs, inlet hydrographs, infiltration rates, and subsurface flows. It contains several sections that are activated based on the input parameter `NTYPE`.

---

## Key Components

- **Purpose**:  
      Plots and processes hydrological data, including rainfall measurements, infiltration, and subsurface flows.

- **Includes**:  
      The subroutine makes use of multiple include files:
      - `TAPES.INC`
      - `STIMER.INC`
      - `TIMER.INC`
      - `INTER.INC`
      - `DETAIL.INC`
      - `GRWTR.INC`
      - `LAB.INC`

- **Dimensioning and Variables**:
      - Arrays such as `X`, `Y`, and `Y1` are used to store coordinate and data values for plotting.
      - Other arrays like `GWFLWB`, `STPOLL`, and `JSTA` manage groundwater and time series data.
      - Character arrays hold titles and descriptions for the plots.
      
- **Data Blocks**:
      - The subroutine defines several `DATA` statements to initialize titles, axis labels, and descriptive text.
      - Variables such as `TITL`, `TFIR`, `TSEC`, `ORIZ`, and others are used to label the graphs.

---

## Processing Logic

### 1. Rainfall Hyetograph / Inlet Hydrograph
- **Activation**: When `NTYPE` equals 1.
- **Operation**:
      - Reads rainfall data from the file associated with the variable `NREIN`.
      - Constructs time series for rainfall data by updating the `X` and `Y` arrays.
      - Utilizes a DO-loop to process each rain event and insert additional points if necessary.
      - Calls the `CURVE` subroutine with processed data to create the plot.
      - Special handling for when the number of points exceeds array bounds (ensuring the maximum index is not exceeded).

### 2. Inlet Hydrograph Flow Plot
- **Activation**: When `NTYPE` equals 2.
- **Operation**:
      - Resets the file pointer for flow data.
      - Reads time and flow values, converting values when using metric units.
      - Uses the `CURVE` call to plot the flow graph after processing the data series.

### 3. Infiltration Rate Plot
- **Activation**: When `NTYPE` equals 3, with a condition on `PRCIMP`.
- **Operation**:
      - Processes infiltration data from flow data files.
      - Conversion factors are applied in metric mode to alter the units.
      - Constructs the `X` and `Y` arrays accordingly and uses the `CURVE` subroutine to create the plot.

### 4. Subsurface Flow, Soil Moisture, and Stage Graphs
- **Activation**: When `NTYPE` equals 4.
- **Operation**:
      - Processes subsurface hydrologic data using the file linked to `NGRND`.
      - Contains a loop to read groundwater data for multiple requested plots.
      - Updates arrays `GWFLWB`, `STPOLL`, among others.
      - Separately calls `CURVE` to plot:
            - Subsurface flow.
            - Soil moisture (fraction) – using separate arrays for different moisture components.
            - Stage graph (water level).
      - **Note**: The subroutine includes a fix (dated 7/1/03 by WCH) that alters the groundwater output loop.

---

## Special Notes and Fixes

- **Groundwater (GW) Output Loop Fix**:  
      A remark by WCH dated 7/1/03 indicates a fix was applied to the GW output loop. This ensures that the groundwater plot accurately represents the data and resolves issues with the loop that processed subsurface data.

- **Dimension Adjustments**:  
      The code includes conditional fixes for adjusting dimensions when the number of data points exceeds the limits (e.g., reassigning the last two values when necessary).

- **Metric Conversions**:  
      For flow and infiltration plots, adjustments are made when the metric flag (`METRIC`) is active. This involves converting values from imperial units to metric equivalents using specific conversion factors.

---

## Conclusion