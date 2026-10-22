April 1, 2002

The files in TransWQ include documentation for a mostly untested version of the Transport Block that includes linked BOD-NOD-DO as an option.  Testing to date does not include the settlng., SOD, and removal options, nor the option for input of parameters for different elements and varying temporally.  However, the BOD-NOD-DO routine produces a "reasonable" DO-sag.  

CAUTION!

As of 4/1/02, the program does not account for anaerobic conditions.  If the DO is driven to zero, decay of BOD and NOD continues according to their decay rates instead of being limited to the rate of supply of DO by reaeration.  This will be corrected "soon."  

Also included is a program TESTK2.EXE to compute flow-driven and wind-driven reaeration coefficients, using Subroutine REAERATE from the Transport Block.  This program is intended to give representative values for K2 for user-input values of water depth, velocity, reaeration coefficient parameters and O'Connors (1983) wind-driven reaeration equations.  Documentation is on-screen for the DOS program and in file TESTK2.DAT.  

Wayne Huber