```fortran 
      SUBROUTINE CLINEDVS
c2013 USE DFLIB
C=======================================================================
C   Option for command line input of file names.
C   Written by Chuck Moore of CDM, 8/93.
# CLINEDVS Subroutine Overview

The file `CLINEDVS.md` contains a Fortran subroutine named `CLINEDVS` designed to handle command-line input for file names. This summary provides an extensive explanation of the subroutine’s purpose, functionality, and historical context.

## Purpose

- **Primary Function:** Processes command-line arguments to extract valid file names.
- **Command-Line Handling:** Uses RMFortran-specific functions, ensuring that even the executable name is counted.
- **Filtering:** Skips arguments that begin with a switch indicator (i.e., '/').

## Detailed Functionality

1. **Inclusions and Declarations:**
      - The file begins by including `CLNAME.INC`, which likely contains necessary declarations (e.g., arrays to hold file name strings).
      - Variables declared include:
        - A character variable `FNAME` (length 128) for storing file names.
        - A character variable `FIRSTCH` to examine the first character of each argument.
        - Integer variables for loop counters and argument counts.

2. **Command-Line Argument Processing:**
      - The subroutine first retrieves the total number of arguments using `NARGS()`.
      - If only one argument (typically the executable itself) exists, the subroutine exits immediately.
      - It iterates over each argument (excluding the program name), extracting the argument into `FNAME` using `GETARG`.

3. **Argument Validation:**
      - For each argument, the subroutine checks the first character.
      - If the first character is a forward slash (`'/'`), the argument is treated as a switch and is skipped.
      - If it does not start with `'/'`, the argument is considered a file name and is stored in the global array `CNAMES` with a counter `NNAMES` tracking the number of valid names.

4. **Design Considerations and Revisions:**
      - **Historical Notes:** Originally written by Chuck Moore (CDM, 8/93) with modifications made in the mid-1990s for Digital Visual Fortran.
      - **Compiler Compatibility:** Special care (such as declaring `FPOS` as an integer) has been taken to address compiler-specific issues, such as those encountered with the Watcom compiler.

## Code Snippet

Below is the complete Fortran code embedded within the file:

```fortran
            SUBROUTINE CLINEDVS
c2013 USE DFLIB
C=======================================================================
C   Option for command line input of file names.
C   Written by Chuck Moore of CDM, 8/93.
C   This subroutine uses RMFortran-specific functions ARGC and ARGV to
C     return number of arguments on command line (including .EXE file)
C     and character values of the arguments, respectively.
C   For Lahey Fortran, must include Function ARGC to accomplish same
C     task.
C=======================================================================
C     WCH, 10/2/96.  Declare FPOS as integer to avoid Watcom compiler
C       errors.  (Bruce LaZerte)
C   Modified for Digital Visual Fortran
C   CIM 6/97
C=======================================================================
            INCLUDE 'CLNAME.INC'
            CHARACTER*128 FNAME
            CHARACTER*1 FIRSTCH
            INTEGER*2 I,IFIL
C
            INAMES=0
            IFIL = NARGS()
            NNAMES=0
            IF (IFIL.EQ.1) RETURN
C=======================================================================
C   Note that program name is first name recognized; hence, ARGC always
C     > 0.
C=======================================================================
            DO 100 I= 1,IFIL-1
            CALL GETARG (I,FNAME)
            FIRSTCH=FNAME
C=======================================================================
C   Allow for RM Fortran switch options to be included on command line.
C=======================================================================
            IF (FIRSTCH.EQ.'/')   GO TO 100
             NNAMES=NNAMES+1
             CNAMES(NNAMES) = FNAME
100    CONTINUE
            RETURN
            END
```

## Conclusion

This subroutine plays a critical role in preprocessing command-line input by:
- Reading and filtering potential file names.
- Ensuring that non-file switch options are ignored.
- Preparing a validated list of file names for further processing by the main program.

The code reflects practical adaptations over time, addressing both functionality improvements and compiler compatibility issues. It stands as a well-documented component in legacy Fortran applications.

C   This subroutine uses RMFortran-specific functions ARGC and ARGV to
C     return number of arguments on command line (including .EXE file)
C     and character values of the arguments, respectively.
C   For Lahey Fortran, must include Function ARGC to accomplish same
C     task.
C=======================================================================
C     WCH, 10/2/96.  Declare FPOS as integer to avoid Watcom compiler
C       errors.  (Bruce LaZerte)
C   Modified for Digital Visual Fortran
C   CIM 6/97
C=======================================================================
      INCLUDE 'CLNAME.INC'
      CHARACTER*128 FNAME
      CHARACTER*1 FIRSTCH
      INTEGER*2 I,IFIL
C
      INAMES=0
      IFIL = NARGS()
      NNAMES=0
      IF (IFIL.EQ.1) RETURN
C=======================================================================
C   Note that program name is first name recognized; hence, ARGC always
C     > 0.
C=======================================================================
      DO 100 I= 1,IFIL-1
      CALL GETARG (I,FNAME)
      FIRSTCH=FNAME
C=======================================================================
C   Allow for RM Fortran switch options to be included on command line.
C=======================================================================
      IF (FIRSTCH.EQ.'/')   GO TO 100
       NNAMES=NNAMES+1
       CNAMES(NNAMES) = FNAME
100    CONTINUE
      RETURN
      END
``` 
