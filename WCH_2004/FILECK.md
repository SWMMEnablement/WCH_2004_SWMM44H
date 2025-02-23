```fortran 
      SUBROUTINE FILECK(IO,FNAME,DEFORM)
C      ADD FOLLOWING LINE FOR DIGITAL VISUAL FORTRAN
c2013  USE DFPORT
C=======================================================================
C     Subroutine to check form (formatted vs. unformatted) of existing
C     file to avoid run-stopping error when desired form does not
C     match existing form.
C     This problem does not occur with an empty file, only one 
C     previously used that contains data in the wrong form.  
C     At program pause, user will have option to allow deletion of
C     existing file (FNAME), and continue run.
C     Note: Subroutine SYSTEM may be Lahey-specific, in which case 
C     do not use this subroutine or references to it, and just suffer
C     through resulting error message.
C     Wayne Huber, 7/25/96.
C     Modified 7/97 to check if file exists  C. Moore CDM 6/25/97
# FILECK Subroutine Overview

This document provides an extensive summary of the Fortran subroutine FILECK, which checks the form (formatted vs. unformatted) of an existing file. The subroutine is enhanced to include a logical variable (EX) for determining file existence and an early return if the file does not exist.

## Key Features

- **Input Parameters:**
      - **IO:** The unit number for the file.
      - **FNAME:** The DOS filename (CHARACTER*128).
      - **DEFORM:** Desired file form (FORMATTED or UNFORMATTED as CHARACTER*11).

- **Logical Checks:**
      - **EX:** A logical variable added to the INQUIRE statement to check if the file exists.
      - **OPEND:** Indicates if the file is already open.
      - The subroutine immediately returns if `IO` is not greater than zero or if the file does not exist.

## Detailed Explanation

1. **Input Validation:**
       - If `IO` is less than or equal to zero, the subroutine performs no action.
       
2. **File Inquiry:**
       - The INQUIRE statement retrieves:
             - **ACFORM:** The actual file form.
             - **OPEND:** Whether the file is open.
             - **EX:** Whether the file exists.
       - A check is added: if the file does not exist (`.NOT.EX`), the subroutine returns immediately.

3. **Form Matching and Rewind:**
       - If `ACFORM` matches `DEFORM`:
             - The file is rewound if it is open.
             - The subroutine returns, allowing normal file operations.
             
4. **Error Handling and Deletion:**
       - If the file exists and its form does not match `DEFORM`, the subroutine attempts to delete the file.
       - Comments indicate tailored commands for different Fortran compilers (e.g., Digital Visual Fortran and Lahey).

## Code Snippet Integration

Below is the section that was integrated at the placeholder `$SELECTION_PLACEHOLDER$`:

```fortran
C     Add EX to logical, INQUIRE, and add check to return if file
```

This line is crucial because it:
- Incorporates the EX logical variable into the INQUIRE call.
- Adds an immediate return if the file does not exist, thus preventing potential run-stopping errors.

## Historical Notes

- **Original Purpose:**  
      The subroutine was designed to prevent errors caused by mismatched file forms, particularly when attempting to reopen a file with a different format.

- **Modifications:**
      - Enhanced logic in 1997 to check for file existence.
      - Updated in 2013 for compatibility with Digital Visual Fortran by adjusting system commands.

## Conclusion

The FILECK subroutine plays a vital role in ensuring that file I/O operations are consistent with the file's intended format. By checking for both the form and existence of the file, it prevents errors that could halt program execution. This robust handling is essential for cross-compiler compatibility and reliable file management.

C     doesn't exist.
C=======================================================================
C     IO    = unit number of file to be checked.
C     FNAME = DOS name of file
C     DEFORM = desired form for file (FORMATTED or UNFORMATTED)
C=======================================================================
      CHARACTER*128 FNAME
      CHARACTER*11 DEFORM,ACFORM
      LOGICAL OPEND,EX
C=======================================================================
C     If no file is specified (IO = 0), do nothing.
C=======================================================================
      IF(IO.LE.0) RETURN
C=======================================================================
C     Otherwise, check form of file FNAME to see if is OK to open.
C     OK if form about to be used matches existing form.  
C     Otherwise, print message and ask user if it is OK to delete
C     the offending file.
C=======================================================================
      INQUIRE(IO,FORM=ACFORM,OPENED=OPEND,exist=EX)
      IF (.NOT.EX) RETURN
      IF(ACFORM.EQ.DEFORM) THEN
           IF(OPEND) REWIND IO
           RETURN
           ELSE
CIM ELIMINATE WRITE AND PAUSE
c           WRITE(*,9000) IO,FNAME,DEFORM,IO
c           PAUSE
C=======================================================================
C     Prepare a delete command by concatenating DOS delete command
C     with offending file name.
C     Caution.  Subroutine SYSTEM allows a DOS command to be executed.
C     This may be Lahey-specific.
C=======================================================================
c the following line is lahey
C           CALL SYSTEM('del '//FNAME)
c  the following line is digital visual fortran
           IOEER = SYSTEM('del '//FNAME)
           ENDIF
      RETURN
C=======================================================================
c 9000 FORMAT(/,' Program is about to stop with error message for unit',
c     1 I3,/,' about ''form'' of file: ',A60,/,
c     2' File momentarily should be of form: ',A11,', but is not.',/,
c     3' At prompt, press <enter> to delete above file and then',/,
c     4' over-write it, or press <control>-C or <control>-break',/,
c     5' to end run and not delete the file.  In this latter case,',/,
c     6' provide new file name on @-line for unit',I3,' and run again.')
      END


      

``` 
