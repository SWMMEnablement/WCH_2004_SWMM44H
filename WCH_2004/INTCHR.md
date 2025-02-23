```fortran 
      FUNCTION INTCHR(J1)
C     RAIN BLOCK
C     CALLED BY GTRAIN NEAR LINE 969
C=======================================================================
C     Change a character to an integer.
C=======================================================================
      CHARACTER*1 J1(3)
      INTEGER      I(3)
      INTCHR   = 0
      DO 100 J = 1,3
      I(J)     = ICHAR(J1(J)) - 48
100   INTCHR   = INTCHR + I(J)*10**(3-J)
      RETURN
      END
# INTCHR Function (Fortran)

This document contains a detailed explanation of the Fortran function INTCHR, which converts a three-character array into its corresponding integer representation. Below is the complete source code and an extensive summary of its functionality and internal logic.

## Source Code

```fortran
      FUNCTION INTCHR(J1)
C     RAIN BLOCK
C     CALLED BY GTRAIN NEAR LINE 969
C=======================================================================
C     Change a character to an integer.
C=======================================================================
      CHARACTER*1 J1(3)
      INTEGER      I(3)
      INTCHR   = 0
      DO 100 J = 1,3
      I(J)     = ICHAR(J1(J)) - 48
100   INTCHR   = INTCHR + I(J)*10**(3-J)
      RETURN
      END
```

## Summary and Explanation

- **Purpose:**  
  The function `INTCHR` converts a three-character array (each character assumed to be a digit) into an integer value. This conversion is handled by interpreting each character as its ASCII numerical digit and then assembling these digits into an integer.

- **Parameter Details:**  
  - `J1`: A 3-element array of single characters. Each element should represent a numeric digit (e.g., '0' to '9').

- **Local Variables:**  
  - `I(3)`: An integer array where each element stores the numeric value of the corresponding character from `J1`.
  - `INTCHR`: The function result; initialized to 0 and computed as the sum of the converted digits multiplied by their positional values.

- **Conversion Process:**  
  1. **Initialization:**  
     The result `INTCHR` is initially set to 0.
  
  2. **Loop through Characters:**  
     The code loops over each character in the array using a `DO` loop. For each iteration:
     - The character is converted to its corresponding integer by using the intrinsic function `ICHAR` to get the ASCII code and then subtracting 48 (the ASCII code for '0').
     - The digit is stored in the array `I`.

  3. **Constructing the Integer:**  
     After each conversion, the digit is multiplied by a power of 10 that depends on its position in the array. This is computed using `10**(3-J)`, ensuring that the leftmost digit is the most significant.
  
  4. **Accumulation:**  
     The resulting product is added to `INTCHR` to form the final number.

- **Usage Context:**  
  - The function is used in a section referred to as the "RAIN BLOCK" and is called by the subroutine `GTRAIN` near line 969 in the larger program.
  - It is a utility for converting character representations of numbers into their integer form within the program.

This detailed markdown summary provides a clear understanding of the INTCHR function and explains each part of the code, offering a solid foundation for further development or debugging within the Fortran program.
