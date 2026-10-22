      SUBROUTINE STRIP(N55)
C=======================================================================
C     THIS SUBROUTINE STRIPS COMMENT LINES FROM THE SWMM INPUT FILE
C     Note to Vax Programmers: An output format error will occur
C                              unless special provisions are made by the
C                              VAX operator.  An alternative is to change
C                              all occurrences of 230 to 120 in the
C                              STRIP subroutine.
C     Changed to allow 2999 characters per line   9/8/00 cim
C     Change in message to screen, WCH, 12/9/94.
C     Add new optional input lines for Runoff, WCH, 12/20/00.
C     Fix case when logical variables not reset to .FALSE when running
C        multiple blocks in one run.  CIM and Mitch Heineman. 7/20/04.
C=======================================================================
      INCLUDE 'TAPES.INC'
      CHARACTER ICODE*1,JCODE*1,ISTAR*1,KCODE*2999,DOLL*1,BLANK*1,
     aLEND*10,IHASH*1
      CHARACTER OUTSTR*3001,SQUO*1
      DATA ISTAR/'*'/,DOLL/'$'/,BLANK/' '/,LEND/'''  ''ENDPR'/,
     ASQUO/''''/,IHASH/'#'/
      LOGICAL EXTRAN,RUNOFF,TRANSPORT,STATS
C=======================================================================
      WRITE(*,999)
      EXTRAN = .FALSE.
      RUNOFF = .FALSE.
	TRANSPORT = .FALSE.
	STATS  = .FALSE.
   10 CONTINUE
      READ(N55,1,END=101,ERR=101) ICODE,JCODE,KCODE
      IF(ICODE.EQ.ISTAR) GO TO 100
cred  get rid of swmm 4.99 command words - 8/31/2004
      IF(ICODE.EQ.IHASH) GO TO 100
      IF (ICODE.EQ.DOLL) THEN
CIM   2/4/04. SET ALL TO FALSE.  This fixes case where you run  
C     blocks back to back in the same run.  
Cwch, 7/20/04. 
           EXTRAN    = .FALSE.
           RUNOFF    = .FALSE.
           TRANSPORT = .FALSE.
           STATS     = .FALSE. 
           IF (JCODE.EQ.'E'.and.KCODE(1:3).EQ.'XTR') EXTRAN    = .TRUE.
           IF (JCODE.EQ.'R'.AND.KCODE(1:3).EQ.'UNO') RUNOFF    = .TRUE.
	     IF (JCODE.EQ.'S'.AND.KCODE(1:3).EQ.'TAT') STATS     = .TRUE.
	     IF (JCODE.EQ.'T'.AND.KCODE(1:3).EQ.'RAN') TRANSPORT = .TRUE.
           ENDIF
cim  further process KCODE to strip stuff after any additional asterisks
cim
      locast = index(kCODE,istar)
      locast =locast-1
      if(locast.le.0) THEN
        Locast = len(kCODE)
      ELSE
        KCODE = KCODE(1:LOCAST)
      ENDIF
CIM STRIP LEADING BLANKS FROM KCODE
      DO I=1,LOCAST
      IF (KCODE(I:I).NE.BLANK) GO TO 11
      ENDDO
C  ALL BLANK
      LOCAST = 1
      GO TO 20
   11 CONTINUE
C***WCH, 9/27/99.  OMIT THIS TASK TO SEE IF FORMATTED READS NOW WORK.
C      KCODE = KCODE(I:LOCAST)
C      LOCAST = LOCAST - I + 1
CIM FIND LAST BLANK TO THE LEFT OF LOCAST
CIM AND REMOVE TRAILING BLANKS
   12 IF (KCODE(LOCAST:LOCAST) .NE. BLANK) GO TO 13
	LOCAST=LOCAST - 1
	IF(LOCAST.GT.0) GO TO 12
C     ALLBLANKS
      LOCAST = 1
      GO TO 20
   13 CONTINUE
CIM  10/98  PAD KCODE WITH 10 ZEROS
cim  only lines with optional inputs
c  extran lines with optional inputs include B0, BA, BB, B1, D1, G1
      IF (EXTRAN.and.ICODE.EQ.'B'.AND.JCODE.EQ.'0') GO TO 15
      IF (EXTRAN.and.ICODE.EQ.'B'.AND.JCODE.EQ.'A') GO TO 15
      IF (EXTRAN.and.ICODE.EQ.'B'.AND.JCODE.EQ.'B') GO TO 15
      IF (EXTRAN.and.ICODE.EQ.'B'.AND.JCODE.EQ.'1') GO TO 15
      IF (EXTRAN.and.ICODE.EQ.'D'.AND.JCODE.EQ.'1') GO TO 15
      IF (EXTRAN.AND.ICODE.EQ.'E'.AND.JCODE.EQ.'1') GO TO 15
      IF (EXTRAN.and.ICODE.EQ.'G'.AND.JCODE.EQ.'1') GO TO 15
	IF (EXTRAN.AND.ICODE.EQ.'H'.AND.JCODE.EQ.'1') GO TO 15
c  RUNOFF lines with optional data input B1, B2, B3, H1, J3
      IF (RUNOFF.and.ICODE.EQ.'B'.AND.JCODE.EQ.'1') GO TO 15
      IF (RUNOFF.and.ICODE.EQ.'B'.AND.JCODE.EQ.'2') GO TO 15
	IF (RUNOFF.and.ICODE.EQ.'B'.AND.JCODE.EQ.'3') GO TO 15
	IF (RUNOFF.and.ICODE.EQ.'H'.AND.JCODE.EQ.'1') GO TO 15
	IF (RUNOFF.and.ICODE.EQ.'J'.AND.JCODE.EQ.'3') GO TO 15
C  New Stats line with optional data input on A1 line
      IF (STATS.AND.ICODE.EQ.'A'.AND.JCODE.EQ.'1') go to 15
CIM### 9/11/99 TRANSPORT OPTIONAL INPUT OF IDETAIL on B0 line
Cwch, 4/18/02. Add F1 line. 
	IF (TRANSPORT.AND.ICODE.EQ.'B'.AND.JCODE.EQ.'0') go to 15
	IF (TRANSPORT.AND.ICODE.EQ.'B'.AND.JCODE.EQ.'1') go to 15
	IF (TRANSPORT.AND.ICODE.EQ.'F'.AND.JCODE.EQ.'1') go to 15
      GO TO 20
   15 CONTINUE
      DO I = 1,10
      KCODE = KCODE(1:LOCAST) // BLANK
      LOCAST = LOCAST + 1
	IF (LOCAST.GT.2999) go to 17
      KCODE = KCODE(1:LOCAST) // '0'
      LOCAST = LOCAST + 1
	IF (LOCAST.GT.2999) go to 17
      ENDDO
	go to 20
   17	WRITE(N6,8000) KCODE
 8000 FORMAT(/,'  ERROR >>> INPUT LINE LONGER THAN 2999 CHARACTERS.',/,
     112X,'YOU MAY NEED TO INSERT ASTERISK TO IDENTIFY START',
     2' OF COMMENT FIELD.',/,12X,'BAD LINE FOLLOWS.',//,12X,A2999)
	STOP 'INPUT LINE TOO LONG'
cim end
   20 CONTINUE
      IF ((ICODE.EQ.' ').AND.(JCODE.EQ.' ').AND.(LOCAST.EQ.1)) GO TO 10
      IF (ICODE.EQ.DOLL) THEN
        OUTSTR = SQUO // ICODE // SQUO // BLANK // JCODE
        OUTSTR = OUTSTR(1:5) // KCODE(1:LOCAST)
        LOCAST = LOCAST + 5
      ELSE
	  IASC = IACHAR(ICODE)
	  IF(ICODE.EQ.' ') THEN
        OUTSTR = ICODE // JCODE // KCODE(1:LOCAST)
        LOCAST = LOCAST + 2
Cwch, 9/14/00.  If first character is a number or decimal pt, leave it.
C     Function IACHAR(C) returns ASCII code of character. 
              ELSEIF (IASC.EQ.46.OR.(IASC.GE.48.AND.IASC.LE.57)) THEN
        OUTSTR = ICODE // JCODE // KCODE(1:LOCAST)
	        ELSE
        OUTSTR = SQUO // ICODE // JCODE // SQUO
        OUTSTR = OUTSTR(1:4) // ' '
        OUTSTR = OUTSTR(1:5) // KCODE(1:LOCAST)
        LOCAST = LOCAST + 5
       ENDIF
      ENDIF
	IF (LOCAST.GT.3001) go to 17
      WRITE(N5,7000) (OUTSTR(I:I),I=1,LOCAST)
 7000 FORMAT(1X,3001A1)
 2000 FORMAT(2A1,A2999)
C     IF(ICODE.EQ.DOLL) THEN
C                       WRITE(N6,2) ICODE,JCODE,KCODE(1:locast)
C                       ELSE IF(ICODE.EQ.BLANK) THEN
C                            WRITE(N6,5) ICODE,JCODE,KCODE(1:locast)
C                            ELSE
C                            WRITE(N6,4) ICODE,JCODE,KCODE(1:locast)
C                            ENDIF
100   GO TO 10
101   WRITE(N5,6) LEND
	WRITE(N5,*) '$ENDPROGRAM'
      CLOSE (N55)
      REWIND N5
1     FORMAT(2A1,A2999)
2     FORMAT('''',A1,'''',1X,A1,A2999)
3     FORMAT(A2999)
4     FORMAT('''',2A1,''' ',A2999)
5     FORMAT(2A1,A2999)
6     FORMAT(A10)
C#### WCH, 12/9/94.  REMOVE "...process is much faster..." stuff.
999   FORMAT(/,' Reading the input file and deleting comment lines.')
C####     +       ' This process is much faster when a RAM drive is used.',/)
      RETURN
      END
