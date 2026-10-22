      SUBROUTINE combine_rain(ICOMB)
C	COMBINE BLOCK
C=======================================================================
cred  part of the combine block that used to be in comb1.for
cred  this was made a separate routine on 6/22/01
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'COMB.INC'
cred  variables for the xp rainfall file
	integer*4 jsta_xp(2000),nsta_xp,mrain_xp,jday_xp
	DOUBLE PRECISION tmday_xp,thisto_xp,qo2_xp(2000)
cred  up the number of rainfall stations to 2000 from 10 - 6/22/01
      CHARACTER JSTA(2000)*8,JSTA1(2000)*8,JSTA2(2000)*8
	CHARACTER*10 first_line
C=======================================================================
C===> Read Rain Block interface file (ICOMB = 6 or 7 8, 9)
C=======================================================================
C                    Read the precipitation station number.
C=======================================================================
                     REWIND LAST
                     REWIND next
	               newnext = nscrat(7)
	               if(newnext.le.0) then
	           write(n6,*) ' Error !! Scratch file #7 is not defined.'
	                                 stop
	                                 endif
	               REWIND newnext
cred                 read the ascii file created using ICOMB = 7 option
                     IF(ICOMB.EQ.8) THEN
				   READ(LAST,1000) first_line
	               READ(LAST,1001) NSTA
	               READ(LAST,1002) MRAIN
 1000                format(a10)
 1001                format(21X,I3) 
 1002                format(45X,I9) 
 1003                format(10x,a10) 
cred                 read the next three descriptive lines
 				   READ(LAST,1000) first_line
				   READ(LAST,1000) first_line
				   READ(LAST,1000) first_line
                     do i = 1,NSTA
	 			   READ(LAST,1003) JSTA(i)
                     enddo
cred                 read the last seven lines in the ascii file
				   READ(LAST,1000) first_line
				   READ(LAST,1000) first_line
				   READ(LAST,1000) first_line
				   READ(LAST,1000) first_line
				   READ(LAST,1000) first_line
				   READ(LAST,1000) first_line
				   READ(LAST,1000) first_line
                     write(next) NSTA,MRAIN,(JSTA(I),I=1,NSTA)

                     elseif(ICOMB.le.7.OR.ICOMB.eq.10) then
cred                 read the unformatted files
                     READ(LAST,ERR=335) NSTA,MRAIN,(JSTA(I),I=1,NSTA)
                     WRITE(N6,2115) NSTA
                     WRITE(N6,2120) (I,JSTA(I),I=1,NSTA)
                     elseif(ICOMB.eq.11) then
cred                 read the binary files
                     READ(LAST,ERR=335) NSTA_xp,MRAIN_xp,
     +                    (JSTA_xp(I),I=1,NSTA_xp)
				      NSTA  = NSTA_XP 
	                  MRAIN = MRAIN_XP
	                  DO I=1,NSTA_xp
c	                     JSTA(I) = Ichar(JSTA_XP(I)) 
	                     enddo
                     WRITE(N6,2115) NSTA
                     WRITE(N6,2120) (I,JSTA(I),I=1,NSTA)
                     elseif(ICOMB.eq.9) then
cred                 read the unformatted files
cred                 assume they have the same number of stations
cred                 and last is the first in the sequence
                     READ(LAST,ERR=335)NSTA1,MRAIN1,(JSTA1(I),I=1,NSTA1)
                     READ(next,ERR=335)NSTA2,MRAIN2,(JSTA2(I),I=1,NSTA2)
                     WRITE(N6,2115) NSTA1
                     WRITE(N6,2120) (I,JSTA1(I),I=1,NSTA1)
                     WRITE(newnext) NSTA1,MRAIN1+MRAIN2,
     +                             (JSTA1(I),I=1,NSTA1)
	               ENDIF

                     IF(ICOMB.EQ.9) THEN
	               do i = 1,mrain1
                     READ(LAST) JDAY,
     +                  TMDAY,THISTO,(QO2(J),J=1,NSTA1)
                     WRITE(newnext) JDAY,
     +                  TMDAY,THISTO,(QO2(J),J=1,NSTA1)
		           enddo
	               do i = 1,mrain2
                     READ(next) JDAY,
     +                  TMDAY,THISTO,(QO2(J),J=1,NSTA2)
                     WRITE(newnext) JDAY,
     +                  TMDAY,THISTO,(QO2(J),J=1,NSTA2)
		           enddo
				   return 
                     endif
C#######################################################################
C#### WCH, 7/25/96.  Write headers for the ASCII file.
C=======================================================================
                     IF(ICOMB.EQ.7) THEN 
                        WRITE(NEXT,9600) NSTA,MRAIN,(I,JSTA(I),I=1,NSTA)
                        WRITE(NEXT,9605) (I,I=1,NSTA)
                        ENDIF
                     IF(ICOMB.EQ.10) THEN
				      NSTA_XP  = NSTA 
	                  MRAIN_XP = MRAIN
	                  DO I=1,NSTA
c	                     JSTA_XP(I) = JSTA(I) 
	                     enddo
                        WRITE(NEXT) NSTA_XP,MRAIN_XP,
     +                             (JSTA_XP(I),I=1,NSTA_xp)
                        ENDIF
                     IF(ICOMB.EQ.11) THEN 
                        WRITE(NEXT) NSTA,MRAIN,(JSTA(I),I=1,NSTA)
                        ENDIF
C=======================================================================
C                    Calculate the total precipitation.
C=======================================================================
                     IF(ICOMB.le.8.OR.ICOMB.ge.10) THEN 
                     DO  324 I = 1,NSTA
  324                QO1(I)   = 0.0
C#### WCH, 7/25/96
                     JFYL     = 0
                     WRITE(*,9615)
                     DO 325 I = 1,MRAIN
                     IF(ICOMB.le.7) READ(LAST,END=330,ERR=330) JDAY,
     +                  TMDAY,THISTO,(QO2(J),J=1,NSTA)
                     IF(ICOMB.eq.8) then
				      READ(LAST,9610,END=9000) JDAY,
     +                  TMDAY,THISTO,(QO2(J),J=1,NSTA)
				      WRITE(NEXT) JDAY,
     +                  TMDAY*3600.0,THISTO,(QO2(J),J=1,NSTA)
	                  endif
                     IF(ICOMB.eq.10) THEN
				      READ(LAST,END=330,ERR=330) JDAY,
     +                  TMDAY,THISTO,(QO2(J),J=1,NSTA)
	                  JDAY_XP  = JDAY
	                  TMDAY_XP = TMDAY
	                  THISTO_XP = THISTO
	                  DO J = 1,NSTA
                           QO2_XP(J) = QO2(J)
	                     ENDDO
					  ENDIF
                     IF(ICOMB.eq.11) READ(LAST,END=330,ERR=330) JDAY,
     +                  TMDAY,THISTO,(QO2(J),J=1,NSTA)
                     JYEAR = JDAY/1000
                     IF (JYEAR.LT.100) THEN
                     JDAY = JDAY - JYEAR*1000
                     JYEAR = JYEAR + 1900
                     JDAY = JDAY + JYEAR*1000
                     ENDIF
                     IF(I.EQ.1) THEN
                                JFIRST = JDAY
                                TFIRST = TMDAY
                                ENDIF
C#### WCH, 7/25/96.
                     IF(ICOMB.EQ.7) WRITE(NEXT,9610) JDAY,
     +                      TMDAY/3600.,THISTO,(QO2(J),J=1,NSTA)
                     IF(ICOMB.EQ.10) 
     +			   WRITE(NEXT) JDAY_xp,
     +                      TMDAY_xp/3600.,THISTO_xp,
     1                      (QO2_xp(J),J=1,NSTA)
                     IF(ICOMB.EQ.11)
     +			   WRITE(NEXT) JDAY,
     +                      TMDAY/3600.,THISTO,(QO2(J),J=1,NSTA)
C#### WCH, 7/25/96.  ADD ON-SCREEN PRINT OF YEAR.
                     JFY      = JDAY/1000
                     IF(JFY.GT.JFYL) THEN
                          WRITE(*,9620) JFY
                          JFYL = JFY
                          ENDIF
cred                 calculate the totals for the output file
                     DO 326 J = 1,NSTA
  326                QO1(J)   = QO1(J)   + QO2(J)*THISTO/3600.0
  325                CONTINUE
  330                CONTINUE

                     WRITE(N6,9550) I,JFIRST,TFIRST,JDAY,TMDAY
                     DO 327 J = 1,NSTA
  327                WRITE(N6,9555) JSTA(J),QO1(J)
                     RETURN
		           endif
  335 WRITE(N6,9500)
      RETURN
 9000 write(n6,*) '  Error!!  I am trying to read to much data from your
     + ascii rainfall file.  Please check the number of data points'
	RETURN
c=======================================================================
 2115 FORMAT(//,
     +' ########################################################',/,
     +' #  Precipitation output created using the Rain Block.  #',/,
     +' #  Number of precipitation stations...',I9,'        #',/,
     +' ########################################################',/)
C#### WCH, 8/1/95.  CHANGE I13 TO A13.
 2120 FORMAT(' Location Station number',/,
     +       ' -------- --------------',/,
     +       10(I9,'. ',A13,/))
 8290 FORMAT(/,
     +' ################################################',/
     +' #   Simple flow statistics from interface file #',/,
     +' ################################################',//,
     +'  Location #   Mean Flow  Total Flow ',99(2X,A8,2X))
 8295 FORMAT('                  cfs      cubic ft. ',
     +    10(2X,A10))
 8296 FORMAT('                  cms     cubic met. ',
     +    10(2X,A10))
 8297 FORMAT('  ----------   ---------',11(2X,A10))
 8301 FORMAT(/,' ===> Error !! Total time was 0.0 hours.')
 8300 FORMAT(1X,I10,1X,F12.5,11(1PE12.4))
 8305 FORMAT(2X,A10,F12.5,11(1PE12.4))
 8310 FORMAT(
     +'  ----------  ----------',11(2X,A10))
 8311 FORMAT(
     +'                --------',11(2X,A10))
 8315 FORMAT(' Total       ',F11.4,11(1PE12.4))
 8316 FORMAT('                  inches   cubic ft. ',
     +    10(2X,A10))
 8317 FORMAT('             millimeters  cubic met. ',
     +    10(2X,A10))
 8320 FORMAT(//,' Total simulation time ===> ',F20.4,' hours',//,
     +          ' Total number of steps ===> ',10X,I10)
 9500 FORMAT(/,' ===> Error !!  This file is probably not a Rain',
     +' or Temp Block interface file.')
 9550 FORMAT(//,' Total number of precipitation data points....',I12,/,
     +      ' First Julian day.............................',I12,/,
     +      ' First Julian day start time (seconds)........',F12.1,/,
     +      ' Last Julian day..............................',I12,/,
     +      ' Last Julian day start time (seconds).........',F12.1,//)
C#### WCH, 8/1/95.  CHANGE I12 TO A12.
 9555 FORMAT(
     +' ###############################################',/,
     +' # Station......................................',A12,/,
     +' # Total Precipitation (inches or millimeters)..',F12.3,/,
     +' ###############################################')
C#### WCH, 7/25/96.
 9600 FORMAT(' Rain Block Interface File',/,
     1 ' Number of stations =',I3,/,
     2 ' Max. possible number of time steps (MRAIN) =',I8,/,
     3 ' Precipitation stations:',/,
     4 ' Number     Station',/,
     5 ' ------   ----------',/,(I6,4X,A10))
 9605 FORMAT(/,' Precipitation intensity will have units of in/hr or mm/
     1hr,',/,' depending on U.S. or metric units in Rain Block.',//,
     2' Julian    Rainfall     Time',/,
     3'   Day    Start Time  Interval  Intensities for station no.:',/,
     4'            (hour)      (sec)',2000I8)
 9610 FORMAT(I7,F13.3,F10.1,2000F8.4)
 9615 FORMAT(/)
 9620 FORMAT('+Processing precipitation data for year ',I4)
C=======================================================================
      END
