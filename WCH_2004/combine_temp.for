      SUBROUTINE combine_temp(ICOMB)
C	COMBINE BLOCK
C=======================================================================
cred  part of the combine block that used to be in comb1.for
cred  this was made a separate routine on 6/22/01
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'COMB.INC'
	real the_means(4)
C=======================================================================
C===> Read temp Block interface file (ICOMB = 12 or 13)
C=======================================================================
C                    Read the temperature station number.
C=======================================================================
                     REWIND LAST
                     READ(LAST,ERR=335) ISTA
	               IF(ICOMB.EQ.12) WRITE(NEXT,9610) ISTA
                     NSTA = 1
                     WRITE(N6,2115) NSTA
                     WRITE(N6,2120) ISTA
                     WRITE(*,9615)
				   number_of_lines = 0 
	               do n = 1,4
	               the_means(n) = 0
	               enddo
                     DO I = 1,10000000
                     READ(LAST,END=330,ERR=330) JDAY,TMAX,TMIN,
     +                                          EVAPOR,WINDER
				   number_of_lines = number_of_lines + 1 
                     the_means(1) = the_means(1) + TMAX
                     the_means(2) = the_means(2) + TMIN
                     the_means(3) = the_means(3) + EVAPOR
                     the_means(4) = the_means(4) + WINDER
                     JYEAR = JDAY/1000
                     IF (JYEAR.LT.100) THEN
                     JDAY = JDAY - JYEAR*1000
                     JYEAR = JYEAR + 1900
                     JDAY = JDAY + JYEAR*1000
                     ENDIF
                     IF(ICOMB.EQ.12) WRITE(NEXT,9610) JDAY,TMAX,TMIN,
     +                                          EVAPOR,WINDER
                     enddo
  330                continue
                     if(number_of_lines.gt.0) then
                     WRITE(N6,2125) number_of_lines,
     +                     the_means(1)/float(number_of_lines),
     +                     the_means(2)/float(number_of_lines),
     +                     the_means(3)/float(number_of_lines),
     +                     the_means(4)/float(number_of_lines)
	               endif
	               return
  335                WRITE(N6,9500)
                     RETURN
C=======================================================================
 2115 FORMAT(//,
     +' ########################################################',/,
     +' #  Precipitation output created using the Temp Block.  #',/,
     +' #  Number of Temperature stations...',I9,'             #',/,
     +' ########################################################',/)
C#### WCH, 8/1/95.  CHANGE I13 TO A13.
 2120 FORMAT(' Location Station number',/,
     +       ' -------- --------------',/,
     +       10(I9,'. ',A13,/))
 2125 FORMAT(/,' Mean Summary',/,
     +       ' Number of data points..........',I12,/,
     +       ' Mean Maximum Temperature.......',F12.3,/,
     +       ' Mean Minimum Temperature.......',F12.3,/,
     +       ' Mean Wind Speed................',F12.3,/,
     +       ' Mean Evaporation...............',F12.3,/)
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
 9610 FORMAT(I7,2000F8.3)
 9615 FORMAT(/)
 9620 FORMAT('+Processing precipitation data for year ',I4)
C=======================================================================
      END
