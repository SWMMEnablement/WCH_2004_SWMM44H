      SUBROUTINE ERROR(K)
C=======================================================================
C     Subroutine Error last updated December, 1990 by Bob Dickinson.
C     MODIFIED FOR CONSISTENCY C. MOORE 6/14/2000
C=======================================================================
      INCLUDE 'TAPES.INC'
C=======================================================================
      WRITE(*,9700)
      WRITE(*,9500)
      WRITE(N6,9700)
      WRITE(N6,9500)
      IF(K.EQ.1) THEN
                 WRITE(*,1000)
                 WRITE(N6,1000)
                 STOP
                 ENDIF
      IF(K.EQ.2) THEN
                 WRITE(*,1002) NLU
                 WRITE(N6,1002) NLU
                 STOP
                 ENDIF
      IF(K.EQ.3) THEN
                 WRITE(*,1003)
                 WRITE(N6,1003)
                 STOP
                 ENDIF
      IF(K.EQ.4) THEN
                 WRITE(*,1004)
                 WRITE(N6,1004)
                 STOP
                 ENDIF
      IF(K.EQ.5) THEN
                 WRITE(*,1005)
                 WRITE(N6,1005)
                 STOP
                 ENDIF
      IF(K.EQ.6) THEN
                 WRITE(*,1006)
                 WRITE(N6,1006)
                 STOP
                 ENDIF
      IF(K.EQ.7) THEN
                 WRITE(*,1007)
                 WRITE(N6,1007)
                 STOP
                 ENDIF
      IF(K.EQ.8) THEN
                 WRITE(*,1008)
                 WRITE(N6,1008)
                 STOP
                 ENDIF
      IF(K.EQ.9) THEN
                 WRITE(*,1009)
                 WRITE(N6,1009)
                 STOP
                 ENDIF
      IF(K.EQ.11) THEN
                  WRITE(*,1011)
                  WRITE(N6,1011)
                  STOP
                  ENDIF
      IF(K.EQ.12) THEN
                  WRITE(*,70)
                  WRITE(N6,70)
                  STOP
                  ENDIF
      IF(K.EQ.13) THEN
                  WRITE(*,740) NG
                  WRITE(N6,740) NG
                  STOP
                  ENDIF
      IF(K.EQ.14) THEN
                  WRITE(*,878) NGW
                  WRITE(N6,878) NGW
                  STOP
                  ENDIF
      IF(K.EQ.15) THEN
                  WRITE(*,2080)
                  WRITE(N6,2080)
                  STOP
                  ENDIF
      IF(K.EQ.16) THEN
                  WRITE(*,2100)
                  WRITE(N6,2100)
                  STOP
                  ENDIF
      IF(K.EQ.17) THEN
                  WRITE(*,2120) NW
                  WRITE(N6,2120) NW
                  STOP
                  ENDIF
      IF(K.EQ.18) THEN
                  WRITE(*,907)
                  WRITE(N6,907)
                  STOP
                  ENDIF
      IF(K.EQ.19) THEN
                  WRITE(*,910)
                  WRITE(N6,910)
                  STOP
                  ENDIF
      IF(K.EQ.20) THEN
                  WRITE(*,920)
                  WRITE(N6,920)
                  STOP
                  ENDIF
      IF(K.EQ.100) THEN
                   WRITE(*,9100)
                   WRITE(N6,9100)
                   STOP
                   ENDIF
      IF(K.EQ.101) THEN
                   WRITE(*,9101)
                   WRITE(N6,9101)
                   STOP
                   ENDIF
      IF(K.EQ.102) THEN
                   WRITE(*,9102)
                   WRITE(N6,9102)
                   STOP
                   ENDIF
      IF(K.EQ.103) THEN
                   WRITE(*,9103)
                   WRITE(N6,9103)
                   STOP
                   ENDIF
      IF(K.EQ.104) THEN
                   WRITE(*,9104)
                   WRITE(N6,9104)
                   STOP
                   ENDIF
      IF(K.EQ.111) THEN
                   WRITE(*,9111)
                   WRITE(N6,9111)
                   STOP
                   ENDIF
      IF(K.EQ.112) THEN
                   WRITE(*,9112)
                   WRITE(N6,9112)
                   STOP
                   ENDIF
      IF(K.EQ.113) THEN
                   WRITE(*,9113)
                   WRITE(N6,9113)
                   STOP
                   ENDIF
      IF(K.EQ.114) THEN
                   WRITE(*,9114)
                   WRITE(N6,9114)
                   STOP
                   ENDIF
      IF(K.EQ.115) THEN
                   WRITE(*,9115)
                   WRITE(N6,9115)
                   STOP
                   ENDIF
      IF(K.EQ.116) THEN
                   WRITE(*,9116)
                   WRITE(N6,9116)
                   STOP
                   ENDIF
      IF(K.EQ.117) THEN
                   WRITE(*,9117)
                   WRITE(N6,9117)
                   STOP
                   ENDIF
      IF(K.EQ.118) THEN
                   WRITE(*,9118)
                   WRITE(N6,9118)
                   STOP
                   ENDIF
      IF(K.EQ.119) THEN
                   WRITE(*,9119)
                   WRITE(N6,9119)
                   STOP
                   ENDIF
      IF(K.EQ.130) THEN
                   WRITE(*,9130)
                   WRITE(N6,9130)
                   STOP
                   ENDIF
      IF(K.EQ.131) THEN
                   WRITE(*,9131)
                   WRITE(N6,9131)
                   STOP
                   ENDIF
      IF(K.EQ.140) THEN
                   WRITE(*,9140)
                   WRITE(N6,9140)
                   STOP
                   ENDIF
      IF(K.EQ.141) THEN
                   WRITE(*,9141)
                   WRITE(N6,9141)
                   STOP
                   ENDIF
      IF(K.EQ.142) THEN
                   WRITE(*,9142)
                   WRITE(N6,9142)
                   STOP
                   ENDIF
      IF(K.EQ.143) THEN
                   WRITE(*,9143)
                   WRITE(N6,9143)
                   STOP
                   ENDIF
      IF(K.EQ.150) THEN
                   WRITE(*,9150)
                   WRITE(N6,9150)
                   STOP
                   ENDIF
      IF(K.EQ.151) THEN
                   WRITE(*,9151)
                   WRITE(N6,9151)
                   STOP
                   ENDIF
      IF(K.EQ.152) THEN
                   WRITE(*,9152)
                   WRITE(N6,9152)
                   STOP
                   ENDIF
      IF(K.EQ.153) THEN
                   WRITE(*,9153)
                   WRITE(N6,9153)
                   STOP
                   ENDIF
      IF(K.EQ.154) THEN
                   WRITE(*,9154)
                   WRITE(N6,9154)
                   STOP
                   ENDIF
C#### WCH, 8/93.
      IF(K.EQ.155) THEN
                   WRITE(*,9155)
                   WRITE(N6,9155)
                   STOP
                   ENDIF
      IF(K.EQ.156) THEN
                   WRITE(*,9156)
                   WRITE(N6,9156)
                   STOP
                   ENDIF
      IF(K.EQ.157) THEN
                   WRITE(*,9157)
                   WRITE(N6,9157)
                   STOP
                   ENDIF
      IF(K.EQ.200) THEN
                   WRITE(*,9600)
                   WRITE(N6,9600)
                   STOP
                   ENDIF
CIMT  ADDITIONAL ERROR MESSAGES ADDED BY C. MOORE CDM
      IF(K.EQ.201) THEN
                   WRITE(*,9610) MQUAL
                   WRITE(N6,9610) MQUAL
                   STOP
                   ENDIF
C=======================================================================
  70  FORMAT(6X,'NSCRAT(2) SHOULD NOT BE EQUAL TO ZERO.')
 740  FORMAT(6X,'THE TOTAL NUMBER OF ASSIGNED',
     1' CHANNEL/PIPES/INLETS  INCLUDING DUMMIES,',/,
     26X,'EXCEEDS THE COMMON STORAGE BLOCK DIMENSIONS ,',I5)
 878  FORMAT(6X,'THE NUMBER OF SUBCATCHMENTS CALLING ',
     1' THE GROUNDWATER ROUTINE IS > ',I5)
 907  FORMAT(6X,'RUN STOPPED DUE TO ERROR IN GROUNDWATER INPUT DATA.')
 910  FORMAT(6X,'VARIABLE THISTO USED WHEN RAINFALL KYTPE = 0.')
 920  FORMAT(6X,'NSCRAT(4) SHOULD NOT BE EQUAL TO ZERO WHEN',
     1' IPRN(2) = 0.')
1000  FORMAT(6X,'JLAND CANNOT BE LESS THAN 1 - Check your input.')
1002  FORMAT(6X,'JLAND CANNOT BE GREATER THAN NLU =',I3,
     1  ' - Check your input.')
1003  FORMAT(6X,'NQS CANNOT BE LESS THAN 1 - CHECK YOUR INPUT.')
1004  FORMAT(6X,'NQS CANNOT BE GREATER THAN MAXIMUM NUMBER OF',
     1' RUNOFF QUALITY CONSTITUENTS (MQUAL).')
1005  FORMAT(6X,'NQS CANNOT BE EQUAL TO THE MAXIMUM NUMBER OF',
     1' CONTSTITUENTS (MQUAL) IF IROS EQUALS 1.')
1006  FORMAT(6X,'DRYDAY IS LESS THAN ZERO AND YOU WANT TO RUN',
     1' CONTINUOUS SWMM - ICRAIN EQUALS 1.')
1007  FORMAT(6X,'RAINIT IS LESS THAN ZERO AND IROS EQUALS 1 -',
     1'  CHANGE YOUR INPUT DATA.')
1008  FORMAT(6X,'YOU HAVE AN INVALID BUILDUP EQUATION TYPE FOR',
     1' LAND USE - CHECK YOUR INPUT DATA.')
1009  FORMAT(6X,'YOU HAVE AN INVALID NUMBER FOR JACGUT - CHECK YOUR',
     1' INPUT DATA.')
1011  FORMAT(6X,'SUBCATCHMENT NUMBER DOES NOT MATCH THE NAMES',
     1' IN ARRAY NAMEW - YOU WERE READING LINE K1.')
2080  FORMAT(6X,'ERROR(S) IN SNOW MELT INPUT DATA GIVEN ABOVE.')
2100  FORMAT(6X,'ISNOW =',I2,' THIS VARIABLE MUST BE SET',
     2'IN ACCORDANCE WITH',/,6X,'THE RUNOFF BLOCK LINE DATA',
     3' INPUT AS DESCRIBED IN USERS MANUAL.')
2120  FORMAT(6X,'THE NUMBER OF SUBCATCHMENTS IS GREATER THAN ',
     1'THE ALLOWABLE MAXIMUM',I5)
9100  FORMAT(6X,'JIN INTERFACE FILE IS NOT DEFINED.  IT SHOULD',
     +' BE GREATER THAN ZERO.')
9101  FORMAT(6X,'JOUT INTERFACE FILE IS NOT DEFINED.  IT SHOULD',
     1' BE GREATER THAN ZERO.')
9102  FORMAT(6X,'NUMX IS NOT DEFINED AND EXTRACT OPTION IS USED.',
     1' NUMX SHOULD BE GREATER THAN ZERO.')
9103  FORMAT(6X,'NSCRAT(1) IS NOT DEFINED IN THE COMBINE BLOCK.')
C#### WCH, 8/93.
9104  FORMAT(6X,'NSCRAT(3) is not defined in the RAIN Block,',/,
     16X,'and IFORM = 4 or 9 or 10.')
9111  FORMAT(6X,'IDECID LE 1 and output interface file is not',
     1' defined.')
9112  FORMAT(6X,'File NSCRAT(7) must be defined for conduit depth',
     1' printout.')
9113  FORMAT(6X,'You already have ten rainfall stations on your',
     1' JOUT file.')
9114  FORMAT(6X,'NSCRAT(2) is not defined in the RAIN Block.',/,
     16X,'The Rain Block is adding new information to',/,
     16X,'an existing interface file and needs file',/,
     16X,'NSCRAT(2) to hold the old rainfall time series.')
9115  FORMAT(6X,'NRGAG greater than rainfall stations on',
     1' precipitation file.')
9116  FORMAT(6X,'Some of your rainfall values are less than THISTO',
     1' units apart.')
9117  FORMAT(6X,'A JIN, JOUT, OR NSCRAT FILE uses file numbers 5, 6,',
     1' or 55.  This is not allowed.')
9118  FORMAT(6X,'NKLASS is greater than 2 on data group C1.')
9119  FORMAT(6X,'NUMST on line E1 is greater than allowed by NVST on',/,
     16X,'the parameter statement on TAPES.INC')
9130  FORMAT(6X,'Input interface file is le zero in Stats.')
9131  FORMAT(6X,'Output interface file is le zero in Stats.')
9140  FORMAT(6X,'No J1 line was found in Runoff input file.')
9141  FORMAT(6X,'No J2 line was found in Runoff input file.')
9142  FORMAT(6X,'No J3 line was found in Runoff input file.')
9143  FORMAT(6X,'IMUL less than zero on line JJ.')
9150  FORMAT(6X,'IVAP = 3 allows only 600 values of evaporation.')
9151  FORMAT(6X,'Your starting year for evaporation <   0.')
9152  FORMAT(6X,'Your starting year for evaporation > 100.')
9153  FORMAT(6X,'Index problem with evaporation in WSHED.')
9154  FORMAT(6X,'Land fractions do not total to 1.0 in QHYDRO.')
9155  FORMAT(6X,'NSCRAT(4) = 0 in Rain Block and 15-min.data ',
     1' are being processed.')
9156  FORMAT(6X,'New data in Rain Block are for 15-min. ',
     1'intervals,',/,6X,'but data to be combined on unit ',
     2'JOUT are not.')
9157  FORMAT(6X,'New data in Rain Block are for hourly intervals',/,
     16X,'but data to be combined on unit JOUT are not.')
9500	FORMAT(//,' ===> ERROR !!  PROGRAM EXECUTION STOPPED FOR THE',
     1' FOLLOWING REASON.')
9600	FORMAT(//,' ===> ERROR !!  PROGRAM EXECUTION STOPPED FOR THE',
     1' ABOVE DESRIBED REASON.')
CIMT
9610  FORMAT(6X,'Number of constituents to be routed in TRANSPORT'
     1,/,6X,'specified by NPOLL on B1 data group exceeds ',/,
     26X,'maximum array size (NTQ) which equals ',I5)
9700  FORMAT(//,' ################################################',/,
     1       ' #  A fatal ERROR was found in your data input. #',/,
     2       ' ################################################',/)
C=======================================================================
      STOP
      END
