      SUBROUTINE TRANNORM(ICALL,DEEP2,DEEP)
C======================================================================
C     CONVERT TO NORMALIZED AREA IF CALLED BY TRANSPORT
C     CALLED ONLY IF KNORM = 1
CIM   EXTRACTED FROM GETCUR BY C. MOORE 
C     WCH, 7/6/01. INCLUDE TOP WIDTH.  NEEDED FOR WATER QUALITY CALCS.
C     USE QCURVE(3) FOR WIDTH, AS IN EXTRAN.  USE NEW QCURVE(4) FOR 
C       FLOW. INCREASE DIMENSION TO 4 OF QCURV AND DUMMYS ALSO. 
C     CHANGES ARE BASICALLY JUST TO ADD NEW QCURVE(4) IN CALCS BELOW. 
C======================================================================
C     Array subscripts:
C	Hydraulic radius   -> QCURVE(1),QCURV(1),DUMMYS(1),QCURV2(1)
C	Cross-section area -> QCURVE(2),QCURV(2),DUMMYS(2),QCURV2(2)
C	Flow               -> QCURVE(4),QCURV(4),DUMMYS(4),QCURV2(3)
C	Top width          -> QCURVE(3),QCURV(3),DUMMYS(3)
C======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'FLODAT.INC'
      DIMENSION QCURV(4,26),DUMMYS(4,26)
CIM IF ICALL = 0 THEN THIS IS THE NORMAL CALL 
CIM                   USES QCURVE AND SAVES RESULTS IN QCURVE
      IF (ICALL.EQ.0) THEN 
           ELDEL = 1.0/25.0
           AREADEL = 1.0/25.0
Cwch, 7/6/01. Put these kinds of statements into a loop. 
           DO 1500 I=1,26
	     DO 1500 J=1,4
 1500      QCURV(J,I) = QCURVE(NATUR,J,I)
          ELSE 
           AREADEL = 1.0/25.0/25.0
           ELDEL = DEEP2/DEEP/25.0
           DO 1505 I=1,26
           QCURV(1,I) = QCURV2(NATUR,1,I)
           QCURV(2,I) = QCURV2(NATUR,2,I)
Cwch, 7/6/01. Note new "equivalence" of QCURV(4) and QCURV2(3)
 1505      QCURV(4,I) = QCURV2(NATUR,3,I) 
           ENDIF
Cwch, 7/6/01.  Assume these subscripts are reversed??
Cwch      DUMMYS(1,1) = 0.0
Cwch      DUMMYS(1,2) = 0.0
Cwch      DUMMYS(1,3) = 0.0
      DO J = 1,4
	   DUMMYS(J,1) = 0.0
	   ENDDO
      I     = 2
      K     = 2
      AA    = AREADEL
 1800 CONTINUE
      IF(QCURV(2,I).GE.AA) THEN
                XDIFF       = AA                - QCURV(2,I-1)
                XSLOPE      = QCURV(2,I) - QCURV(2,I-1)
                DUMMYS(1,K) = QCURV(1,I-1) + (QCURV(1,I) -
     +                QCURV(1,I-1)) * XDIFF/XSLOPE
C=======================================================================
C FOR TRANSPORT, INTERPOLATE TO GET NORMALIZED DEPTH (DUMMYS-2) FOR  
C   EQUAL AREA INCREMENTS.
Cwch, 7/6/01. Flow array is now subscript 4.  Subscript 3 is for top 
C     width. 
C=======================================================================
                DUMMYS(2,K) = FLOAT(I-2)*ELDEL + ELDEL * XDIFF/XSLOPE
         IF(ICALL.EQ.0) DUMMYS(3,K) = QCURV(3,I-1) + (QCURV(3,I) -
     +                QCURV(3,I-1)) * XDIFF/XSLOPE
                DUMMYS(4,K) = QCURV(4,I-1) + (QCURV(4,I) -
     +                QCURV(4,I-1)) * XDIFF/XSLOPE
c      write(n6,*) i,k,aa,qcurv(2,i),qcurv(2,i-1)
c      write(n6,*) qcurv(1,i),qcurv(1,i-1),xdiff,xslope
c      write(n6,*) qcurv(3,i),qcurv(3,i-1)
c      write(n6,*) dummys(1,k),dummys(2,k),dummys(3,k)
                AA          = AA + AREADEL
                K           =  K + 1
                IF(K.EQ.26) GO TO 1810
                ELSE
                I  = I + 1
                ENDIF
           IF (I.LE.26) THEN 
              GO TO 1800
              ELSE
CIM  IF IT GETS TO HERE SOMETHING IS WRONG (ROUNDOFF ERROR??)
              WRITE(N6,*) 'ERROR ???',AA,QCURV(2,26),ICALL,i,k
              ENDIF
 1810 IF (ICALL.EQ.0) THEN 
           DO 1850 I = 1,25
	     DO 1850 J = 1,4
 1850      QCURVE(NATUR,J,I) = DUMMYS(J,I)
cim           QCURVE(NATUR,1,1) = 0.0
cim           QCURVE(NATUR,2,1) = 0.0
cim           QCURVE(NATUR,3,1) = 0.0
cim           QCURVE(NATUR,1,26) = 1.0
cim           QCURVE(NATUR,2,26) = 1.0
cim           QCURVE(NATUR,3,26) = 1.0
           ELSE 
           DO 1860 I = 1,25
           QCURV2(NATUR,1,I) = DUMMYS(1,I)
           QCURV2(NATUR,2,I) = DUMMYS(2,I)
Cwch, 7/6/01.  Use DUMMYS(4), not 3, below
 1860      QCURV2(NATUR,3,I) = DUMMYS(4,I)
cim           QCURV2(NATUR,1,1) = 0.0
cim           QCURV2(NATUR,2,1) = 0.0
cim           QCURV2(NATUR,3,1) = 0.0
cim        QCURV2(NATUR,1,26) = QCURVE(NATUR,1,2)
cim        QCURV2(NATUR,2,26) = QCURVE(NATUR,2,2)
cim        QCURV2(NATUR,3,26) = QCURVE(NATUR,3,2)
           QCURV2(NATUR,2,26) = DEEP2/DEEP
           ENDIF
           END
           