      SUBROUTINE FINDA(PS,AA)
C     TRANSPORT BLOCK
C     CALLED BY ROUTE NEAR LINE 240
C               INITAL NEAR LINE 211
C=======================================================================
C     CALCULATES THE FLOW AREA IN CONDUITS GIVEN THE FLOW RATE.
C
C     UPDATED (NEW COMMON) BY W.C.H., SEPTMEBER 1981.
C     UPDATED 12/31/93 BY RED.  FIX DO LOOP RANGE.
C     UPDATED 09/08/00 BY CIM.  Changes to add option for additional
C                               detail in irregular sections.
C     Use QCURVE(4), not (3), for flow.  WCH, 7/6/01
C     No change to QCURV2.
C     Fix possible error at end of search for correct PS range.  
C        WCH, 5/24/02. 
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      INCLUDE 'FLODAT.INC'
CIM## 9/8/00 add new common
      INCLUDE 'NEWTR.INC'
C=======================================================================
      AA = 0.0
      IF(PS.EQ.0.0) RETURN
      NTPE = NTYPE(M)
C=======================================================================
C     CONDUITS WITH FUNCTIONAL Q-A RELATIONSHIP.
C=======================================================================
      IF(KLASS(NTPE).EQ.1) THEN
                         C2    = -PS
                         ALPHA = 0.2
                         CALL NEWTON(ALPHA,PS,0.0,C2,KFLAG)
                         IF(KFLAG.EQ.2) THEN
                                 IF(JCE.EQ.0) WRITE(N6,910)
     +                                 TIME,N,NOE(M),A(M,1,1)
                                 IF(JCE.EQ.1) WRITE(N6,911)
     +                                 TIME,N,KOE(M),A(M,1,1)
                                 AA = A(M,1,1)
                                 ELSE
                                 AA = ALPHA*AFULL(M)
                                 ENDIF
                         RETURN
                         ENDIF
C=======================================================================
C      SPECIAL ROUTINE FOR HIGH ACCURACY AT LOW FLOWS FOR CIRCULAR PIPE.
C=======================================================================
      IF(NTPE.EQ.1.AND.PS.LE.0.015) THEN
                                  CALL CIRCLE(ALPHA,PS,DN,3)
                                  AA = ALPHA*AFULL(M)
                                  RETURN
                                  ENDIF
C=======================================================================
C     CONDUITS WITH TABULAR Q-A RELATIONSHIP.
C     Note, NTPE also = 16 for parabolic and power functions (changed 
C     from 14 and 15 in Sub. INTRAN).
C=======================================================================
      IF(NTPE.EQ.16) THEN
C=======================================================================
C     PARABOLIC, POWER FUNCTION, OR NATURAL CROSS SECTIONS
C=======================================================================
         MMM     = 26
         KK      = NQC(M)
         DALPHA  = 0.04
C#### RED (WCH), 12/31/93.  CHANGE END OF LOOP TO MMM-1 FROM MMM TO
C####                       AVOID OUT OF BOUNDS ERROR IN LINE 207.
         DO 210 I = 1,MMM-1
         II = I
Cwch, 7/6/01.  Use QCURVE(4), not (3), for flow. 
         IF(PS-QCURVE(KK,4,I+1)) 207,208,210
  210    CONTINUE
         ALPHA = 1.0
         AA = ALPHA*AFULL(M)
         RETURN
  208    I = II
CIM### 9/8/00 Change next if
         IF ((IDETAIL.EQ.0).OR.(I.GE.2)) THEN
            ALPHA = FLOAT(I-1)*0.04
            AA    = ALPHA*AFULL(M)
            RETURN
            ENDIF
  207    I = II
CIM### 9/8/00 Add if statement major changes throughout next section.
         IF ((IDETAIL.EQ.0).OR.(I.GE.2)) THEN
Cwch, 7/6/01.  Use QCURVE(4), not (3), for flow. 
            ALPHA = FLOAT(I-1)*0.04 + (PS-QCURVE(KK,4,I))/
     +                      (QCURVE(KK,4,I+1)- QCURVE(KK,4,I))*DALPHA
C=======================================================================
C        IMPROVE ESTIMATE WITH PARABOLIC INTERPOLATION.
C=======================================================================
            IF(PS.LT.0.015) ALPHA = ALPHA + (PS - QCURVE(KK,4,I)) *
     +        (PS - QCURVE(KK,4,I+1)) * (DALPHA/(QCURVE(KK,4,I+2) -
     +         QCURVE(KK,4,I+1))      - DALPHA/(QCURVE(KK,4,I+1)
     +       - QCURVE(KK,4,I)))/(QCURVE(KK,4,I+2) - QCURVE(KK,4,I))
            AA = ALPHA*AFULL(M)
            RETURN
            ELSE    ! here when IDETAIL.EQ.1 AND I LE 1
            DALPHA = 0.0016
            DO 205 J = 1,MMM-1
            JJ = J
            IF(PS-QCURV2(KK,3,J+1)) 203,204,205
  205       CONTINUE
CIM SHOULD NOT GET HERE
            write(n6,*) 'Error in FINDA - 1',ps,m,kk,qcurv2(kk,3,26)
            STOP ' ERROR IN FINDA - 1'
  203       J=JJ
            ALPHA = FLOAT(J-1)*0.0016 + (PS-QCURV2(KK,3,J))/
     +                    (QCURV2(KK,3,J+1) - QCURV2(KK,3,J))*DALPHA
            AA = ALPHA*AFULL(M)
c     write(n6,*) 'finda3',ps,J,qcurv2(kk,3,J),qcurv2(kk,3,J+1),
c    .alpha,aa,afull(m)
            RETURN
  204       J=JJ
            ALPHA = FLOAT(J-1)*0.0016
            AA = ALPHA*AFULL(M)
c     write(n6,*) 'finda4',ps,J,qcurv2(kk,3,J),QCURV2(KK,3,J+1),
c    .alpha,aa,afull(m)
            RETURN
            ENDIF
CIM### 9/8/00 end of changes.
C=======================================================================
C     OTHER TABULAR CROSS SECTIONS
C=======================================================================
         ELSE  ! Here tabular, but not NTPE = 16. 
         MMM      = MM(NTPE)
         DALPHA   = ANORM(NTPE,2) - ANORM(NTPE,1)
         DO 110 I = 1,MMM-1
         IF(PS-QNORM(NTPE,I+1)) 107,108,110
  107    ALPHA = ANORM(NTPE,I) + (PS-QNORM(NTPE,I))/(QNORM(NTPE,I+1) -
     +                        QNORM(NTPE,I))*DALPHA
C=======================================================================
C        IMPROVE ESTIMATE WITH PARABOLIC INTERPOLATION.
C=======================================================================
         IF(PS.LT.0.015) ALPHA = ALPHA +
     1                       (PS-QNORM(NTPE,I))*(PS-QNORM(NTPE,I+1))*
     2                       (DALPHA/(QNORM(NTPE,I+2)-QNORM(NTPE,I+1))-
     3                       DALPHA/(QNORM(NTPE,I+1) -
     4                       QNORM(NTPE,I)))/
     5                       (QNORM(NTPE,I+2)-QNORM(NTPE,I))
         AA = ALPHA*AFULL(M)
         RETURN
  108    ALPHA = ANORM(NTPE,I+1)
         AA    = ALPHA*AFULL(M)
         RETURN
  110    CONTINUE
C=======================================================================
Cwch, 5/24/02. Since PS should be <= 1.0 and since QNORM-max should be 
C     1.0, should not exit DO-loop this way. but in case we do, because
C     of numerical error or something, we want max. value for area, not
C     zero. (Lew Rossman)
C=======================================================================
C         AA = ALPHA*AFULL(M)
         AA = AFULL(M)
         ENDIF
C=======================================================================
      RETURN
C=======================================================================
  910 FORMAT (/,' ===> WARNING !! NEWTON UNABLE TO FIND AREA GIVEN FLOW.
     1TIME = ',F7.1,', TIME STEP= ',I3,', EXT. ELE. NUM.=',I10,', USE OL
     2D UPSTREAM AREA = ',F6.2)
  911 FORMAT (/,' ===> WARNING !! NEWTON UNABLE TO FIND AREA GIVEN FLOW.
     1TIME = ',F7.1,', TIME STEP= ',I3,', EXT. ELE. NUM.=',A10,', USE OL
     2D UPSTREAM AREA = ',F6.2)
C=======================================================================
      END
