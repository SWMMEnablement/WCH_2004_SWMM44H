      SUBROUTINE GETHV(PFILEO)
C      Block  -  Transport
C      CALLED FROM SUBROUTINE TRANS NEAR LINE 134
CIMHV  PROGRAM TO GET END OF TIME STEP HEAD (depth) AND VELOCITY AND TRANSFER
CIMHV  INTO PFILEO(NPOLL+1) AND PFILLO(NPOLL+2) ARRAYS RESPECTIVELY
CIMHV  PRIOR TO WRITING TO ITERFACE FILE
CIM    C. Moore 9/8/00
      INCLUDE 'TAPES.INC'
      INCLUDE 'NEW81.INC'
      INCLUDE 'HUGO.INC'
      DIMENSION PFILEO(MQUAL,NTHO)
      DO 1000 I = 1,NE
      M = JR(I)
               DO 6666 J = 1,NOUTS
               IF(JCE.EQ.0.AND.NOE(M).NE.JN(J))  GO TO 6666
               IF(JCE.EQ.1.AND.KOE(M).NE.KJN(J)) GO TO 6666
CIMHV   AVERAGE AREA AND FLOW AT UPSTREAM AND DOWNSTREAM ENDS AT END OF TIME PERIOD
                        AREACIM= (A(M,1,2)+A(M,2,2))/2.0
                        FLOWCIM= (Q(M,1,2)+Q(M,2,2))/2.0
                  if (areacim.gt.0.0) then
                        VELOCIM = FLOWCIM/AREACIM
cim need to unitize area uses function DEPTH to compute depth
Cwch 3/21/02.  Caution!  Incorrect for types 10 and 12 for which GEOM1
C     is not full-flow depth.
                        DEPTCIM = GEOM1(M)*DEPTH(AREACIM/afull(m))
                          else
                        velocim = 0.0
                        deptcim = 0.0
                        endif
cim may need to divide by output time step DT to get correct
cim results for total load in stats
cim     stats multiplies values by time step in seconds.
                        PFILEO(NPOLL+1,J) = DEPTCIM
                        PFILEO(NPOLL+2,J) = VELOCIM
cim       if(j.eq.nouts)   write(n6,*) areacim,flowcim,velocim,deptcim,
cim     +                  m,geom1(m)
 6666          CONTINUE
 1000          CONTINUE
      RETURN
      END

