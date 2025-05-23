C=======================================================================
C     NVORF  ==> Number of variable orifices.
C     NVOTIM ==> Number of data points for variable orifices.
C=======================================================================
C#### WCH, 7/30/97.  Move PARAMETER statement to TAPES.INC.
C####      PARAMETER(NVORF=20,NVOTIM=25)
      COMMON/ORF/LORIF(NEO),AORIF(NEO),CORIF(NEO),
     +           VORIF(NVORF,NVOTIM,3),NVOR,NORIF
Cwch, 7/22/04.
cred  add a place holder for F2 type orifice - 4/15/2002
      integer f2_line,f2_limit
      common/f2_orf/f2_line(neo),f2_limit(neo)
CIM START     
CIM=======================================================================
CIM DATA FOR GATES 
      COMMON/ORF2/NOGATES,ICNODE(NEO),OOPEN(NEO),OCLOSE(NEO),
     .OCAREA(NEO),ORATE(NEO),IDIR(NEO),IOPRNT(NEO),IOINV(NEO)
CIM END     
