C   Common block for printout of threshold elevation values for junctions.
C   Added 10/14/99  by C. Moore  CDM
C
      Common /ThreshIN/ NTHRESH,NJUNTHR,THRESH(MTHRESH,NEE),
     A                JUNTHR(NEE),THRESHID(MTHRESH)
      Common /ThreshOUT/TFIRST(MTHRESH,NEE),TDUR(MTHRESH,NEE),
     a                ITCOUNT(MTHRESH,NEE),TLASTTIME(MTHRESH,NEE)
      Character*32 THRESHID
      LOGICAL TLASTTIME
