C#### WCH, 11/19/93.  ADD CHARACTER KUNC1 AND KUNC2 ARRAYS FOR APHANUMERIC.
C                     AND IPOLLU VARIABLE.
CIMT  change dimensions from 10 to MQUAL
      CHARACTER    PNAM2*10,PUNIT2*10,PNAM1*10,PUNIT1*10,
     1             KODEX*10,KODER*10,KUNC1*10,KUNC2*10
      COMMON/COLL1/IPOLLU 
	  COMMON/COLL2/CPOLL(MQUAL,NIE),POLL1(MQUAL,NIE),
     1             POLL2(MQUAL,NIE),POLL3(MQUAL,NIE),POLD1(MQUAL,NIE),
     2             POLD2(MQUAL,NIE),QO1(NIE),QO2(NIE),QO3(NIE),
     3             QQO(NIE),QOLD1(NIE),QOLD2(NIE),TDIF1(NIE),TDIF2(NIE)
      COMMON/COLL2/NODEX(NIE),NODER(NIE),JCOMB(NIE),JUNC1(NIE),
     1             JUNC2(NIE),INPOS1(NIE),INPOS2(NIE),NDIM1(MQUAL),
     2             NDIM2(MQUAL),NPOS1(MQUAL),NPOS2(MQUAL),NUMX,
     3             KODEX(NIE),KODER(NIE),PNAM2(MQUAL),PUNIT2(MQUAL),
     4             PNAM1(MQUAL),PUNIT1(MQUAL),KUNC1(NIE),KUNC2(NIE)
