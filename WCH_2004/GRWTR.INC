Cwch, 7/1/03. New variables for printout, rows 7 and 8. 
Cwch, 7/1/03. Groundwater input can occur for any subcat in sequence.
C Must have all GW params except printout variables dimensioned for NW,
C the way the code is constructed in WSHED. Otherwise, would need
C separate indexing system for GW params, but this would be more work
C to implement now. 
C Let NGW = number of GW subcats for which printouts or plots can be
C obtained. 
      CHARACTER    KMSUB*10,KGWTOG*10,KMSUPR*10,KGWTPR*10,KMSUPL*10
      COMMON/GRWTR/GWFLOW(NW),BELEV(NW),GRELEV(NW),STG(NW),
     1             BC(NW),TW(NW),A1(NW),B1(NW),A2(NW),
     2             B2(NW),POR(NW),WP(NW),FC(NW),HKSAT(NW),
     3             TH1(NW),HCO(NW),PCO(NW),CET(NW),XTRENF(NW),
cim  change dimension of NMSUB FROM NGW to NW.     
     6             A3(NW),DP(NW),DET(NW),NMSUB(NW),KMSUB(NW),
     4             NSCSFG(NW),NGWGW(NW),KGWTOG(NW),NGWTOG(NW),
     5             ETU,ETD,STBPAR,ENFIL,NGWGF,NSVGW,DEPPRC,NOGWSC,
     7             IRFLAG,ETUPR(NGW),ETDPR(NGW),DEPPR(NGW),ENFPR(NGW),
     8             EVGWPR(NGW),KMSUPR(NGW),KGWTPR(NGW),NAMWPR(NGW),
     9             NGWTPR(NGW),NAMWPL(NGW),KMSUPL(NGW) 
