rem run CDM test files
..\swmm44h extran44.doc.txt ext44doc.out
..\swmm44h runoff44.doc.txt run44doc.out
del raindoc.dnt
..\swmm44h rain.doc.txt raindoc.out
..\swmm44h swdat1.dat swdat1.out
..\swmm44h swdat4.dat swdat4.out
..\swmm44h trans.doc.txt transdoc.out
..\swmm44h temp.doc.txt tempdoc.out
..\swmm44h strt.doc.txt strtdoc.out
..\swmm44h stats.doc.txt statsdoc.out

