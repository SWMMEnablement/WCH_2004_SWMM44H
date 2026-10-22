#!/bin/sh
# Build SWMM 4.4H natively with gfortran from a clone of WCH_2004_SWMM44H.
# Usage: build_swmm44h.sh <repo>/WCH_2004 <builddir>
# Patches touch only the build copy; the repo stays as distributed.
set -e
SRC=$1; B=$2
rm -rf "$B"; mkdir -p "$B"
cp "$SRC"/*.FOR "$SRC"/*.for "$SRC"/*.INC "$SRC"/*.inc "$B"/
cd "$B"

# Fixed form: lines that start with a tab may run past column 72 (DEC
# tab-format extension); every other line is cut at column 72 as the
# original compilers did, so trailing text like GROUND.FOR:238's
# "//(5.0.010 - LR)" stays outside the statement field.
for f in *.FOR *.for *.INC *.inc; do
  tr -d '\r\032' < "$f" | awk '/^\t/ {print; next} {print substr($0,1,72)}' > "$f.tmp" && mv "$f.tmp" "$f"
done

# GETCUR: Compaq <KK> variable format -> run-time format string (KK is 3 or 4)
sed -i "s/WRITE(N6,3333)/WRITE(N6,'(3(I3,'\/\/CHAR(48+KK)\/\/'F12.4,5X))')/" GETCUR.FOR
sed -i 's/^ 3333 FORMAT(3(I3,<KK>F12.4,5X))/C3333 FORMAT(3(I3,<KK>F12.4,5X))/' GETCUR.FOR
# INDAT2: DEC backslash edit descriptor -> new line
sed -i 's/FORMAT(\\,/FORMAT(\/,/' INDAT2.FOR
# INTRAN: LINK is SWMM's subroutine, not gfortran's LINK intrinsic
sed -i '55i\      EXTERNAL LINK' INTRAN.FOR
sed -i '40i\      EXTERNAL LINK' TRANS.FOR
# RHYDRO1: Compaq REAL name*8 -> DOUBLE PRECISION
sed -i 's/^      REAL LLONG\*8/      DOUBLE PRECISION LLONG/' RHYDRO1.FOR
# TRANS: QLINTP statement function has REAL dummies; convert the REAL*8 actuals
sed -i 's/0\.0,TFILE,THR)/0.0,TFILE,REAL(THR))/; s/TFLW,TFLOW,TIMEHR)/TFLW,REAL(TFLOW),REAL(TIMEHR))/' TRANS.FOR

# MAIN: gfortran cannot open 'CON'; unit 6 is already the console
sed -i "s/^      Open(unit=6,file='CON')/C     Open(unit=6,file='CON')/" MAIN.FOR

# STRIP: the 1X was carriage control under Compaq CARRIAGECONTROL='FORTRAN'
# (removed in the 2013 port); without it the blank shifts every column
sed -i 's/^ 7000 FORMAT(1X,3001A1)/ 7000 FORMAT(3001A1)/' STRIP.FOR

# Compaq NARGS(): argument count including the program name
printf '      INTEGER FUNCTION NARGS()\n      NARGS = IARGC() + 1\n      END\n' > NARGSSHIM.FOR

FL="-std=legacy -fdec -fallow-argument-mismatch -ffixed-line-length-none -fno-automatic -finit-local-zero -w -O1 -I."
fail=0
for f in *.FOR *.for; do
  gfortran -c $FL "$f" -o "${f%.*}.o" > "${f%.*}.err" 2>&1 || { echo "FAIL $f"; fail=1; }
done
[ $fail = 0 ] || exit 1
gfortran -o swmm44h.exe *.o -static
echo "built $B/swmm44h.exe"
