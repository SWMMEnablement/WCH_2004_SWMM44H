@echo off
echo Converting .for and .inc files to .md...

REM Process all .for files
for %%f in (*.for) do (
    echo Processing %%f to %%~nf.md
    echo ```fortran > %%~nf.md
    type %%f >> %%~nf.md
    echo ``` >> %%~nf.md
)

REM Process all .inc files
for %%f in (*.inc) do (
    echo Processing %%f to %%~nf.md
    echo ```fortran > %%~nf.md
    type %%f >> %%~nf.md
    echo ``` >> %%~nf.md
)

echo Conversion complete!
pause