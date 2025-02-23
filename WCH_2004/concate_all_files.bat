@echo off
setlocal enabledelayedexpansion

set "output_base=swmm_WCH44_combined"
set "line_count=0"
set "file_num=1"
set "current_file=%output_base%_!file_num!.FOR"

type nul > "!current_file!"

REM Process both *.for and *.inc files
for %%x in (for inc) do (
    for %%f in (*."%%x") do (
        echo Processing %%f
        echo. >> "!current_file!"
        echo REM File: %%f >> "!current_file!"
        echo. >> "!current_file!"
        
        for /f "delims=" %%l in ('type "%%f"') do (
            set /a "line_count+=1"
            if !line_count! gtr 125000 (
                set /a "file_num+=1"
                set "line_count=1"
                set "current_file=%output_base%_!file_num!.FOR"
                type nul > "!current_file!"
                echo REM Continuing from %%f >> "!current_file!"
                echo. >> "!current_file!"
            )
            echo %%l >> "!current_file!"
        )
    )
)

echo Files have been split into chunks of 125000 lines each