@echo off

where racket >nul 2>nul
if %errorlevel% equ 0 (
    echo Running Guild on MS-Windows...
    racket src/core.rkt
) else (
    echo You don't seem to have Racket, or it's not in your PATH.
)
