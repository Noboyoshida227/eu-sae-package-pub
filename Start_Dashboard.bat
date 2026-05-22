@echo off
REM ============================================================
REM  EU SAE Dashboard - One-click launcher (Windows)
REM
REM  Double-click this file to start the dashboard.
REM  Your default web browser will open with the dashboard.
REM  Keep this window open while using the dashboard.
REM  Close this window to stop the dashboard.
REM ============================================================

title EU SAE Dashboard
cd /d "%~dp0"

echo ============================================
echo    EU SAE Dashboard
echo ============================================
echo.
echo Starting up...
echo Your web browser will open automatically in a few moments.
echo (First run may take several minutes while packages install.)
echo.
echo IMPORTANT: Keep this window open while using the dashboard.
echo Close this window to stop the dashboard.
echo.
echo --------------------------------------------
echo.

REM ---- Locate Rscript.exe -----------------------------------
set "RSCRIPT="

REM (1) Try PATH first
for /f "delims=" %%I in ('where Rscript.exe 2^>nul') do (
  if not defined RSCRIPT set "RSCRIPT=%%I"
)

REM (2) Try standard 64-bit install location, newest version wins
if not defined RSCRIPT (
  for /d %%D in ("C:\Program Files\R\R-*") do (
    if exist "%%~D\bin\Rscript.exe" set "RSCRIPT=%%~D\bin\Rscript.exe"
  )
)

REM (3) Try 32-bit install location
if not defined RSCRIPT (
  for /d %%D in ("C:\Program Files (x86)\R\R-*") do (
    if exist "%%~D\bin\Rscript.exe" set "RSCRIPT=%%~D\bin\Rscript.exe"
  )
)

REM (4) Try user-local install location
if not defined RSCRIPT (
  for /d %%D in ("%LOCALAPPDATA%\Programs\R\R-*") do (
    if exist "%%~D\bin\Rscript.exe" set "RSCRIPT=%%~D\bin\Rscript.exe"
  )
)

if not defined RSCRIPT (
  echo ERROR: R is not installed on this computer, or could not be found.
  echo.
  echo Please install R 4.2.0 or later from:  https://cran.r-project.org/
  echo Then double-click this file again.
  echo.
  pause
  exit /b 1
)

echo Using R at: %RSCRIPT%
echo.

REM ---- Launch the dashboard ---------------------------------
REM  - Sources install_packages.R if present (installs missing packages on first run)
REM  - Then sources app.R directly. app.R contains its own non-interactive
REM    launcher (shiny::runApp(.app, port=7777, launch.browser=TRUE)) that
REM    fires automatically under Rscript. Calling source('app.R') instead
REM    of shiny::runApp(appDir=...) avoids a double-runApp nesting that
REM    breaks static asset serving (www/eu_poverty_map.png and friends).
"%RSCRIPT%" -e "if (file.exists('install_packages.R')) source('install_packages.R'); source('app.R')"

REM Pause only if R exited with an error so the user can read the message
if errorlevel 1 (
  echo.
  echo --------------------------------------------
  echo The dashboard exited with an error.
  echo --------------------------------------------
  pause
)
