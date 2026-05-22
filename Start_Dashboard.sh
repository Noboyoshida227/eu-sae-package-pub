#!/usr/bin/env bash
# ============================================================
#  EU SAE Dashboard - One-click launcher (Mac / Linux)
#
#  Run this file to start the dashboard:
#    1. Open Terminal
#    2. Navigate to this folder:  cd /path/to/eu-sae-package-pub
#    3. Run:  bash Start_Dashboard.sh
#
#  Your default web browser will open with the dashboard.
#  Keep the terminal open while using the dashboard.
#  Press Ctrl+C to stop the dashboard.
# ============================================================

cd "$(dirname "$0")"

echo "============================================"
echo "   EU SAE Dashboard"
echo "============================================"
echo ""
echo "Starting up..."
echo "Your web browser will open automatically in a few moments."
echo "(First run may take several minutes while packages install.)"
echo ""
echo "IMPORTANT: Keep this terminal open while using the dashboard."
echo "Press Ctrl+C to stop the dashboard."
echo ""
echo "--------------------------------------------"
echo ""

# ---- Locate Rscript ----------------------------------------
RSCRIPT=""

# (1) Try PATH first
if command -v Rscript &> /dev/null; then
  RSCRIPT="$(command -v Rscript)"
fi

# (2) Try common Mac location (Homebrew or CRAN installer)
if [ -z "$RSCRIPT" ] && [ -x "/usr/local/bin/Rscript" ]; then
  RSCRIPT="/usr/local/bin/Rscript"
fi

# (3) Try Mac ARM Homebrew location
if [ -z "$RSCRIPT" ] && [ -x "/opt/homebrew/bin/Rscript" ]; then
  RSCRIPT="/opt/homebrew/bin/Rscript"
fi

# (4) Try CRAN Mac framework location
if [ -z "$RSCRIPT" ]; then
  for d in /Library/Frameworks/R.framework/Versions/*/Resources/bin/Rscript; do
    if [ -x "$d" ]; then
      RSCRIPT="$d"
    fi
  done
fi

if [ -z "$RSCRIPT" ]; then
  echo "ERROR: R is not installed on this computer, or could not be found."
  echo ""
  echo "Please install R 4.2.0 or later from:  https://cran.r-project.org/"
  echo "Then run this script again."
  exit 1
fi

echo "Using R at: $RSCRIPT"
echo ""

# ---- Launch the dashboard -----------------------------------
"$RSCRIPT" -e "if (file.exists('install_packages.R')) source('install_packages.R'); source('app.R')"

if [ $? -ne 0 ]; then
  echo ""
  echo "--------------------------------------------"
  echo "The dashboard exited with an error."
  echo "--------------------------------------------"
fi
