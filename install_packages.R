# ============================================================
# install_packages.R
# Run this script ONCE before launching app.R to ensure all
# required R packages are installed.
#
# Usage:  source("install_packages.R")
#    or:  Rscript install_packages.R
# ============================================================

cat("=== EU SAE Shiny App - Package Installer ===\n\n")

# --- R version check ----------------------------------------
# Current CRAN dependencies used by the pipeline (notably emdi)
# require R 4.2.0 or later.
.min_r_version <- "4.2.0"
if (getRversion() < .min_r_version) {
  stop(sprintf(
    "R >= %s is required (you have %s). Please update R from https://cran.r-project.org/",
    .min_r_version,
    getRversion()
  ), call. = FALSE)
}

# --- Helper: install only if not already available ----------
install_if_missing <- function(pkg, repos = "https://cloud.r-project.org") {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("  Installing:", pkg, "\n")
    install.packages(pkg, repos = repos, quiet = TRUE)
  } else {
    cat("  OK:", pkg, "\n")
  }
}

# --- 1. CRAN packages --------------------------------------
cat("[1/3] Checking CRAN packages...\n")

cran_packages <- c(
  # Shiny app
  "shiny",

  # Data manipulation
  "data.table", "dplyr", "tidyr", "purrr", "stringr",
  "tibble", "rlang", "tidyverse", "reshape2",

  # Small area estimation
  "sae", "msae", "emdi",

  # Survey design
  "survey",


  # Spatial
  "sf", "spdep",

  # Visualisation
  "ggplot2", "patchwork", "viridis", "scales",

  # Tables and reporting
  "gt", "knitr", "rmarkdown", "openxlsx", "writexl", "readxl",

  # Statistical / matrix
  "MASS", "Matrix", "matrixcalc", "moments", "magic", "car", "caret",

  # API / web
  "httr", "jsonlite", "base64enc",

  # Utilities
  "yaml", "here", "tictoc", "conflicted", "pacman", "pins", "tools"
)

for (pkg in cran_packages) {
  install_if_missing(pkg)
}

# --- 2. Optional Quarto CLI check ---------------------------
cat("\n[2/3] Checking optional Quarto support...\n")

quarto_path <- if (requireNamespace("quarto", quietly = TRUE)) {
  tryCatch(quarto::quarto_path(), error = function(e) "")
} else {
  ""
}

quarto_path <- if (is.character(quarto_path) && length(quarto_path) > 0) {
  unname(quarto_path[[1]])
} else {
  ""
}

if (nzchar(quarto_path)) {
  cat("  Optional Quarto CLI found:", quarto_path, "\n")
} else {
  cat("  Optional Quarto CLI not found. This dashboard uses rmarkdown for reports, so Quarto is not required.\n")
}

# --- 3. Verify all packages load ---------------------------
cat("\n[3/3] Verifying all packages can be loaded...\n")

failed <- character()
for (pkg in cran_packages) {
  ok <- requireNamespace(pkg, quietly = TRUE)
  if (!ok) failed <- c(failed, pkg)
}

if (length(failed) == 0) {
  cat("\n  All packages installed successfully.\n")
  cat("  You can now run the app with:  shiny::runApp('app.R')\n\n")
} else {
  cat("\n  WARNING: The following packages could not be loaded:\n")
  cat("    ", paste(failed, collapse = ", "), "\n")
  cat("  Please install them manually and try again.\n\n")
}
