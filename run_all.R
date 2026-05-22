# ============================================================================
# EU SAE Package4 -- Quarto-free Pipeline Orchestrator
#
# Usage:  source("app.R")
# Output: outputs/final_report.html
# ============================================================================

# ---- Anchor working directory to this file's location ----
if (requireNamespace("here", quietly = TRUE)) {
  here::i_am("app.R")
} else {
  stop("Package 'here' is required. Install with: install.packages('here')")
}

# ---- R version check ----
if (getRversion() < "4.1.0") {
  stop(sprintf(
    "R >= 4.1.0 is required (you have %s). Please update R from https://cran.r-project.org/",
    getRversion()
  ))
}

# ---- Required packages check ----
.required_packages <- c(
  # Core pipeline
  "here", "dplyr", "tidyr", "ggplot2", "readxl", "writexl", "sf",
  "survey", "emdi", "msae", "purrr", "stringr", "scales", "patchwork",
  "viridis", "knitr", "rmarkdown",
  # Supporting
  "pacman", "data.table", "tidyverse", "car", "sae", "spdep",
  "MASS", "caret", "gt", "rlang", "tictoc", "matrixcalc",
  "conflicted", "base64enc", "yaml"
)

.missing <- .required_packages[!vapply(.required_packages, requireNamespace,
                                        logical(1), quietly = TRUE)]
if (length(.missing) > 0) {
  message("============================================================")
  message("The following required packages are NOT installed:")
  message("  ", paste(.missing, collapse = ", "))
  message("")
  message("Install them with:")
  message('  install.packages(c("', paste(.missing, collapse = '", "'), '"))')
  message("============================================================")
  stop("Missing packages. Install them and re-run source('app.R').")
}

# ---- Create output directories ----
dirs <- c(
  here::here("outputs", "figures"),
  here::here("outputs", "tables"),
  here::here("outputs", "data")
)
for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

cat("============================================================\n")
cat("EU SAE Package4 -- Quarto-free Pipeline\n")
cat("============================================================\n\n")

# ---- Step 1: Run UFH pipeline ----
cat("[1/4] Running Univariate Fay-Herriot (UFH) pipeline...\n")
source(here::here("R", "01_ufh.R"), local = new.env(parent = globalenv()))
cat("  UFH complete.\n\n")

# ---- Step 2: Run MFH pipeline ----
cat("[2/4] Running Multivariate Fay-Herriot (MFH) pipeline...\n")
source(here::here("R", "02_mfh.R"), local = new.env(parent = globalenv()))
cat("  MFH complete.\n\n")

# ---- Step 3: Run Comparison pipeline ----
cat("[3/4] Running Comparison pipeline...\n")
source(here::here("R", "03_comparison.R"), local = new.env(parent = globalenv()))
cat("  Comparison complete.\n\n")

# ---- Step 4: Render report ----
cat("[4/4] Rendering final report...\n")
if (!rmarkdown::pandoc_available()) {
  .pandoc_candidates <- c(
    Sys.getenv("RSTUDIO_PANDOC"),
    file.path(Sys.getenv("ProgramFiles"), "RStudio",
              "resources", "app", "bin", "quarto", "bin", "tools"),
    file.path(Sys.getenv("LOCALAPPDATA"), "Pandoc")
  )
  for (.p in .pandoc_candidates) {
    if (nzchar(.p) && file.exists(file.path(.p, "pandoc.exe")) ||
        nzchar(.p) && file.exists(file.path(.p, "pandoc"))) {
      Sys.setenv(RSTUDIO_PANDOC = .p)
      break
    }
  }
}
rmarkdown::render(
  input       = here::here("report.Rmd"),
  output_file = here::here("outputs", "final_report.html"),
  encoding    = "UTF-8",
  quiet       = TRUE
)
cat("  Report rendered: outputs/final_report.html\n\n")

# ---- Session info ----
writeLines(capture.output(sessionInfo()),
           here::here("outputs", "session_info.txt"))
cat("Session info saved to: outputs/session_info.txt\n")

cat("\n============================================================\n")
cat("Pipeline complete. Open outputs/final_report.html to view results.\n")
cat("============================================================\n")
