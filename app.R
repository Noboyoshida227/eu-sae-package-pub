library(shiny)

# ---- Startup validation ----
# Check R version (>= 4.2 required by current CRAN dependencies)
if (getRversion() < "4.2.0") {
  stop(sprintf(
    "R >= 4.2.0 is required (you have %s). Please update R from https://cran.r-project.org/",
    getRversion()
  ))
}
if (requireNamespace("here", quietly = TRUE)) here::i_am("app.R")

.detect_app_dir <- function() {
  frame_files <- vapply(
    sys.frames(),
    function(env) {
      ofile <- env$ofile
      if (is.null(ofile) || length(ofile) == 0) "" else as.character(ofile)[1]
    },
    character(1)
  )
  frame_files <- frame_files[nzchar(frame_files)]
  if (length(frame_files) > 0) {
    return(dirname(normalizePath(tail(frame_files, 1), winslash = "/", mustWork = TRUE)))
  }

  if (requireNamespace("here", quietly = TRUE)) {
    here_dir <- tryCatch(here::here(), error = function(e) "")
    if (nzchar(here_dir)) {
      return(normalizePath(here_dir, winslash = "/", mustWork = TRUE))
    }
  }

  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    editor_path <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(editor_path)) {
      return(dirname(normalizePath(editor_path, winslash = "/", mustWork = TRUE)))
    }
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

.app_dir <- .detect_app_dir()
if (!identical(normalizePath(getwd(), winslash = "/", mustWork = TRUE), .app_dir)) {
  setwd(.app_dir)
}

# Check that all required source files exist before loading
.required_source_files <- c(
  "app_support.R",
  "R/validation_checks.R",
  "R/multilingual.R",
  "R/llm_assistant.R",
  "R/brief_generator.R",
  "R/normality_evaluator.R",
  "R/comparison_report_ai.R",
  "R/indicator_helpers.R"
)
.missing_files <- .required_source_files[!file.exists(.required_source_files)]
if (length(.missing_files) > 0) {
  stop(sprintf(
    "Missing required files: %s\nPlease ensure you are running the app from the project root directory and all files are present.",
    paste(.missing_files, collapse = ", ")
  ))
}

# Check critical package dependencies with helpful messages
.critical_packages <- list(
  here       = "install.packages('here')      # needed for analysis scripts",
  yaml       = "install.packages('yaml')",
  dplyr      = "install.packages('dplyr')",
  readxl     = "install.packages('readxl')",
  rmarkdown  = "install.packages('rmarkdown') # needed for report rendering",
  httr       = "install.packages('httr')      # needed for AI Assistant",
  jsonlite   = "install.packages('jsonlite') # needed for AI Assistant",
  sf         = "install.packages('sf')         # requires GDAL/GEOS/PROJ system libraries on macOS/Linux"
)
.missing_pkgs <- names(.critical_packages)[!vapply(names(.critical_packages), requireNamespace, logical(1), quietly = TRUE)]
if (length(.missing_pkgs) > 0) {
  install_hints <- vapply(.missing_pkgs, function(p) .critical_packages[[p]], character(1))
  warning(sprintf(
    paste0(
      "Some recommended packages are not installed:\n  %s\n",
      "Install them with:\n  %s\n",
      "Or run: source('install_packages.R')"
    ),
    paste(.missing_pkgs, collapse = ", "),
    paste(install_hints, collapse = "\n  ")
  ), immediate. = TRUE)
}

# Check that data files exist
.required_data <- c("data/pov_direct3.rds", "data/sae_data.rds", "data/geometries.rds")
.missing_data <- .required_data[!file.exists(.required_data)]
if (length(.missing_data) > 0) {
  warning(sprintf(
    "Default data files not found: %s\nThe app will still work if you upload your own data files.",
    paste(.missing_data, collapse = ", ")
  ), immediate. = TRUE)
}

source("app_support.R")
source("R/validation_checks.R")
source("R/multilingual.R")
source("R/llm_assistant.R")
source("R/brief_generator.R")
source("R/normality_evaluator.R")
source("R/comparison_report_ai.R")
source("R/indicator_helpers.R")

split_csv <- function(x) {
  vals <- trimws(unlist(strsplit(x %||% "", ",")))
  vals[nzchar(vals)]
}

resolve_upload <- function(file_input, fallback = NULL) {
  if (is.null(file_input) || is.null(file_input$datapath) || !nzchar(file_input$datapath)) {
    return(fallback)
  }
  normalizePath(file_input$datapath, winslash = "/", mustWork = TRUE)
}

# Helper: read uploaded or default data and harmonize variable names
load_and_harmonize <- function(survey_path, rhs_path, var_map, rhs_domain,
                               povline_type = "column", povline_value = NULL) {
  survey_raw <- tryCatch(readRDS(survey_path), error = function(e) NULL)
  rhs_raw    <- tryCatch(readRDS(rhs_path),    error = function(e) NULL)

  if (is.null(survey_raw) || is.null(rhs_raw)) return(NULL)

  # Build rename vector from var_map
  rename_vec <- c()
  if (var_map$domain  != "domain")  rename_vec <- c(rename_vec, domain  = var_map$domain)
  if (var_map$psu     != "psu")     rename_vec <- c(rename_vec, psu     = var_map$psu)
  if (var_map$welfare != "welfare") rename_vec <- c(rename_vec, welfare = var_map$welfare)
  if (var_map$weight  != "weight")  rename_vec <- c(rename_vec, weight  = var_map$weight)
  if (var_map$year    != "year")    rename_vec <- c(rename_vec, year    = var_map$year)
  # Only rename povline column when the poverty line comes from data
  if (identical(povline_type, "column") && !is.null(var_map$povline) &&
      var_map$povline != "povline") {
    rename_vec <- c(rename_vec, povline = var_map$povline)
  }

  survey_data <- survey_raw
  for (new_name in names(rename_vec)) {
    old_name <- rename_vec[[new_name]]
    if (old_name %in% names(survey_data)) {
      names(survey_data)[names(survey_data) == old_name] <- new_name
    }
  }

  # When the poverty line is a numeric constant, create the column
  if (identical(povline_type, "numeric") && !is.null(povline_value)) {
    survey_data$povline <- as.numeric(povline_value)
  }

  rhs_data <- rhs_raw
  if (rhs_domain != "domain" && rhs_domain %in% names(rhs_data)) {
    names(rhs_data)[names(rhs_data) == rhs_domain] <- "domain"
  }

  list(survey = survey_data, rhs = rhs_data)
}

# Helper: compute per-year data summaries for diagnostics / brief
# Now indicator-aware: for "mean_welfare" the per-domain summary is
# the (unweighted) domain mean of welfare (optionally on the log scale)
# instead of the FGT.
build_year_summary <- function(survey_data, yr, fgt_alpha = 0L,
                               indicator_type = "poverty",
                               log_transform = FALSE) {
  sv <- survey_data[survey_data$year == yr, ]
  n_domains <- length(unique(sv$domain))

  # Weighted per-domain summary so the dashboard agrees with the
  # `survey::svymean()`-based direct estimates the pipeline actually
  # exports. Using `tapply(..., mean)` would give an unweighted mean
  # which can disagree with the exported Direct column (e.g. a domain's
  # arithmetic weighted mean of welfare can differ noticeably from its
  # unweighted mean if weights are heterogeneous).
  weighted_mean_by_domain <- function(target, domain, weight) {
    domains <- unique(domain)
    out <- setNames(rep(NA_real_, length(domains)), as.character(domains))
    for (d in domains) {
      idx <- domain == d
      ok  <- idx & !is.na(target) & !is.na(weight)
      if (any(ok)) out[as.character(d)] <- stats::weighted.mean(target[ok], weight[ok])
    }
    out
  }

  if (identical(indicator_type, "mean_welfare")) {
    # Always summarize on the arithmetic-mean welfare scale so the
    # dashboard agrees with the exported Direct column (which is now
    # svymean(welfare), not exp(svymean(log welfare))). The model is
    # still fitted on the log scale internally when `log_transform` is
    # TRUE, but that's a model-internal detail and not what the
    # diagnostics tab should display.
    w <- as.numeric(sv$welfare)
    pov_rates <- weighted_mean_by_domain(w, sv$domain, sv$weight)
  } else {
    fgt_vals <- if (fgt_alpha == 0L) {
      as.numeric(sv$welfare < sv$povline)
    } else {
      pmax(0, (sv$povline - sv$welfare) / sv$povline)^fgt_alpha
    }
    pov_rates <- weighted_mean_by_domain(fgt_vals, sv$domain, sv$weight)
  }

  diag <- list(
    year                 = as.character(yr),
    model_type           = "UFH",
    convergence          = TRUE,
    n_domains            = n_domains,
    re_shapiro_pvalue    = NA_real_,
    re_shapiro_pass      = NA,
    resid_shapiro_pvalue = NA_real_,
    resid_shapiro_pass   = NA,
    variance_estimate    = NA_real_
  )

  bench <- list(
    estimate_range   = round(range(pov_rates, na.rm = TRUE), 4),
    estimate_median  = round(median(pov_rates, na.rm = TRUE), 4),
    estimate_mean    = round(mean(pov_rates, na.rm = TRUE), 4),
    cv_median        = NA_real_,
    cv_max           = NA_real_,
    n_cv_above_25pct = NA_integer_,
    mse_median       = NA_real_,
    n_domains        = n_domains,
    n_obs            = nrow(sv)
  )

  list(diag = diag, bench = bench)
}

# Read output Excel files after pipeline run for richer diagnostics
read_pipeline_outputs <- function() {
  ufh_path <- "outputs/data/pov_fh.xlsx"
  mfh_path <- "outputs/data/pov_mfh.xlsx"
  sig_path <- "outputs/tables/statistical_significance_results.csv"

  ufh_shapiro_path <- "outputs/tables/ufh_shapiro_results.csv"
  mfh_shapiro_path <- "outputs/tables/mfh_shapiro_results.csv"

  result <- list()

  if (file.exists(ufh_path) && requireNamespace("readxl", quietly = TRUE)) {
    result$ufh <- tryCatch(readxl::read_excel(ufh_path), error = function(e) NULL)
  }
  if (file.exists(mfh_path) && requireNamespace("readxl", quietly = TRUE)) {
    result$mfh <- tryCatch(readxl::read_excel(mfh_path), error = function(e) NULL)
  }
  if (file.exists(sig_path)) {
    result$significance <- tryCatch(read.csv(sig_path), error = function(e) NULL)
  }
  if (file.exists(ufh_shapiro_path)) {
    result$ufh_shapiro <- tryCatch(read.csv(ufh_shapiro_path), error = function(e) NULL)
  }
  if (file.exists(mfh_shapiro_path)) {
    result$mfh_shapiro <- tryCatch(read.csv(mfh_shapiro_path), error = function(e) NULL)
  }
  result
}

# Enrich diagnostics with Shapiro-Wilk p-values from exported CSV
enrich_diag_with_shapiro <- function(diag, shapiro_df, yr, model_type = "UFH") {
  if (is.null(shapiro_df) || is.null(diag)) return(diag)
  yr_data <- shapiro_df[shapiro_df$year == yr, ]
  if (nrow(yr_data) == 0) return(diag)

  resid_row <- yr_data[yr_data$component == "residual", ]
  re_row    <- yr_data[yr_data$component == "random_effect", ]

  if (nrow(resid_row) > 0 && !is.na(resid_row$p_value[1])) {
    diag$resid_shapiro_pvalue <- resid_row$p_value[1]
    diag$resid_shapiro_pass   <- resid_row$p_value[1] >= 0.05
  }
  if (nrow(re_row) > 0 && !is.na(re_row$p_value[1])) {
    diag$re_shapiro_pvalue <- re_row$p_value[1]
    diag$re_shapiro_pass   <- re_row$p_value[1] >= 0.05
  }
  diag
}

# Build richer diagnostics from pipeline output Excel
enrich_diagnostics_from_output <- function(output_df, yr, model_type = "UFH") {
  if (is.null(output_df)) return(NULL)

  yr_data <- output_df[output_df$year == yr, ]
  if (nrow(yr_data) == 0) return(NULL)

  # Try to extract FH_Bench estimates and CVs
  est_col <- grep("FH_Bench$|MFH_Bench$", names(yr_data), value = TRUE)
  cv_col  <- grep("FH_Bench_CV$|MFH_Bench_CV$", names(yr_data), value = TRUE)
  mse_col <- grep("FH_Bench_MSE$|MFH_Bench_MSE$", names(yr_data), value = TRUE)

  bench <- list(n_domains = nrow(yr_data))

  if (length(est_col) > 0) {
    est_vals <- yr_data[[est_col[1]]]
    bench$estimate_range  <- round(range(est_vals, na.rm = TRUE), 4)
    bench$estimate_median <- round(median(est_vals, na.rm = TRUE), 4)
    bench$estimate_mean   <- round(mean(est_vals, na.rm = TRUE), 4)
  }
  if (length(cv_col) > 0) {
    cv_vals <- yr_data[[cv_col[1]]]
    bench$cv_median       <- round(median(cv_vals, na.rm = TRUE), 4)
    bench$cv_max          <- round(max(cv_vals, na.rm = TRUE), 4)
    bench$n_cv_above_25pct <- sum(cv_vals > 0.25, na.rm = TRUE)
  }
  if (length(mse_col) > 0) {
    bench$mse_median <- round(median(yr_data[[mse_col[1]]], na.rm = TRUE), 6)
  }

  bench
}


# Helper: create a label with an inline tooltip icon
tip_label <- function(label_text, tip_text) {
  span(class = "tt-wrap",
    label_text,
    span(class = "tt-icon", "?"),
    span(class = "tt-text", tip_text)
  )
}

ui <- fluidPage(
  # ---- Cover Page ----
  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('fadeTransition', function(msg) {
        var hide = document.getElementById(msg.hide);
        var show = document.getElementById(msg.show);
        hide.style.transition = 'opacity 0.45s ease';
        hide.style.opacity = '0';
        setTimeout(function(){
          hide.style.display = 'none';
          hide.style.opacity = '1';
          show.style.opacity = '0';
          show.style.display = (show.id === 'main_app') ? 'block' : 'flex';
          show.style.transition = 'opacity 0.45s ease';
          setTimeout(function(){ show.style.opacity = '1'; }, 30);
        }, 450);
      });
    ")),
    tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
    #cover_page {
      position: fixed; top: 0; left: 0; width: 100%; height: 100%;
      background: linear-gradient(160deg, #0f1b3d 0%, #1a2f6b 35%, #1e4d8f 65%, #2a6cb0 100%);
      z-index: 9999; display: flex; flex-direction: column;
      align-items: center; justify-content: center;
      font-family: 'Inter', 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      color: #ffffff; text-align: center;
      animation: fadeIn 1s ease-out;
      overflow: hidden;
    }
    #cover_page::before {
      content: ''; position: absolute; top: -50%; left: -50%;
      width: 200%; height: 200%;
      background: radial-gradient(ellipse at 30% 20%, rgba(109,213,237,0.08) 0%, transparent 50%),
                  radial-gradient(ellipse at 70% 80%, rgba(59,130,200,0.06) 0%, transparent 50%);
      pointer-events: none;
    }
    @keyframes fadeIn { from { opacity: 0; transform: translateY(10px); } to { opacity: 1; transform: translateY(0); } }
    #cover_page .cover-content {
      position: relative; z-index: 1;
      display: flex; flex-direction: column; align-items: center;
      max-width: 780px; padding: 0 24px;
    }
    #cover_page .cover-illustration {
      margin-bottom: 1.4em;
    }
    #cover_page .cover-illustration img {
      border-radius: 10px;
      box-shadow: 0 12px 48px rgba(0,0,0,0.35), 0 2px 12px rgba(0,0,0,0.2);
      border: 1px solid rgba(255,255,255,0.08);
      max-height: 58vh;
      width: auto;
      object-fit: contain;
    }
    #cover_page .cover-label {
      font-size: 0.78em; font-weight: 600; letter-spacing: 3px;
      text-transform: uppercase; color: rgba(157,213,245,0.85);
      margin-bottom: 0.5em;
    }
    #cover_page h1 {
      font-size: 2.6em; font-weight: 700; margin: 0 0 0.2em;
      letter-spacing: -0.5px; line-height: 1.15;
      background: linear-gradient(180deg, #ffffff 30%, rgba(200,225,255,0.85) 100%);
      -webkit-background-clip: text; -webkit-text-fill-color: transparent;
      background-clip: text;
    }
    #cover_page .subtitle {
      font-size: 1.05em; font-weight: 300; color: rgba(220,235,255,0.85);
      max-width: 560px; line-height: 1.55; margin-bottom: 0.3em;
    }
    #cover_page .cover-divider {
      width: 60px; height: 2px; margin: 0.7em auto 0.8em;
      background: linear-gradient(90deg, transparent, rgba(109,213,237,0.5), transparent);
      border: none;
    }
    #cover_page .tagline {
      font-size: 0.85em; font-weight: 400; color: rgba(180,210,240,0.65);
      letter-spacing: 0.5px; margin-bottom: 1.8em;
    }
    #enter_app_btn {
      font-size: 1em; font-weight: 500; padding: 14px 52px;
      background: linear-gradient(135deg, rgba(109,213,237,0.2), rgba(59,123,213,0.25));
      color: #fff; letter-spacing: 0.8px;
      border: 1.5px solid rgba(109,213,237,0.4);
      border-radius: 50px; cursor: pointer;
      transition: all 0.35s ease;
      backdrop-filter: blur(8px);
      box-shadow: 0 2px 16px rgba(0,0,0,0.15);
    }
    #enter_app_btn:hover {
      background: linear-gradient(135deg, rgba(109,213,237,0.35), rgba(59,123,213,0.4));
      border-color: rgba(109,213,237,0.7);
      transform: translateY(-2px);
      box-shadow: 0 6px 28px rgba(109,213,237,0.2);
    }
    #cover_page .cover-footer {
      margin-top: 1.8em;
      text-align: center; font-size: 0.75em; font-weight: 400;
      color: rgba(180,210,240,0.35); letter-spacing: 0.3px;
    }
    #main_app { display: none; }
    #main_app.visible { display: block; }

    /* ---- Tooltips ---- */
    .tt-wrap {
      position: relative;
      display: inline;
    }
    .tt-wrap .tt-icon {
      display: inline-block;
      width: 15px; height: 15px;
      line-height: 15px;
      text-align: center;
      font-size: 10px; font-weight: 700;
      color: #fff;
      background: #6b8db5;
      border-radius: 50%;
      cursor: help;
      margin-left: 3px;
      vertical-align: middle;
    }
    .tt-wrap .tt-text {
      visibility: hidden;
      opacity: 0;
      position: absolute;
      z-index: 1000;
      left: 0; top: 1.6em;
      width: 300px;
      background: #1a2a4a;
      color: #e0ecf8;
      padding: 10px 14px;
      border-radius: 8px;
      font-size: 0.85em;
      font-weight: 400;
      line-height: 1.5;
      box-shadow: 0 4px 20px rgba(0,0,0,0.3);
      border: 1px solid rgba(109,213,237,0.25);
      transition: opacity 0.2s ease, visibility 0.2s ease;
      pointer-events: none;
    }
    .tt-wrap:hover .tt-text {
      visibility: visible;
      opacity: 1;
    }

    /* ---- Guide Page ---- */
    #guide_page {
      display: none; position: fixed; top: 0; left: 0;
      width: 100%; height: 100%; z-index: 9998;
      background: linear-gradient(135deg, #1a2a6c, #2d4899, #3a7bd5);
      color: #ffffff; overflow-y: auto;
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
    }
    #guide_page .guide-container {
      max-width: 860px; margin: 0 auto;
      padding: 40px 30px 60px;
    }
    #guide_page h2 {
      font-size: 2em; font-weight: 700; margin-bottom: 0.3em;
      border-bottom: 2px solid rgba(255,255,255,0.25);
      padding-bottom: 0.3em;
    }
    #guide_page h3 {
      font-size: 1.3em; font-weight: 600; margin-top: 1.6em; margin-bottom: 0.5em;
      color: #9dd5f5;
    }
    #guide_page p, #guide_page li {
      font-size: 1.02em; line-height: 1.7; opacity: 0.92;
    }
    #guide_page ul { padding-left: 1.4em; }
    #guide_page li { margin-bottom: 0.4em; }
    #guide_page .step-card {
      background: rgba(255,255,255,0.08); border: 1px solid rgba(255,255,255,0.15);
      border-radius: 12px; padding: 18px 22px; margin-bottom: 14px;
      display: flex; align-items: flex-start; gap: 16px;
    }
    #guide_page .step-number {
      background: rgba(109,213,237,0.3); border-radius: 50%;
      width: 38px; height: 38px; min-width: 38px;
      display: flex; align-items: center; justify-content: center;
      font-size: 1.15em; font-weight: 700;
    }
    #guide_page .step-text strong { color: #9dd5f5; }
    #guide_page .option-table {
      width: 100%; border-collapse: collapse; margin-top: 0.6em;
    }
    #guide_page .option-table th {
      text-align: left; padding: 8px 12px;
      background: rgba(255,255,255,0.1); font-weight: 600;
      border-bottom: 1px solid rgba(255,255,255,0.2);
    }
    #guide_page .option-table td {
      padding: 8px 12px;
      border-bottom: 1px solid rgba(255,255,255,0.08);
      vertical-align: top;
    }
    #guide_page code {
      background: rgba(255,255,255,0.12); padding: 1px 6px;
      border-radius: 4px; font-size: 0.92em; color: #e0f0ff;
    }
    #guide_page .guide-buttons {
      display: flex; gap: 16px; justify-content: center;
      margin-top: 2.5em;
    }
    .guide-btn {
      font-size: 1.1em; padding: 12px 40px;
      background: rgba(255,255,255,0.15); color: #fff;
      border: 2px solid rgba(255,255,255,0.5);
      border-radius: 50px; cursor: pointer;
      transition: all 0.3s ease; letter-spacing: 0.5px;
    }
    .guide-btn:hover {
      background: rgba(255,255,255,0.28); border-color: #fff;
      transform: scale(1.04);
    }
    .guide-btn-primary {
      background: rgba(109,213,237,0.3);
      border-color: rgba(109,213,237,0.7);
    }
    .guide-btn-primary:hover {
      background: rgba(109,213,237,0.45);
      border-color: #6dd5ed;
    }
  "))),

  div(id = "cover_page",
    div(class = "cover-content",
      div(class = "cover-illustration",
        tags$img(src = "eu_poverty_map.png",
                 style = "width: 90%; max-width: 680px;")
      ),
      div(class = "cover-label", "Small Area Estimation Platform"),
      h1("EU Poverty Mapping"),
      div(class = "subtitle",
        "Poverty rate estimation across NUTS-3 areas using Fay\u2013Herriot models with benchmarking and AI-assisted diagnostics"
      ),
      tags$hr(class = "cover-divider"),
      div(class = "tagline",
        "Univariate & Multivariate FH  \u00b7  Benchmarked Estimates  \u00b7  Automated Reporting"
      ),
      actionButton("enter_app_btn", "Get Started", class = "btn"),
      div(class = "cover-footer",
        "World Bank Group"
      )
    )
  ),

  # ---- Guide Page (page 2) ----
  div(id = "guide_page",
    div(class = "guide-container",

      h2("Welcome to the EU Poverty Mapping App"),
      tags$p("This application helps you produce ",
             tags$strong("Small Area Estimation (SAE)"),
             " of poverty rates for NUTS3 areas using Fay-Herriot models. ",
             "It walks you through the entire pipeline \u2014 from loading data to ",
             "generating benchmarked estimates, diagnostics, and an analysis brief \u2014 ",
             "all within a single guided interface."),

      h3("How it works"),
      tags$p("The pipeline runs in three main steps. Select which ones to execute in the sidebar."),

      # Step 1
      div(class = "step-card",
        div(class = "step-number", "1"),
        div(class = "step-text",
          tags$strong("UFH \u2014 Univariate Fay-Herriot"),
          tags$br(),
          "Fits a univariate Fay-Herriot model for each analysis year separately. ",
          "Performs automated covariate selection, variance smoothing, benchmarking to ",
          "direct regional estimates, and produces diagnostic plots (Q-Q, residuals, maps)."
        )
      ),
      # Step 2
      div(class = "step-card",
        div(class = "step-number", "2"),
        div(class = "step-text",
          tags$strong("MFH \u2014 Multivariate Fay-Herriot"),
          tags$br(),
          "Fits multivariate Fay-Herriot models (MFH1, MFH2, and optionally MFH3) ",
          "that jointly estimate poverty across years, borrowing strength across time. ",
          "Includes benchmarking, significance testing for changes, and model comparisons."
        )
      ),
      # Step 3
      div(class = "step-card",
        div(class = "step-number", "3"),
        div(class = "step-text",
          tags$strong("Comparison"),
          tags$br(),
          "Compares estimates from UFH and MFH models side by side \u2014 maps, scatter plots, ",
          "and tables summarizing gains in precision. Helps you decide which model to report."
        )
      ),

      h3("Key options explained"),

      tags$h4("Data inputs", style = "color: #c2e3f5; margin-top: 1.2em;"),
      tags$p("Upload your own .rds files or use the default Greek province data. ",
             "The variable mapping fields let you tell the app which columns in your data ",
             "correspond to domain, year, weight, welfare, PSU, and poverty line."),

      tags$h4("UFH options", style = "color: #c2e3f5; margin-top: 1.2em;"),
      HTML('<table class="option-table">
        <tr><th>Option</th><th>Values</th><th>What it does</th></tr>
        <tr>
          <td>Transformation (poverty)</td>
          <td><code>arcsin</code> (default) / <code>no</code></td>
          <td>Transformation applied to direct estimates before fitting when the indicator is a poverty rate. The arcsin (arcsine square root) transformation serves two purposes: (1)&nbsp;it ensures that model-based poverty rate estimates are always constrained between 0 and 1, and (2)&nbsp;it stabilizes variance estimates so they remain well-behaved even when poverty rates are close to 0 or 1. Arcsin is recommended for proportions; choose <code>no</code> to fit on the original scale.</td>
        </tr>
        <tr>
          <td>Transformation (mean welfare)</td>
          <td><code>log</code> (default) / <code>no</code></td>
          <td>Transformation applied when the indicator is mean welfare. <code>log</code> fits FH/MFH on log(welfare) to address right-skew, then back-transforms to currency units (observations with welfare &le; 0 are dropped before the log). <code>no</code> fits on the identity (currency) scale directly.</td>
        </tr>
        <tr>
          <td>Bias Correction</td>
          <td><code>bc</code> / <code>bc_sm</code> / <code>none</code></td>
          <td>Bias correction applied when back-transforming the model estimates. For <code>arcsin</code>, use <code>bc</code> to correct the non-linearity of the arcsine transform. For <code>log</code>, use <code>bc_sm</code> &mdash; Duan&rsquo;s smearing estimator &mdash; which multiplies <code>exp(&eta;&#770;)</code> by the empirical mean of <code>exp(residuals)</code> to remove the Jensen-inequality bias from the back-transform without assuming Gaussian residuals. <code>none</code> skips the correction.</td>
        </tr>
        <tr>
          <td>Model selection</td>
          <td><code>BIC</code> (default) / <code>AIC</code></td>
          <td>Information criterion used for stepwise covariate selection. BIC penalizes complexity more heavily and tends to select simpler models.</td>
        </tr>
        <tr>
          <td>Key covariates</td>
          <td>Comma-separated names</td>
          <td>Forces specific covariates into the model. If left blank, the app selects automatically from auxiliary data.</td>
        </tr>
      </table>'),

      tags$h4("MFH options", style = "color: #c2e3f5; margin-top: 1.4em;"),
      HTML('<table class="option-table">
        <tr><th>Option</th><th>Values</th><th>What it does</th></tr>
        <tr>
          <td>Transformation (MFH, mean welfare only)</td>
          <td><code>log</code> (default) / <code>no</code></td>
          <td>Independent from the UFH transformation. <code>log</code> fits MFH on log(welfare) per year and back-transforms each (domain, year) cell with a per-domain-year smearing factor anchored to the survey-weighted arithmetic mean of welfare. MCPE for cross-year change analysis is back-transformed to currency units via a delta-method approximation, so significance tests for changes stay on the EUR scale. <code>no</code> fits on the identity (currency) scale. (For poverty rates, MFH always uses the identity scale &mdash; arcsin is not an option.)</td>
        </tr>
        <tr>
          <td>Bias Correction (MFH, log only)</td>
          <td><code>bc_sm</code> (default) / <code>none</code></td>
          <td>Bias correction for the log &rarr; currency back-transform. <code>bc_sm</code> uses Duan&rsquo;s smearing estimator &mdash; multiplies <code>exp(&eta;&#770;)</code> by the empirical mean of <code>exp(residuals)</code>, robust to non-Gaussian residuals. <code>none</code> returns the naive <code>exp(&eta;&#770;)</code>, which is downward-biased for the mean.</td>
        </tr>
        <tr>
          <td>Model selection</td>
          <td><code>AIC</code> (default) / <code>BIC</code></td>
          <td>Information criterion used for stepwise covariate selection. AIC favours predictive accuracy; BIC penalizes complexity more and selects sparser models.</td>
        </tr>
        <tr>
          <td>Variance option</td>
          <td><code>sm_out</code> (default) / <code>sm_all</code> / <code>direct</code></td>
          <td>Sampling variance input: <code>sm_out</code> replaces only outlier variances (too small or too large) with smoothed (GVF-based) variances; <code>sm_all</code> replaces all variances with smoothed variances; <code>direct</code> uses raw survey variances. The covariance options below update automatically to match.</td>
        </tr>
        <tr>
          <td>Covariance option</td>
          <td>Depends on variance choice (see below)</td>
          <td>How the covariance of sampling errors over time is estimated. Available options depend on the variance choice to ensure consistency: <code>rho_sm_out</code> is available only with <code>sm_out</code>; <code>rho_sm_all</code> only with <code>sm_all</code>; <code>rho_dir</code> only with <code>direct</code>. The <code>direct</code> and <code>zero</code> covariance options are always available.
            <ul style="margin:0.4em 0 0.2em 1.2em; font-size:0.95em;">
              <li><code>rho_sm_out</code> (with <code>sm_out</code>) &ndash; Multiplies the national average autocorrelation (&rho;) with outlier-smoothed variances.</li>
              <li><code>rho_sm_all</code> (with <code>sm_all</code>) &ndash; Multiplies &rho; with fully smoothed variances.</li>
              <li><code>rho_dir</code> &ndash; Multiplies &rho; with direct variance estimates.</li>
              <li><code>direct</code> &ndash; Uses the direct estimates of the variance&ndash;covariance matrix.</li>
              <li><code>zero</code> &ndash; Sets the cross-year sampling covariance to zero (assumes no correlation in sampling errors over time).</li>
            </ul>
          </td>
        </tr>
        <tr>
          <td>Diagnostic model</td>
          <td><code>MFH2</code> / <code>MFH1</code> / <code>MFH3</code></td>
          <td>Which MFH variant drives the Comparison report end-to-end (maps, MCPE change analysis, benchmarked tables). All three are fitted within the multivariate pipeline using the same covariates and sampling variance&ndash;covariance inputs; they differ only in the random-effects structure. UFH is no longer a choice here &mdash; UFH analysis is produced separately by the univariate FH stage (<code>scripts/01_ufh.R</code>).
            <ul style="margin:0.4em 0 0.2em 1.2em; font-size:0.95em;">
              <li><code>MFH1</code> &ndash; Unstructured random-effects covariance across years. Allows free covariance between years but does not impose any temporal pattern. With <em>T</em> years: <em>T</em>(<em>T</em>+1)/2 variance&ndash;covariance parameters.</li>
              <li><code>MFH2</code> (default) &ndash; Homoskedastic AR(1). Assumes a single random-effects variance shared across years and geometric decay of correlation over time. Only 2 parameters (&sigma;&sup2;, &rho;) regardless of the number of years, making it the most parsimonious and easiest to converge.</li>
              <li><code>MFH3</code> &ndash; Heteroskedastic AR(1). Like MFH2 but allows the random-effects variance to differ by year. With <em>T</em> years: <em>T</em>+1 parameters. More flexible than MFH2 but harder to converge.</li>
            </ul>
          </td>
        </tr>
        <tr>
          <td>Try MFH3</td>
          <td>Checkbox</td>
          <td>Whether to also fit MFH3 (random walk model). Adds computation time but may improve estimates when changes are gradual.</td>
        </tr>
      </table>'),

      tags$h4("Other settings", style = "color: #c2e3f5; margin-top: 1.4em;"),
      tags$ul(
        tags$li(tags$strong("PSU consistent:"), " Check this if the same PSU codes refer to the same sampling units across years. ",
                "Affects how cross-year covariance is estimated."),
        tags$li(tags$strong("AI Assistant:"), " Optionally enable an AI-powered assistant that interprets ",
                "diagnostics, evaluates normality assumptions, and enriches the analysis brief. ",
                "Supports Anthropic (Claude) and OpenAI (ChatGPT) API keys. Only aggregate statistics are sent \u2014 never raw microdata."),
        tags$li(tags$strong("Language:"), " Switch language of AI-assisted interpretation. All 24 official EU languages are supported, plus Arabic.")
      ),

      h3("Data Readiness tab"),
      tags$p("After clicking ", tags$strong("Run Pipeline"),
             ", the ", tags$strong("Data Readiness"), " tab displays the results of six automatic diagnostic tests ",
             "that check whether your data are ready for UFH and MFH analysis:"),
      tags$ol(
        tags$li(tags$strong("Year variable presence"), " \u2014 verifies that both the survey data and auxiliary covariates contain a ",
                tags$code("year"), " column."),
        tags$li(tags$strong("Year consistency"), " \u2014 checks that both datasets cover the same set of years (e.g., 2012 and 2013)."),
        tags$li(tags$strong("Auxiliary covariate summary"), " \u2014 computes means, standard errors, observation counts, ",
                "and correlations with the domain-level target indicator (poverty rate or mean welfare, ",
                "matching the Indicator selector) for all numeric covariates."),
        tags$li(tags$strong("Domain consistency"), " \u2014 checks that domain identifiers align across the survey, auxiliary, and geometry datasets."),
        tags$li(tags$strong("Missing poverty rates"), " \u2014 identifies domain-year combinations that lack survey-based poverty estimates."),
        tags$li(tags$strong("National poverty headcount"), " \u2014 reports the weighted national average poverty rate for each year.")
      ),
      tags$p("The tab also shows tables for national poverty rates, domain consistency, missing rates, ",
             "and auxiliary covariate statistics. Results are saved to ", tags$code("outputs/tables/"), " as CSV files."),

      h3("Key output files"),
      tags$p("The pipeline produces the following key files:"),
      HTML('<table class="option-table">
        <tr><th>File</th><th>Description</th></tr>
        <tr>
          <td><code>outputs/final_report.html</code></td>
          <td>Combined report covering UFH, MFH, and Comparison results with scatter plots, precision metrics (MSE, RMSE, CV), normality diagnostics, poverty-level and poverty-change maps, and statistical significance tests.</td>
        </tr>
        <tr>
          <td><code>outputs/comparison_ai_note.html</code></td>
          <td>AI-generated companion note with section-by-section interpretive commentary (overview, normality, rates, precision, significance, maps). Requires the AI Assistant to be enabled.</td>
        </tr>
        <tr>
          <td><code>outputs/data/pov_comparison_detailed.xlsx</code></td>
          <td>Detailed comparison of all poverty estimates (Direct, FH, FH Benchmarked, MFH, MFH Benchmarked) with CVs, MSEs, and RMSEs for every domain and year.</td>
        </tr>
        <tr>
          <td><code>outputs/data/statistical_significance_comparison.xlsx</code></td>
          <td>Combined statistical significance tests for year-on-year poverty changes from both UFH and MFH models, with confidence intervals and significance flags.</td>
        </tr>
      </table>'),

      div(class = "guide-buttons",
        actionButton("guide_back_btn", "Back", class = "guide-btn"),
        actionButton("guide_continue_btn", "Continue to App", class = "guide-btn guide-btn-primary")
      )
    )
  ),

  # ---- Main App (hidden until guide is dismissed) ----
  div(id = "main_app",
    titlePanel("EU Poverty Mapping App"),
    sidebarLayout(
    sidebarPanel(
      # ---- Analysis settings ----
      textInput("years",
        tip_label("Analysis years (two, comma-separated)", "Enter exactly two years separated by a comma. The pipeline estimates poverty for each year and tests for significant changes between them."),
        value = "2012,2013"),
      checkboxGroupInput("steps",
        tip_label("Pipeline steps", "UFH fits a univariate Fay-Herriot model per year. MFH fits multivariate models (MFH1, MFH2, MFH3) that borrow strength across time. Comparison merges both results side by side with maps and precision metrics."),
        choices = c("UFH", "MFH", "Comparison"),
        selected = c("UFH", "MFH", "Comparison")
      ),
      tags$hr(),

      # ---- Data uploads ----
      h4("Data (optional uploads)"),
      fileInput("survey_file",
        tip_label("Survey data (.rds)", "Household-level records with columns for year, domain, PSU, weight, welfare variable, poverty line, and poverty indicator. Leave blank to use the default Greek province data.")),
      fileInput("rhs_file",
        tip_label("Auxiliary covariates (.rds)", "Domain-level covariates used as regressors in the Fay-Herriot models. Leave blank to use the default data.")),
      fileInput("shp_file",
        tip_label("Shapefiles (.rds)", "An sf object with domain polygons for poverty mapping. Leave blank to use the default Greek geometries.")),
      fileInput("regional_benchmark_file",
        tip_label("Regional benchmark targets (optional)",
                  "Optional RDS/CSV/XLSX file with direct regional benchmark estimates by region and year. If supplied, MFH regional benchmarking uses these targets instead of aggregating domain-level direct estimates.")),
      fileInput("population_file",
        tip_label("Domain population sizes (optional)",
                  "Optional RDS/CSV/XLSX file with domain population sizes. Supports long domain-year-population format or wide domain-by-year format; leave blank to use the default sizeprov populations.")),
      tags$hr(),

      # ---- Variable mapping ----
      h4("Variable mapping"),
      textInput("var_year",
        tip_label("year", "Column name in the survey data that identifies the year."),
        "year"),
      textInput("var_domain",
        tip_label("domain", "Column name in the survey data that identifies the small area domain (e.g. NUTS-3 province)."),
        "prov"),
      textInput("var_psu",
        tip_label("psu", "Column name for the Primary Sampling Unit. Used to compute design-based sampling variances."),
        "ea_id"),
      textInput("var_weight",
        tip_label("weight", "Column name for the survey sampling weight."),
        "weight"),
      textInput("var_welfare",
        tip_label("welfare", "Column name for the welfare variable (e.g. income or consumption) used to determine poverty status."),
        "income"),

      # ---- Indicator type ----
      # Top-level choice. Switching to "mean_welfare" hides the
      # poverty-line / FGT inputs (they're irrelevant) and exposes
      # an optional log-transform.
      selectInput("indicator_type",
        tip_label("Indicator",
                  "What is being modelled. 'Poverty (FGT)' uses welfare + a poverty line to compute headcount, gap, or severity. 'Mean welfare' uses the survey-weighted mean of the welfare variable directly."),
        choices  = c("Poverty (FGT)" = "poverty",
                     "Mean welfare"  = "mean_welfare"),
        selected = "poverty"),

      # ---- Poverty line (only shown for poverty indicator) ----
      conditionalPanel(
        condition = "input.indicator_type == 'poverty'",
        radioButtons("povline_type",
          tip_label("Poverty line source",
                    "Choose whether the poverty line comes from a column in the survey data or is a fixed numeric value applied to all observations."),
          choices  = c("Column in data" = "column", "Numeric value" = "numeric"),
          selected = "column", inline = TRUE),
        conditionalPanel(
          condition = "input.povline_type == 'column' && input.indicator_type == 'poverty'",
          textInput("var_povline",
            tip_label("povline", "Column name for the poverty line."),
            "povline")
        ),
        conditionalPanel(
          condition = "input.povline_type == 'numeric' && input.indicator_type == 'poverty'",
          numericInput("povline_numeric",
            tip_label("Poverty line value",
                      "A fixed monetary value used as the poverty line for all households."),
            value = 5000, min = 0)
        ),

        # ---- FGT indicator ----
        selectInput("fgt_alpha",
          tip_label("Poverty measure",
                    "FGT(0) = headcount ratio (share below the line). FGT(1) = poverty gap (average depth of shortfall). FGT(2) = poverty severity (squared gap, emphasises the poorest)."),
          choices = c("FGT(0) \u2013 Headcount ratio" = "0",
                      "FGT(1) \u2013 Poverty gap"      = "1",
                      "FGT(2) \u2013 Poverty severity"  = "2"),
          selected = "0")
      ),

      # ---- Mean welfare options (only shown for mean indicator) ----
      # Note: the log/no choice now lives in the UFH "Transformation"
      # dropdown below (which becomes context-aware when the indicator
      # is mean welfare). The standalone "Log-transform welfare"
      # checkbox has been removed in favor of that single source of
      # truth -- it was replicating the same setting in two places.
      conditionalPanel(
        condition = "input.indicator_type == 'mean_welfare'",
        textInput("currency_symbol",
          tip_label("Currency symbol",
                    "Short label appended to axis titles and table headers for mean welfare estimates."),
          value = "EUR")
      ),

      textInput("rhs_domain",
        tip_label("Auxiliary covariates domain field", "Column name in the auxiliary covariates file that identifies the domain. Used to join covariates to survey data."),
        "prov"),
      textInput("shp_domain",
        tip_label("Shapefile domain field", "Column name in the shapefiles file that identifies the domain. Used to join estimates to map polygons."),
        "prov"),
      tags$hr(),

      # ---- UFH options ----
      h4("UFH options"),
      # Transformation dropdown: choices are context-aware.
      #   - poverty indicator: arcsin / no
      #   - mean_welfare      : log    / no
      # The actual choice list is updated by an observer on input$indicator_type
      # (see server logic below). We seed it with the poverty defaults here.
      selectInput("ufh_transformation",
        tip_label("Transformation",
                  paste(
                    "Transformation of the direct estimates before model fitting.",
                    "For poverty rates: 'arcsin' constrains estimates to [0,1] and stabilizes variances.",
                    "For mean welfare: 'log' addresses the right-skewness of welfare; the back-transform to currency units is bias-corrected via Duan's smearing (see Bias Correction below).",
                    "'no' fits on the original scale.")),
        choices = c("arcsin", "no"), selected = "arcsin"),
      # Bias correction is meaningful under arcsin AND log. For arcsin, it
      # corrects the non-linearity of arcsine; for log, it corrects the
      # Jensen-inequality bias of exp(eta_hat). The available options
      # depend on the chosen transformation (set dynamically server-side).
      conditionalPanel(
        condition = "input.ufh_transformation == 'arcsin' || input.ufh_transformation == 'log'",
        selectInput("ufh_backtrans",
          tip_label("Bias Correction",
                    paste(
                      "How the model estimates are bias-corrected when back-transforming to the original scale.",
                      "For arcsin: 'bc' integrates sin^2(.) against the predictive density (correct under Gaussianity); 'none' returns the naive sin^2(eta_hat).",
                      "For log: 'bc_sm' applies Duan's smearing estimator -- multiplies exp(eta_hat) by the empirical mean of exp(residuals), which is non-parametric and robust to non-Gaussian residuals; 'none' returns the naive exp(eta_hat) (downward-biased for the mean).")),
          choices = c("bc", "none"), selected = "bc")
      ),
      # Variance smoothing menu is only meaningful when no transformation
      # is used (arcsin/log already stabilize variances).
      conditionalPanel(
        condition = "input.ufh_transformation == 'no'",
        selectInput("ufh_var_choice",
          tip_label("Variance option (UFH)", "Sampling variance input for UFH when no transformation is used. 'sm_out' replaces only outlier/zero/NA variances with smoothed (GVF-based) variances (default); 'sm_all' replaces all variances with smoothed variances; 'direct' uses raw survey variances as-is (NA/0 are still backfilled). When arcsin or log is selected, this choice is ignored because the transformation already stabilizes variances."),
          choices = c("sm_out", "sm_all", "direct"), selected = "sm_out")
      ),
      selectInput("ufh_ic_criterion",
        tip_label("Model selection criterion", "Information criterion used for stepwise covariate selection. BIC penalizes complexity more heavily and tends to select simpler models. AIC favours predictive accuracy."),
        choices = c("AIC", "BIC"), selected = "BIC"),
      textInput("ufh_candidates_y1",
        tip_label("UFH covariates for Year 1 (comma-separated, optional)",
                  "Forces specific covariates into the UFH model for the first analysis year. If left blank, the app selects automatically from auxiliary data."),
        value = ""),
      textInput("ufh_candidates_y2",
        tip_label("UFH covariates for Year 2 (comma-separated, optional)",
                  "Forces specific covariates into the UFH model for the second analysis year. If left blank, the app selects automatically from auxiliary data."),
        value = ""),
      tags$hr(),

      # ---- MFH options ----
      h4("MFH options"),
      # Transformation choice for MFH. Independent from UFH so the two
      # models can run on different scales if needed (rare, but
      # supported -- e.g. UFH on identity, MFH on log).
      #   - poverty: hidden (MFH never used arcsin; identity scale only)
      #   - mean_welfare: log / no, default log
      # The bias-correction subchoice mirrors the UFH menu but only
      # offers bc_sm (Duan smearing) for log; arcsin is not an MFH
      # option and so 'bc' (integration-based) is not exposed here.
      conditionalPanel(
        condition = "input.indicator_type == 'mean_welfare'",
        selectInput("mfh_transformation",
          tip_label("Transformation (MFH)",
                    paste(
                      "Transformation applied to the MFH model. Independent of the UFH choice above.",
                      "'log' fits MFH on log(welfare) per year, then back-transforms each (domain, year) cell with a per-domain-year smearing factor anchored to the survey-weighted arithmetic mean of welfare. MCPE is back-transformed to currency units via a delta-method approximation, so cross-year change analysis stays on the EUR scale.",
                      "'no' fits on the identity scale.")),
          choices = c("log", "no"), selected = "log"),
        conditionalPanel(
          condition = "input.mfh_transformation == 'log'",
          selectInput("mfh_backtrans",
            tip_label("Bias Correction (MFH)",
                      paste(
                        "Bias correction for the log -> currency back-transform.",
                        "'bc_sm' applies Duan's smearing estimator (multiplies exp(eta_hat) by the empirical mean of exp(residuals)); robust to non-Gaussian residuals.",
                        "'none' returns the naive exp(eta_hat), which is downward-biased for the mean.")),
            choices = c("bc_sm", "none"), selected = "bc_sm")
        )
      ),
      selectInput("mfh_ic_criterion",
        tip_label("Model selection criterion", "Information criterion used for stepwise covariate selection. AIC favours predictive accuracy; BIC penalizes complexity more and selects sparser models."),
        choices = c("AIC", "BIC"), selected = "AIC"),
      selectInput("mfh_var_choice",
        tip_label("Variance option (MFH)", "Sampling variance input: 'sm_out' replaces only outlier variances with smoothed (GVF-based) variances; 'sm_all' replaces all variances with smoothed variances; 'direct' uses raw survey variances. The covariance options below update automatically to match."),
        choices = c("sm_out", "sm_all", "direct"), selected = "sm_out"),
      selectInput("mfh_cov_choice",
        tip_label("Covariance option", "How the covariance of sampling errors over time is estimated. Available options depend on the variance choice above. 'rho_sm_out' multiplies the national average autocorrelation with outlier-smoothed variances (available with sm_out). 'rho_sm_all' multiplies it with fully smoothed variances (available with sm_all). 'rho_dir' multiplies it with direct variances. 'direct' uses the direct variance-covariance matrix. 'zero' assumes no cross-year sampling correlation."),
        choices = c("rho_sm_out", "rho_dir", "direct", "zero"), selected = "rho_sm_out"),
      selectInput("mfh_diag_model",
        tip_label("Diagnostic model", "Which MFH variant drives the Comparison report end-to-end (maps, MCPE change analysis, benchmarked tables). MFH1: unstructured random-effects covariance (T(T+1)/2 parameters). MFH2 (default): homoskedastic AR(1), only 2 parameters, easiest to converge. MFH3: heteroskedastic AR(1), T+1 parameters, more flexible but harder to converge. UFH is no longer listed here -- UFH analysis is produced by the univariate FH stage (scripts/01_ufh.R)."),
        choices = c("MFH2", "MFH1", "MFH3"), selected = "MFH2"),
      checkboxInput("fit_mfh3",
        tip_label("Try MFH3", "Whether to also fit MFH3 (heteroskedastic AR(1) model). Like MFH2 but allows the random-effects variance to differ by year. More flexible but harder to converge and adds computation time."),
        value = FALSE),
      textInput("mfh_candidates_y1",
        tip_label("MFH covariates for Year 1 (comma-separated, optional)",
                  "Forces specific covariates into the MFH model for the first analysis year. If left blank, the app selects automatically from auxiliary data."),
        value = ""),
      textInput("mfh_candidates_y2",
        tip_label("MFH covariates for Year 2 (comma-separated, optional)",
                  "Forces specific covariates into the MFH model for the second analysis year. If left blank, the app selects automatically from auxiliary data."),
        value = ""),
      tags$hr(),

      # ---- Data assessment ----
      h4("Data assessment"),
      checkboxInput("psu_consistent",
        tip_label("PSU codes are consistent over time", "Check this if the same PSU identifiers refer to the same sampling units across years. Affects how cross-year covariance of sampling errors is estimated."),
        value = FALSE),
      tags$hr(),

      # ---- LLM settings ----
      h4("AI Assistant (optional)"),
      uiOutput("llm_consent_ui"),
      checkboxInput("llm_enabled",
        tip_label("Enable AI Assistant", "Enable an AI-powered assistant that interprets diagnostics, evaluates normality assumptions, and enriches the analysis brief. Supports Anthropic (Claude) and OpenAI (ChatGPT) API keys."),
        value = FALSE),
      conditionalPanel(
        condition = "input.llm_enabled",
        passwordInput("api_key",
          tip_label("API Key", "Enter your Anthropic (sk-ant-...) or OpenAI (sk-...) API key. Only aggregate statistics are sent to the API \u2014 never raw microdata."),
          value = ""),
        selectInput("language",
          tip_label("Language", "Switch language of AI-assisted interpretation. All 24 official EU languages are supported, plus Arabic."),
          choices = supported_languages(),
          selected = "en")
      ),
      tags$hr(),

      # Two-stage launch: first review readiness, then run the analysis.
      actionButton("check_btn", "1. Check Data Readiness", class = "btn-default",
                   style = "margin-right: 8px;"),
      actionButton("run_btn",   "2. Run Analysis",         class = "btn-primary")
    ),

    mainPanel(
      tabsetPanel(
        id = "main_tabs",

        tabPanel("Preflight",
          h4("Startup Preflight"),
          p("Review these checks before running the pipeline. This panel updates automatically as you change inputs."),
          uiOutput("preflight_ui"),
          h4("Recommended actions"),
          verbatimTextOutput("preflight_actions")
        ),

        # ---- Tab: Data Readiness ----
        tabPanel("Data Readiness",
          h4("Data Readiness Assessment"),
          p("Click '1. Check Data Readiness' in the sidebar to generate diagnostics. ",
            "Review the results here before starting the UFH / MFH analysis."),
          verbatimTextOutput("readiness_messages"),
          h4("National Poverty Headcount Rates"),
          tableOutput("readiness_national"),
          h4("Domain Consistency"),
          tableOutput("readiness_domains"),
          h4("Missing Poverty Rates"),
          tableOutput("readiness_missing"),
          h4("Auxiliary Covariate Summary"),
          p("Means, standard errors, observation counts, and correlations with the domain-level target indicator (poverty rate or mean welfare, matching the Indicator selector)."),
          tableOutput("readiness_aux")
        ),

        # ---- Tab: Pipeline Status ----
        tabPanel("Status",
          h4("Current step"),
          textOutput("status"),
          h4("Run log"),
          verbatimTextOutput("logs"),
          h4("Expected outputs"),
          tableOutput("outputs")
        )
      )
    )
  ) # end sidebarLayout
  ) # end main_app div
)

server <- function(input, output, session) {
  # ---- Safe accessor for language (defaults to "en" if not yet available) ----
  get_language <- function() input$language %||% "en"

  # ---- Page navigation helpers ----
  fade_transition <- function(hide_id, show_id) {
    session$sendCustomMessage("fadeTransition", list(hide = hide_id, show = show_id))
  }

  # Cover -> Guide
  observeEvent(input$enter_app_btn, { fade_transition("cover_page", "guide_page") })
  # Guide -> Main App
  observeEvent(input$guide_continue_btn, { fade_transition("guide_page", "main_app") })
  # Guide -> Back to Cover
  observeEvent(input$guide_back_btn, { fade_transition("guide_page", "cover_page") })

  # ---- Link variance and covariance options for MFH ----
  # When the variance option changes, update the available covariance choices
  # so that the smoothing level is consistent.
  observeEvent(input$mfh_var_choice, {
    vc <- input$mfh_var_choice
    if (vc == "sm_out") {
      cov_choices <- c("rho_sm_out", "direct", "zero")
      default_sel <- "rho_sm_out"
    } else if (vc == "sm_all") {
      cov_choices <- c("rho_sm_all", "direct", "zero")
      default_sel <- "rho_sm_all"
    } else {
      # direct variance: rho_dir uses direct variances, which is consistent
      cov_choices <- c("rho_dir", "direct", "zero")
      default_sel <- "rho_dir"
    }
    updateSelectInput(session, "mfh_cov_choice",
                      choices = cov_choices, selected = default_sel)
  })

  status    <- reactiveVal("Idle")
  logs      <- reactiveVal("")
  output_rows <- reactiveVal(data.frame(File = character(), Exists = logical(), stringsAsFactors = FALSE))

  # Reactive stores for validation, diagnostics, and brief
  validation_result <- reactiveVal(NULL)
  diagnostics_data  <- reactiveVal(NULL)
  brief_result      <- reactiveVal(NULL)
  llm_interp        <- reactiveVal(NULL)
  llm_brief         <- reactiveVal(NULL)
  normality_eval    <- reactiveVal(NULL)
  readiness_result  <- reactiveVal(NULL)
  # TRUE once the user has clicked "Check Data Readiness" in the current session.
  # The UFH / MFH / Comparison pipeline can only start after this is TRUE, so the
  # user is forced to review the preflight and readiness results before running
  # the analysis.
  readiness_checked <- reactiveVal(FALSE)

  append_log <- function(msg) {
    stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    logs(paste0(logs(), if (nzchar(logs())) "\n" else "", "[", stamp, "] ", msg))
  }

  # ---- LLM consent text (multilingual) ----
  output$llm_consent_ui <- renderUI({
    t <- translator(get_language())
    helpText(t$get("llm_consent_text"))
  })

  # ---- Create LLM assistant ----
  get_llm <- reactive({
    if (isTRUE(input$llm_enabled) && nchar(input$api_key %||% "") > 0) {
      llm_assistant(api_key = input$api_key, provider = detect_llm_provider(input$api_key))
    } else {
      llm_assistant(enabled = FALSE)
    }
  })

  # ---- Build var_map from inputs ----
  get_var_map <- reactive({
    vm <- list(
      year    = input$var_year,
      domain  = input$var_domain,
      psu     = input$var_psu,
      weight  = input$var_weight,
      welfare = input$var_welfare,
      poor    = "poor"
    )
    # Poverty line: column name or NULL (when numeric)
    if (identical(input$povline_type, "column")) {
      vm$povline <- input$var_povline
    }
    vm
  })

  # ---- Auto-disable arcsin for FGT(1)/FGT(2) ----
  observeEvent(input$fgt_alpha, {
    if (as.integer(input$fgt_alpha %||% 0) > 0) {
      updateSelectInput(session, "ufh_transformation", selected = "no")
      showNotification(
        "Arcsin transformation disabled: it is only valid for the headcount ratio FGT(0).",
        type = "warning", duration = 6
      )
    }
  })

  # ---- Swap transformation choices when indicator_type changes ----
  # Poverty -> arcsin / no  (default arcsin for FGT(0); 'no' enforced for FGT(1/2))
  # Mean welfare -> log / no (default log)
  # Bias correction options also depend on the chosen transformation
  # (see the second observer below).
  observeEvent(input$indicator_type, {
    if (identical(input$indicator_type, "mean_welfare")) {
      updateSelectInput(session, "ufh_transformation",
                       choices  = c("log", "no"),
                       selected = "log")
      showNotification(
        "Mean welfare selected. Transformation choices: 'log' (default, with Duan smearing back-transform) or 'no' (identity scale).",
        type = "default", duration = 6
      )
    } else {
      sel <- if (as.integer(input$fgt_alpha %||% 0) > 0) "no" else "arcsin"
      updateSelectInput(session, "ufh_transformation",
                       choices  = c("arcsin", "no"),
                       selected = sel)
    }
  })

  # ---- Update UFH bias-correction choices based on transformation ----
  # arcsin -> bc / none      (bc = integration-based correction for arcsine)
  # log    -> bc_sm / none   (bc_sm = Duan smearing estimator)
  # no     -> hidden by conditionalPanel; nothing to update.
  observeEvent(input$ufh_transformation, {
    if (identical(input$ufh_transformation, "arcsin")) {
      updateSelectInput(session, "ufh_backtrans",
                       choices  = c("bc", "none"),
                       selected = "bc")
    } else if (identical(input$ufh_transformation, "log")) {
      updateSelectInput(session, "ufh_backtrans",
                       choices  = c("bc_sm", "none"),
                       selected = "bc_sm")
    }
  })

  # ---- MFH transformation: only meaningful for mean welfare ----
  # When indicator switches away from mean_welfare, force MFH back to
  # identity ("no") so the run config never accidentally carries a
  # log setting into a poverty-rate run. (The dropdown is hidden in
  # that case via conditionalPanel, but its last value would persist
  # in the input state otherwise.)
  observeEvent(input$indicator_type, {
    if (!identical(input$indicator_type, "mean_welfare")) {
      updateSelectInput(session, "mfh_transformation",
                       choices  = c("log", "no"),
                       selected = "no")
    } else {
      updateSelectInput(session, "mfh_transformation",
                       choices  = c("log", "no"),
                       selected = "log")
    }
  }, ignoreInit = TRUE)

  # ---- Outputs ----
  output$status  <- renderText(status())
  output$logs    <- renderText(logs())
  output$outputs <- renderTable(output_rows(), striped = TRUE)

  output$preflight_ui <- renderUI({
    build_check <- function(label, ok, detail) {
      color <- if (isTRUE(ok)) "#2e7d32" else "#b26a00"
      symbol <- if (isTRUE(ok)) "OK" else "Check"
      tags$div(
        style = "margin-bottom: 12px; padding: 10px 12px; border: 1px solid #ddd; border-radius: 6px;",
        tags$div(
          style = sprintf("font-weight: 600; color: %s;", color),
          sprintf("%s: %s", symbol, label)
        ),
        tags$div(detail)
      )
    }

    has_file <- function(path) nzchar(path %||% "") && file.exists(path)
    survey_path <- resolve_upload(input$survey_file, "data/pov_direct3.rds")
    rhs_path <- resolve_upload(input$rhs_file, "data/sae_data.rds")
    shp_path <- resolve_upload(input$shp_file, "data/geometries.rds")

    root_ok <- all(file.exists(c("app_support.R", "scripts/01_ufh.R", "scripts/02_mfh.R", "scripts/03_comparison.R")))
    years_vec <- parse_years(input$years)
    years_ok <- length(years_vec) == 2
    data_ok <- has_file(survey_path) && has_file(rhs_path) && has_file(shp_path)
    sf_ok <- requireNamespace("sf", quietly = TRUE)
    rmarkdown_ok <- requireNamespace("rmarkdown", quietly = TRUE)

    checks <- list(
      build_check(
        "Project root",
        root_ok,
        if (root_ok) {
          sprintf("Working directory looks correct: %s", normalizePath(".", winslash = "/", mustWork = FALSE))
        } else {
          "Required app files are missing from the current working directory. Launch the app from the package root folder."
        }
      ),
      build_check(
        "rmarkdown",
        rmarkdown_ok,
        if (rmarkdown_ok) {
          "Package 'rmarkdown' is available for report rendering."
        } else {
          "Package 'rmarkdown' is not installed. Report rendering will fail. Install with: install.packages('rmarkdown')"
        }
      ),
      build_check(
        "Analysis years",
        years_ok,
        if (years_ok) {
          sprintf("Two analysis years detected: %s", paste(years_vec, collapse = ", "))
        } else {
          "Enter exactly two years separated by a comma."
        }
      ),
      build_check(
        "Input data files",
        data_ok,
        sprintf(
          "Survey: %s | Auxiliary: %s | Geometry: %s",
          if (has_file(survey_path)) "available" else "missing",
          if (has_file(rhs_path)) "available" else "missing",
          if (has_file(shp_path)) "available" else "missing"
        )
      ),
      build_check(
        "Spatial dependency",
        sf_ok,
        if (sf_ok) {
          "Package 'sf' is available."
        } else {
          "Package 'sf' is not available. Mapping and geometry-dependent steps may fail."
        }
      )
    )

    tags$div(checks)
  })

  output$preflight_actions <- renderText({
    actions <- character()

    root_ok <- all(file.exists(c("app_support.R", "scripts/01_ufh.R", "scripts/02_mfh.R", "scripts/03_comparison.R")))
    if (!root_ok) {
      actions <- c(actions, "- Open or launch the app from the package root directory.")
    }

    if (!requireNamespace("rmarkdown", quietly = TRUE)) {
      actions <- c(actions, "- Install the `rmarkdown` package for report rendering: install.packages('rmarkdown')")
    }

    years_vec <- parse_years(input$years)
    if (length(years_vec) != 2) {
      actions <- c(actions, "- Set `Analysis years` to exactly two comma-separated years.")
    }

    survey_path <- resolve_upload(input$survey_file, "data/pov_direct3.rds")
    rhs_path <- resolve_upload(input$rhs_file, "data/sae_data.rds")
    shp_path <- resolve_upload(input$shp_file, "data/geometries.rds")
    if (!file.exists(survey_path %||% "")) {
      actions <- c(actions, "- Upload a valid survey `.rds` file or keep the default sample survey data in `data/`.")
    }
    if (!file.exists(rhs_path %||% "")) {
      actions <- c(actions, "- Upload a valid auxiliary covariates `.rds` file or keep the default auxiliary data in `data/`.")
    }
    if (!file.exists(shp_path %||% "")) {
      actions <- c(actions, "- Upload a valid geometry `.rds` file or keep the default geometry data in `data/`.")
    }

    if (!requireNamespace("sf", quietly = TRUE)) {
      actions <- c(actions, "- Install the `sf` package and its system libraries before running geometry-dependent outputs.")
    }

    if (.Platform$OS.type == "windows") {
      actions <- c(actions, "- If files are stored in OneDrive, make sure required inputs are fully available offline before running.")
    }

    if (length(actions) == 0) {
      "No obvious blockers detected. You can run the pipeline."
    } else {
      paste(actions, collapse = "\n")
    }
  })


  # ---- Data Readiness tab outputs ----
  output$readiness_messages <- renderText({
    rr <- readiness_result()
    if (is.null(rr)) return("Click '1. Check Data Readiness' in the sidebar to generate diagnostics.")
    paste(rr$messages, collapse = "\n")
  })

  output$readiness_national <- renderTable({
    rr <- readiness_result()
    if (is.null(rr)) return(NULL)
    df <- rr$national_poverty
    # Format and label the headline statistic according to the chosen
    # indicator. For poverty (FGT) it is a rate in [0, 1] shown as a
    # percentage; for mean welfare it is a level in the configured
    # currency (and on the log scale when log_transform is on, in which
    # case formatting it as "%" would mislead).
    ind <- input$indicator_type %||% "poverty"
    use_log <- identical(input$ufh_transformation, "log") && identical(ind, "mean_welfare")
    if (identical(ind, "mean_welfare")) {
      if (use_log) {
        df$national_rate <- sprintf("%.4f", df$national_rate)
        col_label <- "Mean log welfare"
      } else {
        cur <- input$currency_symbol %||% "EUR"
        df$national_rate <- format(round(df$national_rate, 1),
                                    big.mark = ",", nsmall = 1, trim = TRUE)
        col_label <- sprintf("Mean welfare (%s)", cur)
      }
    } else {
      df$national_rate <- sprintf("%.2f%%", df$national_rate * 100)
      col_label <- "Poverty Rate"
    }
    names(df) <- c("Year", col_label, "Households", "Domains")
    df
  }, striped = TRUE, align = "lrrr")

  output$readiness_domains <- renderTable({
    rr <- readiness_result()
    if (is.null(rr)) return(NULL)
    di <- rr$domain_consistency
    rows <- data.frame(
      Dataset  = c("Survey", "Auxiliary"),
      Domains  = c(length(di$survey_domains), length(di$aux_domains)),
      stringsAsFactors = FALSE
    )
    if (!is.null(di$geo_domains)) {
      rows <- rbind(rows, data.frame(Dataset = "Geometries",
                                     Domains = length(di$geo_domains),
                                     stringsAsFactors = FALSE))
    }
    rows <- rbind(rows, data.frame(Dataset = "All Consistent?",
                                   Domains = ifelse(di$all_consistent, "Yes", "No"),
                                   stringsAsFactors = FALSE))
    rows$Domains <- as.character(rows$Domains)
    rows
  }, striped = TRUE)

  output$readiness_missing <- renderTable({
    rr <- readiness_result()
    if (is.null(rr)) return(NULL)
    if (nrow(rr$missing_poverty) == 0) {
      return(data.frame(Result = "No missing poverty rates - all domains have survey-based estimates."))
    }
    names(rr$missing_poverty) <- c("Domain", "Year", "Reason")
    rr$missing_poverty
  }, striped = TRUE)

  output$readiness_aux <- renderTable({
    rr <- readiness_result()
    if (is.null(rr)) return(NULL)
    df <- rr$aux_summary
    cor_label <- attr(df, "cor_target_label") %||% "Corr. w/ Poverty"
    names(df) <- c("Variable", "Mean", "Std. Error", "N", cor_label)
    df
  }, striped = TRUE, digits = 4)

  output$validation_text <- renderText({
    vr <- validation_result()
    if (is.null(vr)) return("No validation run yet. Click 'Run Pipeline' to start.")
    paste(vr$flags, collapse = "\n")
  })

  output$data_summary_text <- renderText({
    vr <- validation_result()
    if (is.null(vr)) return("")
    paste(capture.output(str(vr$summary)), collapse = "\n")
  })

  output$diagnostics_text <- renderText({
    dd <- diagnostics_data()
    if (is.null(dd)) return("No diagnostics available yet. Run the pipeline first.")
    lines <- character()
    for (yr_name in names(dd$diag)) {
      d <- dd$diag[[yr_name]]
      lines <- c(lines, sprintf("--- Year: %s ---", d$year %||% yr_name))
      lines <- c(lines, sprintf("  Model type:  %s", d$model_type %||% "UFH"))
      lines <- c(lines, sprintf("  Domains:     %s", d$n_domains %||% "N/A"))
      lines <- c(lines, sprintf("  Convergence: %s", if (isTRUE(d$convergence)) "Yes" else "N/A"))
      if (!is.na(d$re_shapiro_pvalue %||% NA)) {
        lines <- c(lines, sprintf("  RE normality (Shapiro p): %.4f [%s]",
                                   d$re_shapiro_pvalue,
                                   if (isTRUE(d$re_shapiro_pass)) "PASS" else "FAIL"))
      }
      if (!is.na(d$resid_shapiro_pvalue %||% NA)) {
        lines <- c(lines, sprintf("  Resid normality (Shapiro p): %.4f [%s]",
                                   d$resid_shapiro_pvalue,
                                   if (isTRUE(d$resid_shapiro_pass)) "PASS" else "FAIL"))
      }
      lines <- c(lines, "")

      # Benchmark summary
      b <- dd$bench[[yr_name]]
      if (!is.null(b)) {
        if (!is.null(b$estimate_range)) {
          lines <- c(lines, sprintf("  Estimate range: [%.4f, %.4f]", b$estimate_range[1], b$estimate_range[2]))
        }
        if (!is.na(b$estimate_median %||% NA)) {
          lines <- c(lines, sprintf("  Median estimate: %.4f", b$estimate_median))
        }
        if (!is.na(b$cv_median %||% NA)) {
          lines <- c(lines, sprintf("  Median CV: %.4f", b$cv_median))
        }
        if (!is.na(b$cv_max %||% NA)) {
          lines <- c(lines, sprintf("  Max CV: %.4f", b$cv_max))
        }
        if (!is.na(b$n_cv_above_25pct %||% NA)) {
          lines <- c(lines, sprintf("  Domains with CV > 25%%: %d", b$n_cv_above_25pct))
        }
        lines <- c(lines, "")
      }
    }
    paste(lines, collapse = "\n")
  })

  output$llm_interpretation <- renderText({
    llm_interp() %||% ""
  })

  output$brief_template <- renderText({
    br <- brief_result()
    if (is.null(br)) return("No brief available yet. Run the pipeline first.")
    br$template_brief
  })

  output$brief_llm_text <- renderText({
    llm_brief() %||% ""
  })

  output$normality_eval_text <- renderText({
    normality_eval() %||% ""
  })

  # ---- Invalidate readiness when inputs that affect it change ----
  # If the user swaps data files or changes the variable mapping after clicking
  # "Check Data Readiness", they must re-check before running the analysis.
  # Only the inputs below create reactive dependencies; readiness_checked() is
  # read with isolate() so that setting it to TRUE in check_btn does not
  # immediately re-trigger this observer and flip it back to FALSE.
  #
  # Readiness output depends on more than just the data files and column
  # mapping: it varies with indicator_type, the FGT alpha, and the
  # per-model transformation choices (because log fits trigger
  # log-specific data checks like non-positive welfare). It also
  # depends on which `steps` will run. Touching all of them here forces
  # the user to re-run "Check Data Readiness" if they change any
  # setting that could change the readiness verdict.
  observe({
    # Touch each input so this observer reacts to any of them.
    list(
      # Files and variable mapping
      input$survey_file, input$rhs_file, input$shp_file,
      input$var_year, input$var_domain, input$var_psu,
      input$var_weight, input$var_welfare, input$var_povline,
      input$rhs_domain, input$shp_domain,
      # Poverty-line config
      input$povline_type, input$povline_numeric,
      # Indicator and FGT (changes which Test 4 statistic is computed
      # and which log-specific checks fire)
      input$indicator_type, input$fgt_alpha,
      # Per-model transformation choices (drive log-specific data
      # checks; readiness now ORs UFH and MFH log flags)
      input$ufh_transformation, input$ufh_backtrans,
      input$mfh_transformation, input$mfh_backtrans,
      # Which steps will run (changes the readiness scope/messages)
      input$steps
    )
    isolate({
      if (isTRUE(readiness_checked())) {
        readiness_checked(FALSE)
        append_log("Data inputs or analysis options changed - please re-run 'Check Data Readiness'.")
      }
    })
  })

  # ---- Check Data Readiness (Stage 1) ----
  # Runs only the validation + data-readiness portion. The user reviews the
  # results in the Preflight and Data Readiness tabs, then clicks Run Analysis.
  observeEvent(input$check_btn, {
    status("Checking data readiness...")
    logs("")
    append_log("Running preflight and data readiness checks...")

    survey_path <- resolve_upload(input$survey_file)
    rhs_path    <- resolve_upload(input$rhs_file)
    shp_path    <- resolve_upload(input$shp_file)
    regional_benchmark_path <- resolve_upload(input$regional_benchmark_file)
    population_path <- resolve_upload(input$population_file)
    var_map     <- get_var_map()

    survey_for_validation <- survey_path %||% "data/pov_direct3.rds"
    rhs_for_validation    <- rhs_path    %||% "data/sae_data.rds"

    harmonized <- tryCatch(
      load_and_harmonize(survey_for_validation, rhs_for_validation,
                         var_map, input$rhs_domain,
                         povline_type  = input$povline_type %||% "column",
                         povline_value = input$povline_numeric),
      error = function(e) {
        append_log(paste("ERROR while loading data:", conditionMessage(e)))
        NULL
      }
    )

    if (is.null(harmonized)) {
      status("Data readiness check failed")
      append_log("WARNING: Could not load data for readiness check.")
      showNotification(
        "Could not load data for readiness check. See the Status tab for details.",
        type = "error", duration = 8
      )
      return()
    }

    # Validation flags feed into the Preflight tab.
    flags <- validate_inputs(harmonized$survey, harmonized$rhs)
    validation_result(flags)
    append_log(sprintf("Validation complete: %d flag(s)", length(flags$flags)))

    # Year-variable checks (pre-harmonization) - mirrors the logic inside run_btn.
    year_msgs <- character()
    survey_raw_check <- tryCatch(readRDS(survey_for_validation), error = function(e) NULL)
    rhs_raw_check    <- tryCatch(readRDS(rhs_for_validation),    error = function(e) NULL)
    year_var_name    <- var_map$year

    if (!is.null(survey_raw_check) && !is.null(rhs_raw_check)) {
      survey_has_year <- year_var_name %in% names(survey_raw_check)
      aux_has_year    <- year_var_name %in% names(rhs_raw_check)

      if (survey_has_year && aux_has_year) {
        year_msgs <- c(year_msgs, sprintf(
          "Test 0a: Both survey data and auxiliary covariates contain the year variable '%s'.",
          year_var_name
        ))
      } else {
        missing_in <- character()
        if (!survey_has_year) missing_in <- c(missing_in, "survey data")
        if (!aux_has_year)    missing_in <- c(missing_in, "auxiliary covariates")
        year_msgs <- c(year_msgs, sprintf(
          "Test 0a: WARNING \u2014 Year variable '%s' is MISSING from: %s.",
          year_var_name, paste(missing_in, collapse = " and ")
        ))
      }

      if (survey_has_year && aux_has_year) {
        survey_years <- sort(unique(survey_raw_check[[year_var_name]]))
        aux_years    <- sort(unique(rhs_raw_check[[year_var_name]]))
        if (identical(as.character(survey_years), as.character(aux_years))) {
          year_msgs <- c(year_msgs, sprintf(
            "Test 0b: Survey and auxiliary covariates cover the same years (%s).",
            paste(survey_years, collapse = ", ")
          ))
        } else {
          in_survey_not_aux <- setdiff(survey_years, aux_years)
          in_aux_not_survey <- setdiff(aux_years, survey_years)
          parts <- character()
          if (length(in_survey_not_aux) > 0)
            parts <- c(parts, sprintf("year(s) %s in survey but not in auxiliary covariates",
                                      paste(in_survey_not_aux, collapse = ", ")))
          if (length(in_aux_not_survey) > 0)
            parts <- c(parts, sprintf("year(s) %s in auxiliary covariates but not in survey",
                                      paste(in_aux_not_survey, collapse = ", ")))
          year_msgs <- c(year_msgs, sprintf(
            "Test 0b: WARNING \u2014 Year mismatch: %s. Survey years: %s. Auxiliary years: %s.",
            paste(parts, collapse = "; "),
            paste(survey_years, collapse = ", "),
            paste(aux_years, collapse = ", ")
          ))
        }
      }
    }

    geo_raw <- tryCatch(
      readRDS(shp_path %||% "data/geometries.rds"),
      error = function(e) NULL
    )
    rr <- assess_data_readiness(
      survey_data    = harmonized$survey,
      aux_data       = harmonized$rhs,
      geo_data       = geo_raw,
      domain_var     = input$var_domain,
      save_to        = "outputs/tables",
      fgt_alpha      = as.integer(input$fgt_alpha %||% 0),
      indicator_type = input$indicator_type %||% "poverty",
      # Readiness assesses log-specific issues (non-positive welfare,
      # extreme tails after logging, etc.) whenever EITHER pipeline
      # plans to fit on log. UFH and MFH now have independent
      # transformation choices, so we OR the two flags.
      log_transform  = identical(input$indicator_type, "mean_welfare") &&
                       (identical(input$ufh_transformation, "log") ||
                        identical(input$mfh_transformation, "log"))
    )
    rr$messages <- c(year_msgs, rr$messages)
    readiness_result(rr)
    append_log(sprintf("Data readiness: %d diagnostic messages", length(rr$messages)))

    # Unlock the Run Analysis button and show the results tab.
    readiness_checked(TRUE)
    status("Data readiness check complete - review the results, then click Run Analysis.")
    updateTabsetPanel(session, "main_tabs", selected = "Data Readiness")
    showNotification(
      "Data readiness check complete. Review the Preflight and Data Readiness tabs, then click '2. Run Analysis'.",
      type = "message", duration = 8
    )
  })

  # ---- Run Pipeline ----
  observeEvent(input$run_btn, {
    # Gate: the user must first click "Check Data Readiness" so they see the
    # preflight and readiness diagnostics before the UFH / MFH / Comparison
    # pipeline runs. This prevents long-running analyses on unreviewed inputs.
    if (!isTRUE(readiness_checked())) {
      showModal(modalDialog(
        title = "Please check data readiness first",
        tags$p(
          "Before running the analysis, please click ",
          tags$strong("1. Check Data Readiness"),
          " in the sidebar and review the Preflight and Data Readiness tabs."
        ),
        tags$p(
          "This ensures your survey data, auxiliary covariates, and geometry ",
          "file are consistent before the UFH / MFH / Comparison pipeline starts."
        ),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      # Also jump to the Data Readiness tab as a hint.
      updateTabsetPanel(session, "main_tabs", selected = "Data Readiness")
      return()
    }

    status("Preparing configuration...")
    logs("")
    llm_interp(NULL)
    llm_brief(NULL)
    normality_eval(NULL)

    # Build ordered list of pipeline stages
    pipeline_stages <- "Validation"
    if ("UFH" %in% input$steps) pipeline_stages <- c(pipeline_stages, "UFH")
    if ("MFH" %in% input$steps) pipeline_stages <- c(pipeline_stages, "MFH")
    if ("Comparison" %in% input$steps) pipeline_stages <- c(pipeline_stages, "Comparison")
    if ("Comparison" %in% input$steps && isTRUE(input$llm_enabled)) pipeline_stages <- c(pipeline_stages, "LLM Interpretation")
    # Always attempt to render the final HTML report after the analysis
    # stages so users get an up-to-date outputs/final_report.html on every
    # pipeline run. Rendering is wrapped in tryCatch below, so a failure
    # logs a warning and the pipeline continues to "Finalizing".
    pipeline_stages <- c(pipeline_stages, "Render Report")
    pipeline_stages <- c(pipeline_stages, "Finalizing")
    n_steps <- length(pipeline_stages)

    progress <- shiny::Progress$new(session, min = 0, max = n_steps)
    progress$set(message = sprintf("[1/%d] Preparing...", n_steps), value = 0)
    on.exit(progress$close())
    step_counter <- 0L

    advance_progress <- function(stage, detail = NULL) {
      step_counter <<- step_counter + 1L
      msg <- sprintf("[%d/%d] %s", step_counter, n_steps, stage)
      progress$set(value = step_counter, message = msg, detail = detail)
    }

    run_id  <- format(Sys.time(), "%Y%m%d_%H%M%S")
    run_dir <- file.path("app_runs", run_id)
    dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

    survey_path <- resolve_upload(input$survey_file)
    rhs_path    <- resolve_upload(input$rhs_file)
    shp_path    <- resolve_upload(input$shp_file)
    regional_benchmark_path <- resolve_upload(input$regional_benchmark_file)
    population_path <- resolve_upload(input$population_file)

    years          <- parse_years(input$years)
    ufh_candidates_y1 <- split_csv(input$ufh_candidates_y1)
    ufh_candidates_y2 <- split_csv(input$ufh_candidates_y2)
    mfh_candidates_y1 <- split_csv(input$mfh_candidates_y1)
    mfh_candidates_y2 <- split_csv(input$mfh_candidates_y2)
    var_map        <- get_var_map()

    # ---- Step 1: Validate input data ----
    advance_progress("Validation", "Checking input data")
    status("Validating input data...")
    append_log("Validating input data...")
    survey_for_validation <- survey_path %||% "data/pov_direct3.rds"
    rhs_for_validation    <- rhs_path    %||% "data/sae_data.rds"

    harmonized <- load_and_harmonize(survey_for_validation, rhs_for_validation,
                                     var_map, input$rhs_domain,
                                     povline_type  = input$povline_type %||% "column",
                                     povline_value = input$povline_numeric)

    # Map legacy covariance name if browser cached an old session
    mfh_cov_val <- input$mfh_cov_choice
    if (identical(mfh_cov_val, "rho_sm")) mfh_cov_val <- "rho_sm_out"

    if (!is.null(harmonized)) {
      flags <- validate_inputs(harmonized$survey, harmonized$rhs)
      validation_result(flags)
      append_log(sprintf("Validation complete: %d flag(s)", length(flags$flags)))

      # Data readiness assessment
      # -- Pre-harmonization checks: year variable in raw data --
      year_msgs <- character()
      survey_raw_check <- tryCatch(readRDS(survey_for_validation), error = function(e) NULL)
      rhs_raw_check    <- tryCatch(readRDS(rhs_for_validation),    error = function(e) NULL)
      year_var_name    <- var_map$year  # the user-specified year column name

      if (!is.null(survey_raw_check) && !is.null(rhs_raw_check)) {
        survey_has_year <- year_var_name %in% names(survey_raw_check)
        aux_has_year    <- year_var_name %in% names(rhs_raw_check)

        if (survey_has_year && aux_has_year) {
          year_msgs <- c(year_msgs, sprintf(
            "Test 0a: Both survey data and auxiliary covariates contain the year variable '%s'.",
            year_var_name
          ))
        } else {
          missing_in <- character()
          if (!survey_has_year) missing_in <- c(missing_in, "survey data")
          if (!aux_has_year)    missing_in <- c(missing_in, "auxiliary covariates")
          year_msgs <- c(year_msgs, sprintf(
            "Test 0a: WARNING \u2014 Year variable '%s' is MISSING from: %s.",
            year_var_name, paste(missing_in, collapse = " and ")
          ))
        }

        if (survey_has_year && aux_has_year) {
          survey_years <- sort(unique(survey_raw_check[[year_var_name]]))
          aux_years    <- sort(unique(rhs_raw_check[[year_var_name]]))
          if (identical(as.character(survey_years), as.character(aux_years))) {
            year_msgs <- c(year_msgs, sprintf(
              "Test 0b: Survey and auxiliary covariates cover the same years (%s).",
              paste(survey_years, collapse = ", ")
            ))
          } else {
            in_survey_not_aux <- setdiff(survey_years, aux_years)
            in_aux_not_survey <- setdiff(aux_years, survey_years)
            parts <- character()
            if (length(in_survey_not_aux) > 0)
              parts <- c(parts, sprintf("year(s) %s in survey but not in auxiliary covariates",
                                        paste(in_survey_not_aux, collapse = ", ")))
            if (length(in_aux_not_survey) > 0)
              parts <- c(parts, sprintf("year(s) %s in auxiliary covariates but not in survey",
                                        paste(in_aux_not_survey, collapse = ", ")))
            year_msgs <- c(year_msgs, sprintf(
              "Test 0b: WARNING \u2014 Year mismatch: %s. Survey years: %s. Auxiliary years: %s.",
              paste(parts, collapse = "; "),
              paste(survey_years, collapse = ", "),
              paste(aux_years, collapse = ", ")
            ))
          }
        }
      }

      geo_raw <- tryCatch(
        readRDS(shp_path %||% "data/geometries.rds"),
        error = function(e) NULL
      )
      rr <- assess_data_readiness(
        survey_data    = harmonized$survey,
        aux_data       = harmonized$rhs,
        geo_data       = geo_raw,
        domain_var     = input$var_domain,
        save_to        = "outputs/tables",
        fgt_alpha      = as.integer(input$fgt_alpha %||% 0),
        indicator_type = input$indicator_type %||% "poverty",
        # OR over UFH and MFH transformation choices -- see longer
        # comment on the first readiness call site above.
        log_transform  = identical(input$indicator_type, "mean_welfare") &&
                         (identical(input$ufh_transformation, "log") ||
                          identical(input$mfh_transformation, "log"))
      )
      # Prepend year-variable checks to readiness messages
      rr$messages <- c(year_msgs, rr$messages)
      readiness_result(rr)
      append_log(sprintf("Data readiness: %d diagnostic messages", length(rr$messages)))

      # Generate data properties note (template-based, no LLM)
      data_note <- generate_data_note(
        validation  = flags,
        var_map     = var_map,
        ufh_options = list(
          transformation     = input$ufh_transformation,
          # Bias correction is reported only when a transformation is in
          # play (arcsin or log); otherwise NA -- there is nothing to
          # back-transform.
          backtransformation = if (input$ufh_transformation %in% c("arcsin", "log"))
                                 input$ufh_backtrans else NA,
          # Variance-smoothing choice is only meaningful on the identity
          # scale; arcsin and log both already stabilize variances.
          var_choice         = if (input$ufh_transformation %in% c("arcsin", "log"))
                                 NA else (input$ufh_var_choice %||% "sm_out"),
          candidate_vars_y1  = ufh_candidates_y1,
          candidate_vars_y2  = ufh_candidates_y2
        ),
        mfh_options = list(
          # Transformation and bias correction are MFH-only here --
          # MFH never used arcsin, so the only meaningful values are
          # 'log' / 'no'. The MFH transformation dropdown is hidden
          # when the indicator is poverty (conditionalPanel), but
          # `input$mfh_transformation` keeps its last selected value
          # (default 'log' on app load) -- so we MUST gate on
          # indicator_type here, otherwise a default poverty run
          # would be reported as MFH transformation = 'log' even
          # though the actual MFH config gates log off.
          # Bias correction is only reported when the indicator is
          # mean_welfare AND MFH is set to log; otherwise NA --
          # there is nothing to back-transform.
          transformation     = if (identical(input$indicator_type, "mean_welfare"))
                                 (input$mfh_transformation %||% "no") else "no",
          backtransformation = if (identical(input$indicator_type, "mean_welfare") &&
                                    identical(input$mfh_transformation, "log"))
                                 input$mfh_backtrans else NA,
          var_choice     = input$mfh_var_choice,
          cov_choice     = mfh_cov_val,
          diag_model     = input$mfh_diag_model,
          fit_mfh3       = isTRUE(input$fit_mfh3),
          regional_benchmark_path = regional_benchmark_path,
          population_path         = population_path,
          candidate_vars_y1 = mfh_candidates_y1,
          candidate_vars_y2 = mfh_candidates_y2
        ),
        steps               = input$steps,
        psu_consistent_user = isTRUE(input$psu_consistent)
      )
      dir.create("outputs/data", showWarnings = FALSE, recursive = TRUE)
      dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
      dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)

      # Build per-year diagnostics from raw data
      yr_list <- sort(unique(harmonized$survey$year))
      if (length(yr_list) >= 2) {
        yr_list <- yr_list[1:2]
      }
      diag_list  <- list()
      bench_list <- list()
      for (yr in yr_list) {
        yr_key <- paste0("y", yr)
        s <- build_year_summary(
          harmonized$survey, yr,
          fgt_alpha      = as.integer(input$fgt_alpha %||% 0),
          indicator_type = input$indicator_type %||% "poverty",
          log_transform  = identical(input$ufh_transformation, "log") &&
                           identical(input$indicator_type, "mean_welfare")
        )
        diag_list[[yr_key]]  <- s$diag
        bench_list[[yr_key]] <- s$bench
      }
      diagnostics_data(list(diag = diag_list, bench = bench_list))

      # Generate template brief (no LLM)
      llm_off <- llm_assistant(enabled = FALSE)
      br <- generate_analysis_brief(
        diagnostics   = diag_list,
        bench_summary = bench_list,
        input_flags   = flags,
        llm           = llm_off,
        language      = get_language(),
        country       = "Greece",
        model_type    = if ("UFH" %in% input$steps) "UFH" else "MFH"
      )
      brief_result(br)
    } else {
      append_log("WARNING: Could not load data for validation.")
      validation_result(list(flags = c("WARNING: Could not load data files for validation."),
                              summary = list(), has_errors = FALSE))
    }

    # ---- Step 2: Build pipeline config and run ----
    # Variance smoothing option for UFH is only active when no transformation
    # is used. arcsin and log both stabilize variances on their own scale, so
    # the R script pins var_choice to "sm_out" (a no-op for non-NA, non-outlier
    # rows) under those branches.
    ufh_var_val <- if (input$ufh_transformation %in% c("arcsin", "log")) {
      "sm_out"
    } else {
      input$ufh_var_choice %||% "sm_out"
    }

    # Bias correction has two complementary representations in the config:
    #   ufh$bias_correction        -- LOGICAL (TRUE = correct, FALSE = naive).
    #                                 This is the wire format the R script reads
    #                                 to decide whether to apply correction.
    #   ufh$bias_correction_method -- STRING label ("bc", "bc_sm", "none")
    #                                 capturing which corrector the user
    #                                 picked. Used for diagnostics, reports,
    #                                 and forward-compat (e.g. wiring "none"
    #                                 through to skip the smearing step).
    #   ufh$backtransformation     -- legacy string alias ("bc"/"bc_sm"/NULL)
    #                                 kept so older configs / consumers
    #                                 continue to parse.
    # Mapping per UI:
    #   arcsin + "bc"     -> bias_correction = TRUE,  method = "bc"
    #   arcsin + "none"   -> bias_correction = FALSE, method = "none"
    #   log    + "bc_sm"  -> bias_correction = TRUE,  method = "bc_sm"
    #   log    + "none"   -> bias_correction = FALSE, method = "none"
    #   no                -> bias_correction = NA,    method = NA
    ufh_bc_method <- if (input$ufh_transformation %in% c("arcsin", "log")) {
      input$ufh_backtrans %||% (if (identical(input$ufh_transformation, "arcsin")) "bc" else "bc_sm")
    } else {
      NA_character_
    }
    ufh_bc_logical <- if (input$ufh_transformation %in% c("arcsin", "log")) {
      !identical(ufh_bc_method, "none")
    } else {
      NA
    }
    # String alias used by older readers and by emdi's `backtransformation`
    # argument (NULL means "no back-transform").
    ufh_bt_string <- if (identical(ufh_bc_method, "none") || is.na(ufh_bc_method)) {
      NULL
    } else {
      ufh_bc_method
    }

    # The transformation actually passed to emdi::fh(). For "log" we
    # apply log() to the LHS in the R script before fitting, so emdi sees an
    # untransformed (identity-scale) LHS and we tell it transformation = "no".
    ufh_emdi_trans <- if (identical(input$ufh_transformation, "log")) {
      "no"
    } else {
      input$ufh_transformation
    }

    ufh_cfg <- list(
      survey_path             = survey_path %||% "data/pov_direct3.rds",
      rhs_path                = rhs_path    %||% "data/sae_data.rds",
      shp_path                = shp_path    %||% "data/geometries.rds",
      var_map                 = var_map,
      rhs_domain              = input$rhs_domain,
      shp_domain              = input$shp_domain,
      years_keep              = years,
      transformation          = ufh_emdi_trans,
      bias_correction         = ufh_bc_logical,
      bias_correction_method  = ufh_bc_method,
      backtransformation      = ufh_bt_string,
      ic_criterion            = input$ufh_ic_criterion,
      var_choice              = ufh_var_val,
      candidate_vars_y1       = if (length(ufh_candidates_y1)) ufh_candidates_y1 else NULL,
      candidate_vars_y2       = if (length(ufh_candidates_y2)) ufh_candidates_y2 else NULL
    )

    # ---- MFH transformation + bias correction ------------------------------
    # The MFH dropdown is independent from UFH. Only meaningful for
    # mean_welfare; for poverty runs we force log_transform = FALSE.
    is_mean <- identical(input$indicator_type, "mean_welfare")
    mfh_log_transform <- is_mean && identical(input$mfh_transformation, "log")
    # Bias-correction representations mirror the UFH layout:
    #   bias_correction        -- LOGICAL wire format read by the R script
    #   bias_correction_method -- STRING label ("bc_sm" / "none" / NA)
    #   backtransformation     -- legacy STRING alias (NULL == none)
    # MFH only ever offers Duan smearing for log; "bc" (integration-based)
    # belongs to arcsin and isn't applicable here.
    mfh_bc_method <- if (mfh_log_transform) {
      input$mfh_backtrans %||% "bc_sm"
    } else {
      NA_character_
    }
    mfh_bc_logical <- if (mfh_log_transform) {
      !identical(mfh_bc_method, "none")
    } else {
      NA
    }
    mfh_bt_string <- if (identical(mfh_bc_method, "none") || is.na(mfh_bc_method)) {
      NULL
    } else {
      mfh_bc_method
    }

    mfh_cfg <- list(
      survey_path             = survey_path %||% "data/pov_direct3.rds",
      rhs_path                = rhs_path    %||% "data/sae_data.rds",
      shp_path                = shp_path    %||% "data/geometries.rds",
      regional_benchmark_path = regional_benchmark_path,
      population_path         = population_path,
      var_map                 = var_map,
      ic_criterion            = input$mfh_ic_criterion,
      rhs_domain              = input$rhs_domain,
      shp_domain              = input$shp_domain,
      years_keep              = years,
      var_choice              = input$mfh_var_choice,
      cov_choice              = mfh_cov_val,
      diag_model              = input$mfh_diag_model,
      fit_mfh3                = isTRUE(input$fit_mfh3),
      # Per-model log/no choice. The MFH R script reads this in preference
      # to the global cfg$log_transform; the global flag remains as a
      # backward-compat shim and is set below to the OR of UFH and MFH.
      log_transform           = mfh_log_transform,
      # MFH never passes log to emdi -- the log step is applied to the
      # LHS in the R script before fitting, so emdi sees an identity-scale
      # outcome. We therefore always store transformation = "no" here.
      transformation          = "no",
      bias_correction         = mfh_bc_logical,
      bias_correction_method  = mfh_bc_method,
      backtransformation      = mfh_bt_string,
      candidate_vars_y1       = if (length(mfh_candidates_y1)) mfh_candidates_y1 else NULL,
      candidate_vars_y2       = if (length(mfh_candidates_y2)) mfh_candidates_y2 else NULL
    )

    # Per-UFH log_transform flag (mirror of the per-MFH one). Lets the
    # UFH R script pick up its own setting independently when both are
    # present; otherwise it falls back to the global cfg$log_transform.
    ufh_cfg$log_transform <- identical(input$ufh_transformation, "log") && is_mean

    cfg <- list(
      years_keep      = years,
      indicator_type  = input$indicator_type %||% "poverty",
      # Global log_transform: TRUE if either UFH or MFH is on log.
      # Kept so that helpers that don't know about per-model flags
      # (R/indicator_helpers.R, generate_data_note, etc.) keep working.
      # Each R script prefers its own per-model flag (cfg$ufh$log_transform
      # or cfg$mfh$log_transform) when present.
      log_transform   = (ufh_cfg$log_transform %||% FALSE) ||
                        (mfh_cfg$log_transform %||% FALSE),
      currency_symbol = input$currency_symbol %||% "EUR",
      fgt_alpha       = as.integer(input$fgt_alpha %||% 0),
      povline_type    = input$povline_type %||% "column",
      povline_value   = if (identical(input$povline_type, "numeric"))
                          input$povline_numeric else input$var_povline,
      run             = list(steps = input$steps),
      ufh             = ufh_cfg,
      mfh             = mfh_cfg
    )

    check <- validate_app_config(cfg)
    if (!check$valid) {
      status("Invalid configuration")
      append_log(paste(check$errors, collapse = " | "))
      return()
    }

    cfg_path <- file.path(run_dir, "app_config.yml")
    write_app_config(cfg, cfg_path)
    append_log(paste("Saved config:", normalizePath(cfg_path, winslash = "/", mustWork = FALSE)))

    status("Running pipeline...")
    ok      <- TRUE
    err_msg <- NULL

    # Persist pipeline log to disk so the user can share it for diagnosis
    # when something goes wrong. File lives at app_runs/<run_id>/run.log.
    run_log_path <- file.path(run_dir, "run.log")
    # Initialize the file so it always exists even if no messages arrive.
    tryCatch({
      cat(sprintf("[%s] Pipeline run started (run_id=%s)\n",
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"), run_id),
          file = run_log_path, append = FALSE)
    }, error = function(e) NULL)

    pipeline_logger <- function(msg) {
      append_log(msg)
      tryCatch({
        cat(sprintf("[%s] %s\n",
                    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                    paste(as.character(msg), collapse = " ")),
            file = run_log_path, append = TRUE)
      }, error = function(e) NULL)
    }

    pipeline_progress <- function(event, label) {
      if (event == "start") {
        detail <- switch(label,
          UFH        = "Univariate Fay-Herriot model",
          MFH        = "Multivariate Fay-Herriot model",
          Comparison = "Comparing UFH and MFH results",
          label
        )
        advance_progress(label, detail)
        status(sprintf("Running %s...", label))
      }
    }

    tryCatch(
      {
        run_pipeline_from_config(
          config_path       = cfg_path,
          logger            = pipeline_logger,
          progress_callback = pipeline_progress
        )
      },
      error = function(e) {
        ok      <<- FALSE
        err_msg <<- conditionMessage(e)
      }
    )

    # ---- Step 2b: Generate AI companion note for Comparison ----
    ai_note_eligible <- ok && "Comparison" %in% input$steps && isTRUE(input$llm_enabled) && nchar(input$api_key %||% "") > 0
    append_log(sprintf("AI note check: ok=%s, Comparison=%s, llm_enabled=%s, api_key_set=%s -> eligible=%s",
                        ok, "Comparison" %in% input$steps, isTRUE(input$llm_enabled),
                        nchar(input$api_key %||% "") > 0, ai_note_eligible))
    # Always remove any stale AI note from a previous run BEFORE we
    # generate a new one (or skip generation). Without this, a run
    # with LLM enabled writes comparison_ai_note.html, and the
    # next run with LLM disabled leaves yesterday's note on disk
    # masquerading as today's output. The MFH script has an analogous
    # cleanup block at the top of scripts/02_mfh.R.
    .stale_ai_note <- "outputs/comparison_ai_note.html"
    if (file.exists(.stale_ai_note)) {
      tryCatch(
        {
          file.remove(.stale_ai_note)
          if (!ai_note_eligible) {
            append_log("Removed stale AI companion note from a previous run (AI Assistant is currently disabled).")
          }
        },
        warning = function(w) NULL,
        error   = function(e) NULL
      )
    }
    if (ai_note_eligible) {
      advance_progress("LLM Interpretation", "Generating AI companion note")
      status("Generating AI companion note...")
      append_log("Generating AI companion note for Comparison report...")
      tryCatch({
        llm <- llm_assistant(api_key = input$api_key, provider = detect_llm_provider(input$api_key))
        ai_comments <- generate_comparison_ai_comments(
          llm             = llm,
          language        = get_language(),
          indicator_type  = input$indicator_type %||% "poverty",
          currency_symbol = input$currency_symbol %||% "EUR",
          # The Comparison report is MFH-driven (per-domain-year smearing
          # back-transform of MFH EBLUPs, MCPE-based change CIs from
          # MFH MCPE), so the AI companion note must describe the MFH
          # scale. Using the UFH flag here would mislabel a run where
          # UFH and MFH chose different transformations (e.g. UFH=no,
          # MFH=log).
          log_transform   = identical(input$mfh_transformation, "log") &&
                            identical(input$indicator_type, "mean_welfare"),
          logger          = function(msg) append_log(msg)
        )
        if (!is.null(ai_comments)) {
          render_comparison_ai_note(
            comments = ai_comments,
            language = get_language(),
            logger   = function(msg) append_log(msg)
          )
          append_log("AI companion note saved: outputs/comparison_ai_note.html")
        } else {
          append_log("WARNING: AI commentary generation returned no results.")
        }
      }, error = function(e) {
        append_log(paste("WARNING: AI companion note failed:", e$message))
      })
    }

    # ---- Step 3: Check outputs and enrich diagnostics ----
    advance_progress("Finalizing", if (ok) "Checking outputs" else "Pipeline failed")
    status(if (ok) "Finalizing..." else "Run failed")
    files <- c(
      "outputs/final_report.html",
      "outputs/data/pov_fh.xlsx",
      "outputs/data/pov_mfh.xlsx",
      "outputs/comparison_ai_note.html",
      "outputs/data/pov_comparison_detailed.xlsx",
      "outputs/data/statistical_significance_comparison.xlsx"
    )
    descriptions <- c(
      "Combined HTML report with UFH, MFH, and Comparison results including diagnostics, maps, and scatter plots",
      "UFH (Fay-Herriot) poverty estimates with benchmarked values, CVs, and MSEs",
      "MFH poverty estimates with benchmarked values, CVs, and MSEs",
      "AI-generated companion note with section-by-section interpretive commentary (overview, normality, rates, precision, significance, maps)",
      "Detailed comparison of UFH and MFH poverty estimates with CVs and MSEs for every domain and year",
      "Combined statistical significance tests for year-on-year poverty changes from both models"
    )
    output_rows(data.frame(
      File        = files,
      Description = descriptions,
      Exists      = file.exists(files),
      stringsAsFactors = FALSE
    ))

    # Try to enrich diagnostics from pipeline output files
    pipeline_out <- read_pipeline_outputs()
    dd <- diagnostics_data()
    if (!is.null(dd)) {
      yr_keys <- names(dd$bench)

      # Enrich UFH diagnostics
      ufh_diag  <- list()
      ufh_bench <- list()
      if (!is.null(pipeline_out$ufh)) {
        for (yr_key in yr_keys) {
          yr_val <- as.integer(gsub("^y", "", yr_key))
          enriched <- enrich_diagnostics_from_output(pipeline_out$ufh, yr_val, "UFH")
          ufh_bench[[yr_key]] <- enriched %||% dd$bench[[yr_key]]
          d <- dd$diag[[yr_key]]
          d$model_type <- "UFH"
          d <- enrich_diag_with_shapiro(d, pipeline_out$ufh_shapiro, yr_val, "UFH")
          ufh_diag[[yr_key]] <- d
        }
      }

      # Enrich MFH diagnostics
      mfh_diag  <- list()
      mfh_bench <- list()
      if (!is.null(pipeline_out$mfh)) {
        for (yr_key in yr_keys) {
          yr_val <- as.integer(gsub("^y", "", yr_key))
          enriched <- enrich_diagnostics_from_output(pipeline_out$mfh, yr_val, "MFH")
          mfh_bench[[yr_key]] <- enriched %||% dd$bench[[yr_key]]
          d <- dd$diag[[yr_key]]
          d$model_type <- "MFH"
          d <- enrich_diag_with_shapiro(d, pipeline_out$mfh_shapiro, yr_val, "MFH")
          mfh_diag[[yr_key]] <- d
        }
      }

      # Store enriched data (keep UFH as primary for backward compat)
      if (length(ufh_bench) > 0) dd$bench <- ufh_bench
      dd$ufh_diag  <- ufh_diag
      dd$ufh_bench <- ufh_bench
      dd$mfh_diag  <- mfh_diag
      dd$mfh_bench <- mfh_bench
      diagnostics_data(dd)

      # Regenerate brief with enriched data (separate UFH/MFH when both available)
      llm_off  <- llm_assistant(enabled = FALSE)
      has_both <- length(ufh_diag) > 0 && length(mfh_diag) > 0
      br <- generate_analysis_brief(
        diagnostics     = dd$diag,
        bench_summary   = dd$bench,
        input_flags     = validation_result(),
        llm             = llm_off,
        language        = get_language(),
        country         = "Greece",
        model_type      = if ("UFH" %in% input$steps) "UFH" else "MFH",
        ufh_diagnostics = if (has_both) ufh_diag  else NULL,
        ufh_bench       = if (has_both) ufh_bench else NULL,
        mfh_diagnostics = if (has_both) mfh_diag  else NULL,
        mfh_bench       = if (has_both) mfh_bench else NULL
      )
      brief_result(br)
    }

    # ---- Step 4: Save brief, diagnostics note, and check outputs ----
    dir.create("outputs/data", showWarnings = FALSE, recursive = TRUE)

    br <- brief_result()

    # Save diagnostics note as markdown (separate UFH / MFH sections)
    dd <- diagnostics_data()
    if (!is.null(dd)) {
      diag_lines <- c("# Model Diagnostics Note", "")

      # Helper to format one model's diagnostics block
      format_diag_block <- function(model_label, diag_list, bench_list) {
        bl <- character()
        bl <- c(bl, sprintf("## %s", model_label), "")
        for (yr_name in names(diag_list)) {
          d <- diag_list[[yr_name]]
          bl <- c(bl, sprintf("### Year: %s", d$year %||% yr_name), "")
          bl <- c(bl, sprintf("- **Domains:** %s", d$n_domains %||% "N/A"))
          bl <- c(bl, sprintf("- **Convergence:** %s",
                               if (isTRUE(d$convergence)) "Yes" else "N/A"))
          if (!is.na(d$re_shapiro_pvalue %||% NA)) {
            bl <- c(bl, sprintf("- **RE normality (Shapiro p):** %.4f [%s]",
                                 d$re_shapiro_pvalue,
                                 if (isTRUE(d$re_shapiro_pass)) "PASS" else "FAIL"))
          }
          if (!is.na(d$resid_shapiro_pvalue %||% NA)) {
            bl <- c(bl, sprintf("- **Resid normality (Shapiro p):** %.4f [%s]",
                                 d$resid_shapiro_pvalue,
                                 if (isTRUE(d$resid_shapiro_pass)) "PASS" else "FAIL"))
          }
          bl <- c(bl, "",
            sprintf("*Review Q-Q plots and kernel density plots in the %s HTML report", model_label),
            "to visually confirm normality of random effects and residuals.*", "")

          b <- bench_list[[yr_name]]
          if (!is.null(b)) {
            bl <- c(bl, "#### Benchmark Summary", "")
            if (!is.null(b$estimate_range))
              bl <- c(bl, sprintf("- **Estimate range:** [%.4f, %.4f]",
                                   b$estimate_range[1], b$estimate_range[2]))
            if (!is.na(b$estimate_median %||% NA))
              bl <- c(bl, sprintf("- **Median estimate:** %.4f", b$estimate_median))
            if (!is.na(b$cv_median %||% NA))
              bl <- c(bl, sprintf("- **Median CV:** %.4f", b$cv_median))
            if (!is.na(b$cv_max %||% NA))
              bl <- c(bl, sprintf("- **Max CV:** %.4f", b$cv_max))
            if (!is.na(b$mse_median %||% NA))
              bl <- c(bl, sprintf("- **Median MSE:** %.6f", b$mse_median))
            if (!is.na(b$n_cv_above_25pct %||% NA))
              bl <- c(bl, sprintf("- **Domains with CV > 25%%:** %d", b$n_cv_above_25pct))
            bl <- c(bl, "")
          }
        }
        bl
      }

      has_both <- !is.null(dd$ufh_diag) && length(dd$ufh_diag) > 0 &&
                  !is.null(dd$mfh_diag) && length(dd$mfh_diag) > 0

      if (has_both) {
        diag_lines <- c(diag_lines,
          format_diag_block("UFH (Univariate Fay-Herriot)", dd$ufh_diag, dd$ufh_bench),
          format_diag_block("MFH (Multivariate Fay-Herriot)", dd$mfh_diag, dd$mfh_bench))

        # Comparison section
        diag_lines <- c(diag_lines, "## UFH vs MFH Comparison", "")
        for (yr_name in names(dd$ufh_bench)) {
          ub <- dd$ufh_bench[[yr_name]]
          mb <- dd$mfh_bench[[yr_name]]
          if (!is.null(ub) && !is.null(mb)) {
            diag_lines <- c(diag_lines, sprintf("### %s", yr_name))
            diag_lines <- c(diag_lines, sprintf("| Metric | UFH | MFH |"))
            diag_lines <- c(diag_lines, "|--------|-----|-----|")
            diag_lines <- c(diag_lines, sprintf("| Median CV | %.4f | %.4f |",
                                                 ub$cv_median %||% NA, mb$cv_median %||% NA))
            diag_lines <- c(diag_lines, sprintf("| Max CV | %.4f | %.4f |",
                                                 ub$cv_max %||% NA, mb$cv_max %||% NA))
            if (!is.na(ub$mse_median %||% NA) && !is.na(mb$mse_median %||% NA))
              diag_lines <- c(diag_lines, sprintf("| Median MSE | %.6f | %.6f |",
                                                   ub$mse_median, mb$mse_median))
            diag_lines <- c(diag_lines, sprintf("| Domains CV>25%% | %s | %s |",
                                                 ub$n_cv_above_25pct %||% "N/A",
                                                 mb$n_cv_above_25pct %||% "N/A"))
            diag_lines <- c(diag_lines, "")
          }
        }
        diag_lines <- c(diag_lines,
          "*The model with lower CV, lower MSE, and better-aligned Q-Q plots is generally preferred.",
          "MFH borrows strength across time periods and may improve estimates for domains",
          "with small samples, but requires the multivariate normality assumption to hold.*",
          "")
      } else {
        # Single model fallback
        for (yr_name in names(dd$diag)) {
          d <- dd$diag[[yr_name]]
          diag_lines <- c(diag_lines, sprintf("## Year: %s", d$year %||% yr_name), "")
          diag_lines <- c(diag_lines, sprintf("- **Model type:** %s", d$model_type %||% "UFH"))
          diag_lines <- c(diag_lines, sprintf("- **Domains:** %s", d$n_domains %||% "N/A"))
          diag_lines <- c(diag_lines, sprintf("- **Convergence:** %s",
                                               if (isTRUE(d$convergence)) "Yes" else "N/A"))
          if (!is.na(d$re_shapiro_pvalue %||% NA))
            diag_lines <- c(diag_lines, sprintf("- **RE normality (Shapiro p):** %.4f [%s]",
                                                 d$re_shapiro_pvalue,
                                                 if (isTRUE(d$re_shapiro_pass)) "PASS" else "FAIL"))
          if (!is.na(d$resid_shapiro_pvalue %||% NA))
            diag_lines <- c(diag_lines, sprintf("- **Resid normality (Shapiro p):** %.4f [%s]",
                                                 d$resid_shapiro_pvalue,
                                                 if (isTRUE(d$resid_shapiro_pass)) "PASS" else "FAIL"))
          diag_lines <- c(diag_lines, "")

          b <- dd$bench[[yr_name]]
          if (!is.null(b)) {
            diag_lines <- c(diag_lines, "### Benchmark Summary", "")
            if (!is.null(b$estimate_range))
              diag_lines <- c(diag_lines, sprintf("- **Estimate range:** [%.4f, %.4f]",
                                                   b$estimate_range[1], b$estimate_range[2]))
            if (!is.na(b$estimate_median %||% NA))
              diag_lines <- c(diag_lines, sprintf("- **Median estimate:** %.4f", b$estimate_median))
            if (!is.na(b$cv_median %||% NA))
              diag_lines <- c(diag_lines, sprintf("- **Median CV:** %.4f", b$cv_median))
            if (!is.na(b$cv_max %||% NA))
              diag_lines <- c(diag_lines, sprintf("- **Max CV:** %.4f", b$cv_max))
            if (!is.na(b$n_cv_above_25pct %||% NA))
              diag_lines <- c(diag_lines, sprintf("- **Domains with CV > 25%%:** %d", b$n_cv_above_25pct))
            diag_lines <- c(diag_lines, "")
          }
        }
      }

      # diagnostics kept in memory only, not written to disk
    }

    # ---- Step (final-1): Render report.Rmd -> outputs/final_report.html ----
    # Previously, the dashboard pipeline did NOT render the final HTML report
    # (only the standalone run_all.R did). That left outputs/final_report.html
    # stale after every dashboard run. We now render it here, wrapped in
    # tryCatch so a rendering failure logs a warning but does not crash the
    # pipeline. Pandoc location is probed because Rscript launches (e.g. via
    # Start_Dashboard.bat) do not inherit RStudio's pandoc PATH.
    if (ok) {
      advance_progress("Render Report", "Generating final_report.html")
      status("Rendering final report...")
      append_log("Rendering final_report.html from report.Rmd...")
      tryCatch({
        if (!requireNamespace("rmarkdown", quietly = TRUE)) {
          append_log("WARNING: rmarkdown package not installed; skipping report rendering.")
        } else if (!file.exists("report.Rmd")) {
          append_log("WARNING: report.Rmd not found; skipping report rendering.")
        } else {
          # ---- Pandoc finder (mirrors run_all.R) ----
          if (!rmarkdown::pandoc_available()) {
            .pandoc_candidates <- c(
              Sys.getenv("RSTUDIO_PANDOC"),
              file.path(Sys.getenv("ProgramFiles"), "RStudio",
                        "resources", "app", "bin", "quarto", "bin", "tools"),
              file.path(Sys.getenv("LOCALAPPDATA"), "Pandoc")
            )
            for (.p in .pandoc_candidates) {
              if (nzchar(.p) &&
                  (file.exists(file.path(.p, "pandoc.exe")) ||
                   file.exists(file.path(.p, "pandoc")))) {
                Sys.setenv(RSTUDIO_PANDOC = .p)
                break
              }
            }
          }

          dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
          rmarkdown::render(
            input       = "report.Rmd",
            output_file = "outputs/final_report.html",
            encoding    = "UTF-8",
            quiet       = TRUE,
            envir       = new.env(parent = globalenv())
          )
          append_log("Final report saved: outputs/final_report.html")
        }
      }, error = function(e) {
        append_log(paste("WARNING: Report rendering failed:", e$message))
      })
    } else {
      # Pipeline already failed earlier; still advance the progress counter
      # so the Finalizing stage line numbers in the log remain correct.
      advance_progress("Render Report", "Skipped (pipeline failed)")
    }

    if (ok) {
      status("Completed successfully")
      progress$set(value = n_steps, detail = "Complete")
      append_log("Pipeline finished.")
    } else {
      status("Run failed")
      progress$set(value = n_steps, detail = "Failed")
      append_log(paste("ERROR:", err_msg))
    }
  })

  # ---- LLM: Interpret Diagnostics ----
  observeEvent(input$interpret_btn, {
    dd  <- diagnostics_data()
    llm <- get_llm()
    if (is.null(dd) || !isTRUE(llm$enabled)) {
      llm_interp("No diagnostics available or LLM not enabled.")
      return()
    }

    llm_interp("Requesting interpretation from Claude...")

    tryCatch({
      lang <- language_label(get_language())

      # Build text for both models when available
      has_both <- !is.null(dd$ufh_diag) && length(dd$ufh_diag) > 0 &&
                  !is.null(dd$mfh_diag) && length(dd$mfh_diag) > 0

      if (has_both) {
        ufh_diag_text  <- paste(capture.output(str(dd$ufh_diag)),  collapse = "\n")
        ufh_bench_text <- paste(capture.output(str(dd$ufh_bench)), collapse = "\n")
        mfh_diag_text  <- paste(capture.output(str(dd$mfh_diag)),  collapse = "\n")
        mfh_bench_text <- paste(capture.output(str(dd$mfh_bench)), collapse = "\n")

        interp_prompt <- paste(
          "Here are diagnostics for BOTH UFH and MFH models in a Small Area Estimation analysis.\n\n",
          "=== UFH (Univariate Fay-Herriot) ===\n",
          "DIAGNOSTICS:\n", ufh_diag_text,
          "\nBENCHMARK SUMMARIES:\n", ufh_bench_text,
          "\n\n=== MFH (Multivariate Fay-Herriot) ===\n",
          "DIAGNOSTICS:\n", mfh_diag_text,
          "\nBENCHMARK SUMMARIES:\n", mfh_bench_text,
          "\n\nPlease provide a structured interpretation:\n",
          "1. UFH Assessment: convergence, normality (discuss Q-Q plots and Shapiro-Wilk),",
          "   precision (CV), accuracy (MSE)\n",
          "2. MFH Assessment: same structure, noting any gains from borrowing strength\n",
          "3. Model Comparison: which approach is better for this data and why\n",
          "4. Domains that may need attention\n",
          "5. Actionable recommendations"
        )
      } else {
        all_diag_text  <- paste(capture.output(str(dd$diag)),  collapse = "\n")
        all_bench_text <- paste(capture.output(str(dd$bench)), collapse = "\n")

        interp_prompt <- paste(
          "Here are the model diagnostics for a Small Area Estimation analysis:\n\n",
          "DIAGNOSTICS:\n", all_diag_text,
          "\n\nBENCHMARK SUMMARIES:\n", all_bench_text,
          "\n\nPlease provide a concise interpretation covering:",
          "1. Whether model assumptions appear satisfied (discuss Q-Q plots and normality)",
          "2. Any diagnostic concerns or red flags",
          "3. Domains that may need attention",
          "4. Actionable recommendations"
        )
      }

      result <- llm$query(
        prompt = interp_prompt,
        system_prompt = paste(
          "You are a statistician helping interpret Small Area Estimation (SAE)",
          "model diagnostics. Be concise but thorough. Focus on actionable insights.",
          "When discussing normality, reference both Shapiro-Wilk test results and",
          "Q-Q plot interpretation (alignment with diagonal, tail behavior, outliers).",
          sprintf("Respond in %s.", lang)
        )
      )

      if (!is.null(result)) {
        llm_interp(result)
      } else {
        llm_interp("LLM request failed. Check your API key and network connection.")
      }
    }, error = function(e) {
      llm_interp(sprintf("Error generating interpretation: %s", e$message))
    })
  })

  # ---- LLM: Evaluate Normality ----
  observeEvent(input$eval_normality_btn, {
    llm <- get_llm()
    if (!isTRUE(llm$enabled)) {
      normality_eval("LLM not enabled. Set API key and enable AI Assistant.")
      return()
    }

    vision_mode <- isTRUE(input$use_vision)
    mode_label <- if (vision_mode) "vision (with plots)" else "text-only"
    normality_eval(paste0("Evaluating normality (", mode_label, " mode)..."))

    tryCatch({
      # Find the emdi fh model objects from pipeline output
      # They are saved as RDS files by the pipeline
      ufh_model_path <- "outputs/data/fh_model_y1.rds"
      ufh_model_path2 <- "outputs/data/fh_model_y2.rds"

      eval_parts <- character()

      for (yr_info in list(
        list(path = ufh_model_path,  label = "UFH Year 1"),
        list(path = ufh_model_path2, label = "UFH Year 2")
      )) {
        if (file.exists(yr_info$path)) {
          fh_mod <- tryCatch(readRDS(yr_info$path), error = function(e) NULL)
          if (!is.null(fh_mod) && inherits(fh_mod, "fh")) {
            detected_provider <- detect_llm_provider(input$api_key)
            result <- evaluate_normality(
              fh_model   = fh_mod,
              api_key    = input$api_key,
              provider   = detected_provider,
              model      = if (detected_provider == "openai") "gpt-4.1" else "claude-sonnet-4-20250514",
              language   = get_language(),
              use_vision = vision_mode
            )
            part <- paste0(
              "=== ", yr_info$label, " ===\n",
              "Standardized Residuals:\n",
              "  Normality holds: ", result$standardized_residuals$normality_holds, "\n",
              "  Shapiro: ", result$standardized_residuals$shapiro_assessment, "\n",
              "  Visual: ", result$standardized_residuals$visual_assessment, "\n",
              if (length(result$standardized_residuals$concerns) > 0)
                paste0("  Concerns: ", paste(result$standardized_residuals$concerns, collapse = "; "), "\n")
              else "",
              "\nRandom Effects:\n",
              "  Normality holds: ", result$random_effects$normality_holds, "\n",
              "  Shapiro: ", result$random_effects$shapiro_assessment, "\n",
              "  Visual: ", result$random_effects$visual_assessment, "\n",
              if (length(result$random_effects$concerns) > 0)
                paste0("  Concerns: ", paste(result$random_effects$concerns, collapse = "; "), "\n")
              else "",
              "\nRecommendation: ", result$overall_recommendation, "\n"
            )
            eval_parts <- c(eval_parts, part)
          }
        }
      }

      if (length(eval_parts) == 0) {
        normality_eval(paste(
          "No saved emdi model objects found. The normality evaluation requires",
          "the fh model objects to be saved as RDS files during pipeline execution.",
          "\n\nTo enable this, the pipeline should save: saveRDS(fh_model, 'outputs/data/fh_model_y1.rds')",
          "\n\nAlternatively, the Shapiro-Wilk results from the CSV exports are",
          "already included in the standard AI interpretation above."
        ))
      } else {
        normality_eval(paste(eval_parts, collapse = "\n\n"))
      }

    }, error = function(e) {
      normality_eval(sprintf("Error evaluating normality: %s", e$message))
    })
  })

  # ---- LLM: Generate Enriched Brief ----
  observeEvent(input$brief_llm_btn, {
    dd  <- diagnostics_data()
    llm <- get_llm()
    vr  <- validation_result()
    if (is.null(dd) || !isTRUE(llm$enabled)) {
      llm_brief("No diagnostics available or LLM not enabled.")
      return()
    }

    llm_brief("Generating AI-enriched brief...")

    tryCatch({
      has_both <- !is.null(dd$ufh_diag) && length(dd$ufh_diag) > 0 &&
                  !is.null(dd$mfh_diag) && length(dd$mfh_diag) > 0
      br <- generate_analysis_brief(
        diagnostics     = dd$diag,
        bench_summary   = dd$bench,
        input_flags     = vr,
        llm             = llm,
        language        = get_language(),
        country         = "Greece",
        model_type      = if ("UFH" %in% input$steps) "UFH" else "MFH",
        ufh_diagnostics = if (has_both) dd$ufh_diag  else NULL,
        ufh_bench       = if (has_both) dd$ufh_bench else NULL,
        mfh_diagnostics = if (has_both) dd$mfh_diag  else NULL,
        mfh_bench       = if (has_both) dd$mfh_bench else NULL
      )

      if (!is.null(br$llm_brief)) {
        llm_brief(br$llm_brief)
      } else {
        llm_brief("LLM request failed. Check your API key and network connection.")
      }
    }, error = function(e) {
      llm_brief(sprintf("Error generating brief: %s", e$message))
    })
  })
}

.app <- shinyApp(ui, server)

# When run from RStudio's "Run App" button or via shiny::runApp(),
# the app object is auto-launched. When run via `Rscript app.R` from a
# terminal, R is non-interactive and we must launch the server explicitly.
#
# IMPORTANT: we pass appDir (a directory) to shiny::runApp rather than the
# .app object. Passing the shinyApp object directly skips Shiny's automatic
# serving of the www/ folder, which 404s the landing-page choropleth
# (www/eu_poverty_map.png) and any other static assets. Passing appDir
# causes Shiny to re-source this file as part of normal app loading; the
# Sys.getenv guard below prevents that from re-entering this branch and
# causing infinite recursion.
if (!interactive() && !nzchar(Sys.getenv("EU_SAE_APP_LAUNCHED"))) {
  Sys.setenv(EU_SAE_APP_LAUNCHED = "1")
  message("Launching Shiny app at http://127.0.0.1:7777 ...")
  message("Open that URL in your browser. Press Ctrl+C in this terminal to stop.")
  shiny::runApp(appDir = .app_dir, host = "127.0.0.1", port = 7777, launch.browser = TRUE)
} else {
  .app
}
