`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

parse_years <- function(x) {
  if (is.numeric(x)) return(sort(as.integer(x)))
  raw <- trimws(unlist(strsplit(as.character(x %||% ""), ",")))
  raw <- raw[nzchar(raw)]
  yrs <- suppressWarnings(as.integer(raw))
  yrs <- yrs[!is.na(yrs)]
  sort(unique(yrs))
}

validate_app_config <- function(cfg) {
  errs <- character()

  years <- parse_years(cfg$years_keep %||% c(2012L, 2013L))
  if (length(years) != 2) {
    errs <- c(errs, "`years_keep` must contain exactly two years.")
  }

  steps <- cfg$run$steps %||% c("UFH", "MFH", "Comparison")
  bad_steps <- setdiff(steps, c("UFH", "MFH", "Comparison"))
  if (length(bad_steps) > 0) {
    errs <- c(errs, paste0("Invalid run steps: ", paste(bad_steps, collapse = ", ")))
  }

  mfh_var <- cfg$mfh$var_choice %||% "sm_out"
  if (!mfh_var %in% c("direct", "sm_out", "sm_all")) {
    errs <- c(errs, "`mfh.var_choice` must be one of: direct, sm_out, sm_all.")
  }

  mfh_cov <- cfg$mfh$cov_choice %||% "rho_sm_out"
  if (!mfh_cov %in% c("direct", "rho_dir", "rho_sm_out", "rho_sm_all", "zero")) {
    errs <- c(errs, "`mfh.cov_choice` must be one of: direct, rho_dir, rho_sm_out, rho_sm_all, zero.")
  }
  optional_mfh_paths <- c(
    regional_benchmark_path = cfg$mfh$regional_benchmark_path,
    population_path = cfg$mfh$population_path
  )
  for (nm in names(optional_mfh_paths)) {
    p <- optional_mfh_paths[[nm]]
    if (!is.null(p) && length(p) > 0 && isTRUE(nzchar(p)) && !file.exists(p)) {
      errs <- c(errs, sprintf("`mfh.%s` does not exist: %s", nm, p))
    }
  }

  # MFH transformation. arcsin is intentionally not exposed for MFH; only
  # "log" (mean welfare) or "no" are accepted. The qmd reads
  # cfg$mfh$log_transform to drive its log-fit branch.
  mfh_trans <- cfg$mfh$transformation %||% "no"
  if (!mfh_trans %in% c("log", "no")) {
    errs <- c(errs, "`mfh.transformation` must be one of: log, no (arcsin is not supported for MFH).")
  }
  mfh_log <- isTRUE(cfg$mfh$log_transform)
  mfh_ind <- cfg$indicator_type %||% "poverty"
  if (mfh_log && !identical(mfh_ind, "mean_welfare")) {
    errs <- c(errs, "`mfh.log_transform = TRUE` is only valid when indicator_type = 'mean_welfare'.")
  }
  # MFH bias-correction method label. Only bc_sm or none are valid for
  # MFH (Duan smearing or naive). bc (integration-based) is for arcsin
  # only and arcsin is not an MFH option.
  mfh_bc_method <- cfg$mfh$bias_correction_method %||% cfg$mfh$backtransformation
  if (!is.null(mfh_bc_method) && !is.na(mfh_bc_method) &&
      !mfh_bc_method %in% c("bc_sm", "naive", "none")) {
    errs <- c(errs, "`mfh.bias_correction_method` must be one of: bc_sm, none (or unset).")
  }
  if (mfh_log && identical(mfh_bc_method, "bc")) {
    errs <- c(errs, "`bc` (integration-based) is only valid for arcsin. Use `bc_sm` (smearing) with MFH log.")
  }

  # The user-facing transformation can be "arcsin" (poverty), "log"
  # (mean welfare), or "no". Note: when the dropdown says "log", the
  # config builder in app.R stores `transformation = "no"` (because the
  # log step is applied in pre-processing, not by emdi::fh) and signals
  # the log fit via `cfg$log_transform = TRUE`. The validator therefore
  # treats "log" here as a valid value for forward-compat and for any
  # configs that record the user-facing choice directly.
  ufh_trans <- cfg$ufh$transformation %||% "arcsin"
  if (!ufh_trans %in% c("arcsin", "log", "no")) {
    errs <- c(errs, "`ufh.transformation` must be one of: arcsin, log, no.")
  }

  # Bias correction has two representations in the config:
  #   ufh$bias_correction        -- LOGICAL (TRUE = correct, FALSE = naive,
  #                                 NA when no transformation is used)
  #   ufh$bias_correction_method -- STRING label ("bc", "bc_sm", "none")
  #   ufh$backtransformation     -- legacy STRING alias ("bc", "bc_sm", "naive", "none")
  # We accept any of them. The "method" field is the source of truth for
  # the user-facing label and is checked here.
  ufh_bc_method <- cfg$ufh$bias_correction_method %||% cfg$ufh$backtransformation
  if (!is.null(ufh_bc_method) && !is.na(ufh_bc_method) &&
      !ufh_bc_method %in% c("bc", "bc_sm", "naive", "none")) {
    errs <- c(errs, "`ufh.bias_correction_method` must be one of: bc, bc_sm, none (or unset).")
  }
  if (identical(ufh_trans, "arcsin") && identical(ufh_bc_method, "bc_sm")) {
    errs <- c(errs, "`bc_sm` is only valid for the log transformation. Use `bc` with arcsin.")
  }
  if (identical(ufh_trans, "log") && identical(ufh_bc_method, "bc")) {
    errs <- c(errs, "`bc` (integration-based) is only valid for arcsin. Use `bc_sm` (smearing) with log.")
  }

  # Variance smoothing option for UFH (mirrors MFH menu). Only meaningful
  # when transformation == "no"; arcsin and log already stabilize variances.
  ufh_var <- cfg$ufh$var_choice %||% "sm_out"
  if (!ufh_var %in% c("direct", "sm_out", "sm_all")) {
    errs <- c(errs, "`ufh.var_choice` must be one of: direct, sm_out, sm_all.")
  }

  fgt <- as.integer(cfg$fgt_alpha %||% 0L)
  if (!fgt %in% c(0L, 1L, 2L)) {
    errs <- c(errs, "`fgt_alpha` must be 0, 1, or 2.")
  }
  if (fgt > 0L && identical(ufh_trans, "arcsin")) {
    errs <- c(errs, "Arcsin transformation is only valid for FGT(0). Set transformation to 'no' for FGT(1)/FGT(2).")
  }
  # Log is welfare-only; reject if combined with the poverty branch.
  ind_type <- cfg$indicator_type %||% "poverty"
  if (identical(ufh_trans, "log") && !identical(ind_type, "mean_welfare")) {
    errs <- c(errs, "Log transformation is only valid when indicator_type = 'mean_welfare'.")
  }

  ptype <- cfg$povline_type %||% "column"
  if (!ptype %in% c("column", "numeric")) {
    errs <- c(errs, "`povline_type` must be 'column' or 'numeric'.")
  }
  if (identical(ptype, "numeric")) {
    pval <- cfg$povline_value
    if (is.null(pval) || !is.numeric(pval) || pval <= 0) {
      errs <- c(errs, "A positive numeric poverty line value is required when povline_type is 'numeric'.")
    }
  }

  list(valid = length(errs) == 0, errors = errs)
}

write_app_config <- function(cfg, path) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required. Install with install.packages('yaml').")
  }
  yaml::write_yaml(cfg, path)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

required_packages_for_steps <- function(steps) {
  step_packages <- list(
    UFH = c(
      "sf", "data.table", "tidyverse", "car", "sae", "survey", "spdep",
      "MASS", "caret", "purrr", "gt", "scales", "viridis", "emdi",
      "rlang", "dplyr", "ggplot2", "writexl", "readxl", "conflicted",
      "tictoc", "matrixcalc", "knitr", "here", "yaml"
    ),
    MFH = c(
      "sf", "data.table", "dplyr", "tidyr", "stringr", "purrr", "tictoc",
      "tidyverse", "car", "msae", "sae", "survey", "spdep", "MASS",
      "caret", "conflicted", "tibble", "emdi", "ggplot2", "gt",
      "viridis", "scales", "patchwork", "knitr", "here", "matrixcalc",
      "Matrix", "magic", "yaml"
    ),
    Comparison = c(
      "dplyr", "tidyr", "ggplot2", "readxl", "writexl", "knitr",
      "patchwork", "sf", "purrr", "stringr", "scales", "emdi",
      "here", "yaml"
    )
  )

  report_packages <- c(
    "rmarkdown", "dplyr", "tidyr", "ggplot2", "readxl", "knitr",
    "patchwork", "sf", "scales", "here"
  )
  unique(c(unlist(step_packages[steps], use.names = FALSE), report_packages))
}

check_pipeline_packages <- function(steps) {
  packages <- required_packages_for_steps(steps)
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) == 0) return(invisible(TRUE))

  install_call <- sprintf(
    "install.packages(c(%s))",
    paste(sprintf("'%s'", missing), collapse = ", ")
  )
  stop(sprintf(
    paste0(
      "Missing required R package(s) for the selected analysis steps: %s\n\n",
      "Run source('install_packages.R') from the dashboard folder, or run:\n%s"
    ),
    paste(missing, collapse = ", "),
    install_call
  ), call. = FALSE)
}

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

# Read output Excel/CSV files after pipeline run for richer diagnostics.
# Paths follow the Package4 outputs/ layout.
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

# ---------------------------------------------------------------------------
# run_pipeline_from_config()
#
# Package4 replacement for the Quarto-based pipeline runner. Instead of
# rendering .qmd files through the Quarto CLI, this version source()'s
# standalone R scripts (scripts/01_ufh.R, scripts/02_mfh.R, scripts/03_comparison.R) and
# renders the final report via rmarkdown::render().
# ---------------------------------------------------------------------------
run_pipeline_from_config <- function(config_path, logger = message, progress_callback = NULL) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required. Install with install.packages('yaml').")
  }

  config_path <- normalizePath(config_path, winslash = "/", mustWork = TRUE)
  cfg <- yaml::read_yaml(config_path)
  v <- validate_app_config(cfg)
  if (!v$valid) {
    stop(paste(v$errors, collapse = "\n"))
  }

  steps <- cfg$run$steps %||% c("UFH", "MFH", "Comparison")
  check_pipeline_packages(steps)

  # Set SAE_APP_CONFIG so the sourced R scripts can read the config
  old_cfg <- Sys.getenv("SAE_APP_CONFIG", unset = "")
  on.exit({
    if (nzchar(old_cfg)) {
      Sys.setenv(SAE_APP_CONFIG = old_cfg)
    } else {
      Sys.unsetenv("SAE_APP_CONFIG")
    }
  }, add = TRUE)
  Sys.setenv(SAE_APP_CONFIG = config_path)

  # Create output directories
  out_dirs <- c("outputs/data", "outputs/tables", "outputs/figures")
  for (d in out_dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  # Step-to-script mapping
  step_scripts <- c(
    UFH        = "scripts/01_ufh.R",
    MFH        = "scripts/02_mfh.R",
    Comparison = "scripts/03_comparison.R"
  )

  # Execute each pipeline step by source()-ing the corresponding R script
  for (step in steps) {
    script <- step_scripts[[step]]
    if (is.null(script) || !file.exists(script)) {
      stop(sprintf("Script for step '%s' not found: %s", step, script %||% "(unmapped)"))
    }

    if (is.function(progress_callback)) {
      progress_callback("start", step)
    }

    logger(sprintf("Running step: %s (%s)", step, script))
    step_unavailable <- FALSE
    tryCatch(
      source(script, local = new.env(parent = globalenv())),
      sae_mfh_unavailable = function(e) {
        step_unavailable <<- TRUE
        logger(conditionMessage(e))
        logger("MFH artifacts were written with unavailable/NA placeholders; continuing to Comparison.")
      }
    )
    logger(sprintf(
      "Completed step: %s%s",
      step,
      if (step_unavailable) " (marked unavailable)" else ""
    ))

    if (is.function(progress_callback)) {
      progress_callback("complete", step)
    }
  }

  # Render final report via rmarkdown
  report_rmd <- "report.Rmd"
  if (file.exists(report_rmd)) {
    if (is.function(progress_callback)) {
      progress_callback("start", "Report")
    }

    logger("Rendering final report...")

    # Auto-detect pandoc for batch mode (outside RStudio)
    if (!rmarkdown::pandoc_available()) {
      pandoc_candidates <- c(
        Sys.getenv("RSTUDIO_PANDOC"),
        file.path(Sys.getenv("ProgramFiles"), "RStudio",
                  "resources", "app", "bin", "quarto", "bin", "tools"),
        file.path(Sys.getenv("LOCALAPPDATA"), "Pandoc")
      )
      for (p in pandoc_candidates) {
        if (nzchar(p) && (file.exists(file.path(p, "pandoc.exe")) ||
                          file.exists(file.path(p, "pandoc")))) {
          Sys.setenv(RSTUDIO_PANDOC = p)
          break
        }
      }
    }

    rmarkdown::render(
      input       = report_rmd,
      output_file = file.path(getwd(), "outputs", "final_report.html"),
      encoding    = "UTF-8",
      quiet       = TRUE
    )

    logger("Report rendered: outputs/final_report.html")

    if (is.function(progress_callback)) {
      progress_callback("complete", "Report")
    }
  } else {
    logger("Warning: report.Rmd not found; skipping report rendering.")
  }

  invisible(TRUE)
}
