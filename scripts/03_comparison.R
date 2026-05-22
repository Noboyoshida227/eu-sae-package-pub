# ============================================================================
# EU SAE Package4 -- 03_comparison.R
# Comparison of UFH and MFH Results
#
# Converted from qmd/Comparison_v2.qmd (computation only, no prose/knitr)
# All paths use here::here() anchored to the project root.
# Figures saved to outputs/figures/, data to outputs/data/,
# tables to outputs/tables/.
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readxl)
  library(writexl)
  library(knitr)
  library(patchwork)
  library(sf)
  library(purrr)
  library(stringr)
  library(scales)
  library(emdi)
  library(here)
})

source(here::here("scripts", "ufh_functions.R"))
if (file.exists(here::here("R", "indicator_helpers.R"))) {
  source(here::here("R", "indicator_helpers.R"))
}

.cmp_cfg_path <- Sys.getenv("SAE_APP_CONFIG", unset = "")
.cmp_cfg <- if (nzchar(.cmp_cfg_path) && file.exists(.cmp_cfg_path)) {
  yaml::read_yaml(.cmp_cfg_path)
} else {
  list()
}

indicator_type  <- if (!is.null(.cmp_cfg$indicator_type)) .cmp_cfg$indicator_type else "poverty"
log_transform   <- isTRUE(.cmp_cfg$ufh$log_transform) && identical(indicator_type, "mean_welfare")
currency_symbol <- if (!is.null(.cmp_cfg$currency_symbol)) .cmp_cfg$currency_symbol else "EUR"
fgt_alpha       <- as.integer(if (!is.null(.cmp_cfg$fgt_alpha)) .cmp_cfg$fgt_alpha else 0L)
pov_lab         <- indicator_label(indicator_type, fgt_alpha,
                                   log_transform = log_transform,
                                   currency_symbol = currency_symbol)

method_colors <- c(
  "Direct"          = "black",
  "FH"              = "#1f77b4",
  "FH Benchmarked"  = "#d62728",
  "MFH"             = "#2ca02c",
  "MFH Benchmarked" = "#ff7f0e"
)

change_colors <- c(
  "FH"              = "#1f77b4",
  "FH Benchmarked"  = "#d62728",
  "MFH"             = "#2ca02c",
  "MFH Benchmarked" = "#ff7f0e"
)

# ---- Diagnostic: report artifact timestamps + file size so we can spot
# stale outputs from a previous render that never got refreshed. We ALSO
# write the diagnostic to output/Comparison/_diagnostics.txt so the user
# can easily share it -- stderr from child R processes doesn't reliably
# flow back to the Shiny app log.
dir.create(here::here("outputs", "data"), recursive = TRUE, showWarnings = FALSE)
.diag_path <- here::here("outputs", "tables", "comparison_diagnostics.txt")
.diag_lines <- c()
.diag_push <- function(x) {
  .diag_lines <<- c(.diag_lines, x)
  message(x)
}
.diag_push(sprintf("=== Comparison diagnostics (rendered %s) ===",
                   format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
.diag_push(paste0("Working directory: ", getwd()))
.diag_files <- c(
  here::here("outputs", "data", "fh_model_y1.rds"),
  here::here("outputs", "data", "fh_model_y2.rds"),
  here::here("outputs", "data", "pov_fh.xlsx"),
  here::here("outputs", "data", "mfh_artifacts.rds"),
  here::here("outputs", "data", "pov_mfh.xlsx")
)
.diag_push("---- Input file inventory ----")
for (.f in .diag_files) {
  if (file.exists(.f)) {
    .fi <- file.info(.f)
    .diag_push(sprintf("  %s  mtime=%s  size=%d bytes",
                       .f,
                       format(.fi$mtime, "%Y-%m-%d %H:%M:%S"),
                       as.integer(.fi$size)))
  } else {
    .diag_push(sprintf("  %s  MISSING", .f))
  }
}

# Log the UFH config the pipeline is using so we can confirm the
# transformation/var_choice that was actually selected for this run.
if (!is.null(.cmp_cfg$ufh)) {
  .diag_push(sprintf(
    "---- UFH config (from %s) ----",
    if (nzchar(.cmp_cfg_path)) basename(.cmp_cfg_path) else "(no path)"
  ))
  .diag_push(sprintf("  transformation: %s",
                     .cmp_cfg$ufh$transformation %||% "(unset)"))
  .diag_push(sprintf("  bias_correction/backtransformation: %s",
                     .cmp_cfg$ufh$bias_correction %||%
                     .cmp_cfg$ufh$backtransformation %||% "(NULL)"))
  .diag_push(sprintf("  var_choice: %s",
                     .cmp_cfg$ufh$var_choice %||% "(unset)"))
  .diag_push(sprintf("  ic_criterion: %s",
                     .cmp_cfg$ufh$ic_criterion %||% "(unset)"))
}

fh_models <- list(
  `2012` = readRDS(here::here("outputs", "data", "fh_model_y1.rds")),
  `2013` = readRDS(here::here("outputs", "data", "fh_model_y2.rds"))
)

mfh_artifacts <- readRDS(here::here("outputs", "data", "mfh_artifacts.rds"))
years_keep <- as.integer(mfh_artifacts$years_keep)
selected_mfh_model <- mfh_artifacts$selected_model
mfh_formula <- mfh_artifacts$formula
diag_model <- if (!is.null(mfh_artifacts$diag_model)) mfh_artifacts$diag_model else "MFH2"

# Detect whether eblupMFH2 was properly executed (random effects estimated).
# When var_choice = "direct" and cov_choice = "direct", the model either
# fails outright or converges to sigma2_u = 0, producing no shrinkage.
.mfh_not_executed <- isTRUE(mfh_artifacts$model_fit_failed) ||
  isTRUE(mfh_artifacts$refvar_zero)

# The Comparison report is driven by whichever MFH variant was chosen in
# 50-mfh_v2.qmd. We build the column names for the selected variant up
# front so that every downstream table/plot pulls the right series out of
# pov_mfh.xlsx (rate_MFH1 / mse_MFH1 / cv_MFH1, etc.). UFH is not a valid
# diag_model anymore -- UFH analysis lives in 40-fh_v2.qmd.
.mfh_rate_col <- paste0("rate_", diag_model)
.mfh_mse_col  <- paste0("mse_",  diag_model)
.mfh_cv_col   <- paste0("cv_",   diag_model)


shp_dt <- readRDS(here::here("data", "geometries.rds")) %>%
  rename(domain = prov) %>%
  mutate(domain = as.integer(domain))

pov_fh <- read_excel(here::here("outputs", "data", "pov_fh.xlsx")) %>%
  mutate(
    domain = as.integer(domain),
    year = as.integer(year)
  )

pov_mfh <- read_excel(here::here("outputs", "data", "pov_mfh.xlsx")) %>%
  mutate(
    domain = as.integer(domain),
    year = as.integer(year)
  )

# ---- Diagnostic: summarise the FH/MFH series we're about to plot. If these
# numbers don't change from one pipeline run to the next, the figures won't
# change either, even if the transformation toggle moved.
.fh_summary <- pov_fh %>%
  group_by(year) %>%
  summarize(
    mean_FH = mean(FH, na.rm = TRUE),
    mean_FH_Bench = mean(FH_Bench, na.rm = TRUE),
    mean_FH_MSE = mean(FH_MSE, na.rm = TRUE),
    .groups = "drop"
  )
.mfh_summary <- pov_mfh %>%
  group_by(year) %>%
  summarize(
    !!paste0("mean_", .mfh_rate_col) := mean(.data[[.mfh_rate_col]], na.rm = TRUE),
    !!paste0("mean_", .mfh_mse_col)  := mean(.data[[.mfh_mse_col]],  na.rm = TRUE),
    .groups = "drop"
  )
.diag_push(sprintf("---- Selected MFH variant for Comparison: %s ----", diag_model))
.diag_push("---- FH summary (pov_fh.xlsx) ----")
for (.ln in utils::capture.output(print(as.data.frame(.fh_summary)))) .diag_push(paste0("  ", .ln))
.diag_push(sprintf("---- %s summary (pov_mfh.xlsx) ----", diag_model))
for (.ln in utils::capture.output(print(as.data.frame(.mfh_summary)))) .diag_push(paste0("  ", .ln))
.diag_push("=== end diagnostics ===")
tryCatch(
  writeLines(.diag_lines, .diag_path),
  error = function(e) message("WARN: could not write diagnostics: ",
                              conditionMessage(e))
)

sig_fh <- read.csv(here::here("outputs", "tables", "statistical_significance_results_unbench.csv")) %>%
  mutate(domain = as.integer(domain))

sig_fh_bench <- read.csv(here::here("outputs", "tables", "statistical_significance_results.csv")) %>%
  mutate(domain = as.integer(domain))

# Read the MFH change-analysis CSVs defensively. The MFH stage cleans
# these files at startup and only rewrites them when the corresponding
# comp12_obj / comp12_bench_obj are produced; with var_choice = "direct"
# either of those can fail to materialise. We must NOT let read.csv()
# stop the render with a "cannot open file" error before the
# unavailable-MFH callout below has a chance to explain what happened.
.empty_change_df <- function() {
  data.frame(
    domain      = integer(0),
    diff        = numeric(0),
    mse         = numeric(0),
    lb          = numeric(0),
    ub          = numeric(0),
    significant = logical(0)
  )
}
.read_change_csv <- function(path) {
  if (!file.exists(path)) return(.empty_change_df())
  tryCatch({
    df <- read.csv(path)
    if (!"domain" %in% names(df)) return(.empty_change_df())
    df %>% mutate(domain = as.integer(domain))
  }, error = function(e) .empty_change_df())
}

sig_mfh       <- .read_change_csv(here::here("outputs", "tables", "comparison_final.csv"))
sig_mfh_bench <- .read_change_csv(here::here("outputs", "tables", "comparison_final_bench.csv"))

# Detect whether the MFH change analysis is available. With
# `var_choice = "direct"`, eblupMFH2() often does not converge inside
# the parametric bootstrap, in which case the upstream MFH stage either
# writes comparison_final.csv with NA placeholders for the change
# columns OR fails to write comparison_final_bench.csv altogether. We
# pick this up here and emit a clear callout downstream so users see an
# explicit "not available" message instead of a render error.
.mfh_change_available <- nrow(sig_mfh) > 0 &&
  "diff" %in% names(sig_mfh) &&
  any(is.finite(suppressWarnings(as.numeric(sig_mfh$diff))))

comparison_dt <- pov_mfh %>%
  transmute(
    domain,
    year,
    Direct = direct_rate,
    Direct_MSE = direct_mse,
    Direct_CV = direct_cv,
    MFH     = .data[[.mfh_rate_col]],
    MFH_MSE = .data[[.mfh_mse_col]],
    MFH_CV  = .data[[.mfh_cv_col]],
    MFH_Bench     = if ("rate_Bench" %in% names(pov_mfh)) rate_Bench else .data[[.mfh_rate_col]],
    MFH_Bench_MSE = if ("mse_Bench"  %in% names(pov_mfh)) mse_Bench  else .data[[.mfh_mse_col]],
    MFH_Bench_CV  = if ("cv_Bench"   %in% names(pov_mfh)) cv_Bench   else .data[[.mfh_cv_col]]
  ) %>%
  left_join(
    pov_fh %>%
      transmute(
        domain,
        year,
        FH = FH,
        FH_MSE = FH_MSE,
        FH_CV = FH_CV,
        FH_Bench = FH_Bench,
        FH_Bench_MSE = FH_Bench_MSE,
        FH_Bench_CV = FH_Bench_CV
      ),
    by = c("domain", "year")
  ) %>%
  mutate(
    Direct_RMSE = sqrt(Direct_MSE),
    FH_RMSE = sqrt(FH_MSE),
    FH_Bench_RMSE = sqrt(FH_Bench_MSE),
    MFH_RMSE = sqrt(MFH_MSE),
    MFH_Bench_RMSE = sqrt(MFH_Bench_MSE)
  )

dir.create(here::here("outputs", "data"), recursive = TRUE, showWarnings = FALSE)

# Use writexl for a conformant XLSX. openxlsx::write.xlsx() has produced
# workbooks with an empty sheet dimension ("A1") and dangling drawing
# refs that fail in some downstream readers. writexl emits a minimal,
# valid XLSX. Strip attributes/labelled classes that writexl rejects.
.write_xlsx_safe <- function(df, path) {
  if (!requireNamespace("writexl", quietly = TRUE)) {
    install.packages("writexl", repos = "https://cloud.r-project.org")
  }
  df <- as.data.frame(df)
  for (col in names(df)) {
    x <- df[[col]]
    attributes(x) <- NULL
    if (inherits(x, c("haven_labelled", "labelled"))) x <- as.vector(x)
    df[[col]] <- x
  }
  writexl::write_xlsx(df, path = path)
}
.write_xlsx_safe(comparison_dt, here::here("outputs", "data", "pov_comparison_detailed.xlsx"))


deparse_formula <- function(x) paste(deparse(x), collapse = "")

make_coef_table <- function(x, coef_name = "estimate", term_names = NULL) {
  if (is.null(x)) {
    return(tibble(term = character(), !!coef_name := numeric()))
  }
  if (is.matrix(x) || is.data.frame(x)) {
    out <- as.data.frame(x)
    if (is.null(rownames(out))) {
      out$term <- paste0("coef_", seq_len(nrow(out)))
    } else {
      out$term <- rownames(out)
    }
    out %>% relocate(term)
  } else {
    if (is.null(term_names)) {
      term_names <- names(x)
    }
    tibble(term = term_names, !!coef_name := as.numeric(x))
  }
}

plot_metric_comparison <- function(year_value, metric_name, include_direct = TRUE) {
  metric_spec <- list(
    rate = c("Direct", "FH", "FH_Bench", "MFH", "MFH_Bench"),
    mse = c("Direct_MSE", "FH_MSE", "FH_Bench_MSE", "MFH_MSE", "MFH_Bench_MSE"),
    rmse = c("Direct_RMSE", "FH_RMSE", "FH_Bench_RMSE", "MFH_RMSE", "MFH_Bench_RMSE"),
    cv = c("Direct_CV", "FH_CV", "FH_Bench_CV", "MFH_CV", "MFH_Bench_CV")
  )

  # Drop MFH series when model was not properly executed

  if (.mfh_not_executed) {
    metric_spec <- lapply(metric_spec, function(cols) {
      cols[!grepl("^MFH", cols)]
    })
  }

  pretty_labels <- c(
    Direct = "Direct",
    FH = "FH",
    FH_Bench = "FH Benchmarked",
    MFH = "MFH",
    MFH_Bench = "MFH Benchmarked",
    Direct_MSE = "Direct",
    FH_MSE = "FH",
    FH_Bench_MSE = "FH Benchmarked",
    MFH_MSE = "MFH",
    MFH_Bench_MSE = "MFH Benchmarked",
    Direct_RMSE = "Direct",
    FH_RMSE = "FH",
    FH_Bench_RMSE = "FH Benchmarked",
    MFH_RMSE = "MFH",
    MFH_Bench_RMSE = "MFH Benchmarked",
    Direct_CV = "Direct",
    FH_CV = "FH",
    FH_Bench_CV = "FH Benchmarked",
    MFH_CV = "MFH",
    MFH_Bench_CV = "MFH Benchmarked"
  )

  y_labels <- c(rate = pov_lab$short, mse = "MSE", rmse = "RMSE", cv = "CV")
  title_stub <- c(
    rate = paste(pov_lab$short, "Comparison"),
    mse = "MSE Comparison",
    rmse = "RMSE Comparison",
    cv = "CV Comparison"
  )

  cols <- metric_spec[[metric_name]]
  if (!include_direct) {
    cols <- cols[cols != "Direct" & cols != "Direct_MSE" & cols != "Direct_RMSE" & cols != "Direct_CV"]
  }

  # Ordering columns: left panel by Direct, right panel by FH
  order_direct <- c(rate = "Direct", mse = "Direct_MSE", rmse = "Direct_RMSE", cv = "Direct_CV")
  order_fh     <- c(rate = "FH",     mse = "FH_MSE",     rmse = "FH_RMSE",     cv = "FH_CV")
  order_label  <- c(rate = tolower(pov_lab$short), mse = "MSE", rmse = "RMSE", cv = "CV")

  sort_col <- if (include_direct) order_direct[[metric_name]] else order_fh[[metric_name]]

  plot_df <- comparison_dt %>%
    filter(year == year_value) %>%
    arrange(.data[[sort_col]]) %>%
    mutate(domain_f = factor(domain, levels = unique(domain))) %>%
    select(domain, domain_f, all_of(cols)) %>%
    pivot_longer(-c(domain, domain_f), names_to = "Method", values_to = "value") %>%
    mutate(
      Method = factor(pretty_labels[Method], levels = names(method_colors))
    )

  x_label <- if (include_direct) {
    paste0("Domain (ordered by increasing Direct ", order_label[[metric_name]], ")")
  } else {
    paste0("Domain (ordered by increasing FH ", order_label[[metric_name]], ")")
  }

  # Only show methods that are actually plotted in the legend
  methods_present <- levels(droplevels(plot_df$Method))
  colors_used <- method_colors[names(method_colors) %in% methods_present]

  plot_df %>%
    ggplot(aes(x = domain_f, y = value, color = Method)) +
    geom_point(size = 2, alpha = 0.85) +
    scale_color_manual(values = colors_used) +
    labs(
      title = paste0(title_stub[[metric_name]], " - ", year_value,
                     if (include_direct) " (with Direct)" else " (models only)"),
      x = x_label,
      y = y_labels[[metric_name]],
      color = "Method"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
}

make_map_data <- function(value_cols, method_labels) {
  imap_dfr(value_cols, function(col_name, label) {
    comparison_dt %>%
      select(domain, year, value = all_of(col_name)) %>%
      mutate(method = method_labels[[label]])
  })
}

prepare_sig_tbl <- function(df, method_label, signif_true = c("TRUE", "Significant")) {
  # Defensive: if upstream change-analysis failed (e.g., pbmcpe bootstrap
  # collapsed with var_choice="direct"), the CSV may lack diff/mse/lb/ub/
  # significant columns. Without this guard, dplyr's data-mask falls back
  # to base::diff (the function) and as.numeric() errors out with
  # "cannot coerce type 'closure' to vector of type 'double'".
  needed <- c("diff", "mse", "lb", "ub", "significant")
  for (.col in needed) {
    if (!.col %in% names(df)) {
      df[[.col]] <- if (.col == "significant") NA else NA_real_
    }
  }
  df %>%
    transmute(
      domain = as.integer(domain),
      diff = as.numeric(diff),
      mse = as.numeric(mse),
      lb = as.numeric(lb),
      ub = as.numeric(ub),
      significant = ifelse(as.character(significant) %in% signif_true, TRUE, FALSE),
      method = method_label
    )
}


if (.mfh_not_executed) {
  cat(
    "\n::: {.callout-important}\n",
    "## eblupMFH2 was not properly executed\n\n",
    "The MFH model (**", diag_model, "**) did not produce valid random-effects estimates. ",
    "With `var_choice = \"direct\"` and `cov_choice = \"direct\"`, `eblupMFH2()` either ",
    "failed outright or converged to $\\sigma^2_u = 0$ (no domain-specific shrinkage).\n\n",
    "**All MFH maps, UFH-vs-MFH comparisons, and poverty change analyses are skipped** ",
    "because the MFH estimates are not meaningful without estimated random effects.\n\n",
    "**Action:** rerun with smoothed variance/covariance options (e.g., ",
    "`var_choice = \"sm_out\"`, `cov_choice = \"rho_sm_out\"`) to obtain valid MFH estimates.\n",
    "\n",
    sep = ""
  )
} else if (!.mfh_change_available) {
  cat(
    "\n::: {.callout-note}\n",
    "## MFH change analysis not available for this run\n\n",
    "The MCPE-based change analysis for MFH is not available. This typically ",
    "happens when `var_choice = \"direct\"` is selected: `eblupMFH2()` does not ",
    "converge inside the parametric bootstrap because the raw direct sampling ",
    "variances are unstable across domains. **This is expected behaviour with ",
    "`direct` -- it is not a code error.**\n\n",
    "Tables and figures that depend on the change analysis (significance counts, ",
    "change confidence intervals) will show NA. Levels-based comparisons ",
    "(rates, MSEs, RMSEs, CVs) are unaffected. ",
    "Re-run with `var_choice = \"sm_out\"` or `\"sm_all\"` to obtain the ",
    "change-significance analysis.\n",
    "\n",
    sep = ""
  )
}


if (.mfh_not_executed) {
  summary_tbl <- comparison_dt %>%
    group_by(year) %>%
    summarize(
      mean_direct = mean(Direct, na.rm = TRUE),
      mean_fh = mean(FH, na.rm = TRUE),
      mean_fh_bench = mean(FH_Bench, na.rm = TRUE),
      mean_fh_mse = mean(FH_MSE, na.rm = TRUE),
      mean_fh_bench_mse = mean(FH_Bench_MSE, na.rm = TRUE),
      .groups = "drop"
    )
} else {
  summary_tbl <- comparison_dt %>%
    group_by(year) %>%
    summarize(
      mean_direct = mean(Direct, na.rm = TRUE),
      mean_fh = mean(FH, na.rm = TRUE),
      mean_fh_bench = mean(FH_Bench, na.rm = TRUE),
      mean_mfh = mean(MFH, na.rm = TRUE),
      mean_mfh_bench = mean(MFH_Bench, na.rm = TRUE),
      mean_fh_mse = mean(FH_MSE, na.rm = TRUE),
      mean_fh_bench_mse = mean(FH_Bench_MSE, na.rm = TRUE),
      mean_mfh_mse = mean(MFH_MSE, na.rm = TRUE),
      mean_mfh_bench_mse = mean(MFH_Bench_MSE, na.rm = TRUE),
      .groups = "drop"
    )
}

if (.mfh_not_executed) {
  sig_summary_tbl <- bind_rows(
    prepare_sig_tbl(sig_fh, "FH"),
    prepare_sig_tbl(sig_fh_bench, "FH Benchmarked")
  ) %>%
    group_by(method) %>%
    summarize(
      significant_domains = sum(significant, na.rm = TRUE),
      total_domains = dplyr::n(),
      .groups = "drop"
    )
} else {
  sig_summary_tbl <- bind_rows(
    prepare_sig_tbl(sig_fh, "FH"),
    prepare_sig_tbl(sig_fh_bench, "FH Benchmarked"),
    prepare_sig_tbl(sig_mfh, "MFH", signif_true = c("Significant")),
    prepare_sig_tbl(sig_mfh_bench, "MFH Benchmarked", signif_true = c("Significant"))
  ) %>%
    group_by(method) %>%
    summarize(
      significant_domains = sum(significant, na.rm = TRUE),
      total_domains = dplyr::n(),
      .groups = "drop"
    )
}

kable(
  summary_tbl,
  digits = 4,
  caption = paste("Mean", tolower(pov_lab$short), "estimates and mean MSEs by year")
)

kable(
  sig_summary_tbl,
  digits = 0,
  caption = paste("Number of domains with statistically significant", tolower(pov_lab$short), "changes")
)


fh_formula_tbl <- tibble(
  Method = "FH",
  Year = years_keep,
  Formula = c(
    deparse_formula(formula(fh_models[[1]])),
    deparse_formula(formula(fh_models[[2]]))
  )
)

mfh_formula_tbl <- tibble(
  Method = "MFH",
  Year = years_keep,
  Formula = purrr::map_chr(mfh_formula, deparse_formula)
)

kable(
  bind_rows(fh_formula_tbl, mfh_formula_tbl),
  caption = "Selected regression formulas used in the FH and MFH pipelines"
)


# ---- Helper: significance stars ----
.signif_stars <- function(p) {
  ifelse(p < 0.001, "***",
  ifelse(p < 0.01,  "**",
  ifelse(p < 0.05,  "*",
  ifelse(p < 0.1,   ".",  ""))))
}

# ---- FH coefficient table with significance ----
fh_coef_tbl <- bind_rows(
  purrr::imap(fh_models, function(model, year_label) {
    .cd <- model$model$coefficients
    data.frame(
      Method    = "FH",
      Year      = as.integer(year_label),
      Term      = rownames(.cd),
      Estimate  = .cd$coefficients,
      Std.Error = .cd$std.error,
      z.value   = .cd$t.value,
      p.value   = .cd$p.value,
      Signif    = .signif_stars(.cd$p.value),
      check.names = FALSE
    )
  })
)

kable(
  fh_coef_tbl,
  digits    = c(0, 0, 0, 6, 6, 3, 4, 0),
  align     = c("l", "c", "l", "r", "r", "r", "r", "c"),
  col.names = c("Method", "Year", "Term", "Estimate", "Std.Error", "z value", "p value", ""),
  caption   = "Estimated FH regression coefficients by year",
  row.names = FALSE
)

# ---- MFH coefficient table with significance ----
.mfh_fit <- if (is.list(selected_mfh_model) && !.mfh_not_executed) selected_mfh_model$fit else NULL
.estcoef <- if (is.list(.mfh_fit)) .mfh_fit$estcoef else NULL

if (!is.null(.estcoef) && is.matrix(.estcoef)) {
  # Split stacked coefficients by time period
  .n_per_period <- sapply(mfh_formula, function(f) {
    length(attr(terms(f), "term.labels")) + 1L
  })
  .mfh_rows <- list()
  .start <- 1L
  for (.t in seq_along(mfh_formula)) {
    .end <- .start + .n_per_period[.t] - 1L
    .ec  <- .estcoef[.start:.end, , drop = FALSE]
    .yr  <- sub("^.*?(\\d{4})$", "\\1", names(mfh_formula)[.t])
    .mfh_rows[[.t]] <- data.frame(
      Method    = diag_model,
      Year      = as.integer(.yr),
      Term      = rownames(.ec),
      Estimate  = .ec[, "beta"],
      Std.Error = .ec[, "std.error"],
      z.value   = .ec[, "t.statistics"],
      p.value   = .ec[, "p.value"],
      Signif    = .signif_stars(.ec[, "p.value"]),
      check.names = FALSE
    )
    .start <- .end + 1L
  }
  mfh_coef_tbl <- do.call(rbind, .mfh_rows)

  kable(
    mfh_coef_tbl,
    digits    = c(0, 0, 0, 6, 6, 3, 4, 0),
    align     = c("l", "c", "l", "r", "r", "r", "r", "c"),
    col.names = c("Method", "Year", "Term", "Estimate", "Std.Error", "z value", "p value", ""),
    caption   = paste0("Estimated ", diag_model, " regression coefficients by year"),
    row.names = FALSE
  )
} else {
  cat(paste0("\n**Note:** ", diag_model, " coefficient table is unavailable ",
             "(model fit object has unexpected structure).\n"))
}

cat("\n*Signif. codes: 0 '\\*\\*\\*' 0.001 '\\*\\*' 0.01 '\\*' 0.05 '.' 0.1 ' ' 1*\n")

# ---- Autocorrelation parameter (rho) for MFH2/MFH3 ----
if (is.list(.mfh_fit) && !is.null(.mfh_fit$rho)) {
  .rho_df <- .mfh_fit$rho
  cat(sprintf(
    "\n**Autocorrelation parameter (rho):** %.4f  (T-stat = %.3f, p-value = %.4f %s)\n",
    .rho_df$rho,
    .rho_df$T.test,
    .rho_df$`p-value`,
    .signif_stars(.rho_df$`p-value`)
  ))
}


# Indicator-aware residual scale.
# Normality assumptions in the FH/MFH model live on the scale the model
# was fitted on. When `log_transform = TRUE` for `mean_welfare`, the FH
# model object already holds log-scale Direct/FH values (so its residuals
# are on the log scale by construction). The MFH `comparison_dt` is built
# from `pov_mfh.xlsx`, which has been back-transformed to the original
# currency scale; we therefore take logs here so MFH residuals match FH on
# the log scale. Mathematically log(Direct_back) - log(MFH_back) recovers
# the log-scale residual exactly (the Duan smearing factor cancels).
.use_log_resid <- isTRUE(log_transform) && identical(indicator_type, "mean_welfare")
.mfh_resid_fn <- function(direct, mfh) {
  if (.use_log_resid) {
    safe_log <- function(x) ifelse(!is.na(x) & x > 0, log(x), NA_real_)
    safe_log(direct) - safe_log(mfh)
  } else {
    direct - mfh
  }
}

fh_diag_long <- bind_rows(
  purrr::imap(fh_models, function(model, year_label) {
    tibble(
      year = as.integer(year_label),
      method = "FH",
      component = "Residual",
      value = model$ind$Direct - model$ind$FH
    ) %>%
      bind_rows(
        tibble(
          year = as.integer(year_label),
          method = "FH",
          component = "Random effect",
          value = as.numeric(model$model$random_effects[, 1])
        )
      )
  })
) %>%
  filter(!is.na(value))

mfh_re_df <- if (!.mfh_not_executed && is.list(selected_mfh_model) && !is.null(selected_mfh_model$randomEffect)) {
  as.data.frame(selected_mfh_model$randomEffect)
} else {
  data.frame(V1 = numeric(0), V2 = numeric(0))
}
if (ncol(mfh_re_df) == 1) {
  mfh_re_df[[2]] <- NA_real_
}

# Detect boundary case: refvar = 0 â†’ all random effects are zero
# Keep skipped/failed fits separate from genuine zero-variance fits.
.mfh_re_all_zero <- !.mfh_not_executed &&
  nrow(mfh_re_df) > 0 &&
  all(unlist(mfh_re_df) == 0, na.rm = TRUE)

if (!.mfh_not_executed) {
  mfh_diag_long <- bind_rows(
    tibble(
      year = years_keep[1],
      method = "MFH",
      component = "Residual",
      value = comparison_dt %>% filter(year == years_keep[1]) %>% mutate(.resid = .mfh_resid_fn(Direct, MFH)) %>% pull(.resid)
    ),
    tibble(
      year = years_keep[2],
      method = "MFH",
      component = "Residual",
      value = comparison_dt %>% filter(year == years_keep[2]) %>% mutate(.resid = .mfh_resid_fn(Direct, MFH)) %>% pull(.resid)
    ),
    tibble(
      year = years_keep[1],
      method = "MFH",
      component = "Random effect",
      value = as.numeric(mfh_re_df[[1]])
    ),
    tibble(
      year = years_keep[2],
      method = "MFH",
      component = "Random effect",
      value = as.numeric(mfh_re_df[[2]])
    )
  ) %>%
    filter(!is.na(value))
} else {
  mfh_diag_long <- tibble(year = integer(0), method = character(0),
                           component = character(0), value = numeric(0))
}

normality_long <- bind_rows(fh_diag_long, mfh_diag_long)

# Safe Shapiro--Wilk wrapper: returns NA for constant or too-short vectors
safe_shapiro <- function(x) {
  x <- x[!is.na(x)]
  if (length(unique(x)) < 3) return(list(statistic = c(W = NA_real_), p.value = NA_real_))
  tryCatch(shapiro.test(x),
           error = function(e) list(statistic = c(W = NA_real_), p.value = NA_real_))
}

normality_tests <- normality_long %>%
  group_by(method, year, component) %>%
  summarize(
    n = dplyr::n(),
    W = safe_shapiro(value)$statistic,
    p_value = safe_shapiro(value)$p.value,
    .groups = "drop"
  )

kable(normality_tests, digits = 4, caption = "Shapiro-Wilk normality tests for FH and MFH diagnostics")

# ---- Export diagnostics for AI companion note ----
diag_summary <- normality_long %>%
  group_by(method, year, component) %>%
  summarize(
    n = dplyr::n(),
    W = safe_shapiro(value)$statistic,
    p_value = safe_shapiro(value)$p.value,
    mean_val = mean(value, na.rm = TRUE),
    sd_val = sd(value, na.rm = TRUE),
    skewness = {
      v <- value[!is.na(value)]; n <- length(v); m <- mean(v); s <- sd(v)
      if (s == 0 || n < 3) NA_real_ else (n / ((n-1)*(n-2))) * sum(((v - m) / s)^3)
    },
    excess_kurtosis = {
      v <- value[!is.na(value)]; n <- length(v); m <- mean(v); s <- sd(v)
      if (s == 0 || n < 4) NA_real_ else ((n*(n+1)) / ((n-1)*(n-2)*(n-3))) * sum(((v - m) / s)^4) - (3*(n-1)^2) / ((n-2)*(n-3))
    },
    min_val = min(value, na.rm = TRUE),
    max_val = max(value, na.rm = TRUE),
    q1 = quantile(value, 0.25, na.rm = TRUE),
    median_val = median(value, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    outliers_beyond_2sd = sum(abs(value - mean(value, na.rm = TRUE)) > 2 * sd(value, na.rm = TRUE), na.rm = TRUE),
    outliers_beyond_3sd = sum(abs(value - mean(value, na.rm = TRUE)) > 3 * sd(value, na.rm = TRUE), na.rm = TRUE),
    qq_correlation = {
      v <- value[!is.na(value)]
      if (length(v) >= 3) {
        qq <- qqnorm(v, plot.it = FALSE)
        cor(qq$x, qq$y)
      } else NA_real_
    },
    .groups = "drop"
  )
dir.create(here::here("outputs", "data"), recursive = TRUE, showWarnings = FALSE)
write.csv(diag_summary, here::here("outputs", "tables", "normality_diagnostics.csv"), row.names = FALSE)
write.csv(normality_long, here::here("outputs", "tables", "normality_raw_values.csv"), row.names = FALSE)


if (.mfh_re_all_zero) {
  cat(
    "\n::: {.callout-warning}\n",
    "## MFH random-effects variance estimated at zero -- likely a numerical artifact\n\n",
    "The random-effects variance ($\\sigma^2_u$) for the selected MFH model (**",
    diag_model, "**) was estimated at zero. All MFH random effects are identically zero, ",
    "so the MFH EBLUP equals the regression-synthetic predictor with no domain-specific shrinkage.\n\n",
    "**This is most likely a numerical artifact, not a true REML boundary.** The `msae` ",
    "package's `eblupMFH*()` functions use unconstrained Newton--Raphson and only clamp ",
    "$\\sigma^2_u \\ge 0$ after iteration ends; the optimizer can therefore converge to a ",
    "spurious zero. This is observed especially with `cov_choice = \"zero\"`. ",
    "MFH random-effect Q-Q plots and Shapiro--Wilk tests below are not meaningful in this case.\n\n",
    "**Workaround:** rerun with a non-zero covariance (`cov_choice = \"rho_sm_out\"`, ",
    "`\"rho_dir\"`, or `\"direct\"`) and compare $\\sigma^2_u$. If the non-zero specification ",
    "gives a positive variance, the zero-covariance result is unreliable and should not be ",
    "used as evidence that between-domain random variation is zero.\n",
    "\n",
    sep = ""
  )
}


plot_qq <- function(data, method_val, year_val, comp_val) {
  df <- data %>% filter(method == method_val, year == year_val, component == comp_val)
  ggplot(df, aes(sample = value)) +
    stat_qq(alpha = 0.7, size = 2) +
    stat_qq_line(color = "red", linewidth = 0.6) +
    labs(
      title = sprintf("Q-Q Plot: %s -- %s -- %d", comp_val, method_val, year_val),
      x = "Theoretical quantiles",
      y = "Sample quantiles"
    ) +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(size = 16, face = "bold"))
}

.diag_x_label <- if (isTRUE(log_transform) && identical(indicator_type, "mean_welfare")) {
  "Diagnostic value (log scale)"
} else {
  "Diagnostic value"
}

plot_density <- function(data, year_val, comp_val) {
  df <- data %>% filter(year == year_val, component == comp_val)
  ggplot(df, aes(x = value, color = method, fill = method)) +
    geom_density(alpha = 0.15) +
    scale_color_manual(values = c("FH" = "#1f77b4", "MFH" = "#2ca02c")) +
    scale_fill_manual(values = c("FH" = "#1f77b4", "MFH" = "#2ca02c")) +
    labs(
      title = sprintf("Density: %s -- %d", comp_val, year_val),
      x = .diag_x_label,
      y = "Density"
    ) +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(size = 16, face = "bold"))
}


plot_qq(normality_long, "FH", 2012, "Residual")


plot_qq(normality_long, "FH", 2013, "Residual")


plot_qq(normality_long, "FH", 2012, "Random effect")


plot_qq(normality_long, "FH", 2013, "Random effect")


plot_qq(normality_long, "MFH", 2012, "Residual")


plot_qq(normality_long, "MFH", 2013, "Residual")


if (.mfh_not_executed || .mfh_re_all_zero) {
  cat("*Skipped -- MFH random effects are unavailable for this run.*\n")
} else {
  plot_qq(normality_long, "MFH", 2012, "Random effect")
}


if (.mfh_not_executed || .mfh_re_all_zero) {
  cat("*Skipped -- MFH random effects are unavailable for this run.*\n")
} else {
  plot_qq(normality_long, "MFH", 2013, "Random effect")
}


plot_density(normality_long, 2012, "Residual")


plot_density(normality_long, 2013, "Residual")


if (.mfh_not_executed || .mfh_re_all_zero) {
  cat("*Skipped -- MFH random effects are unavailable for this run.*\n")
} else {
  plot_density(normality_long, 2012, "Random effect")
}


if (.mfh_not_executed || .mfh_re_all_zero) {
  cat("*Skipped -- MFH random effects are unavailable for this run.*\n")
} else {
  plot_density(normality_long, 2013, "Random effect")
}


plot_metric_comparison(2012, "rate", include_direct = TRUE)


plot_metric_comparison(2012, "rate", include_direct = FALSE)


plot_metric_comparison(2012, "mse", include_direct = TRUE)


plot_metric_comparison(2012, "mse", include_direct = FALSE)


plot_metric_comparison(2012, "rmse", include_direct = TRUE)


plot_metric_comparison(2012, "rmse", include_direct = FALSE)


plot_metric_comparison(2012, "cv", include_direct = TRUE)


plot_metric_comparison(2012, "cv", include_direct = FALSE)


plot_metric_comparison(2013, "rate", include_direct = TRUE)


plot_metric_comparison(2013, "rate", include_direct = FALSE)


plot_metric_comparison(2013, "mse", include_direct = TRUE)


plot_metric_comparison(2013, "mse", include_direct = FALSE)


plot_metric_comparison(2013, "rmse", include_direct = TRUE)


plot_metric_comparison(2013, "rmse", include_direct = FALSE)


plot_metric_comparison(2013, "cv", include_direct = TRUE)


plot_metric_comparison(2013, "cv", include_direct = FALSE)


plot_poverty_map <- function(col_name, method_label, year_val) {
  map_sf <- shp_dt %>%
    left_join(
      comparison_dt %>% filter(year == year_val) %>% transmute(domain, value = .data[[col_name]]),
      by = "domain"
    )
  ggplot(map_sf) +
    geom_sf(aes(fill = value), color = NA) +
    scale_fill_viridis_c(option = "magma", labels = label_number(accuracy = 0.01), na.value = "grey90") +
    labs(
      title = paste0(method_label, " -- ", year_val),
      fill = "Rate"
    ) +
    theme_minimal(base_size = 17) +
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      legend.title = element_text(size = 17),
      legend.text = element_text(size = 15),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
}


plot_poverty_map("Direct", "Direct Map", 2012)


plot_poverty_map("Direct", "Direct Map", 2013)


plot_poverty_map("FH", "FH Map", 2012)


plot_poverty_map("FH", "FH Map", 2013)


plot_poverty_map("FH_Bench", "FH Benchmarked Map", 2012)


plot_poverty_map("FH_Bench", "FH Benchmarked Map", 2013)


plot_poverty_map("MFH", "MFH Map", 2012)


plot_poverty_map("MFH", "MFH Map", 2013)


plot_poverty_map("MFH_Bench", "MFH Benchmarked Map", 2012)


plot_poverty_map("MFH_Bench", "MFH Benchmarked Map", 2013)


if (.mfh_not_executed) {
  sig_plot_dt <- bind_rows(
    prepare_sig_tbl(sig_fh, "FH"),
    prepare_sig_tbl(sig_fh_bench, "FH Benchmarked")
  ) %>%
    mutate(
      significant_label = ifelse(significant, "Significant", "Not Significant"),
      method = factor(method, levels = c("FH", "FH Benchmarked"))
    )
} else {
  sig_plot_dt <- bind_rows(
    prepare_sig_tbl(sig_fh, "FH"),
    prepare_sig_tbl(sig_fh_bench, "FH Benchmarked"),
    prepare_sig_tbl(sig_mfh, "MFH", signif_true = c("Significant")),
    prepare_sig_tbl(sig_mfh_bench, "MFH Benchmarked", signif_true = c("Significant"))
  ) %>%
    mutate(
      significant_label = ifelse(significant, "Significant", "Not Significant"),
      method = factor(method, levels = c("FH", "FH Benchmarked", "MFH", "MFH Benchmarked"))
    )
}

.write_xlsx_safe(sig_plot_dt, here::here("outputs", "data", "statistical_significance_comparison.xlsx"))


plot_significance <- function(data, method_name) {
  df <- data %>% filter(method == method_name)
  ggplot(df, aes(x = factor(domain), y = diff, color = significant_label)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(size = 2.5, alpha = 0.8) +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.25, alpha = 0.5) +
    scale_color_manual(values = c("Not Significant" = "gray60", "Significant" = "red")) +
    labs(
      title = paste("Poverty changes (2013 - 2012):", method_name),
      x = "Domain",
      y = "Estimated change",
      color = "Status"
    ) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}


plot_significance(sig_plot_dt, "FH")


plot_significance(sig_plot_dt, "FH Benchmarked")


plot_significance(sig_plot_dt, "MFH")


plot_significance(sig_plot_dt, "MFH Benchmarked")


change_map_fh <- shp_dt %>%
  select(domain, geometry) %>%
  left_join(sig_plot_dt %>% filter(method == "FH") %>% select(domain, diff), by = "domain")

ggplot(change_map_fh) +
  geom_sf(aes(fill = diff), color = NA) +
  scale_fill_gradient2(
    low = "#2c7bb6",
    mid = "white",
    high = "#d7191c",
    midpoint = 0,
    labels = label_number(accuracy = 0.01),
    na.value = "grey90"
  ) +
  labs(
    title = "1. Poverty Change Map: FH",
    fill = "Change"
  ) +
  theme_minimal(base_size = 17) +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 15),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


change_map_fh_bench <- shp_dt %>%
  select(domain, geometry) %>%
  left_join(sig_plot_dt %>% filter(method == "FH Benchmarked") %>% select(domain, diff), by = "domain")

ggplot(change_map_fh_bench) +
  geom_sf(aes(fill = diff), color = NA) +
  scale_fill_gradient2(
    low = "#2c7bb6",
    mid = "white",
    high = "#d7191c",
    midpoint = 0,
    labels = label_number(accuracy = 0.01),
    na.value = "grey90"
  ) +
  labs(
    title = "2. Poverty Change Map: FH Benchmarked",
    fill = "Change"
  ) +
  theme_minimal(base_size = 17) +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 15),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


change_map_mfh <- shp_dt %>%
  select(domain, geometry) %>%
  left_join(sig_plot_dt %>% filter(method == "MFH") %>% select(domain, diff), by = "domain")

ggplot(change_map_mfh) +
  geom_sf(aes(fill = diff), color = NA) +
  scale_fill_gradient2(
    low = "#2c7bb6",
    mid = "white",
    high = "#d7191c",
    midpoint = 0,
    labels = label_number(accuracy = 0.01),
    na.value = "grey90"
  ) +
  labs(
    title = "3. Poverty Change Map: MFH",
    fill = "Change"
  ) +
  theme_minimal(base_size = 17) +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 15),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


change_map_mfh_bench <- shp_dt %>%
  select(domain, geometry) %>%
  left_join(sig_plot_dt %>% filter(method == "MFH Benchmarked") %>% select(domain, diff), by = "domain")

ggplot(change_map_mfh_bench) +
  geom_sf(aes(fill = diff), color = NA) +
  scale_fill_gradient2(
    low = "#2c7bb6",
    mid = "white",
    high = "#d7191c",
    midpoint = 0,
    labels = label_number(accuracy = 0.01),
    na.value = "grey90"
  ) +
  labs(
    title = "4. Poverty Change Map: MFH Benchmarked",
    fill = "Change"
  ) +
  theme_minimal(base_size = 17) +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 15),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


# ============================================================================
# Export every figure, map, and summary table shown in this document to
# output/Comparison/figures/ so users can copy them straight into reports.
#
# Layout (mirrors the HTML section structure):
#
#   output/Comparison/figures/
#     README.md                    guide to what's in each subfolder
#     tables/                      CSV summaries (Overview, Regression Models,
#                                    Normality Diagnostics)
#     diagnostics/                 Q-Q plots + density plots
#     metric_comparisons/          Rate / MSE / RMSE / CV plots
#     poverty_maps/                Level maps (Direct / FH / MFH, by year)
#     change_figures/              Significance plots + change-over-time maps
#
# Each save is wrapped in tryCatch so a single failing plot doesn't abort
# the whole render; warnings are logged to the render log.
# ============================================================================
figures_root <- here::here("outputs", "figures")
dir.create(figures_root, recursive = TRUE, showWarnings = FALSE)
.figure_subdirs <- c("tables", "diagnostics", "metric_comparisons",
                     "poverty_maps", "change_figures")
for (.sd in .figure_subdirs) {
  dir.create(file.path(figures_root, .sd), recursive = TRUE, showWarnings = FALSE)
}

# ---- OneDrive overwrite workaround ----------------------------------------
# OneDrive's Files On-Demand layer silently drops in-place overwrites of
# already-synced PNGs written via grDevices::png() (used by ggsave()): the
# handle opens, R writes bytes, dev.off() flushes, and OneDrive discards the
# flush, keeping the prior cached version on disk. Text/xlsx writes via R's
# standard file connections (writeLines, write.csv, zip::zipr) are NOT
# affected, which is why README/HTML/CSV/XLSX update correctly but PNGs stay
# stale on the second run. Deleting any pre-existing exports first forces
# every subsequent ggsave() to create a *new* file, which OneDrive accepts.
.existing_outputs <- list.files(figures_root, recursive = TRUE,
                                full.names = TRUE, all.files = TRUE,
                                include.dirs = FALSE)
if (length(.existing_outputs) > 0) {
  unlink(.existing_outputs, force = TRUE)
}

# Dedicated diagnostic log for the figure-export chunk. Written to disk so
# we can see per-call success/failure even when the child render's stderr
# isn't captured by the Shiny app.
.export_log_path <- here::here("outputs", "tables", "comparison_export_log.txt")
tryCatch({
  cat(sprintf("=== save-all-figures-tables started %s ===\n",
              format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      file = .export_log_path, append = FALSE)
}, error = function(e) NULL)
.export_log_push <- function(msg) {
  tryCatch(
    cat(sprintf("[%s] %s\n",
                format(Sys.time(), "%H:%M:%S"),
                paste(as.character(msg), collapse = " ")),
        file = .export_log_path, append = TRUE),
    error = function(e) NULL
  )
  message(msg)
}

.safe_ggsave <- function(plot, path, ...) {
  # Build the plot OUTSIDE ggsave so a build error is reported separately
  # from a device/write error and we can see which stage failed.
  built_plot <- tryCatch(force(plot), error = function(e) {
    .export_log_push(sprintf("  BUILD FAIL %s :: %s", path, conditionMessage(e)))
    NULL
  })
  if (is.null(built_plot)) return(FALSE)
  tryCatch({
    ggsave(path, built_plot, ...)
    .export_log_push(sprintf("  saved %s", path))
    TRUE
  }, error = function(e) {
    .export_log_push(sprintf("  SAVE FAIL %s :: %s",
                             path, conditionMessage(e)))
    FALSE
  }, warning = function(w) {
    # Try once more, forcing grDevices::png as the device -- some Windows
    # installs have a broken default device that surfaces as a warning.
    .export_log_push(sprintf("  SAVE WARN %s :: %s (retrying with device=png)",
                             path, conditionMessage(w)))
    retry_ok <- tryCatch({
      ggsave(path, built_plot, device = "png", ...)
      .export_log_push(sprintf("  saved (retry) %s", path))
      TRUE
    }, error = function(e2) {
      .export_log_push(sprintf("  RETRY FAIL %s :: %s",
                               path, conditionMessage(e2)))
      FALSE
    })
    retry_ok
  })
}
.safe_write_csv <- function(obj, path) {
  tryCatch({
    write.csv(obj, path, row.names = FALSE)
    .export_log_push(sprintf("  saved %s", path))
    TRUE
  }, error = function(e) {
    .export_log_push(sprintf("  SAVE FAIL %s :: %s",
                             path, conditionMessage(e)))
    FALSE
  })
}

# ---- Summary tables (CSV) ----
message("Exporting summary tables ...")
.safe_write_csv(summary_tbl,
                file.path(figures_root, "tables/executive_summary.csv"))
.safe_write_csv(sig_summary_tbl,
                file.path(figures_root, "tables/significance_summary.csv"))
.safe_write_csv(bind_rows(fh_formula_tbl, mfh_formula_tbl),
                file.path(figures_root, "tables/model_formulas.csv"))
.safe_write_csv(fh_coef_tbl,
                file.path(figures_root, "tables/fh_coefficients.csv"))
if (exists("mfh_coef_tbl") && is.data.frame(mfh_coef_tbl)) {
  .safe_write_csv(mfh_coef_tbl,
                  file.path(figures_root, "tables/mfh_coefficients.csv"))
}
.safe_write_csv(normality_tests,
                file.path(figures_root, "tables/normality_tests.csv"))

# ---- Normality diagnostics: Q-Q plots ----
message("Exporting Q-Q plots ...")
.qq_methods <- if (.mfh_not_executed) "FH" else c("FH", "MFH")
for (.m in .qq_methods) {
  for (.yr in years_keep) {
    for (.comp in c("Residual", "Random effect")) {
      if (.mfh_re_all_zero && .m == "MFH" && .comp == "Random effect") next
      .tag <- if (.comp == "Residual") "residual" else "random_effect"
      .fname <- sprintf("qq_%s_%s_%d.png", tolower(.m), .tag, .yr)
      .safe_ggsave(
        plot_qq(normality_long, .m, .yr, .comp),
        file.path(figures_root, "diagnostics", .fname),
        width = 10, height = 6, dpi = 300
      )
    }
  }
}

# ---- Normality diagnostics: density plots ----
message("Exporting density plots ...")
for (.yr in years_keep) {
  for (.comp in c("Residual", "Random effect")) {
    if (.mfh_re_all_zero && .comp == "Random effect") next
    .tag <- if (.comp == "Residual") "residual" else "random_effect"
    .fname <- sprintf("density_%s_%d.png", .tag, .yr)
    .safe_ggsave(
      plot_density(normality_long, .yr, .comp),
      file.path(figures_root, "diagnostics", .fname),
      width = 10, height = 6, dpi = 300
    )
  }
}

# ---- Metric comparisons (rate / MSE / RMSE / CV) ----
message("Exporting metric comparison plots ...")
for (.yr in years_keep) {
  for (.metric in c("rate", "mse", "rmse", "cv")) {
    for (.inc_direct in c(TRUE, FALSE)) {
      .suffix <- if (.inc_direct) "with_direct" else "models_only"
      .fname <- sprintf("compare_%d_%s_%s.png", .yr, .metric, .suffix)
      .safe_ggsave(
        plot_metric_comparison(.yr, .metric, include_direct = .inc_direct),
        file.path(figures_root, "metric_comparisons", .fname),
        width = 12, height = 6, dpi = 300
      )
    }
  }
}

# ---- Poverty level maps ----
message("Exporting poverty level maps ...")
.map_specs <- list(
  list(col = "Direct",    label = "Direct Map",          tag = "direct"),
  list(col = "FH",        label = "FH Map",              tag = "fh"),
  list(col = "FH_Bench",  label = "FH Benchmarked Map",  tag = "fh_benchmarked")
)
if (!.mfh_not_executed) {
  .map_specs <- c(.map_specs, list(
    list(col = "MFH",       label = "MFH Map",             tag = "mfh"),
    list(col = "MFH_Bench", label = "MFH Benchmarked Map", tag = "mfh_benchmarked")
  ))
}
for (.spec in .map_specs) {
  for (.yr in years_keep) {
    .fname <- sprintf("map_%s_%d.png", .spec$tag, .yr)
    .safe_ggsave(
      plot_poverty_map(.spec$col, .spec$label, .yr),
      file.path(figures_root, "poverty_maps", .fname),
      width = 12, height = 10, dpi = 300
    )
  }
}

# ---- Significance plots ----
message("Exporting significance plots ...")
.sig_specs <- list(
  list(method = "FH",              tag = "fh"),
  list(method = "FH Benchmarked",  tag = "fh_benchmarked")
)
if (!.mfh_not_executed) {
  .sig_specs <- c(.sig_specs, list(
    list(method = "MFH",             tag = "mfh"),
    list(method = "MFH Benchmarked", tag = "mfh_benchmarked")
  ))
}
for (.spec in .sig_specs) {
  .fname <- sprintf("significance_%s.png", .spec$tag)
  .safe_ggsave(
    plot_significance(sig_plot_dt, .spec$method),
    file.path(figures_root, "change_figures", .fname),
    width = 14, height = 6, dpi = 300
  )
}

# ---- Change-over-time maps ----
message("Exporting change-over-time maps ...")
.change_map_specs <- list(
  list(method = "FH",              title = "1. Poverty Change Map: FH",
       tag = "fh"),
  list(method = "FH Benchmarked",  title = "2. Poverty Change Map: FH Benchmarked",
       tag = "fh_benchmarked")
)
if (!.mfh_not_executed) {
  .change_map_specs <- c(.change_map_specs, list(
    list(method = "MFH",             title = "3. Poverty Change Map: MFH",
         tag = "mfh"),
    list(method = "MFH Benchmarked", title = "4. Poverty Change Map: MFH Benchmarked",
         tag = "mfh_benchmarked")
  ))
}
for (.spec in .change_map_specs) {
  .fname <- sprintf("change_map_%s.png", .spec$tag)
  .map_sf <- shp_dt %>%
    select(domain, geometry) %>%
    left_join(
      sig_plot_dt %>% filter(method == .spec$method) %>% select(domain, diff),
      by = "domain"
    )
  .p <- ggplot(.map_sf) +
    geom_sf(aes(fill = diff), color = NA) +
    scale_fill_gradient2(
      low = "#2c7bb6", mid = "white", high = "#d7191c",
      midpoint = 0, labels = label_number(accuracy = 0.01),
      na.value = "grey90"
    ) +
    labs(title = .spec$title, fill = "Change") +
    theme_minimal(base_size = 17) +
    theme(
      plot.title   = element_text(size = 24, face = "bold"),
      legend.title = element_text(size = 17),
      legend.text  = element_text(size = 15),
      axis.text    = element_blank(),
      axis.ticks   = element_blank(),
      panel.grid   = element_blank()
    )
  .safe_ggsave(.p, file.path(figures_root, "change_figures", .fname),
               width = 16, height = 10, dpi = 300)
}

# ---- README ----
.readme <- c(
  "# Comparison figures and tables",
  "",
  sprintf("Generated on %s from `qmd/Comparison_v2.qmd`.", format(Sys.Date())),
  "",
  "Each subfolder corresponds to a section of `Comparison_v2.html`. The",
  "PNGs here are the same figures shown in the HTML report, but as",
  "standalone 300 dpi images ready to drop into slides or report drafts.",
  "",
  "## tables/",
  "Summary CSVs:",
  "",
  "- `executive_summary.csv` - mean poverty rate and MSE by year and method",
  "- `significance_summary.csv` - number of domains with significant changes",
  "- `model_formulas.csv` - regression formulas used by the FH and MFH models",
  "- `fh_coefficients.csv` - FH coefficient estimates with p-values",
  "- `mfh_coefficients.csv` - MFH coefficient estimates (if the MFH model ran)",
  "- `normality_tests.csv` - Shapiro-Wilk statistics on residuals / random effects",
  "",
  "## diagnostics/",
  "Q-Q and density plots for FH and MFH residuals and random effects.",
  "Filenames:",
  "",
  "- `qq_<method>_<component>_<year>.png`",
  "- `density_<component>_<year>.png`",
  "",
  "## metric_comparisons/",
  "Side-by-side domain-level comparisons of poverty rate, MSE, RMSE, and CV",
  "across methods. Filenames:",
  "",
  "- `compare_<year>_<metric>_with_direct.png` - includes the Direct estimator",
  "- `compare_<year>_<metric>_models_only.png` - FH / MFH (+ benchmarked) only",
  "",
  "## poverty_maps/",
  "Domain-level poverty maps for each method and year. Filenames:",
  "",
  "- `map_<method>_<year>.png` where method is one of",
  "  `direct`, `fh`, `fh_benchmarked`, `mfh`, `mfh_benchmarked`.",
  "",
  "## change_figures/",
  "Significance plots and maps of the domain-level change between the two",
  "years. Filenames:",
  "",
  "- `significance_<method>.png`",
  "- `change_map_<method>.png`"
)
writeLines(.readme, file.path(figures_root, "README.md"))

message("All comparison figures and tables saved to: ",
        normalizePath(figures_root, winslash = "/", mustWork = FALSE))


# ============================================================
# Inject a standalone tab-switching script and Bootstrap CSS
# fallback directly into the HTML.
#
# When the Quarto _files folder is unavailable (common with
# RStudio-bundled Quarto on Windows where embed-resources
# cannot be used), the page loses its styling and interactive
# tabsets.  This chunk:
#
#   1. Detects at page load whether Bootstrap CSS/JS was loaded
#      by checking for the #quarto-bootstrap stylesheet.
#   2. If missing, injects Bootstrap 5 CSS and JS from a CDN.
#   3. Registers a lightweight tab-switching handler as a
#      fallback that works even without full Bootstrap JS,
#      ensuring panels show one figure at a time.
# ============================================================
cat('
<script>
(function() {
  "use strict";

  function ensureBootstrap(cb) {
    // Check if Quarto already loaded Bootstrap CSS
    if (document.getElementById("quarto-bootstrap")) { cb(); return; }
    // Inject Bootstrap 5 CSS from CDN
    var css = document.createElement("link");
    css.rel = "stylesheet";
    css.href = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.1/dist/css/bootstrap.min.css";
    css.id = "quarto-bootstrap";
    document.head.appendChild(css);
    // Inject Bootstrap 5 JS bundle from CDN
    var js = document.createElement("script");
    js.src = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.1/dist/js/bootstrap.bundle.min.js";
    js.onload = cb;
    js.onerror = cb; // proceed even if CDN fails
    document.head.appendChild(js);
  }

  function initTabs() {
    // Activate Bootstrap-style tabs without requiring full Bootstrap JS
    document.querySelectorAll("[data-bs-toggle=\\"tab\\"]").forEach(function(tab) {
      tab.addEventListener("click", function(e) {
        e.preventDefault();
        var target = document.querySelector(
          this.getAttribute("data-bs-target") || this.getAttribute("href"));
        if (!target) return;
        // Deactivate siblings
        var tabList = this.closest(".nav, [role=tablist]");
        if (tabList) {
          tabList.querySelectorAll("[data-bs-toggle=\\"tab\\"]").forEach(function(t) {
            t.classList.remove("active");
            t.setAttribute("aria-selected", "false");
            var p = document.querySelector(
              t.getAttribute("data-bs-target") || t.getAttribute("href"));
            if (p) { p.classList.remove("active","show"); p.style.display = "none"; }
          });
        }
        // Activate clicked tab + pane
        this.classList.add("active");
        this.setAttribute("aria-selected", "true");
        target.classList.add("active","show");
        target.style.display = "";
      });
    });
    // Initially hide non-active panes
    document.querySelectorAll(".tab-pane").forEach(function(pane) {
      if (!pane.classList.contains("active")) { pane.style.display = "none"; }
    });
  }

  function run() { ensureBootstrap(initTabs); }
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", run);
  } else {
    run();
  }
})();
</script>
')
