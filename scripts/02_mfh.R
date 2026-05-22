# ============================================================================
# EU SAE Package4 -- 02_mfh.R
# Multivariate Fay-Herriot (MFH) Pipeline
#
# Converted from qmd/50-mfh_v2.qmd (computation only, no prose/knitr)
# All paths use here::here() anchored to the project root.
# Figures saved to outputs/figures/, data to outputs/data/,
# tables to outputs/tables/.
# ============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(tictoc)
  library(tidyverse)
  library(car)
  library(msae)
  library(sae)
  library(survey)
  library(spdep)
  library(MASS)
  library(caret)
  library(conflicted)
  library(tibble)
  library(emdi)
  library(ggplot2)
  library(gt)
  library(viridis)
  library(scales)
  library(patchwork)
  library(knitr)
  library(here)
})

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag",    "dplyr")
conflicted::conflict_prefer("first",  "dplyr")
conflicted::conflict_prefer("recode", "dplyr")

source(here::here("scripts", "ufh_functions.R"))
source(here::here("scripts", "mcpe_functions.R"))

# ---- Invalidate stale MFH output artifacts ----
dir.create(here::here("outputs", "data"), showWarnings = FALSE, recursive = TRUE)
.mfh_outputs_to_clean <- c(
  here::here("outputs", "data", "mfh_artifacts.rds"),
  here::here("outputs", "tables", "mfh_shapiro_results.csv"),
  here::here("outputs", "data", "mcpemfh2_obj.rds"),
  here::here("outputs", "data", "mcpe_mfh1_obj.rds"),
  here::here("outputs", "data", "mcpe_mfh2_obj.rds"),
  here::here("outputs", "data", "mcpe_mfh3_obj.rds"),
  here::here("outputs", "tables", "comparison_final.csv"),
  here::here("outputs", "tables", "comparison_final_bench.csv"),
  here::here("outputs", "data", "pov_mfh.xlsx")
)
for (.f in .mfh_outputs_to_clean) {
  if (file.exists(.f)) {
    tryCatch(file.remove(.f),
             warning = function(w) NULL,
             error   = function(e) NULL)
  }
}
cat("Cleaned stale MFH output artifacts before render.\n")

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

# ---- Patch msae convergence check for R >= 4.4 compatibility ----
# The msae package uses `if (kit >= MAXITER && diff >= PRECISION)` where
# `diff` is a vector (length > 1). R >= 4.4 errors on && with vectors.
# We create patched versions that shadow the originals in the global env.
.fix_ast <- function(expr) {
  if (is.call(expr)) {
    if (identical(expr[[1]], as.name("&&")) && length(expr) == 3) {
      rhs <- expr[[3]]
      if (is.call(rhs) && identical(rhs[[1]], as.name(">=")) &&
          length(rhs) == 3 && identical(rhs[[2]], as.name("diff")) &&
          identical(rhs[[3]], as.name("PRECISION"))) {
        expr[[3]] <- call("any", rhs)
        return(expr)
      }
    }
    for (i in seq_along(expr)) {
      expr[[i]] <- .fix_ast(expr[[i]])
    }
  }
  expr
}

# Create patched copies in the global environment so they shadow msae's versions
for (.fn_name in c("eblupMFH1", "eblupMFH2", "eblupMFH3", "eblupUFH")) {
  .fn <- getFromNamespace(.fn_name, "msae")
  body(.fn) <- .fix_ast(body(.fn))
  environment(.fn) <- asNamespace("msae")
  assign(.fn_name, .fn, envir = globalenv())
}
rm(.fix_ast, .fn_name, .fn)

# ---- Robust refit fallback for eblupMFH2 ---------------------------------
# msae's eblupMFH2() uses unconstrained Newton-Raphson on (sigma2_u, rho_u)
# and only clamps sigma2_u >= 0 AFTER iteration ends. In some configurations
# (e.g., highly heterogeneous sm_out variances paired with cov = "zero", or
# near-rank-deficient sampling-error covariance matrices when rho_R is close
# to 1), the optimizer is driven into the negative region during iteration
# and converges to a spurious sigma2_u = 0. The fingerprint is a refvar
# that varies non-monotonically with the inputs, which is inconsistent with
# a true REML boundary.
#
# To handle that case robustly without disturbing well-behaved fits, we:
#   (a) keep the AST-patched eblupMFH2 above as `.eblupMFH2_patched` so it
#       can still be invoked directly when needed; and
#   (b) replace the GLOBAL `eblupMFH2` with a thin wrapper that calls the
#       patched function first, returns its result unchanged when refvar > 0,
#       and only falls back to a constrained optim() refit when refvar = 0.
# This means standard runs continue to use msae's implementation byte-for-
# byte, while pathological boundary cases get auto-corrected. The same
# wrapped `eblupMFH2` is used by every downstream caller (pbmcpe_with_existing,
# bench_regional_mfh, etc.) because they all reach `eblupMFH2` through the
# global env.
.eblupMFH2_patched <- get("eblupMFH2", envir = globalenv())
source(here::here("scripts", "eblupMFH2_robust.R"))
.eblupMFH2_with_robust <- function(formula, vardir, MAXITER = 100,
                                   PRECISION = 1e-04, data) {
  eblupMFH2_robust(formula = formula, vardir = vardir, MAXITER = MAXITER,
                   PRECISION = PRECISION, data = data,
                   .orig_fn = .eblupMFH2_patched)
}
assign("eblupMFH2", .eblupMFH2_with_robust, envir = globalenv())
cat(
  "patch-msae: eblupMFH2 wrapped with robust refit fallback ",
  "(activates only when msae returns refvar = 0).\n",
  sep = ""
)




# ============================================================
# Data loading and harmonization
#
# Objective:
# Load the core data inputs required for the MFH analysis and
# harmonize variable names across sources to a common, canonical
# schema used consistently throughout the document.
#
# This chunk:
# - loads household-level survey microdata, domain-level auxiliary
#   covariates, and spatial geometries;
# - removes redundant domain labels to avoid ambiguity in merges;
# - maps raw variable names to standardized names used in estimation
#   and diagnostics; and
# - defines the set of analysis years retained for MFH estimation.
# ============================================================

# ---- Load data ----
cfg_path <- Sys.getenv("SAE_APP_CONFIG", unset = "")
cfg      <- list()
mfh_cfg  <- list()
if (nzchar(cfg_path) && file.exists(cfg_path)) {
  cfg <- yaml::read_yaml(cfg_path)
  if (!is.null(cfg$mfh)) mfh_cfg <- cfg$mfh
}

cfg_or_default <- function(x, default) {
  if (is.null(x) || length(x) == 0 ||
      (length(x) == 1 && is.na(x)) ||
      (is.character(x) && !isTRUE(nzchar(x)))) default else x
}

# ---- Indicator: poverty (FGT) or mean welfare ----------------------------
if (file.exists(here::here("R", "indicator_helpers.R"))) source(here::here("R", "indicator_helpers.R"))

indicator_type   <- cfg_or_default(cfg$indicator_type,  "poverty")
# MFH-specific log_transform (preferred). Falls back to the global
# cfg$log_transform for back-compat with older app_config.yml files
# that didn't carry per-model flags. Independent from the UFH choice
# so MFH can run on log scale even when UFH ran on identity, or
# vice versa.
.mfh_log_transform_raw <- if (!is.null(cfg$mfh$log_transform)) {
  cfg$mfh$log_transform
} else {
  cfg_or_default(cfg$log_transform, FALSE)
}
log_transform    <- isTRUE(.mfh_log_transform_raw) &&
                    identical(indicator_type, "mean_welfare")

# MFH bias correction. TRUE -> apply Duan smearing (bc_sm) when
# back-transforming the log fit to the original scale. FALSE -> use
# naive exp(eta_hat). Default TRUE if missing. Only meaningful when
# log_transform is on; otherwise ignored.
mfh_bias_correction <- as.logical(cfg_or_default(cfg$mfh$bias_correction, TRUE))
if (is.na(mfh_bias_correction)) mfh_bias_correction <- TRUE
currency_symbol  <- cfg_or_default(cfg$currency_symbol, "EUR")
fgt_alpha        <- as.integer(cfg_or_default(cfg$fgt_alpha, 0L))
povline_type     <- cfg_or_default(cfg$povline_type, "column")
povline_cfg      <- cfg_or_default(cfg$povline_value, "povline")

ind_lab <- indicator_label(indicator_type, fgt_alpha,
                            log_transform = log_transform,
                            currency_symbol = currency_symbol)
pov_lab <- ind_lab    # back-compat alias for downstream chunks

if (identical(indicator_type, "poverty")) {
  cat("Indicator: Poverty --", ind_lab$fgt, "-", ind_lab$short, "\n")
  if (povline_type == "numeric") cat("Poverty line (numeric):", povline_cfg, "\n")
} else {
  cat("Indicator: Mean welfare", if (log_transform) "(log-fit, back-transformed)" else "(identity scale)", "\n")
}

survey_path <- cfg_or_default(mfh_cfg$survey_path, here::here("data", "pov_direct3.rds"))
rhs_path    <- cfg_or_default(mfh_cfg$rhs_path, here::here("data", "sae_data.rds"))
shp_path    <- cfg_or_default(mfh_cfg$shp_path, here::here("data", "geometries.rds"))

survey_raw <- readRDS(survey_path)   # household-level survey data
rhs_dt     <- readRDS(rhs_path)      # domain-level covariates
shp_dt     <- readRDS(shp_path)      # province geometries

# ---- Drop auxiliary domain labels ----
rhs_dt <- rhs_dt %>%
  select(-any_of("provlab"))

shp_dt <- shp_dt %>%
  select(-any_of("provlab"))

survey_raw <- survey_raw %>%
  select(-any_of("provlab"))

# ============================================================
# Variable name mapping
# Raw data  ->  Canonical names used in the code
# ============================================================

var_map <- list(
  year     = "year",      # survey year
  domain   = "prov",      # small area / domain identifier
  psu      = "ea_id",     # primary sampling unit
  weight   = "weight",    # survey weights
  welfare  = "income",    # welfare aggregate
  povline  = "povline",   # poverty line
  poor     = "poor",      # poverty indicator (if exists)
  region   = "region"     # higher-level geographic unit for benchmarking
)

if (!is.null(mfh_cfg$var_map)) {
  var_map[names(mfh_cfg$var_map)] <- mfh_cfg$var_map
}

# ---- Harmonize variable names ----
rename_cols_mfh <- c(
  year = var_map$year, domain = var_map$domain, psu = var_map$psu,
  weight = var_map$weight, welfare = var_map$welfare
)
if (identical(indicator_type, "poverty") &&
    povline_type == "column" && !is.null(var_map$povline)) {
  rename_cols_mfh <- c(rename_cols_mfh, povline = var_map$povline)
}
survey_dt <- survey_raw %>% rename(!!!rename_cols_mfh)
# Mean-welfare runs ignore the poverty line; ensure the column exists for
# any downstream code that references it.
if (!identical(indicator_type, "poverty") && !"povline" %in% names(survey_dt)) {
  survey_dt$povline <- NA_real_
}
# When the poverty line is a numeric constant, create the column
if (povline_type == "numeric") {
  survey_dt$povline <- as.numeric(povline_cfg)
}

# Rename region column if present in survey data (needed for benchmarking)
if (var_map$region %in% colnames(survey_dt) && var_map$region != "region") {
  survey_dt <- survey_dt %>% rename(region = !!var_map$region)
}

rhs_domain_col <- cfg_or_default(mfh_cfg$rhs_domain, "prov")
shp_domain_col <- cfg_or_default(mfh_cfg$shp_domain, "prov")

rhs_dt <- rhs_dt %>%
  rename(domain = all_of(rhs_domain_col))

shp_dt <- shp_dt %>%
  rename(domain = all_of(shp_domain_col))

# ---- Analysis years ----
years_keep <- c(2012L, 2013L)
if (!is.null(mfh_cfg$years_keep) && length(mfh_cfg$years_keep) == 2) {
  years_keep <- sort(as.integer(unlist(mfh_cfg$years_keep)))
}

# ---- Model configuration ----
# Which MFH variant to use for diagnostics, maps, MCPE change analysis,
# and final results downstream. Valid options: "MFH1", "MFH2", "MFH3"
# ("MFH3" requires fit_mfh3 = TRUE).
#
# UFH is intentionally NOT a valid choice here: UFH analysis is handled
# end-to-end by 40-fh_v2.qmd. This stage (50-mfh_v2.qmd) and the
# downstream Comparison report only support multivariate variants so
# that cross-period inference (MCPE) is well-defined.
diag_model <- cfg_or_default(mfh_cfg$diag_model, "MFH2")
if (identical(toupper(diag_model), "UFH")) {
  message(
    "diag_model = 'UFH' is no longer supported in 50-mfh_v2.qmd. ",
    "UFH analysis is produced by 40-fh_v2.qmd. ",
    "Falling back to 'MFH2' for the multivariate pipeline."
  )
  diag_model <- "MFH2"
}

# Set to TRUE to estimate the MFH3 (heteroskedastic) model.
# MFH3 allows the variance of area-time random effects to differ across
# time periods. Use when homoskedasticity of random effects is rejected.
fit_mfh3 <- isTRUE(mfh_cfg$fit_mfh3)

# Record the user's original intent separately from the runtime `fit_mfh3`
# flag, because `fit_mfh3` may be flipped to FALSE later if estimation fails.
# The callout warning near the end of Step 3 uses this to decide whether
# to notify the reader that a requested MFH3 fit fell back to another model.
fit_mfh3_requested <- fit_mfh3 || identical(diag_model, "MFH3")

# Per-year candidate vars (new) or single list (legacy)
mfh_candidate_vars_y1 <- NULL
mfh_candidate_vars_y2 <- NULL
if (!is.null(mfh_cfg$candidate_vars_y1) && length(mfh_cfg$candidate_vars_y1) > 0) {
  mfh_candidate_vars_y1 <- as.character(unlist(mfh_cfg$candidate_vars_y1))
}
if (!is.null(mfh_cfg$candidate_vars_y2) && length(mfh_cfg$candidate_vars_y2) > 0) {
  mfh_candidate_vars_y2 <- as.character(unlist(mfh_cfg$candidate_vars_y2))
}
# Legacy fallback
if (is.null(mfh_candidate_vars_y1) && is.null(mfh_candidate_vars_y2) &&
    !is.null(mfh_cfg$candidate_vars) && length(mfh_cfg$candidate_vars) > 0) {
  mfh_candidate_vars_y1 <- as.character(unlist(mfh_cfg$candidate_vars))
  mfh_candidate_vars_y2 <- mfh_candidate_vars_y1
}
# Validate user-specified candidate vars against actual RHS columns
available_covs <- setdiff(names(rhs_dt), c("domain", "year"))
validate_mfh_covs <- function(vars, label) {
  if (is.null(vars) || length(vars) == 0) return(NULL)
  found   <- vars[vars %in% available_covs]
  missing <- vars[!vars %in% available_covs]
  if (length(missing) > 0) {
    warning(sprintf("MFH %s: dropping unknown covariates: %s",
                    label, paste(missing, collapse = ", ")))
  }
  if (length(found) == 0) return(NULL)
  found
}
mfh_candidate_vars_y1 <- validate_mfh_covs(mfh_candidate_vars_y1, "Year 1")
mfh_candidate_vars_y2 <- validate_mfh_covs(mfh_candidate_vars_y2, "Year 2")
# Per-year candidate vars are applied separately during variable selection

# ---- Benchmarking configuration ----
# Set to TRUE to apply regional benchmarking to MFH estimates
# (consistent with UFH pipeline). Requires region and population data.
do_benchmark <- isTRUE(mfh_cfg$do_benchmark) ||
  cfg_or_default(mfh_cfg$do_benchmark, TRUE)
bench_nB <- as.integer(cfg_or_default(mfh_cfg$bench_nB, 200))

# Model selection criterion: "AIC" (default) or "BIC"
mfh_ic_criterion <- cfg_or_default(mfh_cfg$ic_criterion, "AIC")
if (!mfh_ic_criterion %in% c("AIC", "BIC")) {
  warning(sprintf("Invalid ic_criterion '%s' for MFH; defaulting to AIC.", mfh_ic_criterion))
  mfh_ic_criterion <- "AIC"
}

# ---- Province-to-region mapping (derived from survey data) ----
if ("region" %in% colnames(survey_dt)) {
  region_map <- survey_dt %>%
    select(domain, region) %>%
    distinct()
} else {
  region_map <- NULL
  if (do_benchmark) {
    message("Warning: 'region' column not found in survey data. ",
            "Regional benchmarking will be skipped.")
    do_benchmark <- FALSE
  }
}

# ---- Domain population sizes ----
# Uses sizeprov from the sae package (same as UFH pipeline)
if (do_benchmark) {
  data("sizeprov", package = "sae")
  sizeprov <- sizeprov %>% rename(domain = prov)
}

# Source benchmarking functions for MFH
source(here::here("scripts", "bench_regional_mfh.R"))



survey_dt |> glimpse()


rhs_dt |> glimpse()


shp_dt |> glimpse()



library(tibble)
library(gt)
library(dplyr)

# Create the updated table content with canonical names
mfh_table <- tibble::tibble(
  Dataset = c("`survey_dt`", "`rhs_dt`", "`shp_dt`"),
  `Unit of Observation` = c(
    "Household (or Individual)",
    "Target Area (e.g., Province)",
    "Target Area (Spatial)"
  ),
  `Required Variables` = c(
    "`domain`, `weight`, `psu`, `year`, `welfare`, `povline`",
    "`domain`, `year`, covariates (e.g., `gen`, `educ1`, `schyrs`)",
    "`domain`, `geometry` (from `sf` object)"
  )
)

# Build the gt table
mfh_table %>%
  gt() %>%
  tab_header(
    title = md("**Data Input Checklist for the Multivariate Fay-Herriot Model**"),
    subtitle = md("*Standardized datasets, observation levels, and required variables*")
  ) %>%
  cols_label(
    Dataset = "Dataset Name",
    `Unit of Observation` = "Unit of Observation",
    `Required Variables` = "Required Variables"
  ) %>%
  tab_options(
    table.font.names = "Arial",
    heading.title.font.size = px(20),
    heading.subtitle.font.size = px(14),
    table.font.size = px(14),
    column_labels.font.weight = "bold",
    data_row.padding = px(6),
    table.border.top.width = px(2),
    table.border.bottom.width = px(2),
    heading.align = "left"
  ) %>%
  fmt_markdown(columns = everything())



# ------------------------------------------------------------
# Step 1: Direct estimation of province poverty by year
# - PSU ID is preserved across time to facilitate later 
#   cross-year covariance/correlation calculations.
# ------------------------------------------------------------

# Ensure years_keep is standardized for consistent filtering
years_keep <- sort(as.integer(years_keep))
stopifnot(length(years_keep) == 2)

y1 <- years_keep[1]
y2 <- years_keep[2]

# Helper functions for dynamic column naming based on years_keep
col_poor <- function(y) paste0("poor_", y)
col_mse  <- function(y) paste0("mse_",  y)
col_se   <- function(y) paste0("se_",   y)
col_N    <- function(y) paste0("N_",    y)

# Define reusable column vectors to maintain consistency across the pipeline
poor_cols <- c(col_poor(y1), col_poor(y2))
mse_cols  <- c(col_mse(y1),  col_mse(y2))
se_cols   <- c(col_se(y1),   col_se(y2))
N_cols    <- c(col_N(y1),    col_N(y2))

# Filter and prepare survey data for estimation. The column `poor` is the
# LHS the model is fitted on; we keep that name for back-compat with all
# downstream chunks regardless of which indicator was selected.
survey_step1 <- survey_dt %>%
  filter(year %in% years_keep) %>%
  mutate(
    year = as.integer(year),
    poor = if (identical(indicator_type, "poverty")) {
      ifelse(is.na(welfare) | is.na(povline), NA_real_,
             compute_fgt(welfare, povline, fgt_alpha))
    } else if (isTRUE(log_transform)) {
      ifelse(!is.na(welfare) & welfare > 0, log(welfare), NA_real_)
    } else {
      as.numeric(welfare)
    },
    psu  = psu
  )

# ---- Define Survey Design ----
# Using the canonical PSU and weight names defined in Step 0
des <- svydesign(
  ids     = ~psu,
  weights = ~weight,
  data    = survey_step1
) 

# ---- Direct estimates: Domain x Year ----
dir_est_domain_long <- svyby(
  formula    = ~poor,
  by         = ~year + domain,
  design     = des,
  FUN        = svymean,
  na.rm      = TRUE,
  keep.var   = TRUE
) %>%
  mutate(mse = se^2)

# When fitting on the log scale we additionally compute the survey-
# weighted arithmetic mean of welfare per domain-year. This anchors the
# back-transform so the exported direct_rate equals svymean(welfare)
# exactly and FH/MFH EBLUPs are scaled by the same per-domain factor.
direct_arith_domain_long <- if (identical(indicator_type, "mean_welfare") &&
                                isTRUE(log_transform)) {
  svyby(
    formula  = ~welfare,
    by       = ~year + domain,
    design   = des,
    FUN      = svymean,
    na.rm    = TRUE,
    keep.var = TRUE
  ) %>%
    transmute(
      year         = as.integer(year),
      domain       = domain,
      direct_arith = welfare,
      SD_arith     = se
    )
} else NULL

# Pivot to wide format for the MFH modeling frame (one row per domain)
dir_est_domain_all_years <- dir_est_domain_long %>%
  select(domain, year, poor, mse, se) %>%
  pivot_wider(
    names_from  = year,
    values_from = c(poor, mse, se),
    names_glue  = "{.value}_{year}"
  ) 

# ---- Calculate Sample Sizes per Domain-Year ----
sampsize_dt <- survey_step1 %>%
  group_by(domain, year) %>%
  summarize(N = sum(!is.na(poor)), .groups = "drop") %>%
  pivot_wider(
    names_from  = year,
    values_from = N,
    names_glue  = "N_{year}"
  ) 

dir_est_domain_all_years <- dir_est_domain_all_years %>%
  left_join(sampsize_dt, by = "domain")

# ---- Store consolidated direct variances and sample sizes ----
all_var_hat_domain_dt <- dir_est_domain_all_years %>%
  transmute(
    domain      = as.integer(domain),
    v1          = .data[[mse_cols[1]]],
    v2          = .data[[mse_cols[2]]],
    !!N_cols[1] := .data[[N_cols[1]]],
    !!N_cols[2] := .data[[N_cols[2]]]
  ) %>%
  distinct()

# Cleanup intermediate objects
rm(dir_est_domain_long, sampsize_dt)




# -------------------------------------------------------------------
# Variance smoothing 
# Assumes already defined upstream:
#   - years_keep (sorted), y1, y2
#   - N_cols (length 2) like c("N_2012","N_2013")
#   - all_var_hat_domain_dt with N_cols already present
#   - v_domain_hh_df with v1, v2
# -------------------------------------------------------------------

stopifnot(length(years_keep) == 2, length(N_cols) == 2)

##### Opt 2: Var - Smoothing: All Smooth variances

col_map <- tibble(
  year = years_keep,
  vcol = c("v1", "v2"),
  ncol = N_cols
)

# Detect whether varsmoothie_king() supports a 'y' argument (script-dependent)
has_y_arg <- "y" %in% names(formals(varsmoothie_king))

var_smooth_long <- pmap_dfr(col_map, function(year, vcol, ncol) {

  if (has_y_arg) {
    y_vec <- survey_step1 %>% filter(year == !!year) %>% pull(poor)

    out <- varsmoothie_king(
      domain     = all_var_hat_domain_dt$domain,
      direct_var = all_var_hat_domain_dt[[vcol]],
      sampsize   = all_var_hat_domain_dt[[ncol]],
      y          = y_vec
    )
  } else {
    out <- varsmoothie_king(
      domain     = all_var_hat_domain_dt$domain,
      direct_var = all_var_hat_domain_dt[[vcol]],
      sampsize   = all_var_hat_domain_dt[[ncol]]
    )
  }

  out %>%
    transmute(
      domain     = as.integer(Domain),
      year       = as.integer(year),
      var_smooth = var_smooth
    )
})

# Wide: v1_sm_all, v2_sm_all (stable names regardless of actual years)
var_smooth_wide <- var_smooth_long %>%
  mutate(col = paste0("v", match(year, years_keep), "_sm_all")) %>%
  select(domain, col, var_smooth) %>%
  pivot_wider(names_from = col, values_from = var_smooth)

all_var_hat_domain_dt <- all_var_hat_domain_dt %>%
  left_join(var_smooth_wide, by = "domain")

rm(var_smooth_wide, var_smooth_long, has_y_arg, col_map)

###### Opt 3: Var - Outliers smooth

# sm_out replaces both lower- and upper-tail outlier variances with the
# regression-smoothed value, and keeps the raw direct variance everywhere
# else.
#
#  - Lower tail (absolute): replace if v is NA or v <= lower_thr.
#    Catches degenerate / near-zero direct variances from tiny samples,
#    which would otherwise force `eblupMFH*()` toward boundary fits.
#
#  - Upper tail (relative): replace if v > upper_mult * v_sm_all, where
#    v_sm_all is the regression-smoothed value (predicted from log(n)).
#    Catches direct variances that are wildly larger than the smoothing
#    regression predicts for that domain's sample size -- i.e., genuine
#    upper-tail outliers driven by extreme realised sampling noise rather
#    than by the underlying outcome variability the regression already
#    captures. Using a relative rule (multiplier of the smoothed value)
#    keeps the threshold sensible across indicators (poverty rate, mean
#    welfare, log-welfare, ...) without hard-coding an absolute upper
#    bound that would have to be re-tuned per outcome.
#
# Defaults: lower_thr = 0.001, upper_mult = 5. Both are tunable here.
lower_thr  <- 0.001
upper_mult <- 5

all_var_hat_domain_dt <- all_var_hat_domain_dt %>%
  mutate(
    # The is.na() guard mirrors run_fh_year() in UFH: without it, if_else
    # returns NA whenever v1/v2 is NA and downstream fits fail silently.
    v1_sm_out = case_when(
      is.na(v1) | v1 <= lower_thr                                  ~ v1_sm_all,
      is.finite(v1_sm_all) & v1_sm_all > 0 & v1 > upper_mult * v1_sm_all ~ v1_sm_all,
      TRUE                                                         ~ v1
    ),
    v2_sm_out = case_when(
      is.na(v2) | v2 <= lower_thr                                  ~ v2_sm_all,
      is.finite(v2_sm_all) & v2_sm_all > 0 & v2 > upper_mult * v2_sm_all ~ v2_sm_all,
      TRUE                                                         ~ v2
    ),
    # "direct" safety-backfilled variants: keep raw direct variances but
    # replace NA / exactly-zero values with smoothed ones so downstream MFH
    # fits don't error out. Matches UFH's "direct" safety backfill. (Direct
    # is intentionally NOT upper-tail-trimmed -- that's what sm_out is for.)
    v1_direct = if_else(is.na(v1) | v1 == 0, v1_sm_all, v1),
    v2_direct = if_else(is.na(v2) | v2 == 0, v2_sm_all, v2)
  )

rm(lower_thr, upper_mult)




##### Option 1: Cov - Domain direct 

stopifnot(length(years_keep) == 2, length(N_cols) == 2)

# Ensure factor ordering matches years_keep (do NOT redefine years_keep here)
survey_step1$year <- factor(as.integer(survey_step1$year), levels = years_keep)
domain_list <- sort(unique(survey_step1$domain))

# Make domain-level svyby robust (domains can fail if a year has no data)
safe_svyby <- purrr::possibly(
  function(des_p) {
    out <- svyby(
      ~as.numeric(poor),
      ~year,
      des_p,
      svymean,
      covmat = TRUE,
      na.rm  = TRUE
    )
    vcov(out)
  },
  otherwise = NA_real_
)

svyby_out <- lapply(domain_list, function(p) {
  des_p <- subset(des, domain == p)
  list(domain = p, V = safe_svyby(des_p))
})

# Safely extract covariance using dimnames (robust to ordering)
y1_chr <- as.character(y1)
y2_chr <- as.character(y2)

covariances <- map_dfr(svyby_out, function(x) {
  V <- x$V

  v12 <- NA_real_
  if (is.matrix(V)) {
    rn <- rownames(V)
    cn <- colnames(V)

    v12 <- if (!is.null(rn) && !is.null(cn) && y1_chr %in% rn && y2_chr %in% cn) {
      V[y1_chr, y2_chr]
    } else {
      V[1, 2]
    }
  }

  tibble(
    domain = as.integer(x$domain),
    v12_d  = as.numeric(v12)
  )
})

print(covariances)

all_var_hat_domain_dt <- all_var_hat_domain_dt %>%
  left_join(covariances, by = "domain")

###### Option 2: Using national correlation coefficient

fit_nat_nolink_psu <- svyby(
  ~poor,
  ~year,
  des,
  svymean,
  covmat  = TRUE,
  na.rm   = TRUE,
  vartype = NULL
)

V_nat_nolink <- attr(fit_nat_nolink_psu, "var")
R_nat_nolink <- cov2cor(V_nat_nolink)

print(V_nat_nolink)

# Extract rho_12 robustly
rn <- rownames(R_nat_nolink)
cn <- colnames(R_nat_nolink)

rho_12 <- if (!is.null(rn) && !is.null(cn) && y1_chr %in% rn && y2_chr %in% cn) {
  R_nat_nolink[y1_chr, y2_chr]
} else {
  R_nat_nolink[1, 2]
}

# Keep objects if you want diagnostics
nat_cov_no_link <- V_nat_nolink
nat_cor_no_link <- R_nat_nolink

# Opt 2_1: National correlation coefficient x direct variance estimate.
# Use v1_direct / v2_direct (the NA/zero-backfilled safety variants)
# rather than the raw v1 / v2. This is the covariance paired with
# `var_choice = "direct"` (whose diagonal uses v1_direct / v2_direct),
# so they must come from the same column to keep the sampling-error
# covariance matrix consistent: a raw NA on the diagonal that gets
# backfilled to v_sm_all would otherwise leave the off-diagonal at
# rho * sqrt(NA) * sqrt(.) = NA, and eblupMFH2() would abort with
# "Object vardir contains NA values". For non-NA, non-zero raw values
# v_direct == v exactly, so this is identical to the previous behaviour
# in the common case.
all_var_hat_domain_dt$v12_d_nc <-
  rho_12 * sqrt(all_var_hat_domain_dt$v1_direct) *
           sqrt(all_var_hat_domain_dt$v2_direct)

# Option 2_2: National correlation coefficient x variance (outlier-smoothed)
all_var_hat_domain_dt$v12_d_ncsm <-
  rho_12 * sqrt(all_var_hat_domain_dt$v1_sm_out) *
           sqrt(all_var_hat_domain_dt$v2_sm_out)

# Option 2_3: National correlation coefficient x fully smoothed variances
all_var_hat_domain_dt$v12_d_ncsm_all <-
  rho_12 * sqrt(all_var_hat_domain_dt$v1_sm_all) *
           sqrt(all_var_hat_domain_dt$v2_sm_all)

##### Option 3: Cov = 0

all_var_hat_domain_dt <- all_var_hat_domain_dt %>%
  select(domain, all_of(N_cols), everything())

all_var_hat_domain_dt$v12_zero <- 0

##### Cleanup: intermediate survey objects (recommended)
rm(
  svyby_out,          # per-domain svyby() results (already merged)
  covariances,        # merged already
  fit_nat_nolink_psu  # national svyby() object (used to extract V/R)
)




# ============================================================
# Step X: Build modeling dataset (domain_dt) + define candidates
#         + outcome/uncertainty table (dir_y_dt)
#         + stepwise AIC wrapper (stepAIC_wrapper)
#
# Assumes Pattern B objects already exist upstream:
#   years_keep (sorted), y1, y2
#   poor_cols, mse_cols, se_cols, N_cols
#   dir_est_domain_all_years, shp_dt, rhs_dt
# ============================================================

stopifnot(length(years_keep) == 2,
          length(poor_cols) == 2,
          length(mse_cols)  == 2,
          length(se_cols)   == 2,
          length(N_cols)    == 2)

# ---- Deduplicate RHS covariates (drop year, keep one row per domain) ----
# Covariates are area-level auxiliary variables that do not carry a year suffix.
# If the RHS data repeats identical rows for each year we simply deduplicate;
# if values genuinely differ across years we average them so that a single
# unsuffixed column is available for model selection.
rhs_cov_cols <- setdiff(names(rhs_dt), c("domain", "year"))
rhs_dt_unique <- rhs_dt %>%
  group_by(domain) %>%
  summarize(across(all_of(rhs_cov_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

# ---- Build the domain-level modeling frame ----
domain_dt <- dir_est_domain_all_years %>%
  left_join(shp_dt,        by = "domain") %>%
  left_join(rhs_dt_unique, by = "domain")

# Drop direct-estimate uncertainty columns for selected years from the modeling frame
domain_dt <- domain_dt %>%
  select(-all_of(c(mse_cols, se_cols, N_cols)))

# ---- Candidate regressors (exclude id + outcomes + geometry) ----
candidate_vars_all <- setdiff(
  colnames(domain_dt),
  c("domain", poor_cols, "geometry")
)

# Per-year candidate vars: if user specified per-year lists, filter accordingly;
# otherwise fall back to the full set of available candidates.
candidate_vars_per_year <- setNames(
  lapply(seq_along(poor_cols), function(i) {
    yr_vars <- if (i == 1) mfh_candidate_vars_y1 else mfh_candidate_vars_y2
    if (!is.null(yr_vars) && length(yr_vars) > 0) {
      intersect(candidate_vars_all, yr_vars)
    } else {
      candidate_vars_all
    }
  }),
  poor_cols
)

# Legacy: single candidate_vars used for display / downstream (union of per-year)
candidate_vars <- candidate_vars_all

# ---- Outcome + sampling-uncertainty table (selected years only) ----
dir_y_dt <- dir_est_domain_all_years %>%
  select(domain, all_of(c(poor_cols, se_cols, mse_cols, N_cols))) %>%
  distinct()

# ============================================================
# Stepwise variable selection wrapper (AIC)
#   - drops all-NA columns
#   - complete-case restriction
#   - drops aliased vars + near-linear combos + highly correlated vars
#   - then stepAIC in both directions
# ============================================================

stepAIC_wrapper <- function(dt, xvars, y, cor_thresh = 0.95, ic_criterion = "AIC") {

  dt <- data.table::as.data.table(dt)

  # Drop columns that are entirely NA
  dt <- dt[, which(unlist(lapply(dt, function(x) !all(is.na(x))))), with = FALSE]

  # Keep only requested xvars that still exist
  xvars <- xvars[xvars %in% colnames(dt)]

  # Keep only complete cases on y + xvars
  dt <- na.omit(dt[, c(y, xvars), with = FALSE])

  if (length(xvars) == 0) {
    stop("No candidate xvars remain after NA filtering / complete-case restriction.")
  }

  # Step 1: Remove aliased (perfectly collinear) variables
  model_formula <- as.formula(paste(y, "~", paste(xvars, collapse = " + ")))
  lm_model <- lm(model_formula, data = dt)
  aliased <- is.na(coef(lm_model))
  if (any(aliased)) {
    xvars <- names(aliased)[!aliased & names(aliased) != "(Intercept)"]
  }

  if (length(xvars) == 0) {
    stop("All xvars dropped due to perfect collinearity (aliased coefficients).")
  }

  # Step 2: Remove near-linear combinations
  xmat <- as.matrix(dt[, ..xvars])
  combo_check <- tryCatch(caret::findLinearCombos(xmat), error = function(e) NULL)
  if (!is.null(combo_check) && length(combo_check$remove) > 0) {
    xvars <- xvars[-combo_check$remove]
  }

  if (length(xvars) == 0) {
    stop("All xvars dropped by findLinearCombos().")
  }

  xmat <- as.matrix(dt[, ..xvars])

  # Step 3: Drop highly correlated variables
  cor_mat <- abs(cor(xmat))
  diag(cor_mat) <- 0

  while (any(cor_mat > cor_thresh, na.rm = TRUE) && length(xvars) > 1) {
    cor_pairs <- which(cor_mat == max(cor_mat, na.rm = TRUE), arr.ind = TRUE)[1, ]
    var1 <- colnames(cor_mat)[cor_pairs[1]]
    var2 <- colnames(cor_mat)[cor_pairs[2]]

    drop_var <- if (mean(cor_mat[var1, ], na.rm = TRUE) > mean(cor_mat[var2, ], na.rm = TRUE)) {
      var1
    } else {
      var2
    }

    xvars <- setdiff(xvars, drop_var)

    xmat <- as.matrix(dt[, ..xvars])
    cor_mat <- abs(cor(xmat))
    diag(cor_mat) <- 0
  }

  # Step 4: Warn if still ill-conditioned
  if (ncol(xmat) >= 2) {
    cond_number <- kappa(xmat, exact = TRUE)
    if (cond_number > 1e10) {
      warning("Design matrix is ill-conditioned (condition number > 1e10). Consider reviewing variable selection.")
    }
  }

  # Final model fit
  model_formula <- as.formula(paste(y, "~", paste(xvars, collapse = " + ")))
  full_model <- lm(model_formula, data = dt)

  # Stepwise selection (k = 2 for AIC, k = log(n) for BIC)
  k_val <- if (identical(ic_criterion, "BIC")) log(nrow(dt)) else 2
  MASS::stepAIC(full_model, direction = "both", trace = 0, k = k_val)
}




### the set of outcome variables (Pattern B)
outcome_list <- poor_cols

# Check whether the user forced specific covariates for each year
user_forced_y1 <- !is.null(mfh_candidate_vars_y1) && length(mfh_candidate_vars_y1) > 0
user_forced_y2 <- !is.null(mfh_candidate_vars_y2) && length(mfh_candidate_vars_y2) > 0

### applying variable selection to each outcome variable (per-year candidate vars)
stepaicmodel_list <- setNames(
  lapply(seq_along(outcome_list), function(i) {
    yname <- outcome_list[i]
    user_forced <- if (i == 1) user_forced_y1 else user_forced_y2

    # Use per-year candidate vars, excluding outcome columns
    yr_candidates <- setdiff(candidate_vars_per_year[[yname]], outcome_list)

    if (user_forced) {
      # User specified covariates: use them directly (skip stepwise)
      cat(sprintf("Using user-specified covariates for %s: %s\n",
                  yname, paste(yr_candidates, collapse = ", ")))
      yr_candidates
    } else {
      # No user specification: run stepwise selection
      fit <- stepAIC_wrapper(
        dt           = domain_dt,
        xvars        = yr_candidates,
        y            = yname,
        cor_thresh   = 0.8,
        ic_criterion = mfh_ic_criterion
      )

      # keep only selected regressors (drop intercept)
      sel <- names(coef(fit))
      sel <- setdiff(sel, "(Intercept)")
      sel
    }
  }),
  outcome_list
)

### now create a list of equations (formulas)
mfh_formula <- mapply(
  FUN = function(outcome, rhs_vars) {
    if (length(rhs_vars) == 0) {
      as.formula(paste0(outcome, " ~ 1"))
    } else {
      as.formula(paste0(outcome, " ~ ", paste(rhs_vars, collapse = " + ")))
    }
  },
  outcome_list,
  stepaicmodel_list,
  SIMPLIFY = FALSE
)

mfh_formula




# ============================================================
# Model checking: OLS fits implied by MFH formulas
# ============================================================

# Baseline: fit OLS models using the selected MFH formulas
lmcheck_obj <- lapply(
  X   = mfh_formula,
  FUN = lm,
  data = domain_dt
)

# Inspect baseline results
lm_summaries <- lapply(lmcheck_obj, summary)
lm_summaries









# ============================================================
# Objective: Attach sampling variance/covariance inputs to domain_dt
#            and select the var-cov components to feed into MFH
# ============================================================

# 1) Attach sampling uncertainty columns (v1/v2/etc.) to the modeling frame
domain_dt <- domain_dt %>%
  left_join(all_var_hat_domain_dt, by = "domain")

# 2) Define the available variance and covariance "menus"
#    (These names are stable across years because v1/v2 refer to y1/y2 ordering.)

var_menu <- list(
  direct   = c("v1_direct", "v2_direct"),# domain direct variances, NA/zero backfilled
  sm_out   = c("v1_sm_out", "v2_sm_out"),# outlier-smoothed variances
  sm_all   = c("v1_sm_all", "v2_sm_all") # all variances replaced with smoothed
)

cov_menu <- list(
  direct     = "v12_d",           # domain direct covariance (if available)
  rho_dir    = "v12_d_nc",        # national rho * sqrt(v1*v2)
  rho_sm_out     = "v12_d_ncsm",      # national rho * sqrt(v1_sm_out*v2_sm_out)
  rho_sm_all = "v12_d_ncsm_all",  # national rho * sqrt(v1_sm_all*v2_sm_all)
  zero       = "v12_zero"         # covariance set to zero
)

# 3) Choose which variance and covariance definitions to use
var_choice <- cfg_or_default(mfh_cfg$var_choice, "sm_out")
cov_choice <- cfg_or_default(mfh_cfg$cov_choice, "rho_sm_out")

# 4) Build the chosen column-name vector (to pass downstream)
vardir_cols <- c(var_menu[[var_choice]], cov_menu[[cov_choice]])

# 5) Defensive checks: ensure the chosen columns exist
missing_cols <- setdiff(vardir_cols, names(domain_dt))
if (length(missing_cols) > 0) {
  stop(
    "Chosen variance/covariance columns not found in domain_dt: ",
    paste(missing_cols, collapse = ", "),
    "\nCheck that earlier variance/covariance steps were executed and the choices are valid."
  )
}

vardir_cols



# ============================================================
# Objective: Estimate UFH/MFH models using the chosen sampling
#            variance--covariance inputs, then compare model-based
#            poverty estimates (EBLUP, MSE, CV) against direct
#            estimates in a tidy (wide) table for export.
# ============================================================

# ---- 0) Fit models (UFH + MFH variants) using the chosen vardir columns ----
# Assumes already defined upstream:
#   - mfh_formula (list of formulas, one per outcome)
#   - vardir_cols (chosen variance/covariance columns, e.g., c("v1_sm_out","v2_sm_out","v12_d_ncsm"))
#   - domain_dt (modeling frame, already joined with all_var_hat_domain_dt)

# Upfront input check: eblupMFH*() requires every entry of vardir to be a
# finite number. When var_choice = "direct", some domains may have missing
# variances (e.g., single-PSU domains). We drop those domains from the
# fitting data and fit the model on the valid subset; excluded domains
# get NA results in the output tables.
.vardir_mat <- as.matrix(domain_dt[, vardir_cols, drop = FALSE])
.vardir_bad <- !is.finite(.vardir_mat)
.bad_row_mask <- rowSums(.vardir_bad) > 0
.n_bad_rows <- sum(.bad_row_mask)
.excluded_domains <- if (.n_bad_rows > 0) domain_dt$domain[.bad_row_mask] else integer(0)
.all_domains_dt <- domain_dt

if (.n_bad_rows > 0) {
  cat(
    "\n**Note:** ", .n_bad_rows, " of ", nrow(domain_dt),
    " domain(s) have missing/non-finite vardir values (",
    paste0("`", vardir_cols, "`", collapse = ", "),
    "). These domains are excluded from MFH fitting and will have NA results.\n\n",
    sep = ""
  )
  domain_dt <- domain_dt[!.bad_row_mask, , drop = FALSE]
}

.fit_skipped <- FALSE

model_ufh  <- eblupUFH(mfh_formula, vardir = vardir_cols, data = domain_dt, MAXITER = 100, PRECISION = 1e-04)
model_mfh1 <- eblupMFH1(mfh_formula, vardir = vardir_cols, data = domain_dt, MAXITER = 100, PRECISION = 1e-04)
model_mfh2 <- eblupMFH2(mfh_formula, vardir = vardir_cols, data = domain_dt, MAXITER = 100, PRECISION = 1e-04)
if (fit_mfh3) {
  model_mfh3 <- tryCatch(
    eblupMFH3(mfh_formula, vardir = vardir_cols, data = domain_dt, MAXITER = 100, PRECISION = 1e-04),
    error = function(e) {
      cat(
        "\n**Note:** MFH3 (heteroskedastic) model did not converge.\n",
        "Error: ", conditionMessage(e), "\n",
        "MFH3 estimation is skipped. ",
        "Downstream results will use MFH1/MFH2 (and UFH for diagnostics) only.\n\n",
        sep = ""
      )
      NULL
    }
  )
  if (is.null(model_mfh3)) fit_mfh3 <- FALSE
}
rm(.vardir_mat, .vardir_bad, .bad_row_mask)

# ---- Model lookup for diagnostics and downstream analysis ----
# Only multivariate variants are valid choices for the diagnostic model.
# UFH is still fitted (it appears in diagnostics/comparison tables), but
# it cannot be selected as the driver of the Comparison report -- its
# analysis lives in 40-fh_v2.qmd.
model_lookup <- list(MFH1 = model_mfh1, MFH2 = model_mfh2)
if (fit_mfh3) model_lookup[["MFH3"]] <- model_mfh3

if (!diag_model %in% names(model_lookup)) {
  fallback <- "MFH2"
  cat(
    "\n**Note:** diag_model = \"", diag_model, "\" is not available ",
    "(valid options are ",
    paste(shQuote(names(model_lookup)), collapse = ", "),
    "; MFH3 requires fit_mfh3 = TRUE and successful convergence).\n",
    "Falling back to \"", fallback, "\" for post-estimation diagnostics.\n\n",
    sep = ""
  )
  diag_model <- fallback
}
selected_model <- model_lookup[[diag_model]]

# Detect a wholesale fit failure (selected_model is NULL or its fit object
# is empty). This happens when something further upstream made eblupMFH*()
# error out before producing any object -- most often a NA in vardir that
# the safety backfills missed. Without this guard, the downstream NA-fill
# fallbacks render a successful-looking HTML even though the model itself
# never ran. Surface it loudly so it can't be missed.
.model_fit_failed <- is.null(selected_model) ||
  is.null(selected_model$fit) ||
  is.null(selected_model$fit$refvar) ||
  length(selected_model$fit$refvar) == 0

# Check for boundary random-effects variance (refvar = 0). After the robust
# refit fallback in patch-msae, this should fire only when both msae's
# Newton-Raphson AND the constrained optim() refit conclude sigma2_u = 0 --
# i.e., a genuine boundary, not a numerical artifact.
.refvar_zero <- if (.model_fit_failed) FALSE else
  isTRUE(all(selected_model$fit$refvar == 0))

# Diagnostic: did the robust optim() fallback get used for any of the
# candidate models? Surfaced as a small note so the user can see when
# msae's standard implementation was overridden.
.robust_used <- vapply(
  list(MFH1 = model_mfh1, MFH2 = model_mfh2,
       MFH3 = if (exists("model_mfh3")) model_mfh3 else NULL),
  function(m) isTRUE(attr(m, ".robust_refit_used")),
  logical(1)
)
.robust_failed <- vapply(
  list(MFH1 = model_mfh1, MFH2 = model_mfh2,
       MFH3 = if (exists("model_mfh3")) model_mfh3 else NULL),
  function(m) isTRUE(attr(m, ".robust_refit_failed")),
  logical(1)
)

# Save artifacts needed by Comparison_v2.qmd
dir.create(here::here("outputs", "data"), showWarnings = FALSE, recursive = TRUE)
saveRDS(
  list(
    years_keep       = years_keep,
    selected_model   = selected_model,
    formula          = mfh_formula,
    diag_model       = diag_model,
    refvar_zero      = .refvar_zero,
    model_fit_failed = .model_fit_failed,
    fit_skipped      = .fit_skipped,
    var_choice       = var_choice,
    cov_choice       = cov_choice,
    vardir_cols       = vardir_cols,
    bad_vardir_rows   = .n_bad_rows,
    excluded_domains  = .excluded_domains,
    n_domains         = nrow(.all_domains_dt),
    n_domains_fitted  = nrow(domain_dt)
  ),
  file = here::here("outputs", "data", "mfh_artifacts.rds")
)

# If the MFH fit failed outright, write the minimal artifacts that the
# Comparison step needs, then raise a typed condition.
if (isTRUE(.model_fit_failed)) {
  direct_long_rate <- dir_est_domain_all_years %>%
    select(domain, all_of(poor_cols)) %>%
    pivot_longer(-domain, names_to = "year_raw", values_to = "direct_rate") %>%
    mutate(year = as.integer(gsub("^.*?(\\d{4})$", "\\1", year_raw))) %>%
    select(domain, year, direct_rate)

  direct_long_mse <- dir_est_domain_all_years %>%
    select(domain, all_of(mse_cols)) %>%
    pivot_longer(-domain, names_to = "year_raw", values_to = "direct_mse") %>%
    mutate(year = as.integer(gsub("^.*?(\\d{4})$", "\\1", year_raw))) %>%
    select(domain, year, direct_mse)

  db_wide_all <- left_join(direct_long_rate, direct_long_mse, by = c("domain", "year")) %>%
    mutate(
      direct_cv   = if_else(direct_rate > 0, sqrt(direct_mse) / direct_rate, NA_real_),
      direct_rmse = sqrt(direct_mse)
    ) %>%
    arrange(year, domain)

  for (.model_name in c("UFH", "MFH1", "MFH2", "MFH3")) {
    db_wide_all[[paste0("rate_", .model_name)]] <- NA_real_
    db_wide_all[[paste0("mse_", .model_name)]]  <- NA_real_
    db_wide_all[[paste0("cv_", .model_name)]]   <- NA_real_
    db_wide_all[[paste0("rmse_", .model_name)]] <- NA_real_
  }

  dir.create(here::here("outputs", "data"), showWarnings = FALSE, recursive = TRUE)
  dir.create(here::here("outputs", "tables"), showWarnings = FALSE, recursive = TRUE)
  .write_xlsx_safe(db_wide_all, here::here("outputs", "data", "pov_mfh.xlsx"))

  change_placeholder <- data.frame(
    domain      = sort(unique(as.integer(db_wide_all$domain))),
    diff        = NA_real_,
    mse         = NA_real_,
    lb          = NA_real_,
    ub          = NA_real_,
    significant = NA
  )
  write.csv(change_placeholder,
            here::here("outputs", "tables", "comparison_final.csv"),
            row.names = FALSE)
  write.csv(change_placeholder,
            here::here("outputs", "tables", "comparison_final_bench.csv"),
            row.names = FALSE)
  write.csv(
    data.frame(
      method = character(),
      year = integer(),
      component = character(),
      n = integer(),
      W = numeric(),
      p_value = numeric()
    ),
    here::here("outputs", "tables", "mfh_shapiro_results.csv"),
    row.names = FALSE
  )

  .reason <- sprintf(
    "MFH unavailable: selected model '%s' did not return a valid fit.",
    diag_model
  )
  stop(structure(
    list(message = .reason, call = NULL),
    class = c("sae_mfh_unavailable", "error", "condition")
  ))
}

# Dynamic column names for the selected model
rate_col     <- paste0("rate_", diag_model)
mse_col_diag <- paste0("mse_", diag_model)

domain_vec <- domain_dt$domain

# ---- 1) Helper: convert model outputs (EBLUP + MSE) to long ----
make_long_model <- function(model_obj, model_name, domain_vec) {

  eb  <- as.data.frame(model_obj$eblup)
  mse <- as.data.frame(model_obj$MSE)

  eb_long <- eb %>%
    mutate(domain = domain_vec) %>%
    pivot_longer(-domain, names_to = "year_raw", values_to = "rate")

  mse_long <- mse %>%
    mutate(domain = domain_vec) %>%
    pivot_longer(-domain, names_to = "year_raw", values_to = "mse")

  out <- left_join(eb_long, mse_long, by = c("domain", "year_raw")) %>%
    mutate(
      year  = as.integer(gsub("^.*?(\\d{4})$", "\\1", year_raw)),
      cv    = if_else(rate > 0, sqrt(mse) / rate, NA_real_),
      model = model_name
    ) %>%
    select(domain, year, model, rate, mse, cv)

  out
}

# ---- 2) Stack model results and reshape wide ----
db_long_list <- list(
  make_long_model(model_ufh,  "UFH",  domain_vec),
  make_long_model(model_mfh1, "MFH1", domain_vec),
  make_long_model(model_mfh2, "MFH2", domain_vec)
)
if (fit_mfh3) db_long_list[["MFH3"]] <- make_long_model(model_mfh3, "MFH3", domain_vec)
db_long <- bind_rows(db_long_list)

db_wide <- db_long %>%
  pivot_wider(
    names_from  = model,
    values_from = c(rate, mse, cv),
    names_sep   = "_"
  ) %>%
  arrange(year, domain)

# ---- 3) Direct estimates for the selected years (Pattern B: no hard-codes) ----
# Assumes upstream:
#   - dir_est_domain_all_years has columns poor_cols (e.g., poor_YYYY) and mse_cols (mse_YYYY)
#   - poor_cols, mse_cols defined from years_keep

direct_long_rate <- dir_est_domain_all_years %>%
  select(domain, all_of(poor_cols)) %>%
  pivot_longer(-domain, names_to = "year_raw", values_to = "direct_rate") %>%
  mutate(year = as.integer(gsub("^.*?(\\d{4})$", "\\1", year_raw)))

direct_long_mse <- dir_est_domain_all_years %>%
  select(domain, all_of(mse_cols)) %>%
  pivot_longer(-domain, names_to = "year_raw", values_to = "direct_mse") %>%
  mutate(year = as.integer(gsub("^.*?(\\d{4})$", "\\1", year_raw)))

direct_long <- left_join(direct_long_rate, direct_long_mse, by = c("domain", "year")) %>%
  mutate(direct_cv = if_else(direct_rate > 0, sqrt(direct_mse) / direct_rate, NA_real_)) %>%
  select(domain, year, direct_rate, direct_mse, direct_cv)

# ---- 4) Combine model-based + direct into one comparison table ----
# Use full_join so domains excluded from MFH fitting (missing vardir)
# still appear with direct estimates and NA for model columns.
db_wide_all <- direct_long %>%
  full_join(db_wide, by = c("domain", "year")) %>%
  relocate(direct_rate, direct_mse, direct_cv, .after = year)

# ---- Add RMSE columns (sqrt of each MSE) ----
db_wide_all <- db_wide_all %>%
  mutate(
    direct_rmse = sqrt(direct_mse),
    rmse_UFH    = sqrt(mse_UFH),
    rmse_MFH1   = sqrt(mse_MFH1),
    rmse_MFH2   = sqrt(mse_MFH2)
  )
if (fit_mfh3) {
  db_wide_all <- db_wide_all %>% mutate(rmse_MFH3 = sqrt(mse_MFH3))
}

db_wide_all




if (.model_fit_failed) {
  cat(
    "\n--- IMPORTANT ---\n",
    "## MFH model fit failed -- no random effects were estimated\n\n",
    "The selected diagnostic MFH variant (**", diag_model, "**) returned an empty ",
    "model object. `eblupMFH*()` errored out before producing any fit, so:\n\n",
    "- Random effects, EBLUPs, MSEs, and CVs are **not available** for this run.\n",
    "- The robust REML fallback could not run either (it needs a valid input matrix).\n",
    "- Downstream tables and figures based on this MFH variant will show empty / NA values.\n\n",
    "Inspect the rendered errors above for the specific failure. Common causes ",
    "include: the chosen vardir produces a per-domain R that is not positive-definite, ",
    "or the design matrix has rank deficiency. Rerun with a different ",
    "`var_choice` / `cov_choice` combination (the smoothed `rho_*` options ",
    "are the most robust) or revisit the covariate specification.\n",
    "\n",
    sep = ""
  )
}


# Surface a small note whenever the robust optim()-based REML fallback
# was used (i.e., msae's eblupMFH2 returned refvar = 0 but the constrained
# optimizer recovered a positive sigma2_u). This tells the user which
# model's diagnostics are based on the standard msae fit vs. the robust
# fallback so they can interpret the report accordingly.
.robust_used_models <- names(.robust_used)[.robust_used]
if (length(.robust_used_models) > 0) {
  cat(
    "\n--- NOTE ---\n",
    "## Robust REML fallback was used\n\n",
    "For the following model(s), `msae::eblupMFH2()` returned $\\sigma^2_u = 0$ ",
    "(a known numerical-artifact pattern of its unconstrained Newton-Raphson ",
    "REML iteration). The robust fallback then refit the variance components ",
    "via `optim()` over $\\tau = \\log\\sigma^2_u$ and $\\eta = \\mathrm{atanh}(\\rho_u)$, ",
    "which enforces $\\sigma^2_u > 0$ and $|\\rho_u| < 1$ by construction, ",
    "and recovered a positive $\\sigma^2_u$:\n\n",
    paste0("- **", .robust_used_models, "**\n", collapse = ""),
    "\nAll downstream EBLUPs, MSEs, random-effects diagnostics, and bootstrap ",
    "MCPE values for these models are based on the robust refit.\n",
    "\n",
    sep = ""
  )
}
.robust_failed_models <- names(.robust_failed)[.robust_failed]
if (length(.robust_failed_models) > 0) {
  cat(
    "\n--- WARNING ---\n",
    "## Robust REML fallback also failed\n\n",
    "For the following model(s), `msae::eblupMFH2()` returned $\\sigma^2_u = 0$ ",
    "AND the robust `optim()`-based refit was unable to find a positive ",
    "variance component either:\n\n",
    paste0("- **", .robust_failed_models, "**\n", collapse = ""),
    "\nWith two independent optimizers agreeing on $\\sigma^2_u = 0$, the boundary ",
    "is more likely to reflect a genuine REML solution than a numerical artifact, ",
    "though near-rank-deficient sampling-error covariance matrices can still drive ",
    "this. Treat the model output for these variants as a regression-only predictor ",
    "with no domain-specific shrinkage.\n",
    "\n",
    sep = ""
  )
}


if (.refvar_zero) {
  cat(
    "\n--- WARNING ---\n",
    "## Selected diagnostic model: random-effects variance is zero\n\n",
    "The REML estimator for the random-effects variance ($\\sigma^2_u$) converged to **zero** ",
    "for the selected diagnostic model (**", diag_model, "**), even after the robust ",
    "`optim()`-based refit. All MFH random effects are therefore identically zero, and the ",
    "MFH EBLUP equals the regression-synthetic predictor with no domain-specific shrinkage.\n\n",
    "Because two independent optimizers (msae's Newton-Raphson and a constrained `optim()`) ",
    "both agreed on $\\sigma^2_u = 0$, this is more likely to reflect a true REML boundary ",
    "for this combination of inputs than an optimizer artifact -- though near-rank-deficient ",
    "sampling-error covariance matrices (e.g., when the off-diagonal $\\rho$ used to build $R$ ",
    "is close to $\\pm 1$) can still drive both optimizers there.\n\n",
    "**Implications:**\n\n",
    "- Random-effects diagnostics (Q-Q plots, Shapiro--Wilk tests) are not meaningful here.\n",
    "- The point estimates collapse to a regression-only predictor; reported MSEs reflect ",
    "regression-synthetic uncertainty, not the full FH shrinkage uncertainty.\n",
    "- **Recommended sensitivity check:** rerun with a different covariance specification ",
    "(`cov_choice = \"rho_sm_out\"`, `\"rho_dir\"`, `\"direct\"`, or `\"zero\"`) or a different ",
    "variance specification (`var_choice = \"sm_out\"`, `\"sm_all\"`, or `\"direct\"`). If any ",
    "alternative gives $\\sigma^2_u > 0$, the present result should be treated as ",
    "configuration-specific rather than as evidence of zero between-domain variation.\n",
    "\n",
    sep = ""
  )
}



# ============================================================
# Regional Benchmarking for the Selected MFH Model
#
# This chunk applies ratio-type regional benchmarking to the
# selected MFH model's EBLUP estimates. When MSE bootstrap is
# enabled, it also computes benchmarked MSE and MCPE that
# respect the temporal covariance structure.
# ============================================================

.read_optional_benchmark_table <- function(path) {
  if (is.null(path) || length(path) == 0 || !nzchar(path)) return(NULL)
  if (!file.exists(path)) stop("Optional benchmarking input not found: ", path)
  ext <- tolower(tools::file_ext(path))
  if (ext == "rds") {
    obj <- readRDS(path)
  } else if (ext %in% c("rda", "rdata")) {
    load_env <- new.env(parent = emptyenv())
    loaded <- load(path, envir = load_env)
    if (length(loaded) != 1) {
      stop("Optional benchmarking .RData/.rda file must contain exactly one object.")
    }
    obj <- load_env[[loaded[1]]]
  } else if (ext %in% c("csv", "txt")) {
    obj <- read.csv(path, check.names = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required to read optional Excel benchmarking inputs.")
    }
    obj <- readxl::read_excel(path)
  } else {
    stop("Unsupported optional benchmarking input format: ", path)
  }
  obj
}

.pick_col <- function(nms, candidates) {
  lower_nms <- tolower(nms)
  hit <- match(tolower(candidates), lower_nms)
  hit <- hit[!is.na(hit)]
  if (length(hit) == 0) NULL else nms[hit[1]]
}

.matrix_from_optional_population <- function(path, domain_vec, years_keep) {
  obj <- .read_optional_benchmark_table(path)
  if (is.null(obj)) return(NULL)

  nD <- length(domain_vec)
  nT <- length(years_keep)
  domain_chr <- as.character(domain_vec)
  year_chr <- as.character(years_keep)

  if (is.matrix(obj)) {
    mat <- obj
    if (is.null(rownames(mat))) stop("Population matrix must have domain row names.")
    missing_domains <- setdiff(domain_chr, rownames(mat))
    if (length(missing_domains) > 0) {
      stop("Population matrix is missing domain(s): ",
           paste(missing_domains, collapse = ", "))
    }
    mat <- mat[domain_chr, , drop = FALSE]
    if (ncol(mat) != nT) stop("Population matrix must have one column per analysis year.")
    storage.mode(mat) <- "double"
    colnames(mat) <- year_chr
    return(mat)
  }

  df <- as.data.frame(obj, check.names = FALSE)
  nms <- names(df)
  domain_col <- .pick_col(nms, c("domain", "prov", "area", "area_id"))
  year_col <- .pick_col(nms, c("year", "time", "period"))
  value_col <- .pick_col(nms, c("Nd", "N_d", "population", "pop", "N"))

  if (!is.null(domain_col) && !is.null(year_col) && !is.null(value_col)) {
    mat <- matrix(NA_real_, nrow = nD, ncol = nT,
                  dimnames = list(domain_chr, year_chr))
    for (tt in seq_along(years_keep)) {
      idx <- as.character(df[[year_col]]) == year_chr[tt]
      vals <- as.numeric(df[[value_col]][idx])
      ids <- as.character(df[[domain_col]][idx])
      mat[intersect(domain_chr, ids), tt] <- vals[match(intersect(domain_chr, ids), ids)]
    }
    return(mat)
  }

  if (!is.null(domain_col)) {
    mat <- matrix(NA_real_, nrow = nD, ncol = nT,
                  dimnames = list(domain_chr, year_chr))
    for (tt in seq_along(years_keep)) {
      candidates <- c(
        year_chr[tt],
        paste0("Nd_", year_chr[tt]),
        paste0("N_", year_chr[tt]),
        paste0("population_", year_chr[tt]),
        paste0("pop_", year_chr[tt])
      )
      col <- .pick_col(nms, candidates)
      if (is.null(col)) stop("Population file is missing a column for year ", year_chr[tt], ".")
      ids <- as.character(df[[domain_col]])
      mat[domain_chr, tt] <- as.numeric(df[[col]][match(domain_chr, ids)])
    }
    return(mat)
  }

  stop("Population input must be a matrix with domain row names, a long table ",
       "(domain/year/population), or a wide table with one row per domain.")
}

.matrix_from_optional_regional_benchmark <- function(path, region_vec, years_keep) {
  obj <- .read_optional_benchmark_table(path)
  if (is.null(obj)) return(NULL)

  region_ids <- sort(unique(as.character(region_vec[!is.na(region_vec)])))
  year_chr <- as.character(years_keep)
  nT <- length(years_keep)

  if (is.matrix(obj)) {
    mat <- obj
    if (is.null(rownames(mat))) stop("Regional benchmark matrix must have region row names.")
    missing_regions <- setdiff(region_ids, rownames(mat))
    if (length(missing_regions) > 0) {
      stop("Regional benchmark matrix is missing region(s): ",
           paste(missing_regions, collapse = ", "))
    }
    mat <- mat[region_ids, , drop = FALSE]
    if (ncol(mat) != nT) stop("Regional benchmark matrix must have one column per analysis year.")
    storage.mode(mat) <- "double"
    colnames(mat) <- year_chr
    if (any(!is.finite(mat))) {
      stop("Regional benchmark matrix contains missing/non-finite targets.")
    }
    return(mat)
  }

  df <- as.data.frame(obj, check.names = FALSE)
  nms <- names(df)
  region_col <- .pick_col(nms, c("region", "reg", "region_id"))
  year_col <- .pick_col(nms, c("year", "time", "period"))
  value_col <- .pick_col(nms, c("benchmark", "B_r", "regional_benchmark",
                                "direct", "direct_rate", "poverty_rate"))
  if (is.null(region_col)) {
    stop("Regional benchmark input must contain a region column or matrix row names.")
  }

  mat <- matrix(NA_real_, nrow = length(region_ids), ncol = nT,
                dimnames = list(region_ids, year_chr))

  if (!is.null(year_col) && !is.null(value_col)) {
    for (tt in seq_along(years_keep)) {
      idx <- as.character(df[[year_col]]) == year_chr[tt]
      ids <- as.character(df[[region_col]][idx])
      vals <- as.numeric(df[[value_col]][idx])
      mat[intersect(region_ids, ids), tt] <- vals[match(intersect(region_ids, ids), ids)]
    }
    if (any(!is.finite(mat))) {
      stop("Regional benchmark table is missing benchmark targets for at least one region/year.")
    }
    return(mat)
  }

  for (tt in seq_along(years_keep)) {
    candidates <- c(
      year_chr[tt],
      paste0("B_r_", year_chr[tt]),
      paste0("benchmark_", year_chr[tt]),
      paste0("direct_", year_chr[tt]),
      paste0("poor_", year_chr[tt])
    )
    col <- .pick_col(nms, candidates)
    if (is.null(col)) stop("Regional benchmark file is missing a column for year ", year_chr[tt], ".")
    ids <- as.character(df[[region_col]])
    mat[region_ids, tt] <- as.numeric(df[[col]][match(region_ids, ids)])
  }
  if (any(!is.finite(mat))) {
    stop("Regional benchmark table is missing benchmark targets for at least one region/year.")
  }
  mat
}

bench_result <- NULL

# Skip benchmarking when MFH model was not properly executed
if (.refvar_zero || .model_fit_failed) {
  cat("Regional benchmarking skipped -- eblupMFH2 did not produce valid random effects.\n")
  do_benchmark <- FALSE
}

if (do_benchmark && !is.null(region_map)) {

  # Prepare inputs for benchmarking
  bench_domains <- domain_dt$domain

  # Region and population size vectors aligned to domain_dt
  bench_region <- region_map$region[match(bench_domains, region_map$domain)]
  bench_Nd     <- sizeprov$Nd[match(bench_domains, sizeprov$domain)]
  bench_Nd_mat <- matrix(
    bench_Nd,
    nrow = length(bench_domains),
    ncol = length(years_keep),
    dimnames = list(as.character(bench_domains), as.character(years_keep))
  )

  population_path <- cfg_or_default(mfh_cfg$population_path, "")
  if (nzchar(population_path)) {
    bench_Nd_mat <- .matrix_from_optional_population(
      population_path,
      domain_vec = bench_domains,
      years_keep = years_keep
    )
    bench_Nd <- bench_Nd_mat[, 1]
    cat("Using external domain population sizes for benchmarking:", population_path, "\n")
  }

  regional_benchmark_path <- cfg_or_default(mfh_cfg$regional_benchmark_path, "")
  regional_benchmark_mat <- .matrix_from_optional_regional_benchmark(
    regional_benchmark_path,
    region_vec = bench_region,
    years_keep = years_keep
  )
  if (!is.null(regional_benchmark_mat)) {
    cat("Using external regional benchmarks for benchmarking:", regional_benchmark_path, "\n")
  }

  # Direct estimates matrix (D x nT)
  direct_bench_mat <- as.matrix(domain_dt[, poor_cols, drop = FALSE])
  rownames(direct_bench_mat) <- as.character(bench_domains)
  colnames(direct_bench_mat) <- as.character(years_keep)

  # EBLUP matrix from selected model
  eblup_sel <- as.matrix(selected_model$eblup)
  if (nrow(eblup_sel) != length(bench_domains) ||
      ncol(eblup_sel) != length(years_keep)) {
    stop(
      "Selected MFH EBLUP matrix dimensions do not match the domain/year ",
      "benchmarking frame. This would make regional benchmarking unsafe."
    )
  }
  rownames(eblup_sel) <- as.character(bench_domains)
  colnames(eblup_sel) <- as.character(years_keep)

  formula_outcomes <- vapply(mfh_formula, function(f) all.vars(f)[1], character(1))
  if (!identical(unname(as.character(formula_outcomes)), unname(as.character(poor_cols)))) {
    stop(
      "MFH formula outcome order does not match direct benchmark column order: ",
      paste(formula_outcomes, collapse = ", "), " vs ",
      paste(poor_cols, collapse = ", ")
    )
  }

  # Check that all required vectors are complete
  has_region <- !any(is.na(bench_region))
  has_Nd     <- !any(is.na(bench_Nd_mat))

  if (!has_region || !has_Nd) {
    message("Warning: Missing region or population data for some domains. ",
            "Benchmarking skipped.")
    do_benchmark <- FALSE
  } else {

    cat("\n--- Regional Benchmarking ---\n")
    cat("Model:", diag_model, "\n")
    cat("Domains:", length(bench_domains), "\n")
    cat("Regions:", length(unique(bench_region)), "\n")
    cat("Bootstrap iterations for MSE:", bench_nB, "\n\n")

    # Print regional benchmark targets
    for (tt in seq_along(poor_cols)) {
      if (!is.null(regional_benchmark_mat)) {
        reg_bench <- data.frame(
          region = rownames(regional_benchmark_mat),
          n_domains = as.integer(table(factor(as.character(bench_region),
                                              levels = rownames(regional_benchmark_mat)))),
          B_r = regional_benchmark_mat[, tt],
          source = "external"
        )
      } else {
        reg_bench <- data.frame(
          domain = bench_domains,
          region = bench_region,
          Nd = bench_Nd_mat[, tt],
          direct = direct_bench_mat[, tt],
          eblup = eblup_sel[, tt]
        ) %>%
          filter(is.finite(direct), is.finite(eblup), is.finite(Nd)) %>%
          group_by(region) %>%
          summarize(
            n_domains = n(),
            B_r = weighted.mean(direct, Nd, na.rm = TRUE),
            source = "domain-direct fallback",
            .groups = "drop"
          )
      }
      cat("Regional benchmarks for", poor_cols[tt], ":\n")
      print(as.data.frame(reg_bench))
      cat("\n")
    }

    tictoc::tic("MFH regional benchmarking")

    bench_result <- bench_regional_mfh(
      eblup_mat  = eblup_sel,
      domain_vec = bench_domains,
      region_vec = bench_region,
      Nd_vec     = bench_Nd,
      Nd_mat     = bench_Nd_mat,
      direct_mat = direct_bench_mat,
      regional_benchmark_mat = regional_benchmark_mat,
      model_obj  = selected_model,
      model_type = diag_model,   # dispatch DGP + refit by MFH1/MFH2/MFH3
      formula    = mfh_formula,
      vardir     = vardir_cols,
      data       = domain_dt,
      MSE        = TRUE,
      nB         = bench_nB,
      seed       = 123
    )

    tictoc::toc()

    cat("\nBenchmarking complete.\n")
    cat("Failed bootstrap iterations:", bench_result$fails, "/", bench_nB, "\n")

    # ---- Add benchmarked estimates to the comparison table ----
    bench_long <- tibble(
      domain = rep(bench_domains, ncol(eblup_sel)),
      year   = rep(years_keep, each = length(bench_domains)),
      rate_Bench  = as.vector(bench_result$eblup_bench),
      mse_Bench   = as.vector(bench_result$mse_bench)
    ) %>%
      mutate(
        cv_Bench   = if_else(rate_Bench > 0, sqrt(mse_Bench) / rate_Bench, NA_real_),
        rmse_Bench = sqrt(mse_Bench)
      )

    db_wide_all <- db_wide_all %>%
      left_join(bench_long, by = c("domain", "year"))
  }
}

if (!do_benchmark || is.null(bench_result)) {
  cat("Regional benchmarking was not applied.\n")
}



# ---- Back-transform from log to original scale (mean welfare only) ----
# Per-domain-year smearing anchored to the survey-weighted arithmetic
# mean of welfare:
#   smear_dt = direct_arith_dt / exp(direct_rate_log_dt)
# This guarantees the exported direct_rate equals svymean(welfare) exactly
# (matching the UI promise) and applies the same per-domain-year factor
# to MFH/Bench EBLUPs (standard assumption: within-domain welfare
# variability is similar between direct and model). Variance is
# propagated via the delta method: Var(cÂ·exp(Î·)) â‰ˆ (cÂ·exp(Î·))Â² Â· Var(Î·).
#
# This chunk MUST run before any plotting/diagnostic chunk that reads
# db_wide_all, otherwise the rendered HTML displays log-scale values
# while the exported workbook is on the currency scale.
if (identical(indicator_type, "mean_welfare") && isTRUE(log_transform)) {
  if (is.null(direct_arith_domain_long)) {
    stop("Mean-welfare log-fit back-transform requires direct_arith_domain_long ",
         "to have been computed during direct estimation. Re-run that chunk first.")
  }
  smear_dt <- direct_arith_domain_long %>%
    inner_join(
      db_wide_all %>% select(domain, year, .log_direct_rate = direct_rate),
      by = c("domain", "year")
    ) %>%
    mutate(smear = ifelse(
      is.finite(direct_arith) & is.finite(.log_direct_rate) &
        abs(.log_direct_rate) < 50,
      direct_arith / exp(.log_direct_rate),
      1
    ))
  smear_vec <- smear_dt$smear[match(
    paste(db_wide_all$domain, db_wide_all$year),
    paste(smear_dt$domain,   smear_dt$year)
  )]
  smear_vec[is.na(smear_vec)] <- 1
  # `smear_full` is the bc_sm (Duan smearing) factor anchored to the
  # survey-weighted arithmetic mean. We always use it for `direct_rate`
  # so the Direct column equals svymean(welfare) by construction
  # (matching the UI promise) regardless of the user's bias-correction
  # choice for the MODEL EBLUPs. The choice only affects MFH/UFH/Bench
  # estimates: when bias correction is OFF, those rates use a smear
  # factor of 1 (naive exp(eta)).
  smear_full  <- smear_vec
  smear_model <- if (isTRUE(mfh_bias_correction)) smear_vec else rep(1, length(smear_vec))

  rate_cols  <- intersect(c("direct_rate", "rate_UFH", "rate_UFH_Bench",
                            "rate_MFH1", "rate_MFH2", "rate_MFH3", "rate_Bench"),
                          names(db_wide_all))
  mse_cols   <- intersect(c("direct_mse", "mse_UFH", "mse_UFH_Bench",
                            "mse_MFH1", "mse_MFH2", "mse_MFH3", "mse_Bench"),
                          names(db_wide_all))
  cv_cols    <- intersect(c("direct_cv", "cv_UFH", "cv_UFH_Bench",
                            "cv_MFH1", "cv_MFH2", "cv_MFH3", "cv_Bench"),
                          names(db_wide_all))
  # Note `direct_rmse` IS included here so it gets recomputed once
  # `direct_mse` is overridden with the arithmetic-scale variance below.
  rmse_cols  <- intersect(c("direct_rmse", "rmse_UFH", "rmse_UFH_Bench",
                            "rmse_MFH1", "rmse_MFH2", "rmse_MFH3", "rmse_Bench"),
                          names(db_wide_all))

  for (rc in rate_cols) {
    .smear_for_rc <- if (identical(rc, "direct_rate")) smear_full else smear_model
    db_wide_all[[rc]] <- .smear_for_rc * exp(db_wide_all[[rc]])
  }
  for (mc in mse_cols) {
    rate_match <- sub("^direct_mse$", "direct_rate",
                       sub("^mse_", "rate_", mc))
    if (rate_match %in% names(db_wide_all)) {
      db_wide_all[[mc]] <- (db_wide_all[[rate_match]])^2 * db_wide_all[[mc]]
    }
  }
  for (cc in cv_cols) {
    rate_match <- sub("^direct_cv$", "direct_rate",
                       sub("^cv_", "rate_", cc))
    mse_match  <- sub("^cv_", "mse_", sub("^direct_cv$", "direct_mse", cc))
    if (all(c(rate_match, mse_match) %in% names(db_wide_all))) {
      db_wide_all[[cc]] <- sqrt(pmax(db_wide_all[[mse_match]], 0)) /
                          abs(db_wide_all[[rate_match]])
    }
  }
  for (rc in rmse_cols) {
    mse_match <- sub("^rmse_", "mse_", rc)
    if (mse_match %in% names(db_wide_all)) {
      db_wide_all[[rc]] <- sqrt(pmax(db_wide_all[[mse_match]], 0))
    }
  }

  # Override the Direct column's MSE/CV with the exact arithmetic-scale
  # variance from svyby(~welfare, ...). Recompute direct_rmse afterwards
  # so it stays consistent with the overridden direct_mse.
  if ("direct_mse" %in% names(db_wide_all) &&
      !is.null(direct_arith_domain_long)) {
    arith_lookup <- direct_arith_domain_long %>%
      transmute(domain, year, direct_mse_arith = SD_arith^2)
    aligned <- arith_lookup$direct_mse_arith[match(
      paste(db_wide_all$domain, db_wide_all$year),
      paste(arith_lookup$domain, arith_lookup$year)
    )]
    db_wide_all$direct_mse <- ifelse(is.finite(aligned), aligned, db_wide_all$direct_mse)
    if ("direct_cv" %in% names(db_wide_all)) {
      db_wide_all$direct_cv <- sqrt(pmax(db_wide_all$direct_mse, 0)) /
                                abs(db_wide_all$direct_rate)
    }
    if ("direct_rmse" %in% names(db_wide_all)) {
      db_wide_all$direct_rmse <- sqrt(pmax(db_wide_all$direct_mse, 0))
    }
  }

  attr(db_wide_all, "log_back_transformed") <- TRUE
  attr(db_wide_all, "smearing_anchor")       <- "per_domain_year_arithmetic"
  attr(db_wide_all, "smearing_summary")      <- summary(smear_full[is.finite(smear_full)])
  attr(db_wide_all, "bias_correction_method") <- if (isTRUE(mfh_bias_correction)) "bc_sm" else "none"
  if (isTRUE(mfh_bias_correction)) {
    cat(sprintf(
      "MFH back-transform applied (bias correction = bc_sm; per-domain-year arithmetic anchor; smear range %.4f - %.4f).\n",
      min(smear_full[is.finite(smear_full)]), max(smear_full[is.finite(smear_full)])
    ))
  } else {
    cat(sprintf(
      "MFH back-transform applied (bias correction = none; model rates use naive exp(eta_hat); Direct still uses survey arithmetic mean. Direct-anchor smear range %.4f - %.4f shown for diagnostics.).\n",
      min(smear_full[is.finite(smear_full)]), max(smear_full[is.finite(smear_full)])
    ))
  }

  # ---- Re-benchmark on the EUR scale ----------------------------------
  # rate_Bench currently holds smear_d * exp(Î»_log_r * eblup_log) -- a
  # ratio benchmark applied on the log scale and then exponentiated. That
  # does not satisfy the regional aggregation constraint on the EUR scale.
  # Override rate_Bench with a fresh ratio benchmark computed on the
  # back-transformed estimates and rescale mse_Bench / cv_Bench / rmse_Bench
  # by (lambda_eur / lambda_log_implicit)Â² so the precision columns stay
  # consistent with the new point estimate.
  if ("rate_Bench" %in% names(db_wide_all) &&
      !is.null(region_map) && exists("sizeprov")) {
    eblup_eur_col   <- rate_col            # rate_MFH1/2/3 already back-transformed
    rate_bench_old  <- db_wide_all$rate_Bench  # log-scale-derived bench in EUR
    if (exists("bench_Nd_mat")) {
      bench_pop_lookup <- as.data.frame(as.table(bench_Nd_mat),
                                        stringsAsFactors = FALSE)
      names(bench_pop_lookup) <- c("domain", "year", "Nd")
      bench_pop_lookup <- bench_pop_lookup %>%
        mutate(domain = as.integer(domain), year = as.integer(year))
    } else {
      bench_pop_lookup <- db_wide_all %>%
        select(domain, year) %>%
        left_join(sizeprov, by = "domain") %>%
        select(domain, year, Nd)
    }
    bench_inputs <- db_wide_all %>%
      transmute(
        domain, year,
        direct_eur = direct_rate,
        eblup_eur  = .data[[eblup_eur_col]]
      ) %>%
      left_join(region_map %>% select(domain, region), by = "domain") %>%
      left_join(bench_pop_lookup, by = c("domain", "year"))
    if (exists("regional_benchmark_mat") && !is.null(regional_benchmark_mat)) {
      region_targets <- as.data.frame(as.table(regional_benchmark_mat),
                                      stringsAsFactors = FALSE)
      names(region_targets) <- c("region", "year", "B_eur")
      region_targets <- region_targets %>%
        mutate(region = as.character(region), year = as.integer(year))
      region_lambda_eur <- bench_inputs %>%
        mutate(region = as.character(region)) %>%
        filter(is.finite(eblup_eur), is.finite(Nd)) %>%
        group_by(region, year) %>%
        summarise(
          eblup_bar = stats::weighted.mean(eblup_eur, Nd, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        left_join(region_targets, by = c("region", "year")) %>%
        mutate(lambda_eur = ifelse(is.finite(B_eur) & abs(eblup_bar) > 1e-8,
                                   B_eur / eblup_bar, 1))
    } else {
      region_lambda_eur <- bench_inputs %>%
        filter(is.finite(direct_eur), is.finite(eblup_eur), is.finite(Nd)) %>%
        group_by(region, year) %>%
        summarise(
          B_eur      = stats::weighted.mean(direct_eur, Nd, na.rm = TRUE),
          eblup_bar  = stats::weighted.mean(eblup_eur,  Nd, na.rm = TRUE),
          lambda_eur = ifelse(abs(eblup_bar) > 1e-8, B_eur / eblup_bar, 1),
          .groups    = "drop"
        )
    }
    aligned_lambda <- bench_inputs %>%
      left_join(region_lambda_eur %>% select(region, year, lambda_eur),
                by = c("region", "year")) %>%
      pull(lambda_eur)
    aligned_lambda[!is.finite(aligned_lambda)] <- 1
    rate_bench_new <- aligned_lambda * db_wide_all[[eblup_eur_col]]
    db_wide_all$rate_Bench <- rate_bench_new
    if ("mse_Bench" %in% names(db_wide_all)) {
      # Rescale by (rate_bench_new / rate_bench_old)Â² so mse_Bench stays
      # consistent with the new EUR-scale point estimate. The original
      # mse_Bench was the delta-method propagation of the log-scale
      # bootstrap MSE applied to the log-scale-derived bench point.
      ratio_sq <- (rate_bench_new / pmax(abs(rate_bench_old), 1e-8))^2
      ratio_sq[!is.finite(ratio_sq)] <- 1
      db_wide_all$mse_Bench <- db_wide_all$mse_Bench * ratio_sq
    }
    if ("cv_Bench" %in% names(db_wide_all) && "mse_Bench" %in% names(db_wide_all)) {
      db_wide_all$cv_Bench <- sqrt(pmax(db_wide_all$mse_Bench, 0)) /
                               abs(db_wide_all$rate_Bench)
    }
    if ("rmse_Bench" %in% names(db_wide_all) && "mse_Bench" %in% names(db_wide_all)) {
      db_wide_all$rmse_Bench <- sqrt(pmax(db_wide_all$mse_Bench, 0))
    }
    attr(db_wide_all, "bench_scale") <- "EUR"
    cat("Regional benchmarking re-applied on EUR scale (rate_Bench overwritten; ",
        "mse/cv/rmse_Bench rescaled).\n", sep = "")
  }
}


if (isTRUE(fit_mfh3_requested) && exists("fit_mfh3") && !fit_mfh3) {
  cat(
    "--- WARNING ---\n",
    "## MFH3 Did Not Converge\n\n",
    "The MFH3 (heteroskedastic) model was requested but did not converge during estimation. ",
    "All post-estimation diagnostics, maps, and inference results below are therefore based on the **",
    diag_model,
    "** model (fallback).\n",
    "\n"
  )
}


# ============================================================
# Objective: Residual diagnostics for the selected model
#            Compare domain-level residuals (Direct - EBLUP)
#            against EBLUPs for each year to detect outliers,
#            heteroskedasticity, and systematic patterns.
# ============================================================

stopifnot(length(poor_cols) == 2)

# Ensure the same domain ordering used when mapping model outputs
domain_vec <- domain_dt$domain

# Extract EBLUP columns for the two outcomes (years_keep order)
eb <- as.data.frame(selected_model$eblup)

# Residuals: direct (domain_dt) minus EBLUP (model)
resids_2 <- cbind(
  domain_dt[[poor_cols[1]]] - eb[[poor_cols[1]]],
  domain_dt[[poor_cols[2]]] - eb[[poor_cols[2]]]
)

# Two-panel diagnostic plots -- save to file
png(here::here("outputs", "figures", "mfh_residual_vs_eblup.png"),
    width = 12, height = 6, units = "in", res = 150)
par(mfrow = c(1, 2))
plot(
  eb[[poor_cols[1]]], resids_2[, 1],
  pch = 19,
  xlab = paste0("EBLUPs (", poor_cols[1], ")"),
  ylab = paste0("Residuals (", poor_cols[1], ")")
)
plot(
  eb[[poor_cols[2]]], resids_2[, 2],
  pch = 19,
  xlab = paste0("EBLUPs (", poor_cols[2], ")"),
  ylab = paste0("Residuals (", poor_cols[2], ")")
)
par(mfrow = c(1, 1))
dev.off()


# ============================================================
# Objective: Test for residual--fitted relationships
#            Regress residuals on fitted EBLUPs for each year as
#            a simple check for systematic patterns / misspecification.
# ============================================================

stopifnot(length(poor_cols) == 2)

eb_diag <- as.data.frame(selected_model$eblup)

# Regression of residuals on fitted values for each year (y1 and y2)
lm_resid_fit_y1 <- lm(resids_2[, 1] ~ eb_diag[[poor_cols[1]]])
lm_resid_fit_y2 <- lm(resids_2[, 2] ~ eb_diag[[poor_cols[2]]])

summary(lm_resid_fit_y1)
summary(lm_resid_fit_y2)




# ============================================================
# Objective: Residual normality diagnostics for the selected model
#            Compute residuals (Direct - EBLUP) for each year,
#            run Shapiro--Wilk tests, and visualize distributions
#            using histograms and QQ plots.
# ============================================================

stopifnot(length(poor_cols) == 2)

# Residuals: direct minus EBLUP
eb_diag <- as.data.frame(selected_model$eblup)

resid_dt <- domain_dt %>%
  select(all_of(poor_cols)) %>%
  as.data.frame()

# Ensure columns match before subtraction
missing_in_eb <- setdiff(poor_cols, names(eb_diag))
if (length(missing_in_eb) > 0) {
  stop("EBLUP output is missing expected columns: ", paste(missing_in_eb, collapse = ", "))
}

resid_dt <- resid_dt - eb_diag[, poor_cols, drop = FALSE]
colnames(resid_dt) <- poor_cols  # keep clear labels

# ---- Safe Shapiro--Wilk wrapper (guards against constant columns) ----
safe_shapiro <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x) < 3) {
    list(statistic = c(W = NA_real_), p.value = NA_real_, note = "Too few values")
  } else if (length(unique(x)) <= 1) {
    list(statistic = c(W = NA_real_), p.value = NA_real_, note = "Constant values")
  } else {
    tryCatch(
      shapiro.test(x),
      error = function(e) list(statistic = c(W = NA_real_), p.value = NA_real_, note = "Test failed")
    )
  }
}

# ---- Shapiro tests by year ----
shapiro_obj <- lapply(resid_dt, safe_shapiro)

resid_shapiro_dt <- data.frame(
  Time    = names(shapiro_obj),
  W       = as.numeric(sapply(shapiro_obj, function(x) x$statistic[[1]])),
  p_value = as.numeric(sapply(shapiro_obj, function(x) x$p.value))
) %>%
  mutate(label = ifelse(is.na(W),
                        "Shapiro-Wilk skipped",
                        paste0("W = ", round(W, 3), "\n", "p = ", signif(p_value, 3))))

# ---- Histograms ----
.p_resid_hist <- resid_dt %>%
  as_tibble() %>%
  pivot_longer(cols = everything(), names_to = "Time", values_to = "Residual") %>%
  ggplot(aes(x = Residual)) +
  geom_histogram(bins = 10, color = "white") +
  geom_text(
    data = resid_shapiro_dt,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.1, vjust = 1.2, inherit.aes = FALSE, size = 3.5
  ) +
  facet_wrap(~Time, scales = "free") +
  theme_minimal() +
  labs(title = paste0("Residual histograms by year (Direct - EBLUP, ", diag_model, ")"))
ggsave(here::here("outputs", "figures", "mfh_residual_histogram.png"),
       .p_resid_hist, width = 10, height = 6, dpi = 150)

# ---- QQ plots ----
.p_resid_qq <- resid_dt %>%
  as_tibble() %>%
  pivot_longer(cols = everything(), names_to = "Time", values_to = "Residual") %>%
  ggplot(aes(sample = Residual)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Time, scales = "free") +
  theme_minimal() +
  labs(title = paste0("QQ plots of residuals by year (Direct - EBLUP, ", diag_model, ")"))
ggsave(here::here("outputs", "figures", "mfh_qq_residual.png"),
       .p_resid_qq, width = 10, height = 6, dpi = 150)



# Extract estimated random effects from the selected model
raneff_dt <- as.data.frame(selected_model$randomEffect)

# If a single vector slipped through, coerce to 1-column data frame
if (is.null(ncol(raneff_dt))) {
  raneff_dt <- data.frame(re = as.numeric(raneff_dt))
}

# Check whether all random effects are zero (boundary refvar = 0)
.re_all_zero <- all(unlist(raneff_dt) == 0, na.rm = TRUE)


if (.re_all_zero) {
  cat(
    "\n--- WARNING ---\n",
    "## Random-effects diagnostics skipped\n\n",
    "All estimated random effects are identically zero because the random-effects ",
    "variance ($\\sigma^2_u$) was estimated at the boundary (zero). ",
    "Normality tests and histogram diagnostics are not applicable.\n",
    "\n",
    sep = ""
  )
}



# Shapiro--Wilk tests by column (e.g., by year/outcome)
# Uses safe_shapiro() defined in the residuals chunk above
shapiro_obj <- lapply(raneff_dt, safe_shapiro)

re_shapiro_dt <- data.frame(
  Time    = names(shapiro_obj),
  W       = as.numeric(sapply(shapiro_obj, function(x) x$statistic[[1]])),
  p_value = as.numeric(sapply(shapiro_obj, function(x) x$p.value))
) %>%
  mutate(label = ifelse(is.na(W),
                        "Shapiro-Wilk skipped",
                        paste0("W = ", round(W, 3), "\n", "p = ", signif(p_value, 3))))

# Plot histograms with test annotations
.p_re_hist <- raneff_dt %>%
  as_tibble() %>%
  pivot_longer(cols = everything(), names_to = "Time", values_to = "RandEff") %>%
  ggplot(aes(x = RandEff)) +
  geom_histogram(bins = 10, color = "white") +
  geom_text(
    data = re_shapiro_dt,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.1, vjust = 1.2, inherit.aes = FALSE, size = 3.5
  ) +
  facet_wrap(~Time, scales = "free") +
  theme_minimal() +
  labs(title = paste0("Random-effects histograms by year/outcome (", diag_model, ")"))
ggsave(here::here("outputs", "figures", "mfh_qq_random_effect.png"),
       .p_re_hist, width = 10, height = 6, dpi = 150)



re_shapiro_dt <- data.frame(
  Time    = names(raneff_dt),
  W       = NA_real_,
  p_value = NA_real_
)


# ---- Export Shapiro--Wilk results to CSV for the app ----
mfh_shapiro_results <- data.frame(
  year       = rep(years_keep, 2),
  component  = c(rep("residual", length(years_keep)), rep("random_effect", length(years_keep))),
  W          = c(resid_shapiro_dt$W, re_shapiro_dt$W),
  p_value    = c(resid_shapiro_dt$p_value, re_shapiro_dt$p_value),
  model_type = diag_model
)
write.csv(mfh_shapiro_results, file = here::here("outputs", "tables", "mfh_shapiro_results.csv"), row.names = FALSE)


# ============================================================
# Objective: Compare direct vs model-based estimates across domains
#            for a selected year (year_plot), using MSE and levels.
# ============================================================

year_plot <- y1   # switch to y2 if desired

# ---- Build color palette and column lists dynamically ----
method_colors <- c(
  "direct_mse"  = "black",
  "direct_rate" = "black",
  "mse_UFH"     = "#1f77b4",
  "rate_UFH"    = "#1f77b4",
  "mse_MFH1"    = "#ff7f0e",
  "rate_MFH1"   = "#ff7f0e",
  "mse_MFH2"    = "#d62728",
  "rate_MFH2"   = "#d62728"
)
if (fit_mfh3) {
  method_colors <- c(method_colors, "mse_MFH3" = "#2ca02c", "rate_MFH3" = "#2ca02c")
}

# Column vectors: use any_of() so missing columns are silently skipped
mse_model_cols  <- c("mse_UFH", "mse_MFH1", "mse_MFH2", if (fit_mfh3) "mse_MFH3")
rate_model_cols <- c("rate_UFH", "rate_MFH1", "rate_MFH2", if (fit_mfh3) "rate_MFH3")

## ---------- 1) MSE incl direct ----------
plot_mse_all <- db_wide_all %>%
  filter(year == year_plot) %>%
  arrange(direct_mse) %>%
  mutate(domain_order = row_number()) %>%
  select(domain, domain_order, direct_mse, any_of(mse_model_cols)) %>%
  pivot_longer(
    cols = c(direct_mse, any_of(mse_model_cols)),
    names_to = "Method",
    values_to = "MSE"
  )

p1 <- ggplot(plot_mse_all, aes(x = domain_order, y = MSE, color = Method)) +
  geom_point(size = 2) +
  scale_color_manual(values = method_colors) +
  labs(
    title = paste0("MSE comparison (incl direct) - ", year_plot),
    x = "Domain (ordered by increasing direct MSE)",
    y = "MSE"
  ) +
  theme_minimal()

## ---------- 2) MSE models only ----------
plot_mse_models <- db_wide_all %>%
  filter(year == year_plot) %>%
  arrange(mse_UFH) %>%
  mutate(domain_order = row_number()) %>%
  select(domain, domain_order, any_of(mse_model_cols)) %>%
  pivot_longer(
    cols = any_of(mse_model_cols),
    names_to = "Method",
    values_to = "MSE"
  )

p2 <- ggplot(plot_mse_models, aes(x = domain_order, y = MSE, color = Method)) +
  geom_point(size = 2) +
  scale_color_manual(values = method_colors) +
  labs(
    title = paste0("MSE comparison (models only) - ", year_plot),
    x = "Domain (ordered by increasing UFH MSE)",
    y = "MSE"
  ) +
  theme_minimal()

## ---------- 3) Poverty rate incl direct ----------
plot_rate_all <- db_wide_all %>%
  filter(year == year_plot) %>%
  arrange(direct_rate) %>%
  mutate(domain_order = row_number()) %>%
  select(domain, domain_order, direct_rate, any_of(rate_model_cols)) %>%
  pivot_longer(
    cols = c(direct_rate, any_of(rate_model_cols)),
    names_to = "Method",
    values_to = "Rate"
  )

p3 <- ggplot(plot_rate_all, aes(x = domain_order, y = Rate, color = Method)) +
  geom_point(size = 2) +
  scale_color_manual(values = method_colors) +
  labs(
    title = paste0(pov_lab$short, " comparison (incl direct) - ", year_plot),
    x = "Domain (ordered by increasing direct value)",
    y = pov_lab$short
  ) +
  theme_minimal()

## ---------- 4) Estimate models only ----------
plot_rate_models <- db_wide_all %>%
  filter(year == year_plot) %>%
  arrange(rate_UFH) %>%
  mutate(domain_order = row_number()) %>%
  select(domain, domain_order, any_of(rate_model_cols)) %>%
  pivot_longer(
    cols = any_of(rate_model_cols),
    names_to = "Method",
    values_to = "Rate"
  )

p4 <- ggplot(plot_rate_models, aes(x = domain_order, y = Rate, color = Method)) +
  geom_point(size = 2) +
  scale_color_manual(values = method_colors) +
  labs(
    title = paste0(pov_lab$short, " comparison (models only) - ", year_plot),
    x = "Domain (ordered by increasing UFH value)",
    y = pov_lab$short
  ) +
  theme_minimal()

ggsave(here::here("outputs", "figures", paste0("mfh_mse_all_", year_plot, ".png")), p1, width = 12, height = 6, dpi = 150)
ggsave(here::here("outputs", "figures", paste0("mfh_mse_models_", year_plot, ".png")), p2, width = 12, height = 6, dpi = 150)
ggsave(here::here("outputs", "figures", paste0("mfh_compare_y", match(year_plot, years_keep), ".png")), p3, width = 12, height = 6, dpi = 150)
ggsave(here::here("outputs", "figures", paste0("mfh_rate_models_", year_plot, ".png")), p4, width = 12, height = 6, dpi = 150)

# ---- RMSE color mapping (same hues as MSE columns) ----
rmse_colors <- c(
  "direct_rmse" = "black",
  "rmse_UFH"    = "#1f77b4",
  "rmse_MFH1"   = "#ff7f0e",
  "rmse_MFH2"   = "#d62728"
)
if (fit_mfh3) rmse_colors <- c(rmse_colors, "rmse_MFH3" = "#2ca02c")

rmse_model_cols <- sub("^mse_", "rmse_", mse_model_cols)

## ---------- 5) RMSE incl direct ----------
plot_rmse_all <- db_wide_all %>%
  filter(year == year_plot) %>%
  arrange(direct_rmse) %>%
  mutate(domain_order = row_number()) %>%
  select(domain, domain_order, direct_rmse, any_of(rmse_model_cols)) %>%
  pivot_longer(
    cols      = c(direct_rmse, any_of(rmse_model_cols)),
    names_to  = "Method",
    values_to = "RMSE"
  )

p5 <- ggplot(plot_rmse_all, aes(x = domain_order, y = RMSE, color = Method)) +
  geom_point(size = 2) +
  scale_color_manual(values = rmse_colors) +
  labs(
    title = paste0("RMSE comparison (incl direct) \u2013 ", year_plot),
    x     = "Domain (ordered by increasing direct RMSE)",
    y     = "RMSE"
  ) +
  theme_minimal()

## ---------- 6) RMSE models only ----------
plot_rmse_models <- db_wide_all %>%
  filter(year == year_plot) %>%
  arrange(rmse_UFH) %>%
  mutate(domain_order = row_number()) %>%
  select(domain, domain_order, any_of(rmse_model_cols)) %>%
  pivot_longer(
    cols      = any_of(rmse_model_cols),
    names_to  = "Method",
    values_to = "RMSE"
  )

p6 <- ggplot(plot_rmse_models, aes(x = domain_order, y = RMSE, color = Method)) +
  geom_point(size = 2) +
  scale_color_manual(values = rmse_colors) +
  labs(
    title = paste0("RMSE comparison (models only) \u2013 ", year_plot),
    x     = "Domain (ordered by increasing UFH RMSE)",
    y     = "RMSE"
  ) +
  theme_minimal()

ggsave(here::here("outputs", "figures", paste0("mfh_rmse_all_", year_plot, ".png")), p5, width = 12, height = 6, dpi = 150)
ggsave(here::here("outputs", "figures", paste0("mfh_rmse_models_", year_plot, ".png")), p6, width = 12, height = 6, dpi = 150)





# ============================================================
# Objective: Diagnose MFH smoothing by relating the magnitude of
#            model differences to sample size.
#            We plot |selected_model âˆ’ UFH| against N (log scale).
#            Larger differences at small N are consistent with
#            smoothing where direct information is weak.
# ============================================================

stopifnot(length(years_keep) == 2, length(N_cols) == 2)

# 1) Sample sizes to long: (domain, year, N)
n_long <- all_var_hat_domain_dt %>%
  select(domain, all_of(N_cols)) %>%
  pivot_longer(
    cols      = all_of(N_cols),
    names_to  = "year_raw",
    values_to = "N"
  ) %>%
  mutate(year = as.integer(gsub("^N_", "", year_raw))) %>%
  select(domain, year, N)

# 2) Build plotting dataset: UFH vs selected model rates + sample size
plot_dt <- db_wide_all %>%
  select(domain, year, rate_UFH, all_of(rate_col)) %>%
  left_join(n_long, by = c("domain", "year")) %>%
  filter(year %in% years_keep)

# 3) Smoothing diagnostic: absolute difference vs sample size
plot_dt <- plot_dt %>%
  mutate(abs_diff = abs(.data[[rate_col]] - rate_UFH)) %>%
  filter(is.finite(abs_diff), is.finite(N), !is.na(N), N > 0)

ggplot(plot_dt, aes(x = N, y = abs_diff)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_log10() +
  labs(
    title = paste0(diag_model, " smoothing diagnostic: |", diag_model, " - UFH| vs sample size"),
    subtitle = "Larger differences at small N indicate stronger smoothing where direct information is weak",
    x = "Sample size (N, log scale)",
    y = paste0("|", diag_model, " - UFH|")
  ) +
  theme_minimal()

ggsave(here::here("outputs", "figures", "mfh_smoothing_diagnostic.png"),
       width = 10, height = 6, dpi = 150)


# Skip MCPE bootstrap when MFH model did not produce valid random effects
if (.refvar_zero || .model_fit_failed) {
  cat("MCPE bootstrap skipped -- eblupMFH2 was not properly executed.\n")
  mcpe_obj     <- NULL
  mcpemfh2_obj <- NULL
} else {
  # MCPE is available for MFH1, MFH2 and MFH3. The dispatcher picks the
  # right parametric-bootstrap routine based on `diag_model` (and lazily
  # sources the matching `pbmcpeMFH*_with_existing_eblup.R` wrapper) so
  # downstream code is agnostic to which variant was chosen.
  source(here::here("scripts", "pbmcpe_with_existing_dispatch.R"))

  set.seed(123)
  tictoc::tic(paste0("pbmcpe (", diag_model, ")"))

  mcpe_obj <- tryCatch(
    pbmcpe_with_existing(
      diag_model     = diag_model,
      formula        = mfh_formula,
      vardir         = vardir_cols,
      domain_var     = "domain",
      existing_model = selected_model,
      nB             = 50,
      data           = domain_dt,
      MAXITER        = 1e10,
      PRECISION      = 1e-2
    ),
    error = function(e) {
      cat(
        "\n**Note:** MCPE bootstrap is not available for this MFH run. ",
        conditionMessage(e),
        "\nChange-analysis outputs will be written with NA placeholders.\n\n",
        sep = ""
      )
      NULL
    }
  )

  if (!is.null(mcpe_obj)) {
    # Save with a model-specific filename so multiple runs can coexist on disk.
    saveRDS(mcpe_obj,
            here::here("outputs", "data", paste0("mcpe_", tolower(diag_model), "_obj.rds")))
  }

  # Back-compat alias: older code paths referenced `mcpemfh2_obj`
  mcpemfh2_obj <- mcpe_obj

  tictoc::toc()
}


# Objective:
# This chunk defines a helper function that compares MCPE-based EBLUP
# poverty estimates across two time periods for any MFH variant
# (MFH1/MFH2/MFH3). It computes pairwise differences, constructs
# confidence intervals using the MCPE covariance, and identifies
# domains with statistically significant changes. The function also
# produces a diagnostic plot to visually assess the magnitude and
# significance of intertemporal changes across areas.

#' A function to compare and plot differences from the mcpe object
#'
#' Works for any MFH variant because it only uses the eblup/mse/mcpe
#' slots of the supplied mcpe object, all of which are produced in the
#' same shape by `pbmcpe_with_existing()`.
compare_mfh <- function(period_list = c(1, 2),
                        mcpe_obj = if (exists("mcpe_obj")) mcpe_obj else mcpemfh2_obj,
                        alpha = 0.05,
                        year_labels = NULL) {
  stopifnot(length(period_list) == 2)
  
  # Force 2D objects even when there is only 1 column
  eblup_mat <- as.matrix(mcpe_obj$eblup)
  mse_mat   <- as.matrix(mcpe_obj$mse)
  mcpe_mat  <- as.matrix(mcpe_obj$mcpe)
  
  # Column name for covariance (t1,t2)
  col_chr <- paste0("(", period_list[1], ",", period_list[2], ")")
  
  # If mcpe_mat has no colnames (common when nT=2 and earlier code dropped),
  # assume it's the single (1,2) covariance and set the name.
  if (is.null(colnames(mcpe_mat)) && ncol(mcpe_mat) == 1) {
    colnames(mcpe_mat) <- paste0("(", min(period_list), ",", max(period_list), ")")
  }
  
  # If user asks (2,1) but the stored name is (1,2), map it
  if (!(col_chr %in% colnames(mcpe_mat))) {
    alt <- paste0("(", period_list[2], ",", period_list[1], ")")
    if (alt %in% colnames(mcpe_mat)) {
      col_chr <- alt
    } else if (ncol(mcpe_mat) == 1) {
      # last resort: use the only column
      col_chr <- colnames(mcpe_mat)[1]
    } else {
      stop("Requested MCPE column not found. Available: ",
           paste(colnames(mcpe_mat), collapse = ", "))
    }
  }
  
  diff <- eblup_mat[, period_list[2]] - eblup_mat[, period_list[1]]
  # Variance of the change: MSE_t1 + MSE_t2 - 2Â·MCPE(t1, t2). Bootstrap
  # noise (especially with small nB) can make the covariance term large
  # enough to push the result negative; previously this was silently
  # passed through, producing NaN sqrt() / zero-width CIs / spurious
  # significance flags. Fall back to the independence-assumption MSE
  # whenever the covariance-adjusted value is non-positive or smaller
  # than 1% of the independence MSE. Conservative (overestimates
  # variance for those rows) but cannot manufacture false certainty.
  # The guard lives here in compare_mfh() so it applies uniformly to
  # every caller (unbenchmarked comp12_obj, benchmarked comp12_bench_obj,
  # poverty / mean-welfare / log-mean-welfare runs alike).
  mse_indep    <- mse_mat[, period_list[1]] + mse_mat[, period_list[2]]
  mse_with_cov <- mse_indep - 2 * mcpe_mat[, col_chr]
  mse_floor    <- 0.01 * mse_indep
  use_cov      <- is.finite(mse_with_cov) & mse_with_cov >= mse_floor
  mse          <- ifelse(use_cov, mse_with_cov, mse_indep)
  n_fallback   <- sum(!use_cov, na.rm = TRUE)
  if (n_fallback > 0) {
    message(sprintf(
      "compare_mfh(): %d of %d domain(s) had non-positive or near-zero covariance-adjusted MSE; falling back to the independence-assumption MSE for those rows. Consider raising the bootstrap nB.",
      n_fallback, length(use_cov)
    ))
  }

  df <- tibble::tibble(
    domain = mcpe_obj$domain,  # Added: extract domain from mcpe_obj
    diff = diff,
    mse = mse,
    mse_fallback_used = !use_cov,
    alpha = rep(alpha, nrow(eblup_mat)),
    zq = qnorm(alpha / 2, lower.tail = FALSE)
  ) |>
    dplyr::mutate(
      lb = diff - zq * sqrt(mse),
      ub = diff + zq * sqrt(mse),
      significant = ifelse(lb > 0 | ub < 0, "Significant", "Not Significant"),
      index = dplyr::row_number()
    )
  
  # Build axis/title labels from year_labels if provided, else use period indices
  if (!is.null(year_labels)) {
    plot_title  <- paste(pov_lab$short, "Changes Between", year_labels[1], "and", year_labels[2])
    y_label     <- paste0("Change in ", pov_lab$short, " (", year_labels[2], " - ", year_labels[1], ")")
  } else {
    plot_title  <- paste0("Change in ", tolower(pov_lab$short),
                           " based on MCPE between period ",
                           period_list[1], " and ", period_list[2])
    y_label     <- "Difference between time periods"
  }

  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = domain, y = diff, color = significant)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_point(size = 2.5, alpha = 0.7) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lb, ymax = ub), width = 0.3, alpha = 0.5) +
    ggplot2::scale_color_manual(
      values = c("Significant" = "red", "Not Significant" = "gray60"),
      name = "Statistical\nSignificance"
    ) +
    ggplot2::labs(
      title    = plot_title,
      subtitle = "With 95% Confidence Intervals",
      x        = "Domain",
      y        = y_label,
      caption  = "Red points indicate statistically significant changes (\u03b1 = 0.05)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = 14),
      legend.position = "right"
    )
  
  list(df = df, plot = p1)
}

# Back-compat alias: earlier versions of this pipeline exposed the same
# helper under the MFH2-specific name `compare_mfh2`.
compare_mfh2 <- compare_mfh


if (is.null(mcpe_obj)) {
  comp12_obj <- NULL
} else {
comp12_obj <- compare_mfh(mcpe_obj = mcpe_obj, year_labels = years_keep)

# For mean_welfare + log_transform we back-transform comp12_obj$df to the
# EUR scale here, BEFORE the plot and table are printed. Otherwise the
# rendered HTML shows model-scale (log) diffs and tiny MSEs while the
# exported CSV is on the currency scale. The back-transform uses
# db_wide_all (already on EUR scale by this point -- see the
# `back-transform-mean-welfare` chunk) for the per-year point estimates,
# and reads per-year MSEs / cross-year MCPE on the log scale directly
# from `mcpe_obj` since those were the inputs to compare_mfh().
if (identical(indicator_type, "mean_welfare") && isTRUE(log_transform)) {
  eblup_eur_lookup <- db_wide_all %>%
    dplyr::select(domain, year, eblup_eur = dplyr::all_of(rate_col)) %>%
    tidyr::pivot_wider(names_from = year, values_from = eblup_eur,
                       names_prefix = "eblup_eur_")
  comp12_obj$df <- comp12_obj$df %>%
    dplyr::left_join(eblup_eur_lookup, by = "domain")
  est_y1_e <- comp12_obj$df[[paste0("eblup_eur_", y1)]]
  est_y2_e <- comp12_obj$df[[paste0("eblup_eur_", y2)]]
  mse_mat_log <- as.matrix(mcpe_obj$mse)
  mcpe_mat_log <- as.matrix(mcpe_obj$mcpe)
  if (is.null(colnames(mcpe_mat_log)) && ncol(mcpe_mat_log) == 1) {
    colnames(mcpe_mat_log) <- "(1,2)"
  }
  mse_y1_log <- as.numeric(mse_mat_log[, 1])
  mse_y2_log <- as.numeric(mse_mat_log[, 2])
  mcpe_log   <- if ("(1,2)" %in% colnames(mcpe_mat_log)) {
    as.numeric(mcpe_mat_log[, "(1,2)"])
  } else as.numeric(mcpe_mat_log[, 1])
  comp12_obj$df$diff <- est_y2_e - est_y1_e
  mse_indep_e    <- est_y1_e^2 * mse_y1_log + est_y2_e^2 * mse_y2_log
  mse_with_cov_e <- mse_indep_e - 2 * est_y1_e * est_y2_e * mcpe_log
  use_cov_e      <- is.finite(mse_with_cov_e) & mse_with_cov_e >= 0.01 * mse_indep_e
  comp12_obj$df$mse <- ifelse(use_cov_e, mse_with_cov_e, mse_indep_e)
  comp12_obj$df$mse_fallback_used <- !use_cov_e
  zq_vec <- comp12_obj$df$zq
  se_e   <- sqrt(comp12_obj$df$mse)
  comp12_obj$df$lb <- comp12_obj$df$diff - zq_vec * se_e
  comp12_obj$df$ub <- comp12_obj$df$diff + zq_vec * se_e
  comp12_obj$df$significant <- ifelse(
    comp12_obj$df$lb > 0 | comp12_obj$df$ub < 0,
    "Significant", "Not Significant"
  )
  # Rebuild the plot from the EUR-scale data so the displayed figure
  # matches the table.
  comp12_obj$plot <- ggplot2::ggplot(
    comp12_obj$df,
    ggplot2::aes(x = domain, y = diff, color = significant)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_point(size = 2.5, alpha = 0.7) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lb, ymax = ub), width = 0.3, alpha = 0.5) +
    ggplot2::scale_color_manual(
      values = c("Significant" = "red", "Not Significant" = "gray60"),
      name = "Statistical\nSignificance"
    ) +
    ggplot2::labs(
      title    = paste(pov_lab$short, "Changes Between", years_keep[1], "and", years_keep[2]),
      subtitle = "With 95% Confidence Intervals (back-transformed to EUR)",
      x        = "Domain",
      y        = paste0("Change in ", pov_lab$short, " (", years_keep[2], " - ", years_keep[1], ")"),
      caption  = "Red points indicate statistically significant changes (Î± = 0.05)"
    ) +
    ggplot2::theme_minimal()
  attr(comp12_obj$df, "log_back_transformed") <- TRUE
}

}

if (!is.null(comp12_obj)) {
  print(comp12_obj$plot)
  ggsave(here::here("outputs", "figures", "mfh_change_preview.png"),
         comp12_obj$plot, width = 12, height = 6, dpi = 150)
  print(comp12_obj$df %>% head() %>% kable())
} else {
  cat("Change analysis skipped -- eblupMFH2 was not properly executed.\n")
}



comp12_bench_obj <- NULL

if (do_benchmark && !is.null(bench_result) && !is.null(bench_result$mcpe_bench)) {

  # Build a pseudo mcpe_obj for compare_mfh2() using benchmarked quantities
  bench_mcpe_obj <- list(
    domain = domain_dt$domain,
    eblup  = bench_result$eblup_bench,
    mse    = bench_result$mse_bench,
    mcpe   = bench_result$mcpe_bench
  )

  comp12_bench_obj <- compare_mfh(
    period_list = c(1, 2),
    mcpe_obj    = bench_mcpe_obj,
    year_labels = years_keep
  )

  # Mirror the unbenchmarked back-transform: rebuild diff/mse/lb/ub/sig
  # on the EUR scale BEFORE printing, so the rendered HTML and the
  # exported comparison_final_bench.csv tell the same story.
  if (identical(indicator_type, "mean_welfare") && isTRUE(log_transform)) {
    bench_eur_lookup <- db_wide_all %>%
      dplyr::select(domain, year, bench_eur = rate_Bench) %>%
      tidyr::pivot_wider(names_from = year, values_from = bench_eur,
                         names_prefix = "bench_eur_")
    comp12_bench_obj$df <- comp12_bench_obj$df %>%
      dplyr::left_join(bench_eur_lookup, by = "domain")
    est_y1_b <- comp12_bench_obj$df[[paste0("bench_eur_", y1)]]
    est_y2_b <- comp12_bench_obj$df[[paste0("bench_eur_", y2)]]
    mse_y1_b_log <- as.numeric(bench_result$mse_bench[, 1])
    mse_y2_b_log <- as.numeric(bench_result$mse_bench[, 2])
    mcpe_b_log   <- if (!is.null(bench_result$mcpe_bench)) {
      as.numeric(bench_result$mcpe_bench[, 1])
    } else 0
    comp12_bench_obj$df$diff <- est_y2_b - est_y1_b
    mse_indep_b    <- est_y1_b^2 * mse_y1_b_log + est_y2_b^2 * mse_y2_b_log
    mse_with_cov_b <- mse_indep_b - 2 * est_y1_b * est_y2_b * mcpe_b_log
    use_cov_b      <- is.finite(mse_with_cov_b) & mse_with_cov_b >= 0.01 * mse_indep_b
    comp12_bench_obj$df$mse <- ifelse(use_cov_b, mse_with_cov_b, mse_indep_b)
    comp12_bench_obj$df$mse_fallback_used <- !use_cov_b
    zq_b <- comp12_bench_obj$df$zq
    se_b <- sqrt(comp12_bench_obj$df$mse)
    comp12_bench_obj$df$lb <- comp12_bench_obj$df$diff - zq_b * se_b
    comp12_bench_obj$df$ub <- comp12_bench_obj$df$diff + zq_b * se_b
    comp12_bench_obj$df$significant <- ifelse(
      comp12_bench_obj$df$lb > 0 | comp12_bench_obj$df$ub < 0,
      "Significant", "Not Significant"
    )
    comp12_bench_obj$plot <- ggplot2::ggplot(
      comp12_bench_obj$df,
      ggplot2::aes(x = domain, y = diff, color = significant)
    ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::geom_point(size = 2.5, alpha = 0.7) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lb, ymax = ub), width = 0.3, alpha = 0.5) +
      ggplot2::scale_color_manual(
        values = c("Significant" = "red", "Not Significant" = "gray60"),
        name = "Statistical\nSignificance"
      ) +
      ggplot2::labs(
        title    = paste("Benchmarked", pov_lab$short, "Changes Between",
                          years_keep[1], "and", years_keep[2]),
        subtitle = "With 95% Confidence Intervals (back-transformed to EUR)",
        x        = "Domain",
        y        = paste0("Change in benchmarked ", pov_lab$short),
        caption  = "Red points indicate statistically significant changes (Î± = 0.05)"
      ) +
      ggplot2::theme_minimal()
    attr(comp12_bench_obj$df, "log_back_transformed") <- TRUE
  }

  cat("### Benchmarked ", diag_model, " Change Analysis\n\n", sep = "")
  .bench_plot <- comp12_bench_obj$plot +
    ggplot2::labs(title = paste("Benchmarked", pov_lab$short, "Changes Between",
                                years_keep[1], "and", years_keep[2]))
  print(.bench_plot)
  ggsave(here::here("outputs", "figures", "mfh_change_bench_preview.png"),
         .bench_plot, width = 12, height = 6, dpi = 150)
  print(comp12_bench_obj$df %>% head() %>% kable())
}

if (is.null(comp12_bench_obj)) {
  cat("Benchmarked change analysis not available ",
      "(benchmarking not applied or MCPE not computed).\n")
}


if (exists("comp12_obj") && !is.null(comp12_obj)) {
  p_change_unbench <- ggplot(comp12_obj$df, aes(x = domain, y = diff, color = significant)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(size = 2.5, alpha = 0.7) +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.3, alpha = 0.5) +
    scale_color_manual(
      values = c("Not Significant" = "gray60", "Significant" = "red"),
      name = "Statistical\nSignificance"
    ) +
    labs(
      title = paste(pov_lab$short, "Changes Between", years_keep[1], "and", years_keep[2],
                    "(Without Benchmarking)"),
      subtitle = paste0(diag_model, " MCPE-based with 95% Confidence Intervals"),
      x = "Domain",
      y = paste0("Change in ", pov_lab$short, " (", years_keep[2], " - ", years_keep[1], ")"),
      caption = "Red points indicate statistically significant changes (\u03b1 = 0.05)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right"
    )
  print(p_change_unbench)
  ggsave(here::here("outputs", "figures", "mfh_change_unbench_points.png"),
         p_change_unbench, width = 12, height = 6, dpi = 150)
}


if (exists("comp12_obj") && !is.null(comp12_obj)) {
  p_change_unbench_hist <- ggplot(comp12_obj$df, aes(x = diff, fill = significant)) +
    geom_histogram(bins = 20, alpha = 0.7, color = "black") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
    scale_fill_manual(
      values = c("Not Significant" = "lightblue", "Significant" = "coral"),
      name = "Statistical\nSignificance"
    ) +
    labs(
      title = paste0("Distribution of ", pov_lab$short, " Changes (Without Benchmarking)"),
      x = paste0("Change in ", pov_lab$short, " (", years_keep[2], " - ", years_keep[1], ")"),
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))
  print(p_change_unbench_hist)
  ggsave(here::here("outputs", "figures", "mfh_change_unbench_hist.png"),
         p_change_unbench_hist, width = 10, height = 6, dpi = 150)
}


if (exists("comp12_bench_obj") && !is.null(comp12_bench_obj)) {
  p_change_bench <- ggplot(comp12_bench_obj$df, aes(x = domain, y = diff, color = significant)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(size = 2.5, alpha = 0.7) +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.3, alpha = 0.5) +
    scale_color_manual(
      values = c("Not Significant" = "gray60", "Significant" = "red"),
      name = "Statistical\nSignificance"
    ) +
    labs(
      title = paste(pov_lab$short, "Changes Between", years_keep[1], "and", years_keep[2],
                    "(With Benchmarking)"),
      subtitle = paste0("Benchmarked ", diag_model, " MCPE-based with 95% Confidence Intervals"),
      x = "Domain",
      y = paste0("Change in ", pov_lab$short, " (", years_keep[2], " - ", years_keep[1], ")"),
      caption = "Red points indicate statistically significant changes (\u03b1 = 0.05)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right"
    )
  print(p_change_bench)
  ggsave(here::here("outputs", "figures", "mfh_change_bench_points.png"),
         p_change_bench, width = 12, height = 6, dpi = 150)
}


if (exists("comp12_bench_obj") && !is.null(comp12_bench_obj)) {
  p_change_bench_hist <- ggplot(comp12_bench_obj$df, aes(x = diff, fill = significant)) +
    geom_histogram(bins = 20, alpha = 0.7, color = "black") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
    scale_fill_manual(
      values = c("Not Significant" = "lightblue", "Significant" = "coral"),
      name = "Statistical\nSignificance"
    ) +
    labs(
      title = paste0("Distribution of ", pov_lab$short, " Changes (With Benchmarking)"),
      x = paste0("Change in ", pov_lab$short, " (", years_keep[2], " - ", years_keep[1], ")"),
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))
  print(p_change_bench_hist)
  ggsave(here::here("outputs", "figures", "mfh_change_bench_hist.png"),
         p_change_bench_hist, width = 10, height = 6, dpi = 150)
}



if (exists("comp12_obj") && !is.null(comp12_obj) &&
    exists("comp12_bench_obj") && !is.null(comp12_bench_obj)) {

  comparison_df <- tibble(
    domain       = comp12_obj$df$domain,
    sig_unbench  = comp12_obj$df$significant,
    sig_bench    = comp12_bench_obj$df$significant,
    diff_unbench = comp12_obj$df$diff,
    diff_bench   = comp12_bench_obj$df$diff
  ) %>%
    mutate(
      status = case_when(
        sig_unbench == "Significant" & sig_bench == "Significant"         ~ "Significant in both",
        sig_unbench == "Not Significant" & sig_bench == "Not Significant" ~ "Not significant in either",
        sig_unbench == "Significant" & sig_bench == "Not Significant"     ~ "Significant only without benchmarking",
        sig_unbench == "Not Significant" & sig_bench == "Significant"     ~ "Significant only with benchmarking"
      )
    )

  cat(sprintf("=== Impact of Benchmarking on %s Significance Conclusions ===\n\n",
              diag_model))
  status_tbl <- table(comparison_df$status)
  for (s in names(status_tbl)) {
    cat(sprintf("  %s: %d domains\n", s, status_tbl[s]))
  }

  # Descriptive statistics comparison
  desc_stats <- data.frame(
    Statistic = c("Mean difference", "Median difference", "Std. deviation",
                  "Minimum", "Maximum",
                  "Significant domains"),
    Unbench = c(
      mean(comp12_obj$df$diff), median(comp12_obj$df$diff), sd(comp12_obj$df$diff),
      min(comp12_obj$df$diff), max(comp12_obj$df$diff),
      sum(comp12_obj$df$significant == "Significant")
    ),
    Bench = c(
      mean(comp12_bench_obj$df$diff), median(comp12_bench_obj$df$diff),
      sd(comp12_bench_obj$df$diff),
      min(comp12_bench_obj$df$diff), max(comp12_bench_obj$df$diff),
      sum(comp12_bench_obj$df$significant == "Significant")
    )
  )
  names(desc_stats)[2:3] <- c(diag_model, paste0(diag_model, "_Bench"))
  cat("\n")
  suppressWarnings({
    print(kable(desc_stats, digits = 6,
                col.names = c("Statistic",
                              paste0(diag_model, " (Unbenchmarked)"),
                              paste0(diag_model, " Benchmarked")),
                caption = paste0("Descriptive Statistics: ", pov_lab$short, " Changes")))
  })

  # Show domains where significance changed
  changed <- comparison_df %>% filter(sig_unbench != sig_bench)
  if (nrow(changed) > 0) {
    cat("\nDomains where benchmarking changed the significance conclusion:\n")
    suppressWarnings({
      print(kable(changed %>% select(domain, diff_unbench, diff_bench, status),
                  digits = 4,
                  col.names = c("Domain", "Diff (Unbenchmarked)", "Diff (Benchmarked)", "Status"),
                  caption = "Domains with Different Significance Conclusions"))
    })
  } else {
    cat("\nBenchmarking did not change any significance conclusions.\n")
  }
} else {
  cat("Comparison requires both benchmarked and unbenchmarked change analysis.\n")
}



plots <- lapply(X = years_keep,
       FUN = function(x){

         shp_dt |>
           merge(db_wide_all %>%
                   filter(year == x) |>
                   select(domain, modelpov = all_of(rate_col)),
                 by = "domain") |>
           ggplot() +
           geom_sf(aes(fill = modelpov), color = NA) +
            scale_fill_viridis(
              name = pov_lab$short,
              option = "magma",
              direction = -1,
              limits = range(db_wide_all[[rate_col]], na.rm = TRUE)
            ) +
            theme_minimal(base_size = 16) +
            labs(
              title = paste0("Spatial Distribution of ", pov_lab$short,
                              " (", diag_model, ") ", x),
              caption = "Data source: Author Calculation"
            ) +
            theme(
              legend.position = "bottom",
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank()
            )
       })
for (i in seq_along(plots)) {
  ggsave(here::here("outputs", "figures", paste0("mfh_map_y", i, ".png")),
         plots[[i]], width = 12, height = 10, dpi = 150)
}



### lets work with our shapefile
# Extract poverty rates from the selected model
longpov_dt <-
  db_wide_all |>
  select(domain, year, modelpov = all_of(rate_col))

### lets perform a regression for each area of poverty rates against year 
### and then we divide the slope variable by the average poverty rate

shp_dt$growth_rate <- 
  longpov_dt |>
  group_split(domain) %>%
  lapply(X = .,
         FUN = function(x){
           
           y <- lm(modelpov ~ year, data = x)  # Simplified formula
           
           y <- coef(y)[2]
           
           delta <- y / (mean(x$modelpov, na.rm = TRUE))
           
           return(delta)
           
         }) |>
  unlist() |>
  unname()




# Cap growth rates at 1 to reduce the influence of extreme outliers
shp_dt <- shp_dt |>
  mutate(growth_rate_capped = pmin(growth_rate, 1))

p_growth <- ggplot(shp_dt) +
  geom_sf(aes(fill = growth_rate_capped), color = NA) +
  scale_fill_viridis(
    name   = "Growth Rate (capped at 1)",
    limits = c(min(shp_dt$growth_rate_capped, na.rm = TRUE), 0.5),
    oob    = squish,
    option = "magma"
  ) +
  labs(
    title    = "Spatial Distribution of Poverty Growth Rates",
    subtitle = "Growth rates capped at 1 to reduce outlier influence",
    caption  = "Data source: Author calculation"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 12),
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    panel.grid      = element_blank()
  )
print(p_growth)
ggsave(here::here("outputs", "figures", "mfh_growth_rate_map.png"),
       p_growth, width = 12, height = 10, dpi = 150)




# ============================================================
# Save Results: Export estimates and comparison tables
#
# This chunk creates two output files:
# 1. comparison_final.csv: MCPE-based differences with selected
#    model (`diag_model` = MFH1/MFH2/MFH3) and direct estimates
#    by year.
# 2. pov_mfh.xlsx: Complete estimation results (all models,
#    all years, with MSEs and CVs)
# ============================================================

# Back-transform / rebenchmark for mean-welfare-with-log was applied
# earlier (right after `db_wide_all` and `bench_result` were finalized,
# before any plotting chunks read those objects). See the
# back-transform-mean-welfare chunk near the bottom of the
# benchmarking section.

# ---- Prepare comparison table ----
# Reshape selected model and direct estimates from long to wide format by year
model_wide <- db_wide_all %>%
  select(domain, year, all_of(rate_col)) %>%
  pivot_wider(
    names_from = year,
    values_from = all_of(rate_col),
    names_prefix = paste0(tolower(diag_model), "_")
  )

direct_wide <- db_wide_all %>%
  select(domain, year, direct_rate) %>%
  pivot_wider(
    names_from = year,
    values_from = direct_rate,
    names_prefix = "direct_"
  )

# Merge MCPE differences with year-specific estimates. MCPE is now
# produced for every MFH variant (MFH1/MFH2/MFH3) by pbmcpe_with_existing(),
# so comparison_final gets the full diff/mse/lb/ub/significant columns
# whenever `comp12_obj` is available.
if (exists("comp12_obj") && !is.null(comp12_obj) && is.data.frame(comp12_obj$df)) {
  comparison_final <- comp12_obj$df %>%
    left_join(model_wide, by = "domain") %>%
    left_join(direct_wide, by = "domain")
} else {
  # The MCPE-based change analysis (`comp12_obj`) is not available. With
  # `var_choice = "direct"` this is an expected outcome: the raw direct
  # variances are noisy enough that `eblupMFH2()` does not converge inside
  # the parametric bootstrap, so MCPE cannot be computed. We write
  # `comparison_final.csv` with NA placeholders for the change columns
  # (diff / mse / lb / ub / significant) so that the Comparison report
  # still renders with a clear "not available" message rather than a
  # cryptic schema error.
  cat(
    "\n**Note:** the MCPE-based change analysis is not available for this run. ",
    "When `var_choice = \"direct\"`, `eblupMFH2()` often does not converge inside the ",
    "parametric bootstrap because the raw direct sampling variances are unstable. ",
    "This is expected behaviour with `direct`; use `\"sm_out\"` or `\"sm_all\"` if you ",
    "need the change-significance analysis.\n\n",
    sep = ""
  )
  comparison_final <- model_wide %>%
    left_join(direct_wide, by = "domain") %>%
    mutate(
      diff        = NA_real_,
      mse         = NA_real_,
      lb          = NA_real_,
      ub          = NA_real_,
      significant = NA
    )
}

# ---- Add benchmarked estimates to comparison_final if available ----
if (do_benchmark && !is.null(bench_result) && "rate_Bench" %in% colnames(db_wide_all)) {
  bench_wide <- db_wide_all %>%
    select(domain, year, rate_Bench) %>%
    pivot_wider(
      names_from = year,
      values_from = rate_Bench,
      names_prefix = "bench_"
    )
  comparison_final <- comparison_final %>%
    left_join(bench_wide, by = "domain")
}

# ---- Back-transform diff and mse for mean-welfare-with-log ----
# comp12_obj$df reports diff = eblup_y2 - eblup_y1 and mse = MSE(diff)
# on the log scale. We re-express on the original scale using the
# back-transformed rate columns (already in db_wide_all by now) and
# the delta method:
#   diff_orig = est_y2 - est_y1
#   mse_orig  = est_y1^2 * mse_y1_log + est_y2^2 * mse_y2_log
#               - 2 * est_y1 * est_y2 * mcpe_log(y1, y2)
# When MCPE between years is unavailable we fall back to the
# independence approximation (cov = 0).
if (identical(indicator_type, "mean_welfare") && isTRUE(log_transform) &&
    "diff" %in% names(comparison_final) &&
    # Skip if the change-analysis chunk earlier already back-transformed
    # comp12_obj$df (and therefore comparison_final inherits EUR values).
    exists("comp12_obj") && !is.null(comp12_obj) &&
    !isTRUE(attr(comp12_obj$df, "log_back_transformed"))) {
  rate_y1_col <- paste0(tolower(diag_model), "_", y1)
  rate_y2_col <- paste0(tolower(diag_model), "_", y2)
  if (all(c(rate_y1_col, rate_y2_col) %in% names(comparison_final))) {
    est_y1 <- comparison_final[[rate_y1_col]]
    est_y2 <- comparison_final[[rate_y2_col]]
    # MSE columns (log scale) -- read back from db_wide_all if cached
    mse_lookup <- db_wide_all %>%
      select(domain, year, any_of(c("mse_MFH1", "mse_MFH2", "mse_MFH3"))) %>%
      pivot_longer(starts_with("mse_"), names_to = "model", values_to = "mse") %>%
      filter(model == paste0("mse_", diag_model))
    # Already back-transformed in db_wide_all, so we need to "unback" to log scale
    # by dividing by est^2. This is the inverse of the delta method we applied above.
    est_lookup <- db_wide_all %>%
      select(domain, year, all_of(rate_col))
    log_mse <- mse_lookup %>% left_join(est_lookup, by = c("domain", "year")) %>%
      mutate(mse_log = mse / pmax(.data[[rate_col]]^2, 1e-12)) %>%
      select(domain, year, mse_log) %>%
      pivot_wider(names_from = year, values_from = mse_log,
                  names_prefix = "mse_log_")
    comparison_final <- comparison_final %>% left_join(log_mse, by = "domain")
    mse_y1_log <- comparison_final[[paste0("mse_log_", y1)]]
    mse_y2_log <- comparison_final[[paste0("mse_log_", y2)]]
    # Cross-year MCPE on the LOG scale, taken straight from the
    # unbenchmarked bootstrap output so the back-transformed MSE of the
    # change is consistent with how `compare_mfh()` builds it on the log
    # scale (mse = mse_y1 + mse_y2 - 2*MCPE). Without this, the back-
    # transformed unbenchmarked MFH MSE silently treats the two years
    # as independent and is inflated relative to the benchmarked path,
    # which does carry the MCPE through. Match the column name to whatever
    # `mcpe_obj$mcpe` exposes (typically "(1,2)" but compare_mfh() also
    # accepts a single unnamed column).
    mcpe_y1y2_log <- if (exists("mcpe_obj") && !is.null(mcpe_obj$mcpe)) {
      mcpe_mat_unbench <- as.matrix(mcpe_obj$mcpe)
      target_col <- paste0("(", 1, ",", 2, ")")
      if (target_col %in% colnames(mcpe_mat_unbench)) {
        as.numeric(mcpe_mat_unbench[, target_col])
      } else if (ncol(mcpe_mat_unbench) == 1) {
        as.numeric(mcpe_mat_unbench[, 1])
      } else {
        rep(0, nrow(comparison_final))
      }
    } else {
      rep(0, nrow(comparison_final))
    }
    comparison_final$diff <- est_y2 - est_y1
    # Delta-method MSE of the change. When the MCPE term overshoots the
    # diagonal (sampling noise in the bootstrap, especially with a small
    # nB) the formula can return a negative or implausibly small value.
    # Previously we clamped to 0, which produced zero-width CIs and
    # spurious "significant" flags whenever diff was non-zero. Instead
    # we fall back to the independence-assumption MSE (cov = 0)
    # whenever the covariance-adjusted value is non-positive or smaller
    # than 1% of the independence MSE. This is conservative
    # (overestimates variance) but cannot manufacture false certainty.
    mse_indep    <- est_y1^2 * mse_y1_log + est_y2^2 * mse_y2_log
    mse_with_cov <- mse_indep - 2 * est_y1 * est_y2 * mcpe_y1y2_log
    mse_floor    <- 0.01 * mse_indep
    use_cov      <- is.finite(mse_with_cov) & mse_with_cov >= mse_floor
    comparison_final$mse <- ifelse(use_cov, mse_with_cov, mse_indep)
    n_fallback <- sum(!use_cov, na.rm = TRUE)
    if (n_fallback > 0) {
      cat(sprintf(
        "Note: %d of %d domain(s) had a non-positive or near-zero ",
        n_fallback, length(use_cov)
      ))
      cat("delta-method MSE for the change (likely bootstrap MCPE noise); ")
      cat("fell back to the independence-assumption MSE for those rows.\n")
    }
    if (all(c("lb", "ub") %in% names(comparison_final))) {
      se <- sqrt(comparison_final$mse)
      comparison_final$lb <- comparison_final$diff - 1.96 * se
      comparison_final$ub <- comparison_final$diff + 1.96 * se
      comparison_final$significant <- with(comparison_final,
                                           sign(lb) == sign(ub) & lb != 0)
    }
    attr(comparison_final, "log_back_transformed") <- TRUE
  }
}

# ---- Export results ----
write.csv(
  comparison_final,
  file = here::here("outputs", "tables", "comparison_final.csv"),
  row.names = FALSE
)

# Back-transform the benchmarked change analysis from log scale to the
# original currency scale (mean welfare with log_transform only). This
# mirrors the back-transform applied to comparison_final above. Without
# this step, comparison_final_bench.csv stays on the log scale while
# comparison_final.csv is in EUR -- leading to a spurious "MFH Benchmarked
# has many significant changes" pattern in the Comparison report
# because tiny log-scale `diff` values clear the |diff| > 1.96Â·sqrt(mse)
# threshold while large EUR-scale `diff` values do not.
if (exists("comp12_bench_obj") && !is.null(comp12_bench_obj) &&
    identical(indicator_type, "mean_welfare") && isTRUE(log_transform) &&
    # Skip if the bench-change-analysis chunk earlier already
    # back-transformed comp12_bench_obj$df.
    !isTRUE(attr(comp12_bench_obj$df, "log_back_transformed"))) {
  bench_rate_wide <- db_wide_all %>%
    select(domain, year, rate_Bench) %>%
    pivot_wider(names_from = year, values_from = rate_Bench,
                names_prefix = "bench_eur_")
  comp12_bench_obj$df <- comp12_bench_obj$df %>%
    left_join(bench_rate_wide, by = "domain")
  est_y1_eur <- comp12_bench_obj$df[[paste0("bench_eur_", y1)]]
  est_y2_eur <- comp12_bench_obj$df[[paste0("bench_eur_", y2)]]
  # Per-year log-scale MSE and the cross-year MCPE come straight from
  # the parametric bootstrap output (bench_result$* are log-scale because
  # the model was fitted on log(welfare); the back-transform earlier
  # touched db_wide_all, not bench_result).
  mse_y1_log  <- as.numeric(bench_result$mse_bench[, 1])
  mse_y2_log  <- as.numeric(bench_result$mse_bench[, 2])
  mcpe_log    <- if (!is.null(bench_result$mcpe_bench)) {
    as.numeric(bench_result$mcpe_bench[, 1])
  } else 0
  comp12_bench_obj$df$diff <- est_y2_eur - est_y1_eur
  # Same negative-variance guard as the unbenchmarked path: fall back to
  # the independence MSE when the covariance-adjusted value is non-
  # positive or smaller than 1% of the independence MSE, rather than
  # clamping to 0 (which would create zero-width CIs and false
  # significance).
  mse_indep_b    <- est_y1_eur^2 * mse_y1_log + est_y2_eur^2 * mse_y2_log
  mse_with_cov_b <- mse_indep_b - 2 * est_y1_eur * est_y2_eur * mcpe_log
  mse_floor_b    <- 0.01 * mse_indep_b
  use_cov_b      <- is.finite(mse_with_cov_b) & mse_with_cov_b >= mse_floor_b
  comp12_bench_obj$df$mse <- ifelse(use_cov_b, mse_with_cov_b, mse_indep_b)
  n_fb_bench <- sum(!use_cov_b, na.rm = TRUE)
  if (n_fb_bench > 0) {
    cat(sprintf(
      "Note: %d of %d benchmarked-change row(s) fell back to the independence-MSE (negative or near-zero covariance-adjusted variance).\n",
      n_fb_bench, length(use_cov_b)
    ))
  }
  se <- sqrt(comp12_bench_obj$df$mse)
  zq <- comp12_bench_obj$df$zq
  comp12_bench_obj$df$lb <- comp12_bench_obj$df$diff - zq * se
  comp12_bench_obj$df$ub <- comp12_bench_obj$df$diff + zq * se
  comp12_bench_obj$df$significant <- ifelse(
    comp12_bench_obj$df$lb > 0 | comp12_bench_obj$df$ub < 0,
    "Significant", "Not Significant"
  )
  attr(comp12_bench_obj$df, "log_back_transformed") <- TRUE
  cat("Benchmarked-change CSV back-transformed to EUR scale.\n")
}

# Export benchmarked change analysis if available
if (exists("comp12_bench_obj") && !is.null(comp12_bench_obj)) {
  write.csv(
    comp12_bench_obj$df,
    file = here::here("outputs", "tables", "comparison_final_bench.csv"),
    row.names = FALSE
  )
}

# Export complete model comparison results (now includes Bench columns).
# Use writexl rather than openxlsx -- the latter has been observed to
# produce workbooks where the sheet dimension is "A1" and relationships
# point to missing drawing parts even though the cell data is present,
# which causes some downstream readers to open an empty workbook.
# writexl writes a minimal, conformant XLSX that all common readers
# parse cleanly. Strip ggplot/Hmisc attributes that cause writexl to
# refuse columns of class "labelled" or with non-trivial attributes.
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
.write_xlsx_safe(db_wide_all, here::here("outputs", "data", "pov_mfh.xlsx"))

# ---- Display summary ----
cat("Results exported successfully:\n")
cat("  - comparison_final.csv:", nrow(comparison_final), "domains\n")
if (exists("comp12_bench_obj") && !is.null(comp12_bench_obj)) {
  cat("  - comparison_final_bench.csv:", nrow(comp12_bench_obj$df), "domains (benchmarked)\n")
}
cat("  - pov_mfh.xlsx:", nrow(db_wide_all), "domain-year combinations\n")
