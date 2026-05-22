# ============================================================
# indicator_helpers.R
#
# Single source of truth for the "indicator" the FH/MFH model
# is fitted on. Two indicator families are supported:
#
#   * "poverty"      -- FGT(0/1/2) computed from welfare + povline.
#                      LHS lives on [0, 1]; arcsin transform is
#                      available; benchmarking ratios stay on the
#                      original scale.
#   * "mean_welfare" -- population-weighted mean of the welfare
#                      variable. Optionally fitted on the log scale
#                      and back-transformed (with Duan smearing)
#                      for display, benchmarking, and significance
#                      tests on changes in means.
#
# All UFH/MFH/Comparison code should call these helpers rather
# than hard-coding indicator-specific behaviour or labels.
# ============================================================

# ----- compatibility shim for fgt_label() ----------------------------------
# Some callers (notably the Quarto reports) use fgt_label() directly. Keep
# that exported symbol but route everything through indicator_label() so the
# label table lives in one place.
if (!exists("fgt_label", mode = "function")) {
  fgt_label <- function(alpha) {
    alpha <- as.integer(alpha %||% 0L)
    switch(as.character(alpha),
      "0" = list(fgt = "FGT(0)", short = "Headcount ratio",
                 noun_singular = "poverty rate",
                 noun_plural   = "poverty rates",
                 axis_label    = "Poverty rate"),
      "1" = list(fgt = "FGT(1)", short = "Poverty gap",
                 noun_singular = "poverty gap",
                 noun_plural   = "poverty gaps",
                 axis_label    = "Poverty gap"),
      "2" = list(fgt = "FGT(2)", short = "Poverty severity",
                 noun_singular = "poverty severity",
                 noun_plural   = "poverty severity values",
                 axis_label    = "Poverty severity"),
      list(fgt = "FGT(0)", short = "Headcount ratio",
           noun_singular = "poverty rate",
           noun_plural   = "poverty rates",
           axis_label    = "Poverty rate")
    )
  }
}

#' Label and metadata for the chosen indicator
#'
#' Returns a list with display names and units used everywhere
#' downstream (axis labels, table headers, LLM prompts, file
#' naming).
#'
#' @param indicator_type "poverty" or "mean_welfare"
#' @param fgt_alpha integer 0/1/2; only used for the poverty branch
#' @param log_transform logical; only used for the mean_welfare branch
#' @param currency_symbol short currency unit shown next to numbers
indicator_label <- function(indicator_type = "poverty",
                            fgt_alpha = 0L,
                            log_transform = FALSE,
                            currency_symbol = "EUR") {
  indicator_type <- match.arg(indicator_type, c("poverty", "mean_welfare"))
  if (indicator_type == "poverty") {
    out <- fgt_label(fgt_alpha)
    out$indicator_type   <- "poverty"
    out$is_rate          <- TRUE
    out$units            <- ""
    out$short_indicator  <- "pov"
    out$value_format_fn  <- function(x) format(round(x, 4), nsmall = 4, trim = TRUE)
    return(out)
  }
  # mean_welfare
  unit_suffix <- if (nzchar(currency_symbol)) paste0(" (", currency_symbol, ")") else ""
  list(
    fgt              = NA_character_,
    short            = if (log_transform) "Mean welfare (log-fit, back-transformed)" else "Mean welfare",
    noun_singular    = "mean welfare",
    noun_plural      = "mean welfare values",
    axis_label       = paste0("Mean welfare", unit_suffix),
    indicator_type   = "mean_welfare",
    is_rate          = FALSE,
    units            = currency_symbol,
    short_indicator  = "mean",
    log_transform    = isTRUE(log_transform),
    value_format_fn  = function(x) format(round(x, 1), nsmall = 1, big.mark = ",", trim = TRUE)
  )
}

#' Build the per-observation target column the model is fitted on
#'
#' For "poverty" this returns the FGT(α) indicator. For
#' "mean_welfare" it returns either welfare or log(welfare) depending
#' on `log_transform`. Zero/negative welfare values are NA-ed when
#' log-transforming and a warning is issued.
#'
#' @param survey_dt Data frame with at least `welfare` and (for
#'   poverty) `povline`.
#' @param indicator_type "poverty" or "mean_welfare"
#' @param fgt_alpha integer 0/1/2 for poverty
#' @param log_transform logical, mean_welfare only
#' @param compute_fgt FGT helper from ufh_functions.R; passed in to
#'   avoid an order-of-source dependency.
#' @return Numeric vector aligned with `survey_dt`.
build_target_column <- function(survey_dt,
                                indicator_type = "poverty",
                                fgt_alpha = 0L,
                                log_transform = FALSE,
                                compute_fgt_fn = NULL) {
  indicator_type <- match.arg(indicator_type, c("poverty", "mean_welfare"))
  if (indicator_type == "poverty") {
    if (is.null(compute_fgt_fn)) {
      if (exists("compute_fgt", mode = "function")) {
        compute_fgt_fn <- get("compute_fgt", mode = "function")
      } else {
        stop("build_target_column: compute_fgt() not available; pass compute_fgt_fn explicitly.")
      }
    }
    return(compute_fgt_fn(survey_dt$welfare, survey_dt$povline, fgt_alpha))
  }
  # mean_welfare branch
  w <- as.numeric(survey_dt$welfare)
  if (isTRUE(log_transform)) {
    bad <- !is.na(w) & w <= 0
    if (any(bad)) {
      warning(sprintf(
        "%d observation(s) have welfare <= 0 and were NA-ed before log transform.",
        sum(bad)
      ), call. = FALSE)
      w[bad] <- NA_real_
    }
    return(log(w))
  }
  w
}

#' Whether arcsin transformation is sensible for the chosen indicator
#'
#' Arcsin is only valid on [0, 1]. This is used to gate the UFH
#' transformation option.
indicator_supports_arcsin <- function(indicator_type = "poverty") {
  identical(indicator_type, "poverty")
}

#' Back-transform an EBLUP fit on the log scale to the original scale
#'
#' Uses Duan's smearing estimator when `residuals_log` is supplied,
#' otherwise falls back to the parametric correction
#' \eqn{\exp(\hat\eta + \frac{1}{2}\hat\sigma^{2})}.
#' Variance is propagated by the delta method:
#' \eqn{\widehat{\mathrm{Var}}(Y) \approx (\partial g)^{2}\,\widehat{\mathrm{Var}}(\eta)}
#' with \eqn{g(\eta) = \exp(\eta)}.
#'
#' @param eblup_log EBLUP on the log scale (numeric vector or matrix).
#' @param mse_log MSE on the log scale (same shape as `eblup_log`).
#' @param residuals_log optional vector of model residuals on the log
#'   scale, used to compute Duan's smearing factor
#'   \eqn{\frac{1}{n}\sum_i \exp(\hat\varepsilon_i)}.
#' @param sigma2_log optional scalar variance of residuals; only used
#'   if `residuals_log` is NULL.
#' @return List with `estimate` and `mse` on the original scale.
back_transform_log <- function(eblup_log, mse_log = NULL,
                               residuals_log = NULL, sigma2_log = NULL) {
  smear <- if (!is.null(residuals_log) && length(residuals_log) > 0) {
    mean(exp(residuals_log[is.finite(residuals_log)]), na.rm = TRUE)
  } else if (!is.null(sigma2_log) && is.finite(sigma2_log)) {
    exp(0.5 * as.numeric(sigma2_log))
  } else {
    1
  }
  est <- smear * exp(eblup_log)
  mse <- if (!is.null(mse_log)) (est)^2 * mse_log else NULL
  list(estimate = est, mse = mse, smearing_factor = smear)
}

#' MSE / variance of the difference between two estimates
#'
#' For UFH (univariate FH) the cross-time covariance is taken to be 0
#' (independent fits per year). For MFH the bootstrap MCPE between
#' time pairs is available and should be passed in.
#'
#' Used to test "significant change" in any indicator (rate, mean,
#' or back-transformed mean) without duplicating logic across the
#' pipeline.
mse_of_change <- function(mse_t1, mse_t2, mcpe_t1_t2 = 0) {
  pmax(mse_t1 + mse_t2 - 2 * mcpe_t1_t2, 0)
}

#' Build a friendly indicator-aware filename suffix
#'
#' Used so that running the package twice with different indicators
#' does not silently overwrite results in output/.
indicator_filename_tag <- function(indicator_type = "poverty",
                                   fgt_alpha = 0L,
                                   log_transform = FALSE) {
  if (identical(indicator_type, "poverty")) {
    return(paste0("pov_fgt", as.integer(fgt_alpha %||% 0L)))
  }
  if (isTRUE(log_transform)) "mean_log" else "mean"
}
