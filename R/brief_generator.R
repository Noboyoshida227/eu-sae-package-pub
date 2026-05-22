# ============================================================
# brief_generator.R  --  Analysis brief generation for SAE
#
# Generates a structured analysis brief from diagnostics and
# benchmark summaries, optionally enriched by LLM commentary.
# Presents UFH and MFH results separately and compares them.
# ============================================================

# Null-coalesce operator (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

# ===========================================================================
# generate_data_note  --  Template-based data properties & initial assessment
#
# Purely deterministic: no LLM needed.
# ===========================================================================

#' Generate a data properties and initial assessment note
#'
#' @param validation  Output from validate_inputs()
#' @param var_map     Named list of variable mappings (domain, psu, welfare, etc.)
#' @param ufh_options Named list of UFH settings selected in the UI
#' @param mfh_options Named list of MFH settings selected in the UI
#' @param steps       Character vector of pipeline steps selected (e.g. c("UFH","MFH","Comparison"))
#' @param psu_consistent_user Logical or NULL -- user's answer to PSU consistency question
#' @return Character string (Markdown)
generate_data_note <- function(validation,
                               var_map,
                               ufh_options = NULL,
                               mfh_options = NULL,
                               steps       = c("UFH", "MFH"),
                               psu_consistent_user = NULL) {

  s <- validation$summary
  ln <- character()

  ln <- c(ln, "# Data Properties and Initial Assessment", "")

  # ---- 1. Observations ----
  ln <- c(ln, "## 1. Observations", "")
  ln <- c(ln, sprintf("- **Total observations:** %s", s$n_obs %||% "N/A"))
  ln <- c(ln, sprintf("- **Unit of observation:** household (survey micro-data)"))

  if (!is.null(s$years)) {
    ln <- c(ln, sprintf("- **Analysis years:** %s",
                         paste(s$years, collapse = ", ")))
    for (yr in as.character(s$years)) {
      n <- s$n_obs_per_year[[yr]] %||% "N/A"
      ln <- c(ln, sprintf("  - Year %s: %s observations", yr, n))
    }
  }
  ln <- c(ln, "")

  # ---- 2. Domain variable ----
  ln <- c(ln, "## 2. Domain Variable", "")
  domain_name <- var_map$domain %||% "domain"
  ln <- c(ln, sprintf("- **Domain variable:** `%s`", domain_name))
  ln <- c(ln, sprintf("- **Number of domains:** %s", s$n_domains %||% "N/A"))

  if (!is.null(s$n_domains_per_year)) {
    for (yr in names(s$n_domains_per_year)) {
      ln <- c(ln, sprintf("  - Year %s: %s domains", yr, s$n_domains_per_year[[yr]]))
    }
  }

  if (!is.null(s$domains_consistent_across_years)) {
    ln <- c(ln, sprintf("- **Domains consistent across years:** %s",
                         if (s$domains_consistent_across_years) "Yes" else "No"))
  }

  if (!is.null(s$domains_aligned)) {
    ln <- c(ln, sprintf("- **Domains aligned between survey and auxiliary data:** %s",
                         if (s$domains_aligned) "Yes" else "No"))
  }

  ln <- c(ln, sprintf("- **Domain size (observations):** min = %s, median = %s, max = %s",
                       s$min_domain_size %||% "N/A",
                       s$median_domain_size %||% "N/A",
                       s$max_domain_size %||% "N/A"))
  ln <- c(ln, "")

  # ---- 3. PSU distribution ----
  ln <- c(ln, "## 3. Primary Sampling Units (PSU)", "")
  psu_name <- var_map$psu %||% "psu"
  ln <- c(ln, sprintf("- **PSU variable:** `%s`", psu_name))
  ln <- c(ln, sprintf("- **PSUs per domain:** min = %s, mean = %s, max = %s",
                       s$psu_per_domain_min %||% "N/A",
                       s$psu_per_domain_mean %||% "N/A",
                       s$psu_per_domain_max %||% "N/A"))

  if (!is.null(s$psu_consistent_over_time)) {
    ln <- c(ln, sprintf("- **PSU codes identical across years (data check):** %s",
                         if (s$psu_consistent_over_time) "Yes" else "No"))
  }
  if (!is.null(psu_consistent_user)) {
    ln <- c(ln, sprintf("- **PSU codes consistent over time (user confirmed):** %s",
                         if (psu_consistent_user) "Yes" else "No"))
  }
  ln <- c(ln, "")

  # ---- 4. Validation flags ----
  ln <- c(ln, "## 4. Data Validation Flags", "")
  for (f in validation$flags) {
    ln <- c(ln, sprintf("- %s", f))
  }
  ln <- c(ln, "")

  # ---- 5. Selected analysis options ----
  ln <- c(ln, "## 5. Selected Analysis Options", "")
  ln <- c(ln, sprintf("- **Pipeline steps:** %s", paste(steps, collapse = ", ")))
  ln <- c(ln, "")

  if ("UFH" %in% steps && !is.null(ufh_options)) {
    ln <- c(ln, "### UFH Options")
    ufh_trans <- ufh_options$transformation %||% "N/A"
    ln <- c(ln, sprintf("- **Transformation:** %s", ufh_trans))
    # Bias correction (back-transformation) is meaningful for both
    # arcsin (integration-based 'bc') and log (Duan smearing 'bc_sm').
    # Variance-smoothing is only user-configurable when transformation
    # is 'no' -- arcsin and log both stabilize variances on their own
    # scale.
    if (ufh_trans %in% c("arcsin", "log")) {
      bt <- ufh_options$backtransformation
      if (is.null(bt) || (length(bt) == 1 && is.na(bt))) bt <- "none"
      ln <- c(ln, sprintf("- **Bias correction:** %s", bt))
    } else {
      vc <- ufh_options$var_choice
      if (is.null(vc) || (length(vc) == 1 && is.na(vc))) vc <- "sm_out"
      ln <- c(ln, sprintf("- **Variance option:** %s", vc))
    }
    has_y1 <- !is.null(ufh_options$candidate_vars_y1) && length(ufh_options$candidate_vars_y1) > 0
    has_y2 <- !is.null(ufh_options$candidate_vars_y2) && length(ufh_options$candidate_vars_y2) > 0
    if (has_y1 || has_y2) {
      ln <- c(ln, sprintf("- **Key covariates (Year 1):** %s",
                           if (has_y1) paste(ufh_options$candidate_vars_y1, collapse = ", ") else "(automatic selection)"))
      ln <- c(ln, sprintf("- **Key covariates (Year 2):** %s",
                           if (has_y2) paste(ufh_options$candidate_vars_y2, collapse = ", ") else "(automatic selection)"))
    } else {
      ln <- c(ln, "- **Key covariates:** (automatic selection)")
    }
    ln <- c(ln, "")
  }

  if ("MFH" %in% steps && !is.null(mfh_options)) {
    ln <- c(ln, "### MFH Options")
    # MFH transformation: only log/no are valid (arcsin never offered).
    # Default to 'no' if missing so older configs that didn't carry
    # this field still render cleanly.
    mfh_trans <- mfh_options$transformation %||% "no"
    ln <- c(ln, sprintf("- **Transformation:** %s", mfh_trans))
    if (identical(mfh_trans, "log")) {
      # Bias correction (back-transformation) is only meaningful under log.
      mfh_bt <- mfh_options$backtransformation
      if (is.null(mfh_bt) || (length(mfh_bt) == 1 && is.na(mfh_bt))) mfh_bt <- "none"
      ln <- c(ln, sprintf("- **Bias correction:** %s", mfh_bt))
    }
    ln <- c(ln, sprintf("- **Variance option:** %s", mfh_options$var_choice %||% "N/A"))
    ln <- c(ln, sprintf("- **Covariance option:** %s", mfh_options$cov_choice %||% "N/A"))
    ln <- c(ln, sprintf("- **Diagnostic model:** %s", mfh_options$diag_model %||% "N/A"))
    ln <- c(ln, sprintf("- **Fit MFH3:** %s",
                         if (isTRUE(mfh_options$fit_mfh3)) "Yes" else "No"))
    has_y1 <- !is.null(mfh_options$candidate_vars_y1) && length(mfh_options$candidate_vars_y1) > 0
    has_y2 <- !is.null(mfh_options$candidate_vars_y2) && length(mfh_options$candidate_vars_y2) > 0
    if (has_y1 || has_y2) {
      ln <- c(ln, sprintf("- **Key covariates (Year 1):** %s",
                           if (has_y1) paste(mfh_options$candidate_vars_y1, collapse = ", ") else "(automatic selection)"))
      ln <- c(ln, sprintf("- **Key covariates (Year 2):** %s",
                           if (has_y2) paste(mfh_options$candidate_vars_y2, collapse = ", ") else "(automatic selection)"))
    } else {
      ln <- c(ln, "- **Key covariates:** (automatic selection)")
    }
    ln <- c(ln, "")
  }

  ln <- c(ln, paste(rep("-", 60), collapse = ""))
  paste(ln, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Helper: format one model section (diagnostics + benchmarks + normality note)
# ---------------------------------------------------------------------------
format_model_section <- function(section_num, model_label, diagnostics,
                                 bench_summary) {
  lines <- character()
  lines <- c(lines, sprintf("%d. %s MODEL", section_num, model_label))
  lines <- c(lines, paste(rep("-", 40), collapse = ""))
  lines <- c(lines, "")

  # -- Diagnostics per year --
  lines <- c(lines, sprintf("   %s Diagnostics", model_label))
  for (yr_name in names(diagnostics)) {
    d <- diagnostics[[yr_name]]
    lines <- c(lines, sprintf("   Year: %s", d$year %||% yr_name))
    lines <- c(lines, sprintf("   Convergence: %s",
                               if (isTRUE(d$convergence)) "Yes" else "No"))
    lines <- c(lines, sprintf("   Domains:     %s", d$n_domains %||% "N/A"))

    re_p    <- d$re_shapiro_pvalue %||% NA
    re_pass <- isTRUE(d$re_shapiro_pass)
    lines <- c(lines, sprintf("   RE normality (Shapiro p): %.4f  [%s]",
                               re_p, if (re_pass) "PASS" else "FAIL"))

    resid_p    <- d$resid_shapiro_pvalue %||% NA
    resid_pass <- isTRUE(d$resid_shapiro_pass)
    lines <- c(lines, sprintf("   Resid normality (Shapiro p): %.4f  [%s]",
                               resid_p, if (resid_pass) "PASS" else "FAIL"))

    # Q-Q plot / normality assessment narrative
    lines <- c(lines, "")
    lines <- c(lines, sprintf("   Normality Assessment (%s, %s):", model_label, d$year %||% yr_name))
    if (!is.na(re_p) && !is.na(resid_p)) {
      if (re_pass && resid_pass) {
        lines <- c(lines,
          "   Both random-effect and residual Q-Q plots should align closely with the")
        lines <- c(lines, sprintf(
          "   diagonal, consistent with Shapiro-Wilk PASS results (RE p=%.4f, Resid p=%.4f).",
          re_p, resid_p))
        lines <- c(lines,
          "   The normality assumption appears well satisfied.")
      } else if (!re_pass && !resid_pass) {
        lines <- c(lines,
          "   Q-Q plots likely show departures from the diagonal for both random effects")
        lines <- c(lines, sprintf(
          "   and residuals. Shapiro-Wilk tests FAIL (RE p=%.4f, Resid p=%.4f).",
          re_p, resid_p))
        lines <- c(lines,
          "   The normality assumption is violated; consider model re-specification or",
          "   data transformations.")
      } else {
        fail_which <- if (!re_pass) "random effects" else "residuals"
        pass_which <- if (re_pass) "random effects" else "residuals"
        lines <- c(lines, sprintf(
          "   Q-Q plot for %s may show deviations from normality (Shapiro FAIL),",
          fail_which))
        lines <- c(lines, sprintf(
          "   while %s appear normally distributed (Shapiro PASS).",
          pass_which))
        lines <- c(lines,
          "   Investigate the source of non-normality in the failing component.")
      }
    } else {
      lines <- c(lines,
        "   Shapiro-Wilk results not available; review Q-Q plots in the HTML report",
        "   to visually assess normality of random effects and residuals.")
    }
    lines <- c(lines, "")
  }

  # -- Benchmark summary per year --
  lines <- c(lines, sprintf("   %s Benchmark Summary", model_label))
  for (yr_name in names(bench_summary)) {
    b <- bench_summary[[yr_name]]
    lines <- c(lines, sprintf("   Year: %s", yr_name))
    if (!is.null(b$estimate_range)) {
      lines <- c(lines, sprintf("   Estimate range: [%.4f, %.4f]",
                                 b$estimate_range[1], b$estimate_range[2]))
    }
    lines <- c(lines, sprintf("   Median estimate: %.4f", b$estimate_median %||% NA))
    lines <- c(lines, sprintf("   Median CV:       %.4f", b$cv_median %||% NA))
    lines <- c(lines, sprintf("   Max CV:          %.4f", b$cv_max %||% NA))
    if (!is.na(b$mse_median %||% NA)) {
      lines <- c(lines, sprintf("   Median MSE:      %.6f", b$mse_median))
    }
    if (!is.null(b$n_cv_above_25pct)) {
      lines <- c(lines, sprintf("   Domains with CV > 25%%: %d", b$n_cv_above_25pct))
    }
    lines <- c(lines, "")
  }

  lines
}

# ---------------------------------------------------------------------------
# Helper: build the UFH vs MFH comparison section
# ---------------------------------------------------------------------------
format_comparison_section <- function(section_num, ufh_bench, mfh_bench) {
  lines <- character()
  lines <- c(lines, sprintf("%d. UFH vs MFH COMPARISON", section_num))
  lines <- c(lines, paste(rep("-", 40), collapse = ""))
  lines <- c(lines, "")

  yr_names <- union(names(ufh_bench), names(mfh_bench))

  for (yr_name in yr_names) {
    ub <- ufh_bench[[yr_name]]
    mb <- mfh_bench[[yr_name]]
    lines <- c(lines, sprintf("   Year: %s", yr_name))

    # CV comparison
    ufh_cv_med <- ub$cv_median %||% NA
    mfh_cv_med <- mb$cv_median %||% NA
    lines <- c(lines, sprintf("   Median CV -- UFH: %.4f  |  MFH: %.4f",
                               ufh_cv_med, mfh_cv_med))

    ufh_cv_max <- ub$cv_max %||% NA
    mfh_cv_max <- mb$cv_max %||% NA
    lines <- c(lines, sprintf("   Max CV    -- UFH: %.4f  |  MFH: %.4f",
                               ufh_cv_max, mfh_cv_max))

    # MSE comparison
    ufh_mse <- ub$mse_median %||% NA
    mfh_mse <- mb$mse_median %||% NA
    lines <- c(lines, sprintf("   Median MSE -- UFH: %s  |  MFH: %s",
                               if (is.na(ufh_mse)) "N/A" else sprintf("%.6f", ufh_mse),
                               if (is.na(mfh_mse)) "N/A" else sprintf("%.6f", mfh_mse)))

    # Domains with high CV
    ufh_hicv <- ub$n_cv_above_25pct %||% NA
    mfh_hicv <- mb$n_cv_above_25pct %||% NA
    lines <- c(lines, sprintf("   Domains CV>25%% -- UFH: %s  |  MFH: %s",
                               if (is.na(ufh_hicv)) "N/A" else as.character(ufh_hicv),
                               if (is.na(mfh_hicv)) "N/A" else as.character(mfh_hicv)))

    # Recommendation
    if (!is.na(mfh_cv_med) && !is.na(ufh_cv_med)) {
      if (mfh_cv_med < ufh_cv_med) {
        lines <- c(lines,
          "   -> MFH achieves lower median CV, suggesting improved precision from",
          "      borrowing strength across time periods.")
      } else if (ufh_cv_med < mfh_cv_med) {
        lines <- c(lines,
          "   -> UFH achieves lower median CV; the multivariate extension may not",
          "      provide additional precision gains for this year.")
      } else {
        lines <- c(lines,
          "   -> Both models yield similar median CV.")
      }
    }

    if (!is.na(mfh_mse) && !is.na(ufh_mse)) {
      if (mfh_mse < ufh_mse) {
        lines <- c(lines,
          "   -> MFH has lower median MSE, indicating more accurate estimates.")
      } else if (ufh_mse < mfh_mse) {
        lines <- c(lines,
          "   -> UFH has lower median MSE for this year.")
      }
    }
    lines <- c(lines, "")
  }

  lines <- c(lines,
    "   Note: Review Q-Q plots and residual diagnostics in both HTML reports",
    "   (40-fh_v2.html for UFH, 50-mfh_v2.html for MFH) to visually",
    "   compare normality assumptions. The model with better-aligned Q-Q plots",
    "   and lower CV/MSE is generally preferred.",
    "")

  lines
}

#' Generate an analysis brief with separate UFH and MFH sections
#'
#' @param diagnostics   Named list of diagnostic lists (one per year).
#'                       Each element should have a $model_type field.
#' @param bench_summary Named list of benchmark summary lists (one per year)
#' @param input_flags   Output from validate_inputs()
#' @param llm           LLM assistant object from llm_assistant()
#' @param language      Language code for the brief
#' @param country       Country name
#' @param model_type    Model type string (e.g. "UFH", "MFH") -- used as
#'                       fallback when only one model was run
#' @param ufh_diagnostics  Optional: UFH-specific diagnostics (per year)
#' @param ufh_bench        Optional: UFH-specific benchmark summary (per year)
#' @param mfh_diagnostics  Optional: MFH-specific diagnostics (per year)
#' @param mfh_bench        Optional: MFH-specific benchmark summary (per year)
#' @return List with template_brief and optionally llm_brief
generate_analysis_brief <- function(diagnostics,
                                    bench_summary,
                                    input_flags,
                                    llm,
                                    language   = "en",
                                    country    = "Greece",
                                    model_type = "UFH",
                                    ufh_diagnostics = NULL,
                                    ufh_bench       = NULL,
                                    mfh_diagnostics = NULL,
                                    mfh_bench       = NULL) {

  has_both <- !is.null(ufh_diagnostics) && !is.null(mfh_diagnostics)

  # --- Build template brief from local data ---
  lines <- character()
  lines <- c(lines, sprintf("ANALYSIS BRIEF: %s -- Small Area Estimation", country))
  lines <- c(lines, paste(rep("=", 60), collapse = ""))
  lines <- c(lines, "")

  # Section 1: Input data summary
  lines <- c(lines, "1. INPUT DATA")
  lines <- c(lines, paste(rep("-", 40), collapse = ""))
  if (!is.null(input_flags)) {
    for (f in input_flags$flags) {
      lines <- c(lines, sprintf("   %s", f))
    }
    if (!is.null(input_flags$summary$n_obs)) {
      lines <- c(lines, sprintf("   Observations: %d", input_flags$summary$n_obs))
    }
    if (!is.null(input_flags$summary$n_domains)) {
      lines <- c(lines, sprintf("   Domains: %d", input_flags$summary$n_domains))
    }
  }
  lines <- c(lines, "")

  sec <- 2

  if (has_both) {
    # --- Separate UFH section ---
    lines <- c(lines, format_model_section(sec, "UFH (Univariate Fay-Herriot)",
                                           ufh_diagnostics, ufh_bench))
    sec <- sec + 1

    # --- Separate MFH section ---
    lines <- c(lines, format_model_section(sec, "MFH (Multivariate Fay-Herriot)",
                                           mfh_diagnostics, mfh_bench))
    sec <- sec + 1

    # --- Comparison section ---
    lines <- c(lines, format_comparison_section(sec, ufh_bench, mfh_bench))
    sec <- sec + 1

  } else {
    # Single-model fallback (backwards compatible)
    lines <- c(lines, format_model_section(sec, model_type,
                                           diagnostics, bench_summary))
    sec <- sec + 1
  }

  lines <- c(lines, paste(rep("=", 60), collapse = ""))
  template_brief <- paste(lines, collapse = "\n")

  # --- LLM-enriched brief (optional) ---
  llm_brief <- NULL
  if (!is.null(llm) && isTRUE(llm$enabled)) {
    if (has_both) {
      prompt <- paste(
        "Based on the following SAE analysis brief that presents BOTH UFH and MFH",
        "model results, provide a detailed interpretation. Structure your response as:\n\n",
        "## UFH (Univariate Fay-Herriot) Assessment\n",
        "- Comment on convergence, normality (Shapiro-Wilk results and what Q-Q plots",
        "  would show), benchmark quality (CV, MSE), and any domains of concern.\n\n",
        "## MFH (Multivariate Fay-Herriot) Assessment\n",
        "- Same structure as UFH. Note whether borrowing strength across time periods",
        "  improved precision.\n\n",
        "## Which Model is Preferred?\n",
        "- Compare UFH vs MFH on: (1) normality of residuals and random effects,",
        "  (2) precision (CV), (3) accuracy (MSE), (4) number of problematic domains.",
        "- State which approach is better for this dataset and why.\n",
        "- Note any caveats (e.g., one model may be better for one year but not another).\n\n",
        "## Recommendations\n",
        "- Actionable next steps for the analyst.\n\n",
        template_brief
      )
    } else {
      prompt <- paste(
        "Based on the following SAE analysis brief, provide a concise",
        "interpretation with recommendations. Focus on:",
        "- Key findings and any diagnostic concerns",
        "- Whether the model assumptions appear satisfied (discuss Q-Q plots and normality)",
        "- Domains that may need attention (high CV, etc.)",
        "- Actionable next steps for the analyst\n\n",
        template_brief
      )
    }
    llm_brief <- llm$query(prompt)
  }

  list(
    template_brief = template_brief,
    llm_brief      = llm_brief
  )
}
