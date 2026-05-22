# ============================================================
# validation_checks.R  --  Input validation for SAE data
#
# Provides validate_inputs() which checks survey and auxiliary
# data for common issues before model fitting.
# ============================================================

#' Validate SAE input data
#'
#' @param survey_data data.frame with survey observations
#' @param aux_data    data.frame with auxiliary / covariate data
#' @return list with components:
#'   - flags: character vector of warning/info messages
#'   - summary: list of safe-to-share summary statistics
#'   - has_errors: logical, TRUE if any blocking issues found
validate_inputs <- function(survey_data, aux_data) {
  flags   <- character()
  summary <- list()


  # --- Survey data checks ---
  if (is.null(survey_data) || nrow(survey_data) == 0) {
    return(list(flags = "ERROR: survey_data is empty or NULL",
                summary = list(), has_errors = TRUE))
  }

  n_obs <- nrow(survey_data)
  summary$n_obs <- n_obs

  # Missing values
  na_counts <- colSums(is.na(survey_data))
  cols_with_na <- na_counts[na_counts > 0]
  if (length(cols_with_na) > 0) {
    for (col in names(cols_with_na)) {
      pct <- round(100 * cols_with_na[[col]] / n_obs, 1)
      flags <- c(flags, sprintf("WARNING: Column '%s' has %d missing values (%.1f%%)",
                                col, cols_with_na[[col]], pct))
    }
  }
  summary$missing_by_col <- as.list(cols_with_na)

  # Domain counts
  if ("domain" %in% names(survey_data)) {
    domain_tab <- table(survey_data$domain)
    summary$n_domains <- length(domain_tab)
    summary$min_domain_size <- min(domain_tab)
    summary$max_domain_size <- max(domain_tab)
    summary$median_domain_size <- median(as.numeric(domain_tab))

    small <- names(domain_tab[domain_tab < 5])
    if (length(small) > 0) {
      flags <- c(flags, sprintf("WARNING: %d domain(s) have fewer than 5 observations",
                                length(small)))
    }
  }

  # Per-year domain and observation counts
  if ("year" %in% names(survey_data)) {
    years <- sort(unique(survey_data$year))
    summary$years <- years
    summary$n_obs_per_year <- as.list(table(survey_data$year))

    if ("domain" %in% names(survey_data)) {
      domains_per_year <- lapply(years, function(yr) {
        sort(unique(survey_data$domain[survey_data$year == yr]))
      })
      names(domains_per_year) <- as.character(years)
      summary$domains_per_year <- domains_per_year
      summary$n_domains_per_year <- lapply(domains_per_year, length)

      # Domain consistency across years
      if (length(years) >= 2) {
        all_same <- all(vapply(domains_per_year[-1], function(d) {
          identical(d, domains_per_year[[1]])
        }, logical(1)))
        summary$domains_consistent_across_years <- all_same
        if (!all_same) {
          flags <- c(flags, "INFO: Domain set differs across years")
        }
      }
    }
  }

  # PSU distribution per domain
  if ("psu" %in% names(survey_data) && "domain" %in% names(survey_data)) {
    psu_per_domain <- tapply(survey_data$psu, survey_data$domain,
                             function(x) length(unique(x)))
    summary$psu_per_domain_min  <- min(psu_per_domain, na.rm = TRUE)
    summary$psu_per_domain_mean <- round(mean(psu_per_domain, na.rm = TRUE), 1)
    summary$psu_per_domain_max  <- max(psu_per_domain, na.rm = TRUE)

    # PSU consistency over time (same PSU codes appear in every year)
    if ("year" %in% names(survey_data) && length(summary$years) >= 2) {
      psus_per_year <- lapply(summary$years, function(yr) {
        sort(unique(survey_data$psu[survey_data$year == yr]))
      })
      psu_consistent <- all(vapply(psus_per_year[-1], function(p) {
        identical(p, psus_per_year[[1]])
      }, logical(1)))
      summary$psu_consistent_over_time <- psu_consistent
    }
  }

  # Welfare variable
  if ("welfare" %in% names(survey_data)) {
    welfare_clean <- survey_data$welfare[!is.na(survey_data$welfare)]
    if (length(welfare_clean) > 0) {
      summary$welfare_mean   <- round(mean(welfare_clean), 2)
      summary$welfare_sd     <- round(sd(welfare_clean), 2)
      summary$welfare_min    <- round(min(welfare_clean), 2)
      summary$welfare_max    <- round(max(welfare_clean), 2)
      if (any(welfare_clean < 0)) {
        flags <- c(flags, "INFO: Negative welfare values detected")
      }
    }
  }

  # --- Auxiliary data checks ---
  if (!is.null(aux_data) && nrow(aux_data) > 0) {
    summary$n_aux_vars <- ncol(aux_data) - 1  # exclude domain column

    # Zero-variance columns
    numeric_cols <- names(aux_data)[vapply(aux_data, is.numeric, logical(1))]
    numeric_cols <- setdiff(numeric_cols, "domain")
    zero_var <- character()
    for (col in numeric_cols) {
      if (sd(aux_data[[col]], na.rm = TRUE) == 0) {
        zero_var <- c(zero_var, col)
      }
    }
    if (length(zero_var) > 0) {
      flags <- c(flags, sprintf("WARNING: Zero-variance column(s) in auxiliary data: %s",
                                paste(zero_var, collapse = ", ")))
      summary$zero_var_cols <- zero_var
    }

    # Domain alignment (survey vs auxiliary)
    if ("domain" %in% names(survey_data) && "domain" %in% names(aux_data)) {
      survey_domains <- sort(unique(survey_data$domain))
      aux_domains    <- sort(unique(aux_data$domain))
      missing_in_aux    <- setdiff(survey_domains, aux_domains)
      missing_in_survey <- setdiff(aux_domains, survey_domains)
      summary$domains_aligned <- length(missing_in_aux) == 0 && length(missing_in_survey) == 0
      if (length(missing_in_aux) > 0) {
        flags <- c(flags, sprintf("WARNING: %d survey domain(s) missing from auxiliary data",
                                  length(missing_in_aux)))
      }
      if (length(missing_in_survey) > 0) {
        flags <- c(flags, sprintf("INFO: %d auxiliary domain(s) not in survey data",
                                  length(missing_in_survey)))
      }
    }
  }

  if (length(flags) == 0) {
    flags <- c("OK: All validation checks passed")
  }

  list(
    flags      = flags,
    summary    = summary,
    has_errors = FALSE
  )
}


# ============================================================
# assess_data_readiness  --  Pre-analysis readiness checks
#
# Performs four diagnostic tests on the three input datasets
# (survey, auxiliary covariates, geometries) to assess whether
# they are ready for UFH and MFH analysis.
# ============================================================

#' Assess data readiness for UFH / MFH analysis
#'
#' @param survey_data  Harmonised household survey data.frame
#'   (columns: domain, year, welfare, povline, weight, psu)
#' @param aux_data     Domain-level auxiliary covariates data.frame
#'   (columns: domain, year, plus numeric covariates)
#' @param geo_data     Geometry / spatial data.frame or sf object
#'   (column: domain)
#' @param domain_var   Name of the domain column in geo_data
#'   (default "domain"; set to "prov" when using raw geometries.rds)
#' @param save_to      If non-NULL, directory path where CSV results
#'   are written (created if needed). Default "output/Diagnostics".
#' @return A list with:
#'   - aux_summary: data.frame of covariate statistics
#'   - domain_consistency: list with alignment details
#'   - missing_poverty: data.frame of domains with missing rates
#'   - national_poverty: data.frame of national headcount rates
#'   - messages: character vector of diagnostic messages
assess_data_readiness <- function(survey_data,
                                  aux_data,
                                  geo_data       = NULL,
                                  domain_var     = "domain",
                                  save_to        = "output/Diagnostics",
                                  fgt_alpha      = 0L,
                                  indicator_type = "poverty",
                                  log_transform  = FALSE) {

  msgs <- character()

  # Indicator-aware noun used in messages and table column header.
  indicator_type <- match.arg(indicator_type, c("poverty", "mean_welfare"))
  fgt_noun <- if (identical(indicator_type, "mean_welfare")) {
    if (isTRUE(log_transform)) "log mean welfare" else "mean welfare"
  } else {
    switch(as.character(fgt_alpha),
      "0" = "poverty headcount rate",
      "1" = "poverty gap",
      "2" = "poverty severity",
      "poverty rate"
    )
  }

  # ------------------------------------------------------------------
  # 0. Derive per-observation target and domain-level target rates
  # ------------------------------------------------------------------
  if (identical(indicator_type, "mean_welfare")) {
    # NA-out non-positive welfare before log to avoid -Inf/NaN
    w <- as.numeric(survey_data$welfare)
    if (isTRUE(log_transform)) {
      w[!is.na(w) & w <= 0] <- NA_real_
      survey_data$poor <- log(w)
    } else {
      survey_data$poor <- w
    }
  } else if (fgt_alpha == 0L) {
    survey_data$poor <- as.integer(survey_data$welfare < survey_data$povline)
  } else {
    survey_data$poor <- pmax(0, (survey_data$povline - survey_data$welfare) /
                                 survey_data$povline)^fgt_alpha
  }

  years <- sort(unique(survey_data$year))

  # Domain-level target (weighted) per year. Column is named `poverty_rate`
  # for back-compat with downstream code, but for indicator_type ==
  # "mean_welfare" it actually holds the weighted mean (or mean log)
  # welfare per domain-year.
  pov_rates <- do.call(rbind, lapply(years, function(yr) {
    sv <- survey_data[survey_data$year == yr, ]
    domains <- unique(sv$domain)
    out <- data.frame(
      domain = domains,
      year   = yr,
      poverty_rate = NA_real_,
      n_obs  = NA_integer_,
      stringsAsFactors = FALSE
    )
    for (i in seq_along(domains)) {
      dd <- sv[sv$domain == domains[i], ]
      ok <- !is.na(dd$poor) & !is.na(dd$weight)
      out$poverty_rate[i] <- if (any(ok)) {
        stats::weighted.mean(dd$poor[ok], dd$weight[ok])
      } else NA_real_
      out$n_obs[i] <- nrow(dd)
    }
    out
  }))

  # ------------------------------------------------------------------
  # Test 1. Summary statistics for auxiliary covariates
  # ------------------------------------------------------------------
  id_cols  <- c("domain", "year", "provlab", "prov")
  aux_vars <- setdiff(names(aux_data), id_cols)
  aux_vars <- aux_vars[vapply(aux_data[aux_vars], is.numeric, logical(1))]

  # Merge auxiliary data with poverty rates for correlation
  aux_merged <- merge(aux_data, pov_rates[, c("domain", "year", "poverty_rate")],
                      by = c("domain", "year"), all.x = TRUE)

  aux_summary <- data.frame(
    variable    = aux_vars,
    mean        = NA_real_,
    se          = NA_real_,
    n_obs       = NA_integer_,
    cor_poverty = NA_real_,
    stringsAsFactors = FALSE
  )
  for (i in seq_along(aux_vars)) {
    v   <- aux_vars[i]
    x   <- aux_merged[[v]]
    ok  <- !is.na(x)
    n   <- sum(ok)
    aux_summary$n_obs[i] <- n
    if (n > 0) {
      aux_summary$mean[i] <- round(mean(x[ok]), 6)
      aux_summary$se[i]   <- round(sd(x[ok]) / sqrt(n), 6)
    }
    if (n > 2 && sum(!is.na(aux_merged$poverty_rate[ok])) > 2) {
      aux_summary$cor_poverty[i] <- round(
        stats::cor(x[ok], aux_merged$poverty_rate[ok], use = "complete.obs"),
        4
      )
    }
  }

  # Carry the indicator-aware label alongside the table so callers can
  # render a meaningful column header (the column itself stays named
  # `cor_poverty` for back-compat with the saved CSV and any downstream
  # consumers that read it by name).
  attr(aux_summary, "cor_target_label") <- sprintf("Corr. w/ %s",
    tools::toTitleCase(fgt_noun))

  msgs <- c(msgs, sprintf(
    "Test 1: Auxiliary covariate summary computed for %d variables across %d domain-year observations (correlation against %s).",
    length(aux_vars), nrow(aux_data), fgt_noun
  ))

  # ------------------------------------------------------------------
  # Test 2. Domain consistency across all three datasets
  # ------------------------------------------------------------------
  survey_domains <- sort(unique(survey_data$domain))
  aux_domains    <- sort(unique(aux_data$domain))

  domain_info <- list(
    survey_domains = survey_domains,
    aux_domains    = aux_domains,
    geo_domains    = NULL,
    in_survey_not_aux = setdiff(survey_domains, aux_domains),
    in_aux_not_survey = setdiff(aux_domains, survey_domains),
    in_survey_not_geo = NULL,
    in_geo_not_survey = NULL,
    all_consistent    = FALSE
  )

  if (!is.null(geo_data)) {
    geo_dom <- sort(unique(geo_data[[domain_var]]))
    domain_info$geo_domains       <- geo_dom
    domain_info$in_survey_not_geo <- setdiff(survey_domains, geo_dom)
    domain_info$in_geo_not_survey <- setdiff(geo_dom, survey_domains)
  }

  n_issues <- length(domain_info$in_survey_not_aux) +
              length(domain_info$in_aux_not_survey) +
              length(domain_info$in_survey_not_geo) +
              length(domain_info$in_geo_not_survey)
  domain_info$all_consistent <- (n_issues == 0)

  if (domain_info$all_consistent) {
    msgs <- c(msgs, sprintf(
      "Test 2: Domain variables are consistent across all datasets (%d domains).",
      length(survey_domains)
    ))
  } else {
    parts <- character()
    if (length(domain_info$in_survey_not_aux) > 0)
      parts <- c(parts, sprintf("%d survey domain(s) missing from auxiliary data",
                                length(domain_info$in_survey_not_aux)))
    if (length(domain_info$in_aux_not_survey) > 0)
      parts <- c(parts, sprintf("%d auxiliary domain(s) missing from survey data",
                                length(domain_info$in_aux_not_survey)))
    if (length(domain_info$in_survey_not_geo) > 0)
      parts <- c(parts, sprintf("%d survey domain(s) missing from geometry data",
                                length(domain_info$in_survey_not_geo)))
    if (length(domain_info$in_geo_not_survey) > 0)
      parts <- c(parts, sprintf("%d geometry domain(s) missing from survey data",
                                length(domain_info$in_geo_not_survey)))
    msgs <- c(msgs, paste("Test 2: Domain INCONSISTENCY detected:", paste(parts, collapse = "; ")))
  }

  # ------------------------------------------------------------------
  # Test 3. Missing poverty rates for any domain
  # ------------------------------------------------------------------
  # For each year, check which auxiliary domains lack a poverty rate
  missing_pov <- data.frame(
    domain = integer(), year = integer(),
    reason = character(), stringsAsFactors = FALSE
  )
  for (yr in years) {
    aux_yr     <- aux_data$domain[aux_data$year == yr]
    pov_yr     <- pov_rates[pov_rates$year == yr, ]
    # Domains in auxiliary data without a poverty rate
    no_rate    <- setdiff(aux_yr, pov_yr$domain)
    if (length(no_rate) > 0) {
      missing_pov <- rbind(missing_pov, data.frame(
        domain = no_rate, year = yr,
        reason = "no survey observations",
        stringsAsFactors = FALSE
      ))
    }
    # Domains with NA poverty rate (e.g., all welfare or povline missing)
    na_rate <- pov_yr$domain[is.na(pov_yr$poverty_rate)]
    if (length(na_rate) > 0) {
      missing_pov <- rbind(missing_pov, data.frame(
        domain = na_rate, year = yr,
        reason = "poverty rate is NA",
        stringsAsFactors = FALSE
      ))
    }
  }

  if (nrow(missing_pov) == 0) {
    msgs <- c(msgs, "Test 3: No missing poverty rates -- all domains have survey-based estimates.")
  } else {
    msgs <- c(msgs, sprintf(
      "Test 3: WARNING -- %d domain-year(s) have missing poverty rates.",
      nrow(missing_pov)
    ))
  }

  # ------------------------------------------------------------------
  # Test 4. National headline statistic, indicator-aware:
  #   - poverty:               weighted mean of `poor` (FGT(0))
  #   - mean welfare:          weighted mean of `welfare`
  #   - mean welfare (log fit): weighted mean of log(welfare) for w > 0
  # Previously this always used `sv$poor` regardless of indicator,
  # which produced NA for mean_welfare runs (the `poor` column is
  # absent or empty when no poverty line is configured).
  # ------------------------------------------------------------------
  is_mean    <- identical(indicator_type, "mean_welfare")
  is_log_mean <- is_mean && isTRUE(log_transform)
  national_pov <- do.call(rbind, lapply(years, function(yr) {
    sv <- survey_data[survey_data$year == yr, ]
    target <- if (is_log_mean) {
      # Drop non-positive welfare before logging (NA-ed). Domains where
      # all observations are non-positive contribute NA, but at the
      # national aggregate non-positive rows just drop out.
      w <- as.numeric(sv$welfare)
      ifelse(!is.na(w) & w > 0, log(w), NA_real_)
    } else if (is_mean) {
      as.numeric(sv$welfare)
    } else {
      sv$poor
    }
    nat <- if (any(!is.na(target) & !is.na(sv$weight))) {
      ok <- !is.na(target) & !is.na(sv$weight)
      stats::weighted.mean(target[ok], sv$weight[ok])
    } else {
      NA_real_
    }
    data.frame(
      year           = yr,
      national_rate  = round(nat, 6),
      n_households   = nrow(sv),
      n_domains      = length(unique(sv$domain)),
      stringsAsFactors = FALSE
    )
  }))

  # Format the headline value according to the indicator. For poverty
  # (FGT) it is a rate in [0, 1] shown as a percentage; for arithmetic
  # mean welfare it is a level in the configured currency; for log mean
  # welfare it is a raw log value with four decimals (multiplying by 100
  # would mislead).
  for (i in seq_len(nrow(national_pov))) {
    val <- national_pov$national_rate[i]
    formatted <- if (is_log_mean) {
      sprintf("%.4f (log)", val)
    } else if (is_mean) {
      format(round(val, 1), big.mark = ",", nsmall = 1, trim = TRUE)
    } else {
      sprintf("%.2f%%", val * 100)
    }
    msgs <- c(msgs, sprintf(
      "Test 4: National %s in %d = %s (%d households, %d domains).",
      fgt_noun, national_pov$year[i], formatted,
      national_pov$n_households[i], national_pov$n_domains[i]
    ))
  }

  # ------------------------------------------------------------------
  # Save to disk
  # ------------------------------------------------------------------
  if (!is.null(save_to)) {
    dir.create(save_to, showWarnings = FALSE, recursive = TRUE)
    utils::write.csv(aux_summary, file.path(save_to, "aux_covariate_summary.csv"),
                     row.names = FALSE)
    utils::write.csv(national_pov, file.path(save_to, "national_poverty.csv"),
                     row.names = FALSE)
    utils::write.csv(pov_rates, file.path(save_to, "domain_poverty_rates.csv"),
                     row.names = FALSE)
    if (nrow(missing_pov) > 0) {
      utils::write.csv(missing_pov, file.path(save_to, "missing_poverty.csv"),
                       row.names = FALSE)
    }
    # Domain consistency report
    consistency_df <- data.frame(
      check   = c("survey_domains", "aux_domains",
                   if (!is.null(geo_data)) "geo_domains" else NULL,
                   "all_consistent"),
      value   = c(length(domain_info$survey_domains),
                  length(domain_info$aux_domains),
                  if (!is.null(geo_data)) length(domain_info$geo_domains) else NULL,
                  domain_info$all_consistent),
      stringsAsFactors = FALSE
    )
    utils::write.csv(consistency_df, file.path(save_to, "domain_consistency.csv"),
                     row.names = FALSE)
    writeLines(msgs, file.path(save_to, "readiness_messages.txt"))
    msgs <- c(msgs, sprintf("Results saved to %s/", save_to))
  }

  # ------------------------------------------------------------------
  # Return
  # ------------------------------------------------------------------
  list(
    aux_summary        = aux_summary,
    domain_consistency = domain_info,
    missing_poverty    = missing_pov,
    national_poverty   = national_pov,
    domain_poverty     = pov_rates,
    messages           = msgs
  )
}
