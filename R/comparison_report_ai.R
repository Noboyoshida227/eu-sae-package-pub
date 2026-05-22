format_num <- function(x, digits = 4) {
  # Vectorised: works inside dplyr::transmute
  out <- ifelse(is.na(x), "NA",
                format(round(as.numeric(x), digits), nsmall = digits, trim = TRUE))
  if (length(out) == 0) return("NA")
  out
}

comparison_ai_sections <- function() {
  c(
    overview = "Overview",
    normality = "Normality Diagnostics",
    rates = "Poverty Rate Comparisons",
    precision = "MSE, RMSE, and CV Comparisons",
    change_significance = "Statistical Significance of Poverty Changes",
    poverty_maps = "Poverty Maps",
    change_maps = "Poverty Change Maps"
  )
}

html_escape_text <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

comparison_ai_language_label <- function(language = "en") {
  if (exists("language_label", mode = "function")) {
    return(language_label(language))
  }
  # Fallback table -- kept in sync with language_label() in R/llm_assistant.R
  # and supported_languages() in R/multilingual.R.
  labels <- c(
    en = "English",     fr = "French",      de = "German",
    es = "Spanish",     it = "Italian",     pt = "Portuguese",
    nl = "Dutch",       pl = "Polish",      ro = "Romanian",
    cs = "Czech",       sk = "Slovak",      sl = "Slovenian",
    hu = "Hungarian",   sv = "Swedish",     da = "Danish",
    fi = "Finnish",     et = "Estonian",    lv = "Latvian",
    lt = "Lithuanian",  mt = "Maltese",     ga = "Irish",
    hr = "Croatian",    bg = "Bulgarian",   el = "Greek",
    ar = "Arabic"
  )
  labels[[language]] %||% "English"
}

load_comparison_ai_data <- function() {
  .libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required for report commentary.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("tidyr", quietly = TRUE)) {
    stop("Packages 'dplyr' and 'tidyr' are required for report commentary.")
  }

  pov_fh <- readxl::read_excel("outputs/data/pov_fh.xlsx")
  pov_mfh <- readxl::read_excel("outputs/data/pov_mfh.xlsx")

  comparison_dt <- dplyr::left_join(
    dplyr::transmute(
      pov_mfh,
      domain = as.integer(domain),
      year = as.integer(year),
      Direct = direct_rate,
      Direct_MSE = direct_mse,
      Direct_CV = direct_cv,
      MFH = rate_MFH2,
      MFH_MSE = mse_MFH2,
      MFH_CV = cv_MFH2,
      MFH_Bench = if ("rate_Bench" %in% names(pov_mfh)) rate_Bench else rate_MFH2,
      MFH_Bench_MSE = if ("mse_Bench" %in% names(pov_mfh)) mse_Bench else mse_MFH2,
      MFH_Bench_CV = if ("cv_Bench" %in% names(pov_mfh)) cv_Bench else cv_MFH2
    ),
    dplyr::transmute(
      pov_fh,
      domain = as.integer(domain),
      year = as.integer(year),
      FH = FH,
      FH_MSE = FH_MSE,
      FH_CV = FH_CV,
      FH_Bench = FH_Bench,
      FH_Bench_MSE = FH_Bench_MSE,
      FH_Bench_CV = FH_Bench_CV
    ),
    by = c("domain", "year")
  )

  comparison_dt <- dplyr::mutate(
    comparison_dt,
    Direct_RMSE = sqrt(Direct_MSE),
    FH_RMSE = sqrt(FH_MSE),
    FH_Bench_RMSE = sqrt(FH_Bench_MSE),
    MFH_RMSE = sqrt(MFH_MSE),
    MFH_Bench_RMSE = sqrt(MFH_Bench_MSE)
  )

  sig_fh <- utils::read.csv("outputs/tables/statistical_significance_results_unbench.csv")
  sig_fh_bench <- utils::read.csv("outputs/tables/statistical_significance_results.csv")
  sig_mfh <- utils::read.csv("outputs/tables/comparison_final.csv")
  sig_mfh_bench <- utils::read.csv("outputs/tables/comparison_final_bench.csv")

  prepare_sig_tbl_local <- function(df, method_label, signif_true = c("TRUE", "Significant")) {
    dplyr::transmute(
      df,
      domain = as.integer(domain),
      diff = as.numeric(diff),
      mse = as.numeric(mse),
      lb = as.numeric(lb),
      ub = as.numeric(ub),
      significant = as.character(significant) %in% signif_true,
      method = method_label
    )
  }

  sig_plot_dt <- dplyr::bind_rows(
    prepare_sig_tbl_local(sig_fh, "FH"),
    prepare_sig_tbl_local(sig_fh_bench, "FH Benchmarked"),
    prepare_sig_tbl_local(sig_mfh, "MFH", signif_true = c("Significant")),
    prepare_sig_tbl_local(sig_mfh_bench, "MFH Benchmarked", signif_true = c("Significant"))
  )

  normality_diag <- if (file.exists("outputs/tables/normality_diagnostics.csv")) {
    utils::read.csv("outputs/tables/normality_diagnostics.csv")
  } else {
    # Fall back to old separate CSVs if combined file not available
    dplyr::bind_rows(
      if (file.exists("outputs/tables/ufh_shapiro_results.csv")) {
        ufh <- utils::read.csv("outputs/tables/ufh_shapiro_results.csv")
        dplyr::transmute(ufh, method = "FH", year = year, component = component, n = NA_integer_, W = W, p_value = p_value)
      },
      if (file.exists("outputs/tables/mfh_shapiro_results.csv")) {
        mfh <- utils::read.csv("outputs/tables/mfh_shapiro_results.csv")
        dplyr::transmute(mfh, method = "MFH", year = year, component = component, n = NA_integer_, W = W, p_value = p_value)
      }
    )
  }

  # Compute Q-Q correlations from raw values if available and not already in summary
  if (!is.null(normality_diag) && nrow(normality_diag) > 0 &&
      !("qq_correlation" %in% names(normality_diag)) &&
      file.exists("outputs/tables/normality_raw_values.csv")) {
    raw_vals <- tryCatch(utils::read.csv("outputs/tables/normality_raw_values.csv"), error = function(e) NULL)
    if (!is.null(raw_vals) && nrow(raw_vals) > 0) {
      qq_tbl <- dplyr::group_by(raw_vals, method, year, component) |>
        dplyr::summarise(
          qq_correlation = {
            v <- value[!is.na(value)]
            if (length(v) >= 3) {
              qq <- stats::qqnorm(v, plot.it = FALSE)
              stats::cor(qq$x, qq$y)
            } else NA_real_
          },
          .groups = "drop"
        )
      normality_diag <- dplyr::left_join(
        normality_diag, qq_tbl,
        by = c("method", "year", "component")
      )
    }
  }

  list(
    comparison = comparison_dt,
    normality_diag = normality_diag,
    significance = sig_plot_dt
  )
}

build_comparison_ai_prompts <- function(language = "en",
                                         indicator_type = "poverty",
                                         currency_symbol = "EUR",
                                         log_transform = FALSE) {
  dplyr <- asNamespace("dplyr")

  # Indicator-aware noun substitutions. For poverty the phrasing is
  # unchanged (back-compat); for mean welfare we substitute "mean
  # welfare" and the configured currency unit so the LLM does not
  # describe estimates as "poverty rates" when running on welfare.
  if (identical(indicator_type, "mean_welfare")) {
    indicator_noun        <- "mean welfare"
    indicator_noun_plural <- "mean welfare values"
    indicator_unit_short  <- currency_symbol
    indicator_unit_phrase <- paste0("on the ", currency_symbol, " currency scale",
                                     if (isTRUE(log_transform))
                                       " (back-transformed from a log-scale fit)" else "")
  } else {
    indicator_noun        <- "poverty rate"
    indicator_noun_plural <- "poverty rates"
    indicator_unit_short  <- "rate"
    indicator_unit_phrase <- "as a fraction between 0 and 1"
  }

  dat <- load_comparison_ai_data()
  comparison_dt <- dat$comparison
  sig_plot_dt <- dat$significance

  rate_accuracy <- dplyr$bind_rows(
    dplyr$transmute(comparison_dt, year, method = "FH", mean_abs_error = abs(FH - Direct), mean_benchmark_shift = abs(FH_Bench - FH)),
    dplyr$transmute(comparison_dt, year, method = "FH Benchmarked", mean_abs_error = abs(FH_Bench - Direct), mean_benchmark_shift = abs(FH_Bench - FH)),
    dplyr$transmute(comparison_dt, year, method = "MFH", mean_abs_error = abs(MFH - Direct), mean_benchmark_shift = abs(MFH_Bench - MFH)),
    dplyr$transmute(comparison_dt, year, method = "MFH Benchmarked", mean_abs_error = abs(MFH_Bench - Direct), mean_benchmark_shift = abs(MFH_Bench - MFH))
  ) |>
    dplyr$group_by(year, method) |>
    dplyr$summarise(
      mean_abs_error = mean(mean_abs_error, na.rm = TRUE),
      mean_benchmark_shift = mean(mean_benchmark_shift, na.rm = TRUE),
      .groups = "drop"
    )

  precision_tbl <- dplyr$bind_rows(
    dplyr$transmute(comparison_dt, year, method = "Direct", MSE = Direct_MSE, RMSE = Direct_RMSE, CV = Direct_CV),
    dplyr$transmute(comparison_dt, year, method = "FH", MSE = FH_MSE, RMSE = FH_RMSE, CV = FH_CV),
    dplyr$transmute(comparison_dt, year, method = "FH Benchmarked", MSE = FH_Bench_MSE, RMSE = FH_Bench_RMSE, CV = FH_Bench_CV),
    dplyr$transmute(comparison_dt, year, method = "MFH", MSE = MFH_MSE, RMSE = MFH_RMSE, CV = MFH_CV),
    dplyr$transmute(comparison_dt, year, method = "MFH Benchmarked", MSE = MFH_Bench_MSE, RMSE = MFH_Bench_RMSE, CV = MFH_Bench_CV)
  ) |>
    dplyr$group_by(year, method) |>
    dplyr$summarise(
      mean_mse = mean(MSE, na.rm = TRUE),
      mean_rmse = mean(RMSE, na.rm = TRUE),
      mean_cv = mean(CV, na.rm = TRUE),
      .groups = "drop"
    )

  benchmark_impact <- comparison_dt |>
    dplyr$group_by(year) |>
    dplyr$summarise(
      fh_mse_ratio = mean(FH_Bench_MSE / FH_MSE, na.rm = TRUE),
      mfh_mse_ratio = mean(MFH_Bench_MSE / MFH_MSE, na.rm = TRUE),
      fh_cv_change = mean(FH_Bench_CV - FH_CV, na.rm = TRUE),
      mfh_cv_change = mean(MFH_Bench_CV - MFH_CV, na.rm = TRUE),
      fh_rmse_ratio = mean(FH_Bench_RMSE / FH_RMSE, na.rm = TRUE),
      mfh_rmse_ratio = mean(MFH_Bench_RMSE / MFH_RMSE, na.rm = TRUE),
      fh_rmse_reduction_from_direct = 1 - mean(FH_RMSE, na.rm = TRUE) / mean(Direct_RMSE, na.rm = TRUE),
      mfh_rmse_reduction_from_direct = 1 - mean(MFH_RMSE, na.rm = TRUE) / mean(Direct_RMSE, na.rm = TRUE),
      .groups = "drop"
    )

  overview_tbl <- comparison_dt |>
    dplyr$group_by(year) |>
    dplyr$summarise(
      mean_direct = mean(Direct, na.rm = TRUE),
      mean_fh = mean(FH, na.rm = TRUE),
      mean_fh_bench = mean(FH_Bench, na.rm = TRUE),
      mean_mfh = mean(MFH, na.rm = TRUE),
      mean_mfh_bench = mean(MFH_Bench, na.rm = TRUE),
      mean_abs_fh_direct = mean(abs(FH - Direct), na.rm = TRUE),
      mean_abs_mfh_direct = mean(abs(MFH - Direct), na.rm = TRUE),
      .groups = "drop"
    )

  sig_counts <- sig_plot_dt |>
    dplyr$group_by(method) |>
    dplyr$summarise(
      significant_domains = sum(significant, na.rm = TRUE),
      total_domains = dplyr$n(),
      mean_abs_change = mean(abs(diff), na.rm = TRUE),
      .groups = "drop"
    )

  sig_overlap <- dplyr$full_join(
    dplyr$select(dplyr$filter(sig_plot_dt, method == "FH"), domain, fh_sig = significant),
    dplyr$select(dplyr$filter(sig_plot_dt, method == "MFH"), domain, mfh_sig = significant),
    by = "domain"
  )

  overlap_text <- paste0(
    "FH and MFH agree on significance status in ",
    sum(sig_overlap$fh_sig == sig_overlap$mfh_sig, na.rm = TRUE),
    " of ",
    nrow(sig_overlap),
    " domains."
  )

  normality_diag <- dat$normality_diag

  normality_text <- if (!is.null(normality_diag) && nrow(normality_diag) > 0) {
    paste0(
      normality_diag$method, " ", normality_diag$year, " ", normality_diag$component,
      ": W = ", format_num(normality_diag$W, 4),
      ", p = ", format_num(normality_diag$p_value, 4),
      ifelse(!is.na(normality_diag$p_value) & normality_diag$p_value >= 0.05,
             " (passes at 5%)", " (fails at 5%)"),
      ", n = ", ifelse(is.na(normality_diag$n), "NA", normality_diag$n)
    ) |> paste(collapse = "\n")
  } else {
    "No Shapiro-Wilk outputs were available."
  }

  normality_detail_text <- if (!is.null(normality_diag) && nrow(normality_diag) > 0 &&
                                "skewness" %in% names(normality_diag)) {
    has_qq <- "qq_correlation" %in% names(normality_diag)
    paste0(
      normality_diag$method, " ", normality_diag$year, " ", normality_diag$component, ":",
      " skewness = ", format_num(normality_diag$skewness, 3),
      ", excess_kurtosis = ", format_num(normality_diag$excess_kurtosis, 3),
      ", range = [", format_num(normality_diag$min_val, 4), ", ", format_num(normality_diag$max_val, 4), "]",
      ", outliers_beyond_2sd = ", normality_diag$outliers_beyond_2sd,
      ", outliers_beyond_3sd = ", normality_diag$outliers_beyond_3sd,
      if (has_qq) paste0(", qq_correlation = ", format_num(normality_diag$qq_correlation, 4)) else ""
    ) |> paste(collapse = "\n")
  } else {
    ""
  }

  top_rates <- dplyr$bind_rows(
    dplyr$transmute(comparison_dt, year, method = "Direct", domain, value = Direct),
    dplyr$transmute(comparison_dt, year, method = "FH", domain, value = FH),
    dplyr$transmute(comparison_dt, year, method = "FH Benchmarked", domain, value = FH_Bench),
    dplyr$transmute(comparison_dt, year, method = "MFH", domain, value = MFH),
    dplyr$transmute(comparison_dt, year, method = "MFH Benchmarked", domain, value = MFH_Bench)
  ) |>
    dplyr$group_by(year, method) |>
    dplyr$slice_max(order_by = value, n = 3, with_ties = FALSE) |>
    dplyr$summarise(
      top_domains = paste0(domain, " (", format_num(value, 3), ")", collapse = ", "),
      .groups = "drop"
    )

  change_extremes <- sig_plot_dt |>
    dplyr$group_by(method) |>
    dplyr$summarise(
      largest_increase = paste0(domain[which.max(diff)], " (", format_num(max(diff, na.rm = TRUE), 3), ")"),
      largest_decrease = paste0(domain[which.min(diff)], " (", format_num(min(diff, na.rm = TRUE), 3), ")"),
      .groups = "drop"
    )

  lang_label <- comparison_ai_language_label(language)
  lang_reminder <- if (!identical(language, "en")) {
    sprintf("\n\nIMPORTANT: Write your entire response in %s. The instructions above are in English for clarity, but your output MUST be in %s.", lang_label, lang_label)
  } else {
    ""
  }

  prompt_system <- paste(
    "You are a statistician writing short commentary blocks for a Small Area Estimation comparison report.",
    sprintf("The indicator being modelled is the %s, expressed %s.",
            indicator_noun, indicator_unit_phrase),
    sprintf("Refer to the estimates as '%s' or '%s' rather than generic phrases.",
            indicator_noun, indicator_noun_plural),
    "Use only the supplied aggregate summaries.",
    "Do not mention raw data, prompts, or APIs.",
    sprintf("Write ALL output in %s.", lang_label),
    "CRITICAL FORMATTING RULES:",
    "- Return ONLY plain text paragraphs separated by blank lines.",
    "- Do NOT use any markdown formatting: no headers (##), no bold (**), no bullet points (-), no numbered lists.",
    "- Do NOT start your response with a title or header line.",
    "- Each paragraph should be a continuous block of sentences.",
    "- Follow the structural instructions for each section exactly (number of paragraphs, level of detail).",
    "- Report specific numbers from the data provided. Round to 4 decimal places for rates/MSE, 1 decimal place for percentages.",
    "- Use a neutral, technical tone. Avoid subjective qualifiers like 'dramatic', 'remarkable', 'striking'."
  )

  list(
    system = prompt_system,
    overview = paste0(paste(
      "Write exactly 3 paragraphs for the report overview.",
      "",
      "Paragraph 1: State that this report compares Fay-Herriot (FH) and Multivariate Fay-Herriot (MFH) small area estimation models.",
      "Explain that FH models each domain independently while MFH borrows strength across correlated variables.",
      "Report the mean direct, FH, and MFH poverty rates for each year from the table below.",
      "",
      "Paragraph 2: Compare how closely FH and MFH track the direct estimator using the mean absolute deviation values.",
      "State which method tracks the direct estimator more closely in each year.",
      "",
      "Paragraph 3: Describe the role of benchmarking. Report how many domains show significant changes by method.",
      "Note whether FH and MFH agree on which domains show significant changes.",
      "",
      "Mean levels and direct-tracking summary by year:",
      paste(capture.output(print(as.data.frame(overview_tbl), row.names = FALSE)), collapse = "\n"),
      "",
      "Significant-change counts by method:",
      paste(capture.output(print(as.data.frame(sig_counts), row.names = FALSE)), collapse = "\n")
    ), lang_reminder),
    normality = paste0(paste(
      "Write a concise commentary for the normality diagnostics section.",
      "Keep the total length short to avoid truncation. Structure as exactly 10 paragraphs:",
      "",
      "Paragraph 1 (opening): State that this section synthesizes three sources of evidence",
      "(Shapiro-Wilk test, Q-Q plot, density plot) and that the Shapiro-Wilk test can be sensitive to sample size.",
      "Write 2-3 sentences only.",
      "",
      "Paragraphs 2-9: One paragraph for EACH of the 8 model/year/component combinations listed below, in order.",
      "Each paragraph must follow this exact template (5 sentences):",
      "- First sentence: State the method, year, and component, then report W and p-value.",
      "- Second sentence: State whether the test rejects normality at the 5% level.",
      "- Third sentence: Report skewness and excess kurtosis values and describe the distributional shape.",
      "- Fourth sentence: Discuss the Q-Q plot alignment. If a qq_correlation value is provided in the data below,",
      "  report it and interpret: values near 1.0 indicate strong alignment with the normal diagonal,",
      "  values below 0.98 suggest visible departure, and values below 0.95 indicate clear deviation.",
      "  If qq_correlation is not available, infer Q-Q plot appearance from the skewness, kurtosis, and outlier counts",
      "  (e.g., high skewness implies a curved Q-Q plot; outliers beyond 3sd imply points far from the diagonal in the tails).",
      "- Fifth sentence: State whether the Shapiro-Wilk test, distributional shape, and Q-Q plot evidence agree or conflict.",
      "Do NOT add sub-headers before these paragraphs.",
      "",
      "Paragraph 10 (closing): Summarize which components show the most concern based on all three diagnostics",
      "(Shapiro-Wilk, distributional shape, and Q-Q plot alignment).",
      "State whether the overall picture is reassuring for inference. Write 3-4 sentences.",
      "",
      "IMPORTANT: You MUST include ALL 8 combinations. Do not skip or truncate any.",
      "",
      "Shapiro-Wilk results by model, year, and component:",
      normality_text,
      "",
      "Distributional diagnostics by model, year, and component (includes Q-Q correlation where available;",
      "qq_correlation is the Pearson correlation between theoretical and sample quantiles from the Q-Q plot,",
      "where 1.0 = perfect normal alignment):",
      normality_detail_text
    ), lang_reminder),
    rates = paste0(paste(
      "Write exactly 2 paragraphs for the poverty-rate comparison section.",
      "",
      "Paragraph 1: Compare the mean absolute errors for each method and year.",
      "State which method tracks the direct estimator most closely in each year.",
      "Report the specific mean_abs_error values.",
      "",
      "Paragraph 2: Discuss the benchmark shift magnitudes.",
      "State whether benchmarking materially changes the level estimates or produces only modest adjustments.",
      "Report the mean_benchmark_shift values.",
      "",
      "Aggregate rate-comparison summary:",
      paste(capture.output(print(as.data.frame(rate_accuracy), row.names = FALSE)), collapse = "\n")
    ), lang_reminder),
    precision = paste0(paste(
      "Write exactly 3 paragraphs for the MSE, RMSE, and CV comparisons.",
      "Do NOT use sub-headers or titles within your response. Just write 3 continuous paragraphs.",
      "",
      "Paragraph 1 (MSE): Report the mean MSE for each method by year.",
      "Quantify how much benchmarking inflates MSE (percentage increase) for FH and MFH.",
      "Compare whether benchmarking inflates MSE more for FH or MFH, and whether this gap changes across years.",
      "Note that these patterns are consistent with the expectation that benchmarking adds variance.",
      "",
      "Paragraph 2 (RMSE): Explain that RMSE expresses estimation error on the same scale as the poverty rate.",
      "Report the mean RMSE for Direct, FH, and MFH by year.",
      "Quantify the percentage reduction in RMSE from the direct estimator for each model-based method.",
      "Report the benchmarking inflation of RMSE (percentage increase from unbenchmarked to benchmarked) for FH and MFH.",
      "Compare FH vs MFH RMSE and note whether the gap narrows or widens across years.",
      "Note whether FH Benchmarked and MFH Benchmarked converge to similar RMSE levels after benchmarking.",
      "Use the fh_rmse_ratio, mfh_rmse_ratio, fh_rmse_reduction_from_direct, and mfh_rmse_reduction_from_direct",
      "columns from the benchmarking impact table for these calculations.",
      "",
      "Paragraph 3 (CV): Explain that CV (coefficient of variation) measures relative precision as a percentage of the estimate itself,",
      "making it comparable across domains with different poverty levels.",
      "Report the mean CV for Direct, FH, FH Benchmarked, MFH, and MFH Benchmarked by year from the mean_cv column.",
      "Quantify the CV reduction achieved by model-based methods relative to the direct estimator.",
      "Report the mean CV change due to benchmarking for FH and MFH (fh_cv_change and mfh_cv_change columns).",
      "Note whether FH or MFH achieves lower CVs, and whether benchmarking narrows or widens this gap.",
      "",
      "Mean precision metrics:",
      paste(capture.output(print(as.data.frame(precision_tbl), row.names = FALSE)), collapse = "\n"),
      "",
      "Benchmarking impact summary (includes RMSE ratios and reductions from direct):",
      paste(capture.output(print(as.data.frame(benchmark_impact), row.names = FALSE)), collapse = "\n")
    ), lang_reminder),
    change_significance = paste0(paste(
      "Write exactly 2 paragraphs for the statistical-significance section.",
      "",
      "Paragraph 1: Report how many domains are significant for each method (FH, FH Benchmarked, MFH, MFH Benchmarked) out of the total.",
      "Note that MFH detects more significant domains than FH.",
      "",
      "Paragraph 2: Report the agreement between FH and MFH on significance status using the overlap figure below.",
      "Report the mean absolute changes for each method and discuss what they imply about volatility versus detection power.",
      "",
      "Counts and mean absolute changes:",
      paste(capture.output(print(as.data.frame(sig_counts), row.names = FALSE)), collapse = "\n"),
      "",
      overlap_text
    ), lang_reminder),
    poverty_maps = paste0(paste(
      "Write exactly 2 paragraphs for the poverty-level maps section.",
      "",
      "Paragraph 1: Identify the consistent high-poverty domains that appear across multiple methods.",
      "Report the top domains and their poverty rates from the table below.",
      "",
      "Paragraph 2: Discuss how model-based methods compare to direct estimates in magnitude.",
      "Note the effect of benchmarking on the spatial patterns.",
      "",
      "Top three domains by method and year:",
      paste(capture.output(print(as.data.frame(top_rates), row.names = FALSE)), collapse = "\n")
    ), lang_reminder),
    change_maps = paste0(paste(
      "Write exactly 2 paragraphs for the poverty-change maps section.",
      "",
      "Paragraph 1: Report the domains with the largest poverty increases and decreases for FH and MFH methods.",
      "Include the specific change magnitudes from the table below.",
      "",
      "Paragraph 2: Compare the benchmarked and unbenchmarked versions.",
      "State whether benchmarking changes the spatial story or leaves it largely intact.",
      "",
      "Largest increases and decreases by method:",
      paste(capture.output(print(as.data.frame(change_extremes), row.names = FALSE)), collapse = "\n")
    ), lang_reminder)
  )
}

generate_comparison_ai_comments <- function(llm, language = "en",
                                              indicator_type = "poverty",
                                              currency_symbol = "EUR",
                                              log_transform = FALSE,
                                              logger = message) {
  if (is.null(llm) || !isTRUE(llm$enabled)) {
    return(NULL)
  }

  prompts <- build_comparison_ai_prompts(
    language        = language,
    indicator_type  = indicator_type,
    currency_symbol = currency_symbol,
    log_transform   = log_transform
  )
  section_keys <- c(
    "overview",
    "normality",
    "rates",
    "precision",
    "change_significance",
    "poverty_maps",
    "change_maps"
  )

  comments <- list()
  last_warning_msg <- NULL
  for (key in section_keys) {
    logger(sprintf("Generating AI commentary for %s section...", key))
    comment_text <- tryCatch(
      withCallingHandlers(
        llm$query(prompts[[key]], system_prompt = prompts$system),
        warning = function(w) {
          last_warning_msg <<- conditionMessage(w)
          logger(sprintf("  Warning (%s): %s", key, conditionMessage(w)))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        logger(sprintf("  Error (%s): %s", key, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(comment_text) && nzchar(trimws(comment_text))) {
      comments[[key]] <- trimws(comment_text)
    }
  }

  if (length(comments) == 0) {
    if (!is.null(last_warning_msg)) {
      logger(sprintf("All AI commentary sections failed. Last warning: %s", last_warning_msg))
      logger("Possible causes: invalid API key, network/firewall blocking the API, or rate limiting.")
    }
    return(NULL)
  }

  comments
}

write_comparison_ai_note_html <- function(comments,
                                          output_file = "outputs/comparison_ai_note.html",
                                          report_file = "Comparison_v2.html",
                                          language = "en",
                                          logger = message) {
  if (is.null(comments) || length(comments) == 0) {
    stop("No AI comments were supplied for the companion note.")
  }

  section_labels <- comparison_ai_sections()
  available_keys <- intersect(names(section_labels), names(comments))
  if (!length(available_keys)) {
    stop("No recognized AI commentary sections were supplied.")
  }

  report_ref <- html_escape_text(report_file)
  section_html <- vapply(available_keys, function(key) {
    paragraphs <- unlist(strsplit(trimws(as.character(comments[[key]])), "\\n\\s*\\n", perl = TRUE))
    paragraphs <- paragraphs[nzchar(trimws(paragraphs))]
    body <- paste0(
      "<p>",
      vapply(paragraphs, function(p) html_escape_text(trimws(p)), character(1)),
      "</p>",
      collapse = "\n"
    )
    paste0(
      "<section class=\"note-section\">",
      "<h2>", html_escape_text(section_labels[[key]]), "</h2>",
      body,
      "</section>"
    )
  }, character(1))

  html <- paste0(
    "<!DOCTYPE html><html><head><meta charset=\"utf-8\">",
    "<title>AI Companion Note for Comparison_v2</title>",
    "<style>",
    "body{font-family:Georgia,'Times New Roman',serif;max-width:980px;margin:40px auto;padding:0 24px;line-height:1.65;color:#1f2933;}",
    "h1{font-size:32px;margin-bottom:8px;}h2{font-size:24px;margin-top:32px;margin-bottom:10px;border-bottom:2px solid #d9e2ec;padding-bottom:6px;}",
    "p{font-size:18px;margin:0 0 14px 0;} .meta{font-size:16px;color:#52606d;margin-bottom:26px;}",
    ".note-section{margin-bottom:22px;} .banner{background:#f0f4f8;border-left:5px solid #486581;padding:16px 18px;margin:20px 0 28px 0;}",
    "</style></head><body>",
    "<h1>AI Companion Note for Comparison_v2</h1>",
    "<p class=\"meta\">Language: ", html_escape_text(comparison_ai_language_label(language)), "</p>",
    "<div class=\"banner\">",
    "<p>This note is a separate AI-generated interpretation of the sections in <strong>", report_ref, "</strong>.</p>",
    "<p>The main comparison report remains the primary statistical output. This companion note adds short interpretive comments only.</p>",
    "</div>",
    paste(section_html, collapse = "\n"),
    "</body></html>"
  )

  writeLines(html, output_file, useBytes = TRUE)
  logger(sprintf("Created %s", output_file))
  invisible(output_file)
}

render_comparison_ai_note <- function(comments,
                                      language = "en",
                                      logger = message) {
  if (is.null(comments) || length(comments) == 0) {
    return(invisible(NULL))
  }

  if (!dir.exists("outputs")) {
    dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
  }

  html_file <- "outputs/comparison_ai_note.html"

  write_comparison_ai_note_html(
    comments = comments,
    output_file = html_file,
    report_file = "Comparison_v2.html",
    language = language,
    logger = logger
  )

  invisible(html_file)
}
