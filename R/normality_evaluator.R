# ============================================================
# normality_evaluator.R  --  AI-assisted normality evaluation
#
# Evaluates Fay-Herriot model normality assumptions using an LLM
# (Anthropic Claude or OpenAI). Two modes:
#   - TEXT-ONLY (default): ~500 tokens, uses numeric diagnostics only
#   - VISION mode: ~4,200 tokens, adds base64-encoded Q-Q/density plots
#
# Three main functions:
#   1. extract_fh_diagnostics()  -- extract data from emdi fh object
#   2. build_normality_prompt()  -- construct API payload (text or multimodal)
#   3. evaluate_normality()      -- call LLM API and parse response
#
# Privacy: only aggregate diagnostic statistics and model-level
# plots are sent -- never raw microdata.
# ============================================================

#' Capture a plot-producing function to a base64-encoded PNG
#'
#' @param plot_fn  A zero-argument function that produces one plot
#' @param width  Image width in pixels
#' @param height Image height in pixels
#' @param res    Resolution in DPI
#' @return Character string: base64-encoded PNG
capture_plot_base64 <- function(plot_fn, width = 800, height = 600, res = 150) {
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    stop("The base64enc package is required. Install with: install.packages('base64enc')")
  }

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(tmp, width = width, height = height, res = res)
  tryCatch(
    plot_fn(),
    finally = grDevices::dev.off()
  )

  base64enc::base64encode(tmp)
}

# ============================================================
# 1. Extract diagnostics from an emdi fh model object
# ============================================================

#' Extract normality diagnostics from an emdi Fay-Herriot model
#'
#' @param fh_model     An object of class "fh" returned by emdi::fh()
#' @param include_plots Logical; if TRUE, capture Q-Q and density plots
#'   as base64-encoded PNGs (adds ~3,200 API tokens). Default FALSE.
#' @return Named list with:
#'   \item{normality_table}{Data frame with Skewness, Kurtosis, Shapiro_W, Shapiro_p
#'         for Standardized_Residuals and Random_Effects}
#'   \item{residuals}{Numeric vector of standardized residuals}
#'   \item{random_effects}{Numeric vector of random effects}
#'   \item{n_domains}{Integer: number of domains}
#'   \item{qq_correlation}{Named list with residuals and random_effects Q-Q
#'         correlation coefficients (numeric summary of Q-Q plot alignment)}
#'   \item{plots}{Named list of base64-encoded PNG strings (only if include_plots=TRUE)}
extract_fh_diagnostics <- function(fh_model, include_plots = FALSE) {

  if (!inherits(fh_model, "fh")) {
    stop("fh_model must be an object of class 'fh' (from emdi::fh())")
  }

  # ---- Extract numeric diagnostics from summary ----
  s <- summary(fh_model)
  normality_table <- tryCatch(
    as.data.frame(s$normality),
    error = function(e) NULL
  )

  # ---- Extract residuals and random effects ----
  std_resid <- tryCatch(fh_model$model$std_real_residuals, error = function(e) NULL)
  rand_eff  <- tryCatch(fh_model$model$random_effects,     error = function(e) NULL)

  # Fallback: compute Shapiro directly if summary table extraction fails
  if (is.null(normality_table)) {
    rows <- list()
    for (nm in list(
      list(name = "Standardized_Residuals", vals = std_resid),
      list(name = "Random_Effects",         vals = rand_eff)
    )) {
      v <- nm$vals
      if (!is.null(v) && length(v) >= 3) {
        sw <- shapiro.test(v)
        rows[[nm$name]] <- data.frame(
          Skewness  = if (requireNamespace("moments", quietly = TRUE))
                        moments::skewness(v) else NA_real_,
          Kurtosis  = if (requireNamespace("moments", quietly = TRUE))
                        moments::kurtosis(v) else NA_real_,
          Shapiro_W = sw$statistic[[1]],
          Shapiro_p = sw$p.value,
          row.names = nm$name
        )
      }
    }
    if (length(rows) > 0) {
      normality_table <- do.call(rbind, rows)
    }
  }

  # ---- Q-Q correlation: numeric summary of Q-Q plot alignment ----
  # This gives Claude the same information as the Q-Q plot without images.
  # A correlation near 1.0 means points follow the diagonal closely.
  qq_corr <- list()
  if (!is.null(std_resid) && length(std_resid) >= 3) {
    qq <- stats::qqnorm(std_resid, plot.it = FALSE)
    qq_corr$residuals <- stats::cor(qq$x, qq$y)
  }
  if (!is.null(rand_eff) && length(rand_eff) >= 3) {
    qq <- stats::qqnorm(rand_eff, plot.it = FALSE)
    qq_corr$random_effects <- stats::cor(qq$x, qq$y)
  }

  # ---- Tail behavior: min/max of standardized values ----
  tail_stats <- list()
  if (!is.null(std_resid) && length(std_resid) >= 3) {
    z <- (std_resid - mean(std_resid)) / stats::sd(std_resid)
    tail_stats$resid_min_z <- min(z)
    tail_stats$resid_max_z <- max(z)
  }
  if (!is.null(rand_eff) && length(rand_eff) >= 3) {
    z <- (rand_eff - mean(rand_eff)) / stats::sd(rand_eff)
    tail_stats$re_min_z <- min(z)
    tail_stats$re_max_z <- max(z)
  }

  n_domains <- max(length(std_resid), length(rand_eff), 0L)

  # ---- Optionally capture plots as base64 PNGs ----
  plots <- list()

  if (isTRUE(include_plots)) {
    if (!is.null(std_resid) && length(std_resid) >= 3) {
      # Use closures so local variables are captured by reference
      sr <- std_resid  # local copy for closure
      plots$qq_residuals <- capture_plot_base64(function() {
        stats::qqnorm(sr, main = "Q-Q Plot: Standardized Residuals",
                       pch = 19, col = "steelblue")
        stats::qqline(sr, col = "red", lwd = 2)
      })

      plots$density_residuals <- capture_plot_base64(function() {
        d <- stats::density(sr)
        plot(d, main = "Density: Standardized Residuals",
             xlab = "Standardized Residuals", col = "steelblue", lwd = 2)
        x_seq <- seq(min(d$x), max(d$x), length.out = 200)
        lines(x_seq, stats::dnorm(x_seq, mean(sr), stats::sd(sr)),
              col = "red", lwd = 2, lty = 2)
        legend("topright", legend = c("Empirical", "Normal"),
               col = c("steelblue", "red"), lwd = 2, lty = c(1, 2), cex = 0.8)
      })
    }

    if (!is.null(rand_eff) && length(rand_eff) >= 3) {
      re <- rand_eff  # local copy for closure
      plots$qq_random_effects <- capture_plot_base64(function() {
        stats::qqnorm(re, main = "Q-Q Plot: Random Effects",
                       pch = 19, col = "darkorange")
        stats::qqline(re, col = "red", lwd = 2)
      })

      plots$density_random_effects <- capture_plot_base64(function() {
        d <- stats::density(re)
        plot(d, main = "Density: Random Effects",
             xlab = "Random Effects", col = "darkorange", lwd = 2)
        x_seq <- seq(min(d$x), max(d$x), length.out = 200)
        lines(x_seq, stats::dnorm(x_seq, mean(re), stats::sd(re)),
              col = "red", lwd = 2, lty = 2)
        legend("topright", legend = c("Empirical", "Normal"),
               col = c("darkorange", "red"), lwd = 2, lty = c(1, 2), cex = 0.8)
      })
    }
  }

  list(
    normality_table = normality_table,
    residuals       = std_resid,
    random_effects  = rand_eff,
    n_domains       = n_domains,
    qq_correlation  = qq_corr,
    tail_stats      = tail_stats,
    plots           = plots
  )
}

# ============================================================
# 2. Build the Claude API prompt (text-only or multimodal)
# ============================================================

#' Build a Claude API message for normality evaluation
#'
#' @param diagnostics  Output of extract_fh_diagnostics()
#' @param use_vision   Logical; if TRUE, include base64 images (requires
#'   include_plots=TRUE in extract_fh_diagnostics). Default FALSE.
#' @return A list suitable for the "messages" field of the Anthropic API body.
build_normality_prompt <- function(diagnostics, use_vision = FALSE) {

  # ---- Text: numeric diagnostics ----
  norm_tbl <- diagnostics$normality_table
  if (!is.null(norm_tbl)) {
    tbl_text <- paste(utils::capture.output(print(norm_tbl, digits = 4)), collapse = "\n")
  } else {
    tbl_text <- "(Shapiro-Wilk table not available)"
  }

  n_domains <- diagnostics$n_domains
  qq_corr   <- diagnostics$qq_correlation
  tail      <- diagnostics$tail_stats

  # Build compact text summary with Q-Q correlation (replaces images)
  qq_text <- ""
  if (!is.null(qq_corr$residuals)) {
    qq_text <- paste0(qq_text, sprintf(
      "Q-Q correlation (residuals): %.4f (1.0 = perfect normality)\n", qq_corr$residuals))
  }
  if (!is.null(qq_corr$random_effects)) {
    qq_text <- paste0(qq_text, sprintf(
      "Q-Q correlation (random effects): %.4f\n", qq_corr$random_effects))
  }

  tail_text <- ""
  if (!is.null(tail$resid_min_z)) {
    tail_text <- paste0(tail_text, sprintf(
      "Residual tail range (z-scores): [%.2f, %.2f]\n", tail$resid_min_z, tail$resid_max_z))
  }
  if (!is.null(tail$re_min_z)) {
    tail_text <- paste0(tail_text, sprintf(
      "Random effect tail range (z-scores): [%.2f, %.2f]\n", tail$re_min_z, tail$re_max_z))
  }

  instruction_text <- paste0(
    "Evaluate the normality assumptions of a Fay-Herriot small area estimation model.\n",
    "Two assumptions to check:\n",
    "1. Standardized residuals ~ Normal\n",
    "2. Random effects ~ Normal\n\n",
    "Shapiro-Wilk test results, skewness, and kurtosis:\n\n",
    "```\n", tbl_text, "\n```\n\n",
    "Number of domains: ", n_domains, "\n",
    qq_text, tail_text, "\n",
    "Note: For small samples (n < 50), Shapiro-Wilk has limited power; ",
    "Q-Q correlation and tail behavior provide complementary evidence.\n\n",
    "Return a JSON object with this structure:\n",
    "```json\n",
    "{\n",
    "  \"standardized_residuals\": {\n",
    "    \"normality_holds\": true/false,\n",
    "    \"shapiro_assessment\": \"text\",\n",
    "    \"visual_assessment\": \"text based on Q-Q correlation and tail stats\",\n",
    "    \"concerns\": []\n",
    "  },\n",
    "  \"random_effects\": {\n",
    "    \"normality_holds\": true/false,\n",
    "    \"shapiro_assessment\": \"text\",\n",
    "    \"visual_assessment\": \"text\",\n",
    "    \"concerns\": []\n",
    "  },\n",
    "  \"overall_recommendation\": \"text\"\n",
    "}\n",
    "```\n",
    "Return ONLY the JSON object."
  )

  # ---- Build content blocks ----
  content_blocks <- list(
    list(type = "text", text = instruction_text)
  )

  # Optionally append image blocks (vision mode)
  if (isTRUE(use_vision) && length(diagnostics$plots) > 0) {
    plot_labels <- c(
      qq_residuals           = "Q-Q Plot: Standardized Residuals",
      density_residuals      = "Density Plot: Standardized Residuals",
      qq_random_effects      = "Q-Q Plot: Random Effects",
      density_random_effects = "Density Plot: Random Effects"
    )

    for (plot_name in names(plot_labels)) {
      b64 <- diagnostics$plots[[plot_name]]
      if (!is.null(b64) && nchar(b64) > 0) {
        content_blocks <- c(content_blocks, list(
          list(type = "text", text = paste0("\n### ", plot_labels[[plot_name]], "\n"))
        ))
        content_blocks <- c(content_blocks, list(
          list(
            type   = "image",
            source = list(
              type       = "base64",
              media_type = "image/png",
              data       = b64
            )
          )
        ))
      }
    }
  }

  list(
    list(
      role    = "user",
      content = content_blocks
    )
  )
}

# ============================================================
# 3. Call the Claude API and parse the structured response
# ============================================================

#' Evaluate normality assumptions using an LLM API
#'
#' @param fh_model    An emdi fh model object
#' @param api_key     API key (default: from env var ANTHROPIC_API_KEY)
#' @param provider    "anthropic" or "openai" (default: auto-detected from key)
#' @param model       Model identifier (default depends on provider)
#' @param language    Language for any additional commentary (default: "en")
#' @param use_vision  Logical; if TRUE, send diagnostic plots as images.
#'   Increases token usage from ~500 to ~4,200. Default FALSE (text-only).
#' @return Named list with structured evaluation results:
#'   \item{standardized_residuals}{list with normality_holds, shapiro_assessment,
#'         visual_assessment, concerns}
#'   \item{random_effects}{same structure}
#'   \item{overall_recommendation}{character string}
#'   \item{raw_response}{character: raw text from LLM (for debugging)}
#'   \item{diagnostics}{the extracted diagnostics object (for UI display)}
#'   \item{mode}{character: "text" or "vision"}
evaluate_normality <- function(fh_model,
                               api_key    = Sys.getenv("ANTHROPIC_API_KEY"),
                               provider   = NULL,
                               model      = NULL,
                               language   = "en",
                               use_vision = FALSE) {

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("The httr package is required. Install with: install.packages('httr')")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The jsonlite package is required. Install with: install.packages('jsonlite')")
  }
  if (isTRUE(use_vision) && !requireNamespace("base64enc", quietly = TRUE)) {
    stop("The base64enc package is required for vision mode. Install with: install.packages('base64enc')")
  }

  if (is.null(api_key) || nchar(api_key) == 0) {
    stop("API key is required. Set ANTHROPIC_API_KEY / OPENAI_API_KEY or pass api_key argument.")
  }

  # Auto-detect provider from key if not specified
  if (is.null(provider)) {
    provider <- if (exists("detect_llm_provider", mode = "function")) {
      detect_llm_provider(api_key)
    } else {
      "anthropic"
    }
  }
  if (is.null(model) || !nzchar(model)) {
    model <- if (provider == "openai") "gpt-4.1" else "claude-sonnet-4-20250514"
  }

  # Step 1: Extract diagnostics
  diagnostics <- extract_fh_diagnostics(fh_model, include_plots = use_vision)

  # Step 2: Build prompt (Anthropic format)
  messages <- build_normality_prompt(diagnostics, use_vision = use_vision)

  # Convert image content blocks for OpenAI (image_url instead of source)
  if (provider == "openai") {
    for (i in seq_along(messages)) {
      if (is.list(messages[[i]]$content)) {
        messages[[i]]$content <- lapply(messages[[i]]$content, function(block) {
          if (identical(block$type, "image") && !is.null(block$source)) {
            list(
              type      = "image_url",
              image_url = list(
                url = paste0("data:", block$source$media_type, ";base64,", block$source$data)
              )
            )
          } else {
            block
          }
        })
      }
    }
  }

  # Step 3: Call the LLM API
  lang_label <- if (exists("language_label", mode = "function")) {
    language_label(language)
  } else {
    "English"
  }

  sys_prompt <- paste(
    "You are an expert statistician evaluating Fay-Herriot small area estimation",
    "model diagnostics. Be precise and concise.",
    if (isTRUE(use_vision))
      "Analyze both the numeric test results and the visual diagnostic plots carefully."
    else
      "Use the numeric statistics, Q-Q correlations, and tail statistics to assess normality.",
    sprintf("Respond in %s.", lang_label)
  )

  body <- if (provider == "openai") {
    list(
      model = model,
      temperature = 0,
      max_completion_tokens = if (isTRUE(use_vision)) 2048 else 1024,
      messages = c(
        list(list(role = "system", content = sys_prompt)),
        messages
      )
    )
  } else {
    list(
      model      = model,
      temperature = 0,
      max_tokens = if (isTRUE(use_vision)) 2048 else 1024,
      system     = sys_prompt,
      messages   = messages
    )
  }

  resp <- tryCatch({
    if (provider == "openai") {
      httr::POST(
        url = "https://api.openai.com/v1/chat/completions",
        httr::add_headers(
          Authorization    = paste("Bearer", api_key),
          `content-type`   = "application/json"
        ),
        body    = jsonlite::toJSON(body, auto_unbox = TRUE),
        encode  = "raw",
        httr::timeout(120)
      )
    } else {
      httr::POST(
        url    = "https://api.anthropic.com/v1/messages",
        httr::add_headers(
          `x-api-key`         = api_key,
          `anthropic-version` = "2023-06-01",
          `content-type`      = "application/json"
        ),
        body    = jsonlite::toJSON(body, auto_unbox = TRUE),
        encode  = "raw",
        httr::timeout(120)
      )
    }
  }, error = function(e) {
    stop(sprintf("LLM API request failed: %s", e$message))
  })

  status <- httr::status_code(resp)
  if (status != 200) {
    err_body <- httr::content(resp, as = "text", encoding = "UTF-8")
    stop(sprintf("LLM API returned HTTP %d: %s", status, err_body))
  }

  # Step 4: Parse the response
  parsed <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )

  raw_text <- ""
  if (provider == "openai") {
    choices <- parsed$choices
    if (is.list(choices) && length(choices) > 0) {
      msg_content <- choices[[1]]$message$content
      if (is.character(msg_content)) raw_text <- msg_content[[1]]
    }
  }
  cnt <- parsed$content
  if (is.list(cnt) && length(cnt) > 0) {
    for (block in cnt) {
      if (!is.null(block$text)) {
        raw_text <- paste0(raw_text, block$text)
      }
    }
  }

  if (nchar(raw_text) == 0) {
    stop("LLM API returned an empty response.")
  }

  # Step 5: Extract JSON from response (handle markdown code fences)
  json_text <- raw_text
  json_match <- regmatches(json_text, regexpr("\\{[\\s\\S]*\\}", json_text, perl = TRUE))
  if (length(json_match) == 1) {
    json_text <- json_match
  }

  result <- tryCatch(
    jsonlite::fromJSON(json_text, simplifyVector = TRUE),
    error = function(e) {
      warning(sprintf("Could not parse JSON from Claude response: %s", e$message))
      NULL
    }
  )

  if (is.null(result)) {
    return(list(
      standardized_residuals = list(
        normality_holds     = NA,
        shapiro_assessment  = "Could not parse structured response",
        visual_assessment   = raw_text,
        concerns            = character(0)
      ),
      random_effects = list(
        normality_holds     = NA,
        shapiro_assessment  = "Could not parse structured response",
        visual_assessment   = "",
        concerns            = character(0)
      ),
      overall_recommendation = raw_text,
      raw_response           = raw_text,
      diagnostics            = diagnostics,
      mode                   = if (use_vision) "vision" else "text"
    ))
  }

  result$raw_response <- raw_text
  result$diagnostics  <- diagnostics
  result$mode         <- if (use_vision) "vision" else "text"
  result
}
