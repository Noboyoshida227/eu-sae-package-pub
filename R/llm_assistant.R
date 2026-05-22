# ============================================================
# llm_assistant.R  --  LLM integration for SAE Dashboard
#
# Wraps calls to the Anthropic API (Claude) or OpenAI API for:
#   - Diagnostic interpretation
#   - Dashboard help / explanations
#   - Analysis brief enrichment
#
# Privacy: only aggregate statistics are sent -- never raw microdata.
# ============================================================

#' Detect the LLM provider from an API key
#'
#' Anthropic keys start with "sk-ant-"; OpenAI keys start with "sk-"
#' (but not "sk-ant-"). Falls back to "anthropic" if unrecognised.
#'
#' @param api_key Character string: the API key
#' @return "anthropic" or "openai"
detect_llm_provider <- function(api_key) {
  if (is.null(api_key) || !nzchar(api_key)) return("anthropic")
  if (grepl("^sk-ant-", api_key)) return("anthropic")
  if (grepl("^sk-", api_key)) return("openai")
  "anthropic"
}

#' Create an LLM assistant
#'
#' @param api_key  Provider API key (default: from env vars)
#' @param enabled  Logical; set FALSE to disable all API calls
#' @param provider API provider: "anthropic" or "openai"
#' @param model    Model identifier (defaults depend on provider)
#' @return List with helper methods: $interpret_diagnostics(), $explain_dashboard(), $query()
llm_assistant <- function(api_key = NULL,
                          enabled = TRUE,
                          provider = c("anthropic", "openai"),
                          model   = NULL) {

  provider <- match.arg(provider)
  if (is.null(api_key) || !nzchar(api_key)) {
    api_key <- if (provider == "openai") Sys.getenv("OPENAI_API_KEY") else Sys.getenv("ANTHROPIC_API_KEY")
  }
  if (is.null(model) || !nzchar(model)) {
    model <- if (provider == "openai") "gpt-4.1" else "claude-sonnet-4-20250514"
  }

  is_enabled <- enabled && nchar(api_key) > 0

  parse_openai_chat_content <- function(parsed) {
    choices <- parsed$choices
    if (!is.list(choices) || length(choices) == 0 || !is.list(choices[[1]])) {
      return(NULL)
    }

    message_obj <- choices[[1]]$message
    if (!is.list(message_obj) || is.null(message_obj$content)) {
      return(NULL)
    }

    content <- message_obj$content
    if (is.character(content) && length(content) > 0) {
      return(content[[1]])
    }
    if (is.list(content) && length(content) > 0) {
      text_parts <- vapply(content, function(item) {
        if (is.list(item) && !is.null(item$text)) {
          return(as.character(item$text))
        }
        ""
      }, character(1))
      text_parts <- text_parts[nzchar(text_parts)]
      if (length(text_parts) > 0) {
        return(paste(text_parts, collapse = "\n"))
      }
    }
    NULL
  }

  # Internal helper to call the provider API
  call_api <- function(system_prompt, user_message) {
    if (!is_enabled) return(NULL)

    if (!requireNamespace("httr", quietly = TRUE) ||
        !requireNamespace("jsonlite", quietly = TRUE)) {
      warning("httr and jsonlite packages are required for LLM features")
      return(NULL)
    }

    body <- if (provider == "openai") {
      list(
        model = model,
        temperature = 0,
        messages = list(
          list(role = "system", content = system_prompt),
          list(role = "user", content = user_message)
        ),
        max_completion_tokens = 4096
      )
    } else {
      list(
        model      = model,
        temperature = 0,
        max_tokens = 4096,
        system     = system_prompt,
        messages   = list(
          list(role = "user", content = user_message)
        )
      )
    }

    resp <- tryCatch({
      if (provider == "openai") {
        httr::POST(
          url = "https://api.openai.com/v1/chat/completions",
          httr::add_headers(
            Authorization = paste("Bearer", api_key),
            `content-type` = "application/json"
          ),
          body = jsonlite::toJSON(body, auto_unbox = TRUE),
          encode = "raw"
        )
      } else {
        httr::POST(
          url    = "https://api.anthropic.com/v1/messages",
          httr::add_headers(
            `x-api-key`         = api_key,
            `anthropic-version` = "2023-06-01",
            `content-type`      = "application/json"
          ),
          body   = jsonlite::toJSON(body, auto_unbox = TRUE),
          encode = "raw"
        )
      }
    }, error = function(e) {
      warning(sprintf("LLM API call failed: %s", e$message))
      return(NULL)
    })

    if (is.null(resp)) return(NULL)

    if (httr::status_code(resp) != 200) {
      warning(sprintf("LLM API returned status %d", httr::status_code(resp)))
      return(NULL)
    }

    parsed <- tryCatch(
      jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                         simplifyVector = FALSE),
      error = function(e) {
        warning(sprintf("Failed to parse LLM response: %s", e$message))
        return(NULL)
      }
    )
    if (is.null(parsed)) return(NULL)

    if (provider == "openai") {
      return(parse_openai_chat_content(parsed))
    }

    cnt <- parsed$content
    if (is.list(cnt) && length(cnt) > 0 && is.list(cnt[[1]]) && !is.null(cnt[[1]]$text)) {
      return(cnt[[1]]$text)
    }
    NULL
  }

  # --- Public methods ---

  interpret_diagnostics <- function(diagnostics, language = "en") {
    sys <- paste(
      "You are a statistician helping interpret Small Area Estimation (SAE)",
      "model diagnostics. Be concise but thorough. Focus on actionable insights.",
      sprintf("Respond in %s.", language_label(language))
    )
    msg <- paste("Here are the model diagnostics:\n",
                 paste(capture.output(str(diagnostics)), collapse = "\n"))
    call_api(sys, msg)
  }

  explain_dashboard <- function(section, language = "en") {
    sys <- paste(
      "You are a helpful assistant explaining sections of a Small Area Estimation",
      "dashboard to non-technical users.",
      sprintf("Respond in %s.", language_label(language))
    )
    msg <- sprintf("Please explain the '%s' section of the SAE dashboard.", section)
    call_api(sys, msg)
  }

  query <- function(prompt, system_prompt = NULL) {
    if (is.null(system_prompt)) {
      system_prompt <- "You are a statistical analysis assistant for Small Area Estimation."
    }
    call_api(system_prompt, prompt)
  }

  list(
    enabled                = is_enabled,
    provider               = provider,
    model                  = model,
    interpret_diagnostics  = interpret_diagnostics,
    explain_dashboard      = explain_dashboard,
    query                  = query
  )
}

# Helper to map language code to label.
# Labels are the English names of each language so that LLM prompts
# read naturally (e.g. "Write your entire response in German.").
# Codes must stay in sync with supported_languages() in R/multilingual.R.
language_label <- function(code) {
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
  idx <- match(code, names(labels))
  if (is.null(code) || length(code) == 0 || is.na(idx)) {
    return("English")
  }
  unname(labels[idx])
}
