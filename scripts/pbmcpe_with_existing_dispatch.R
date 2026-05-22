#' Dispatch: Parametric Bootstrap MCPE for any MFH variant (with existing EBLUP)
#'
#' A thin dispatcher that calls the appropriate parametric-bootstrap MCPE
#' routine for the selected MFH variant and returns a uniform object shape
#' (matching pbmcpeMFH2_with_existing()). All three wrappers reuse the
#' EBLUPs from the already-fit model (\code{existing_model$eblup}) and only
#' bootstrap the MSE/MCPE. This keeps the Comparison report's point
#' estimates consistent with the MFH pipeline regardless of which variant
#' was selected.
#'
#' @param diag_model  One of "MFH1", "MFH2", "MFH3".
#' @param formula     List of formulas (one per time period), as passed to msae.
#' @param vardir      Character vector of vardir column names.
#' @param domain_var  Column in `data` holding domain identifiers.
#' @param existing_model Fitted msae model object (eblupMFH1/2/3 output).
#' @param nB          Bootstrap iterations (default 50).
#' @param data        Modeling data frame.
#' @param ...         Extra args forwarded (e.g. MAXITER, PRECISION).
#'
#' @return A list with fields `domain`, `eblup`, `mse`, `mcpe`, `fails`
#'         (same shape as pbmcpeMFH2_with_existing()).

pbmcpe_with_existing <- function(diag_model,
                                 formula,
                                 vardir,
                                 domain_var,
                                 existing_model,
                                 nB   = 50,
                                 data,
                                 ...) {

  diag_model <- toupper(as.character(diag_model))
  if (!diag_model %in% c("MFH1", "MFH2", "MFH3")) {
    stop("pbmcpe_with_existing: unsupported diag_model '", diag_model,
         "'. Must be one of MFH1/MFH2/MFH3.")
  }

  # Lazily source the requested wrapper so we don't load bootstrap code
  # for variants the user isn't running.
  script_path <- switch(
    diag_model,
    "MFH1" = "scripts/pbmcpeMFH1_with_existing_eblup.R",
    "MFH2" = "scripts/pbmcpeMFH2_with_existing_eblup.R",
    "MFH3" = "scripts/pbmcpeMFH3_with_existing_eblup.R"
  )
  fn_name <- switch(
    diag_model,
    "MFH1" = "pbmcpeMFH1_with_existing",
    "MFH2" = "pbmcpeMFH2_with_existing",
    "MFH3" = "pbmcpeMFH3_with_existing"
  )

  if (!exists(fn_name, mode = "function")) {
    if (!file.exists(script_path)) {
      stop("pbmcpe_with_existing: missing bootstrap script for ",
           diag_model, " (expected at ", script_path, ")")
    }
    source(script_path)
  }
  fn <- get(fn_name, mode = "function")

  fn(formula        = formula,
     vardir         = vardir,
     domain_var     = domain_var,
     existing_model = existing_model,
     nB             = nB,
     data           = data,
     ...)
}
