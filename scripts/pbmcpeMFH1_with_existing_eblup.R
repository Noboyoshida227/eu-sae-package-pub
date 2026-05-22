#' Parametric Bootstrap MCPE for MFH1 Model (Using existing EBLUP)
#'
#' Computes Mean Squared Error (MSE) and Model-based Covariance Prediction
#' Error (MCPE) for multivariate Fay-Herriot type 1 (MFH1) models using
#' parametric bootstrap. Mirrors the structure of
#' \code{pbmcpeMFH2_with_existing()} but assumes random effects are
#' independent across time (no AR(1)), with variance that may differ by
#' time period.
#'
#' Returns the same shape as \code{pbmcpeMFH2_with_existing()} so the
#' Comparison report and downstream code don't need to branch on model.
#'
#' @inheritParams pbmcpeMFH2_with_existing

library(MASS)
library(Matrix)
library(matrixcalc)

pbmcpeMFH1_with_existing <- function(formula, vardir, domain_var,
                                     existing_model, nB = 100, data,
                                     max_attempts = NULL, ...) {
  # max_attempts: hard cap on total bootstrap iterations to prevent the
  # loop from spinning forever when refits keep failing to converge (see
  # the corresponding pbmcpeMFH2 wrapper for the longer rationale).
  if (is.null(max_attempts)) max_attempts <- 5L * nB

  nD <- nrow(data)
  nT <- length(formula)
  M <- nD * nT

  domain_ids <- data[[domain_var]]
  if (is.null(domain_ids)) {
    stop("domain_var '", domain_var, "' not found in data")
  }

  X_list <- lapply(formula, function(f) model.matrix(f, data))
  p_list <- sapply(X_list, ncol)

  result <- existing_model
  if (!isTRUE(result$fit$convergence)) stop("Existing model did not converge")

  beta_list <- list()
  estcoef_mat <- result$fit$estcoef
  start_row <- 1
  for (t in 1:nT) {
    end_row <- start_row + p_list[t] - 1
    beta_list[[t]] <- estcoef_mat[start_row:end_row, 1]
    start_row <- end_row + 1
  }

  # MFH1: refvar is (potentially) a vector of length nT; rho is unused.
  varu2_vec <- as.numeric(result$fit$refvar)
  if (length(varu2_vec) == 1L) varu2_vec <- rep(varu2_vec, nT)
  if (length(varu2_vec) != nT) {
    stop("pbmcpeMFH1_with_existing: refvar length ", length(varu2_vec),
         " is incompatible with nT = ", nT)
  }

  sigmaedts <- as.matrix(data[, vardir, drop = FALSE])
  storage.mode(sigmaedts) <- "double"
  Sigmaed <- array(0, c(nT, nT, nD))
  for (d in 1:nD) {
    idx <- nT + 1
    for (t1 in 1:(nT - 1)) {
      Sigmaed[t1, t1, d] <- sigmaedts[d, t1]
      for (t2 in (t1 + 1):nT) {
        Sigmaed[t1, t2, d] <- sigmaedts[d, idx]
        Sigmaed[t2, t1, d] <- sigmaedts[d, idx]
        idx <- idx + 1
      }
    }
    Sigmaed[nT, nT, d] <- sigmaedts[d, nT]
    if (!is.positive.definite(Sigmaed[,,d])) {
      Sigmaed[,,d] <- as.matrix(nearPD(Sigmaed[,,d], keepDiag = TRUE)$mat)
    }
  }

  mcpedt <- matrix(0, nrow = nD, ncol = nT + nT * (nT - 1) / 2)
  countfail <- 0
  b <- 1
  attempts <- 0L

  while (b <= nB) {
    attempts <- attempts + 1L
    if (attempts > max_attempts) {
      warning(sprintf(
        "pbmcpeMFH1_with_existing(): hit max_attempts=%d after %d successful and %d failed bootstrap refits; aborting bootstrap. Returning NULL so the caller can fall back to a no-MCPE path.",
        max_attempts, b - 1L, countfail
      ))
      return(NULL)
    }
    message(sprintf("Bootstrap %d (MFH1)", b))

    # Independent random effects across time (no AR(1))
    udt_b <- as.numeric(
      matrix(
        rnorm(M, mean = 0, sd = rep(sqrt(varu2_vec), times = nD)),
        nrow = nD, ncol = nT, byrow = TRUE
      )
    )
    # Reorder to domain-then-time layout that matches downstream reshape
    udt_flat <- rep(0, M)
    udt_mat  <- matrix(udt_b, nrow = nD, ncol = nT, byrow = FALSE)
    for (d in seq_len(nD)) {
      idx_d <- (d - 1) * nT + seq_len(nT)
      udt_flat[idx_d] <- udt_mat[d, ]
    }
    udt_b <- udt_flat

    edt_b <- rep(0, M)
    meandt_b <- rep(0, M)
    i <- 1
    for (d in seq_len(nD)) {
      edt_b[i:(i + nT - 1)] <- mvrnorm(1, mu = rep(0, nT),
                                       Sigma = Sigmaed[,,d])
      for (tt in seq_len(nT)) {
        meandt_b[i + tt - 1] <- X_list[[tt]][d, ] %*% beta_list[[tt]]
      }
      i <- i + nT
    }

    mudt_b <- meandt_b + udt_b
    ydt_b  <- mudt_b + edt_b

    ydt.mat  <- matrix(0, nrow = nD, ncol = nT)
    mudt.mat <- matrix(0, nrow = nD, ncol = nT)
    for (tt in 1:nT) {
      ydt.mat[, tt]  <- ydt_b[seq(from = tt, to = M, by = nT)]
      mudt.mat[, tt] <- mudt_b[seq(from = tt, to = M, by = nT)]
    }

    ydt.df <- setNames(as.data.frame(ydt.mat), paste0("Y", 1:nT))

    formula.b <- lapply(1:nT, function(t) {
      rhs <- paste(attr(terms(formula[[t]]), "term.labels"), collapse = " + ")
      as.formula(paste0("Y", t, " ~ ", rhs))
    })

    used_vars <- unique(unlist(lapply(formula, all.vars)))
    data.b <- cbind(ydt.df, data[, used_vars, drop = FALSE], sigmaedts)

    result.b <- tryCatch({
      eblupMFH1(formula = formula.b, vardir = vardir, data = data.b)
    }, error = function(e) NULL)

    if (is.null(result.b) || !isTRUE(result.b$fit$convergence)) {
      countfail <- countfail + 1
      next
    }

    dif <- result.b$eblup - mudt.mat
    dif.b <- matrix(0, nrow = nD, ncol = nT + nT * (nT - 1) / 2)
    pos <- nT
    for (t1 in 1:(nT - 1)) {
      dif.b[, t1] <- dif[, t1]^2
      for (t2 in (t1 + 1):nT) {
        pos <- pos + 1
        dif.b[, pos] <- dif[, t1] * dif[, t2]
      }
    }
    dif.b[, nT] <- dif[, nT]^2

    mcpedt <- mcpedt + dif.b
    b <- b + 1
  }

  n_valid <- b - 1L
  if (n_valid <= 0L) {
    warning("pbmcpeMFH1_with_existing(): no successful bootstrap refits; returning NULL.")
    return(NULL)
  }
  mcpedt.b <- mcpedt / n_valid

  mse  <- mcpedt.b[, 1:nT, drop = FALSE]
  mcpe <- if (nT >= 2)
    mcpedt.b[, (nT + 1):(nT + nT * (nT - 1) / 2), drop = FALSE]
  else NULL

  if (!is.null(mcpe)) {
    colnames(mcpe) <- apply(combn(nT, 2), 2,
                            function(pair) paste0("(", pair[1], ",", pair[2], ")"))
  }

  list(
    domain = domain_ids,
    eblup  = result$eblup,
    mse    = mse,
    mcpe   = mcpe,
    fails  = countfail
  )
}
