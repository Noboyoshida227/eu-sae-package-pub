#' Parametric Bootstrap MCPE for MFH2 Model
#'
#' Computes Mean Squared Error (MSE) and Model-based Covariance Prediction Error (MCPE) 
#' for multivariate Fay-Herriot type 2 (MFH2) models using parametric bootstrap.
#'
#' @param formula A list of formulas, one for each time point or outcome (e.g., list(y1 ~ x1 + x2, y2 ~ x1)).
#' @param vardir A character vector of column names in \code{data} specifying the sampling error variances
#'   and covariances. The first \code{nT} elements correspond to the variances, followed by
#'   \code{nT*(nT - 1)/2} covariances (in row-wise order).
#' @param nB Number of bootstrap replications. Default is 100.
#' @param data A data frame containing all variables used in \code{formula} and \code{vardir}.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{eblup}{The EBLUP estimates from the base MFH2 model.}
#'   \item{mse}{Estimated mean squared errors for each domain and time point.}
#'   \item{mcpe}{Estimated MCPEs (covariances between errors) across outcomes.}
#'   \item{fails}{Number of bootstrap iterations that failed to converge.}
#' }
#' @importFrom MASS mvrnorm
#' @importFrom Matrix nearPD
#' @import matrixcalc
#' @export

library(MASS)
library(Matrix)
library(matrixcalc)

pbmcpeMFH2 <- function(formula, vardir, nB = 100, data, max_attempts = NULL, ...) {
  # max_attempts: hard cap on total bootstrap iterations to prevent the
  # loop from spinning forever when refits keep failing to converge.
  if (is.null(max_attempts)) max_attempts <- 5L * nB

  nD <- nrow(data)
  nT <- length(formula)
  M <- nD * nT
  
  # Build list of design matrices
  X_list <- lapply(formula, function(f) model.matrix(f, data))
  p_list <- sapply(X_list, ncol)
  
  # Fit base model
  result <- eblupMFH2(formula = formula, 
                      vardir = vardir, 
                      data = data,
                      ...)
  if (!result$fit$convergence) stop("Base model did not converge")
  
  # Extract beta coefficients from matrix
  beta_list <- list()
  estcoef_mat <- result$fit$estcoef
  start_row <- 1
  for (t in 1:nT) {
    end_row <- start_row + p_list[t] - 1
    beta_list[[t]] <- estcoef_mat[start_row:end_row, 1]
    start_row <- end_row + 1
  }
  
  varu2 <- result$fit$refvar
  rho <- result$fit$rho[, 1]
  Unomenrho2_05 <- (1 - rho^2)^(-0.5)
  
  # Prepare sampling error covariance matrices
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
        "pbmcpeMFH2(): hit max_attempts=%d after %d successful and %d failed bootstrap refits; aborting bootstrap. Returning NULL so the caller can fall back to a no-MCPE path.",
        max_attempts, b - 1L, countfail
      ))
      return(NULL)
    }
    message(sprintf("Bootstrap %d", b))
    
    adt_b <- rnorm(M, mean = 0, sd = sqrt(varu2))
    udt_b <- rep(0, M)
    edt_b <- rep(0, M)
    meandt_b <- rep(0, M)
    i <- 1
    for (d in 1:nD) {
      udt_b[i] <- Unomenrho2_05 * adt_b[i]
      edt_b[i:(i + nT - 1)] <- mvrnorm(1, mu = rep(0, nT), Sigma = Sigmaed[,,d])
      for (t in 1:nT) {
        meandt_b[i + t - 1] <- X_list[[t]][d, ] %*% beta_list[[t]]
      }
      for (t in 2:nT) {
        i <- i + 1
        udt_b[i] <- rho * udt_b[i - 1] + adt_b[i]
      }
      i <- i + 1
    }
    
    mudt_b <- meandt_b + udt_b
    ydt_b <- mudt_b + edt_b
    
    ydt.mat <- matrix(0, nrow = nD, ncol = nT)
    mudt.mat <- matrix(0, nrow = nD, ncol = nT)
    for (t in 1:nT) {
      ydt.mat[, t] <- ydt_b[seq(from = t, to = M, by = nT)]
      mudt.mat[, t] <- mudt_b[seq(from = t, to = M, by = nT)]
    }
    
    ydt.df <- setNames(as.data.frame(ydt.mat), paste0("Y", 1:nT))
    
    # Rebuild formulas with same RHS
    formula.b <- lapply(1:nT, function(t) {
      rhs <- paste(attr(terms(formula[[t]]), "term.labels"), collapse = " + ")
      as.formula(paste0("Y", t, " ~ ", rhs))
    })
    
    used_vars <- unique(unlist(lapply(formula, all.vars)))
    data.b <- cbind(ydt.df, data[, used_vars, drop = FALSE], sigmaedts)
    
    result.b <- tryCatch({
      eblupMFH2(formula = formula.b, vardir = vardir, data = data.b)
    }, error = function(e) NULL)
    
    if (is.null(result.b) || !result.b$fit$convergence) {
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
    warning("pbmcpeMFH2(): no successful bootstrap refits; returning NULL.")
    return(NULL)
  }
  mcpedt.b <- mcpedt / n_valid
  
  mse  <- mcpedt.b[, 1:nT, drop = FALSE]
  mcpe <- mcpedt.b[, (nT + 1):(nT + nT * (nT - 1) / 2), drop = FALSE]
  
  # Optional: always name columns (nice for downstream code)
  if (nT >= 2) {
    colnames(mcpe) <- apply(combn(nT, 2), 2, function(pair) paste0("(", pair[1], ",", pair[2], ")"))
  }
  
  list(
    eblup  = result$eblup,
    mse    = mse,
    mcpe   = mcpe,
    fails  = countfail
  )
}
















