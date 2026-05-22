#' Robust REML wrapper for eblupMFH2()
#'
#' msae's `eblupMFH2()` uses unconstrained Newton-Raphson to estimate the
#' random-effects variance components (sigma2_u, rho_u). During iteration
#' sigma2_u can be driven negative; the function clamps it to 0 only AFTER
#' the loop terminates, which means the optimizer frequently converges to
#' a spurious boundary at sigma2_u = 0. The fingerprint is a refvar that
#' jumps non-monotonically as inputs vary smoothly -- strong evidence that
#' the zero is a numerical artifact rather than a true REML boundary.
#'
#' This wrapper:
#'   1. Calls the original `eblupMFH2()` first (the AST-patched version
#'      that fixes the R 4.4 `&&` issue, but otherwise unchanged).
#'   2. If it returns a positive refvar, returns the result unchanged --
#'      so well-behaved fits keep using the standard msae implementation.
#'   3. If refvar = 0 (boundary), refits the variance components via
#'      `optim()` over the unconstrained parameterization
#'         tau = log(sigma2_u),  eta = atanh(rho_u)
#'      so sigma2_u > 0 and |rho_u| < 1 are enforced by construction.
#'      Multiple starting points are tried to reduce the chance of being
#'      trapped in a local optimum. After optim() converges, the full fit
#'      object (eblup, MSE, randomEffect, fit$*) is reconstructed using
#'      the same plug-in formulas as msae's post-iteration block, so
#'      downstream code (`pbmcpe_*`, `compare_mfh`, benchmarking) sees an
#'      interchangeable structure.
#'
#'   4. If `optim()` itself fails or also returns a near-zero variance,
#'      the original (boundary) fit is returned unchanged with attribute
#'      `.robust_refit_failed = TRUE` so callers can detect it.
#'
#' Parameter contract: same as `eblupMFH2()` plus an extra `.orig_fn`
#' argument that callers MUST pass (see the patch-msae chunk in
#' qmd/50-mfh_v2.qmd) to avoid infinite recursion when the global
#' `eblupMFH2` has been replaced by this wrapper.
#'
#' Result contract: same shape as `eblupMFH2()`, with the addition of an
#' attribute `.robust_refit_used = TRUE` on the result whenever the
#' optim-based refit was used (so downstream diagnostics can surface it).

# Local copy of msae's df2matR -- replicated rather than ::: so this module
# is namespace-safe and works regardless of which msae version is installed.
.df2matR_local <- function(var.df, r) {
  if (dim(var.df)[2] != sum(1:r))
    stop(".df2matR_local: ncol(var.df) does not match r")
  var.df <- as.data.frame(var.df)
  n <- nrow(var.df)
  R <- lapply(var.df, diag)
  R_1n <- matrix()
  for (i in 1:r) {
    R.row <- R[[i]]
    for (j in i:r) {
      if (i != j) R.row <- cbind(R.row, R[[sum((r - i):r) + j - r]])
    }
    if (i == 1) {
      R_1n <- R.row
    } else {
      tmp <- matrix(rep(0, n * n * (i - 1)), n, n * (i - 1))
      R.row <- cbind(tmp, R.row)
      R_1n <- rbind(R_1n, R.row)
    }
  }
  for (i in 1:(r * n)) {
    for (j in i:(r * n)) {
      if (R_1n[j, i] != R_1n[i, j]) R_1n[j, i] <- R_1n[i, j]
    }
  }
  R_1n
}

# Build Omega_AR (r x r), G (r x r), and GI (rn x rn) given (sigma2_u, rho).
.build_G_GI <- function(sigma2_u, rho, r, n, I_n) {
  Omega_AR <- matrix(0, r, r)
  for (i in 1:r) for (j in 1:r) {
    Omega_AR[i, j] <- rho^abs(i - j) / (1 - rho^2)
  }
  G <- sigma2_u * Omega_AR
  GI <- kronecker(G, I_n)
  list(Omega_AR = Omega_AR, G = G, GI = GI)
}

#' Construct the full fit object given converged (sigma2_u, rho_u).
#'
#' Mirrors msae::eblupMFH2()'s post-iteration block exactly, so the
#' returned list has the same shape (eblup, MSE, randomEffect, Rmatrix,
#' fit$method/convergence/iterations/estcoef/refvar/rho/informationFisher).
.eblupMFH2_finalize <- function(sigma2_u, rho_u, y.matrix, x.matrix, R,
                                r, n, y.var, convergence, iterations) {
  I_n <- diag(n)
  Id  <- diag(r)
  bg  <- .build_G_GI(sigma2_u, rho_u, r, n, I_n)
  Omega_AR <- bg$Omega_AR
  GI <- bg$GI
  Omega <- solve(GI + R)              # = V_inv
  Xto   <- t(Omega %*% x.matrix)
  Qh    <- solve(Xto %*% x.matrix)    # = (X'V_inv X)_inv
  P     <- Omega - t(Xto) %*% Qh %*% Xto

  # Derivative matrices (for Fisher information & g3)
  d.Omega <- list()
  d.Omega[[1]] <- kronecker(Omega_AR, I_n)
  d.Omega[[2]] <- matrix(NA, r, r)
  for (i in 1:r) for (j in 1:r) {
    k <- abs(i - j)
    d.Omega[[2]][i, j] <- sigma2_u *
      (k * rho_u^(k - 1) + (2 - k) * rho_u^(k + 1)) /
      ((1 - rho_u^2)^2)
  }
  d.Omega[[2]] <- kronecker(d.Omega[[2]], I_n)

  # Fisher information (expected) at converged (sigma2_u, rho_u)
  iF <- matrix(unlist(lapply(d.Omega, function(x)
    lapply(d.Omega, function(y) 0.5 * sum(diag(P %*% x %*% P %*% y))))), 2)
  FI <- tryCatch(solve(iF), error = function(e) matrix(NA, 2, 2))

  # Coefficients
  beta <- Qh %*% Xto %*% y.matrix
  res  <- y.matrix - x.matrix %*% beta
  eblup <- x.matrix %*% beta + GI %*% Omega %*% res

  eblup.df <- data.frame(matrix(eblup, n, r))
  names(eblup.df) <- y.var

  se.b  <- sqrt(diag(Qh))
  t.val <- beta / se.b
  pv    <- 2 * pnorm(abs(t.val), lower.tail = FALSE)
  coef  <- cbind(beta, se.b, t.val, pv)
  colnames(coef) <- c("beta", "std.error", "t.statistics", "p.value")

  # MSE: g1 + g2 + 2*g3
  d_proj <- kronecker(Id, I_n) - GI %*% Omega
  gg1 <- diag(GI %*% Omega %*% R)
  gg2 <- diag(d_proj %*% x.matrix %*% Qh %*% t(x.matrix) %*% t(d_proj))
  dg  <- lapply(d.Omega, function(x) x %*% Omega - GI %*% Omega %*% x %*% Omega)
  g3  <- list()
  for (i in 1:2) for (j in 1:2) {
    g3[[(i - 1) * 2 + j]] <-
      (if (all(is.finite(FI))) FI[i, j] else 0) *
      (dg[[i]] %*% (GI + R) %*% t(dg[[j]]))
  }
  gg3 <- diag(Reduce("+", g3))
  mse <- gg1 + gg2 + 2 * gg3

  mse.df <- data.frame(matrix(0, n, r))
  names(mse.df) <- y.var
  for (i in 1:r) mse.df[, i] <- mse[((i - 1) * n + 1):(i * n)]

  u.cap <- GI %*% Omega %*% res
  u.cap.df <- as.data.frame(matrix(u.cap, n, r))
  names(u.cap.df) <- y.var

  T.test <- if (all(is.finite(FI))) rho_u / sqrt(FI[2, 2]) else NA_real_
  p.val  <- if (is.finite(T.test)) 2 * pnorm(abs(T.test), lower.tail = FALSE) else NA_real_
  rho.df <- data.frame(rho = signif(rho_u, 5),
                       T.test = signif(T.test, 5),
                       `p-value` = signif(p.val, 5),
                       check.names = FALSE)

  list(
    eblup        = signif(eblup.df, digits = 5),
    MSE          = signif(mse.df, digits = 5),
    randomEffect = signif(u.cap.df, digits = 5),
    Rmatrix      = signif(R, digits = 5),
    fit = list(
      method            = "REML (robust: optim, log(sigma2_u))",
      convergence       = convergence,
      iterations        = iterations,
      estcoef           = coef,
      refvar            = signif(sigma2_u, digits = 5),
      rho               = rho.df,
      informationFisher = signif(iF, digits = 5)
    )
  )
}

#' Refit (sigma2_u, rho_u) by maximizing the REML profile log-likelihood
#' with optim(), using the unconstrained parameterization
#'   tau = log(sigma2_u), eta = atanh(rho_u).
#' Returns a finalized fit object on success, or NULL on failure.
.eblupMFH2_optim_refit <- function(formula, vardir, data) {
  r <- length(formula)
  y_terms <- lapply(formula, function(f) model.frame(f, data, na.action = na.omit))
  y.matrix <- unlist(lapply(y_terms, function(mf) mf[[1]]))
  if (!requireNamespace("magic", quietly = TRUE)) {
    stop(".eblupMFH2_optim_refit: package 'magic' is required (msae uses it for adiag).")
  }
  x.matrix <- Reduce(magic::adiag,
                     lapply(formula, function(f) model.matrix(f, data)))
  n <- length(y.matrix) / r
  y.var <- sapply(formula, "[[", 2)

  vardir_df <- data[, vardir, drop = FALSE]
  if (any(is.na(vardir_df))) {
    return(NULL)
  }
  R <- .df2matR_local(vardir_df, r)
  I_n <- diag(n)

  neg_reml <- function(par) {
    tau <- par[1]; eta <- par[2]
    sigma2_u <- exp(tau)
    rho      <- tanh(eta)
    if (!is.finite(sigma2_u) || !is.finite(rho) || abs(rho) > 1 - 1e-9) {
      return(1e10)
    }
    bg <- .build_G_GI(sigma2_u, rho, r, n, I_n)
    V  <- bg$GI + R
    Vc <- tryCatch(chol(V), error = function(e) NULL)
    if (is.null(Vc)) return(1e10)
    log_det_V <- 2 * sum(log(diag(Vc)))
    Vinv <- chol2inv(Vc)
    XtVinvX <- t(x.matrix) %*% Vinv %*% x.matrix
    Mc <- tryCatch(chol(XtVinvX), error = function(e) NULL)
    if (is.null(Mc)) return(1e10)
    log_det_M <- 2 * sum(log(diag(Mc)))
    Minv <- chol2inv(Mc)
    VinvX <- Vinv %*% x.matrix
    P <- Vinv - VinvX %*% Minv %*% t(VinvX)
    yPy <- as.numeric(t(y.matrix) %*% P %*% y.matrix)
    val <- 0.5 * (log_det_V + log_det_M + yPy)
    if (!is.finite(val)) return(1e10)
    val
  }

  diag_R <- diag(R)
  base_var <- max(mean(diag_R, na.rm = TRUE), 1e-8)
  start_grid <- list(
    c(log(0.05 * base_var),  atanh(0.30)),
    c(log(0.20 * base_var),  atanh(0.00)),
    c(log(0.50 * base_var),  atanh(0.50)),
    c(log(0.01 * base_var),  atanh(0.70)),
    c(log(1.00 * base_var),  atanh(-0.30))
  )

  best <- NULL
  best_val <- Inf
  for (init_par in start_grid) {
    opt <- tryCatch(
      optim(init_par, neg_reml, method = "BFGS",
            control = list(maxit = 500, reltol = 1e-9)),
      error = function(e) NULL
    )
    if (is.null(opt) || !is.finite(opt$value)) next
    if (opt$value < best_val) {
      best_val <- opt$value
      best <- opt
    }
  }
  if (is.null(best)) return(NULL)

  sigma2_u <- exp(best$par[1])
  rho_u    <- tanh(best$par[2])
  if (!is.finite(sigma2_u) || sigma2_u < 1e-10) return(NULL)

  .eblupMFH2_finalize(
    sigma2_u    = sigma2_u,
    rho_u       = rho_u,
    y.matrix    = y.matrix,
    x.matrix    = x.matrix,
    R           = R,
    r           = r,
    n           = n,
    y.var       = y.var,
    convergence = (best$convergence == 0),
    iterations  = best$counts[["function"]]
  )
}

#' Public wrapper. Drop-in replacement for eblupMFH2().
#'
#' `.orig_fn` MUST be the underlying (non-wrapped) eblupMFH2() -- typically
#' the AST-patched copy stashed at `.eblupMFH2_patched` by the patch-msae
#' chunk. This is required because callers usually invoke `eblupMFH2(...)`
#' which has been globally replaced by this wrapper, so passing the
#' wrapper itself as `.orig_fn` would loop forever.
eblupMFH2_robust <- function(formula, vardir, MAXITER = 100, PRECISION = 1e-04,
                             data, .orig_fn = NULL) {
  if (is.null(.orig_fn)) {
    if (exists(".eblupMFH2_patched", envir = globalenv()) &&
        is.function(get(".eblupMFH2_patched", envir = globalenv()))) {
      .orig_fn <- get(".eblupMFH2_patched", envir = globalenv())
    } else {
      .orig_fn <- getFromNamespace("eblupMFH2", "msae")
    }
  }

  base_fit <- tryCatch(
    .orig_fn(formula = formula, vardir = vardir, MAXITER = MAXITER,
             PRECISION = PRECISION, data = data),
    error = function(e) NULL
  )

  base_refvar <- if (!is.null(base_fit)) {
    v <- base_fit$fit$refvar
    if (is.null(v) || any(is.na(v))) 0 else as.numeric(v)
  } else NA_real_

  # Standard path: original eblupMFH2 returned a usable positive refvar.
  # Return as-is -- no robust refit needed.
  if (!is.na(base_refvar) && base_refvar > 1e-12) return(base_fit)

  # Boundary fallback: refit via optim() with constrained parameterization.
  refit <- tryCatch(
    .eblupMFH2_optim_refit(formula = formula, vardir = vardir, data = data),
    error = function(e) NULL
  )
  if (is.null(refit)) {
    if (!is.null(base_fit)) attr(base_fit, ".robust_refit_failed") <- TRUE
    return(base_fit)
  }
  attr(refit, ".robust_refit_used") <- TRUE
  refit
}
