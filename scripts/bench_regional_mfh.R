#' Regional Benchmarking for MFH Model Outputs
#'
#' Applies ratio-type regional benchmarking to Multivariate Fay-Herriot
#' (MFH) EBLUP estimates. For each region r and time period t, a ratio
#' factor lambda_r_t = B_r_t / EBLUP_bar_r_t is applied so that the
#' population-weighted mean of benchmarked EBLUPs within the region
#' equals the population-weighted mean of direct estimates (the
#' regional benchmark).
#'
#' When MSE = TRUE, a parametric bootstrap is used to estimate the MSE
#' and MCPE of the benchmarked estimates. The bootstrap respects the
#' temporal covariance structure of whichever MFH variant was chosen
#' via the `model_type` argument ("MFH1", "MFH2", or "MFH3"). MFH2
#' uses a homoskedastic AR(1) DGP; MFH1 uses independent random effects
#' with possibly time-varying variance; MFH3 uses a heteroskedastic
#' AR(1) DGP.
#'
#' @param eblup_mat  D x nT matrix of EBLUP estimates (from model$eblup).
#' @param mse_mat    D x nT matrix of MSE estimates (from model$MSE or
#'                   bootstrap MSE).
#' @param mcpe_mat   D x ncol matrix of MCPE estimates (from bootstrap).
#'                   Can be NULL if not available.
#' @param domain_vec Character/integer vector of domain identifiers (length D).
#' @param region_vec Character/integer vector of region identifiers (length D).
#' @param Nd_vec     Numeric vector of domain population sizes (length D).
#'                   Used for all time periods unless `Nd_mat` is supplied.
#' @param Nd_mat     Optional D x nT matrix of domain population sizes by
#'                   time period. Rows must align with `domain_vec`; columns
#'                   must align with `eblup_mat` / `direct_mat`.
#' @param direct_mat D x nT matrix of direct poverty rate estimates.
#' @param regional_benchmark_mat Optional R x nT matrix of externally computed
#'                   regional benchmark targets. Row names must match region
#'                   identifiers in `region_vec`; columns must align with time.
#'                   If NULL, benchmarks are computed from `direct_mat`.
#' @param model_obj  The fitted MFH model object (from eblupMFH1/2/3), needed
#'                   for bootstrap MSE. Can be NULL if MSE = FALSE.
#' @param model_type Character. One of "MFH1", "MFH2", "MFH3". Controls both
#'                   the bootstrap DGP (how random effects are generated) and
#'                   which msae estimator is used to refit the bootstrap sample.
#'                   Default "MFH2" for backward compatibility.
#' @param formula    List of formulas used in MFH fitting. Needed for bootstrap.
#' @param vardir     Character vector of vardir column names. Needed for bootstrap.
#' @param data       The domain_dt data frame used for model fitting.
#'                   Needed for bootstrap.
#' @param MSE        Logical. If TRUE, compute bootstrap MSE and MCPE for
#'                   benchmarked estimates. Default FALSE.
#' @param nB         Number of bootstrap replications for MSE. Default 200.
#' @param seed       Random seed. Default 123.
#'
#' @return A list with components:
#'   \item{eblup_bench}{D x nT matrix of benchmarked EBLUPs.}
#'   \item{mse_bench}{D x nT matrix of benchmarked MSEs (NULL if MSE = FALSE).}
#'   \item{mcpe_bench}{D x ncol matrix of benchmarked MCPEs (NULL if MSE = FALSE or nT < 2).}
#'   \item{lambda}{D x nT matrix of ratio adjustment factors applied.}
#'   \item{fails}{Number of failed bootstrap iterations (0 if MSE = FALSE).}

bench_regional_mfh <- function(eblup_mat,
                                mse_mat    = NULL,
                                mcpe_mat   = NULL,
                                domain_vec,
                                region_vec,
                                Nd_vec,
                                Nd_mat     = NULL,
                                direct_mat,
                                regional_benchmark_mat = NULL,
                                model_obj  = NULL,
                                model_type = "MFH2",
                                formula    = NULL,
                                vardir     = NULL,
                                data       = NULL,
                                MSE        = FALSE,
                                nB         = 200,
                                seed       = 123,
                                max_attempts = NULL) {
  # max_attempts: hard cap on total bootstrap iterations to prevent the
  # benchmarked-MCPE loop from spinning forever when refits keep failing
  # to converge (typically with var_choice = "direct"). If reached, the
  # function returns lambda + benchmarked EBLUPs but with NULL MSE/MCPE
  # and a warning.
  if (is.null(max_attempts)) max_attempts <- 5L * nB

  model_type <- toupper(as.character(model_type))
  if (!model_type %in% c("MFH1", "MFH2", "MFH3")) {
    stop("bench_regional_mfh: model_type must be one of 'MFH1', 'MFH2', 'MFH3' (got '",
         model_type, "')")
  }

  eps_denom <- 1e-8
  eblup_mat  <- as.matrix(eblup_mat)
  direct_mat <- as.matrix(direct_mat)
  nD <- nrow(eblup_mat)
  nT <- ncol(eblup_mat)

  stopifnot(length(domain_vec) == nD)
  stopifnot(length(region_vec) == nD)
  stopifnot(length(Nd_vec) == nD)
  stopifnot(nrow(direct_mat) == nD && ncol(direct_mat) == nT)

  .ids_equal <- function(x, y) identical(as.character(x), as.character(y))
  .check_domain_order <- function(mat, label) {
    rn <- rownames(mat)
    if (!is.null(rn) && !.ids_equal(rn, domain_vec)) {
      stop(label, " row names do not match domain_vec order.")
    }
  }
  .check_time_order <- function(mat, label) {
    cn_ref <- colnames(eblup_mat)
    cn <- colnames(mat)
    if (!is.null(cn_ref) && !is.null(cn) && !.ids_equal(cn, cn_ref)) {
      stop(label, " column names do not match eblup_mat column order.")
    }
  }
  .check_domain_order(eblup_mat, "eblup_mat")
  .check_domain_order(direct_mat, "direct_mat")
  .check_time_order(direct_mat, "direct_mat")

  if (is.null(Nd_mat)) {
    Nd_mat <- matrix(Nd_vec, nrow = nD, ncol = nT)
  } else {
    Nd_mat <- as.matrix(Nd_mat)
    stopifnot(nrow(Nd_mat) == nD && ncol(Nd_mat) == nT)
    .check_domain_order(Nd_mat, "Nd_mat")
    .check_time_order(Nd_mat, "Nd_mat")
  }
  storage.mode(Nd_mat) <- "double"

  if (!is.null(regional_benchmark_mat)) {
    regional_benchmark_mat <- as.matrix(regional_benchmark_mat)
    stopifnot(ncol(regional_benchmark_mat) == nT)
    if (is.null(rownames(regional_benchmark_mat))) {
      stop("regional_benchmark_mat must have row names matching region identifiers.")
    }
    .check_time_order(regional_benchmark_mat, "regional_benchmark_mat")
  }

  # ---- Point-estimate benchmarking ----
  apply_bench <- function(est_mat, direct_mat, region_vec, Nd_mat,
                          regional_benchmark_mat = NULL) {
    # Returns a list: benchmarked matrix + lambda matrix
    bench_mat  <- matrix(NA_real_, nrow = nD, ncol = nT)
    lambda_mat <- matrix(1, nrow = nD, ncol = nT)
    reg_ids <- sort(unique(region_vec[!is.na(region_vec)]))

    for (tt in seq_len(nT)) {
      for (r in reg_ids) {
        rows <- which(region_vec == r)
        Nd_t <- Nd_mat[, tt]

        if (!is.null(regional_benchmark_mat)) {
          region_key <- as.character(r)
          if (!region_key %in% rownames(regional_benchmark_mat)) {
            stop("regional_benchmark_mat is missing region '", region_key, "'.")
          }
          B_r <- regional_benchmark_mat[region_key, tt]
          basis_rows <- rows[is.finite(est_mat[rows, tt]) & is.finite(Nd_t[rows])]
        } else {
          # Internal fallback: compute the regional benchmark from the same
          # domain subset used for the model-side regional mean. This avoids
          # mixing a direct-estimate subset with an all-domain EBLUP mean.
          basis_rows <- rows[
            is.finite(direct_mat[rows, tt]) &
              is.finite(est_mat[rows, tt]) &
              is.finite(Nd_t[rows])
          ]
          B_r <- if (length(basis_rows) > 0) {
            weighted.mean(direct_mat[basis_rows, tt], Nd_t[basis_rows])
          } else {
            NA_real_
          }
        }

        # Population-weighted mean of model EBLUPs on the same basis as B_r
        est_bar_r <- if (length(basis_rows) > 0) {
          weighted.mean(est_mat[basis_rows, tt], Nd_t[basis_rows])
        } else {
          NA_real_
        }

        # Ratio factor
        if (!is.na(B_r) && !is.na(est_bar_r) && abs(est_bar_r) > eps_denom) {
          lambda_r <- B_r / est_bar_r
        } else {
          lambda_r <- 1
        }

        lambda_mat[rows, tt] <- lambda_r
        bench_mat[rows, tt]  <- est_mat[rows, tt] * lambda_r
      }
    }
    list(bench = bench_mat, lambda = lambda_mat)
  }

  point_result <- apply_bench(eblup_mat, direct_mat, region_vec, Nd_mat,
                              regional_benchmark_mat)
  eblup_bench  <- point_result$bench
  lambda_mat   <- point_result$lambda

  # Preserve column names from original
  colnames(eblup_bench) <- colnames(eblup_mat)
  colnames(lambda_mat)  <- colnames(eblup_mat)

  # ---- Bootstrap MSE + MCPE for benchmarked estimates ----
  mse_bench  <- NULL
  mcpe_bench <- NULL
  countfail  <- 0

  if (MSE) {
    if (is.null(model_obj)) stop("model_obj required when MSE = TRUE")
    if (is.null(formula))   stop("formula required when MSE = TRUE")
    if (is.null(vardir))    stop("vardir required when MSE = TRUE")
    if (is.null(data))      stop("data required when MSE = TRUE")

    if (!is.null(seed)) set.seed(seed)

    M <- nD * nT

    # Extract model parameters
    beta_list <- list()
    X_list <- lapply(formula, function(f) model.matrix(f, data))
    p_list <- sapply(X_list, ncol)

    estcoef_mat_fit <- model_obj$fit$estcoef
    start_row <- 1
    for (tt in seq_len(nT)) {
      end_row <- start_row + p_list[tt] - 1
      beta_list[[tt]] <- estcoef_mat_fit[start_row:end_row, 1]
      start_row <- end_row + 1
    }

    # ---- Extract variance-component parameters by model type ----
    # MFH2: scalar refvar and scalar rho (stationary AR(1))
    # MFH3: vector refvar (length nT) and scalar rho (het. AR(1))
    # MFH1: vector refvar (length nT), no rho (independent random effects)
    varu2_raw <- model_obj$fit$refvar
    rho_raw   <- model_obj$fit$rho

    # Normalise refvar into a length-nT numeric
    varu2_vec <- as.numeric(varu2_raw)
    if (length(varu2_vec) == 1L) varu2_vec <- rep(varu2_vec, nT)
    if (length(varu2_vec) != nT) {
      stop("bench_regional_mfh: refvar from model_obj has unexpected length ",
           length(varu2_vec), " (expected 1 or ", nT, ")")
    }

    # Normalise rho into a scalar (0 for MFH1)
    if (identical(model_type, "MFH1") || is.null(rho_raw)) {
      rho_scalar <- 0
    } else if (is.matrix(rho_raw) || is.data.frame(rho_raw)) {
      rho_scalar <- as.numeric(rho_raw[, 1])[1]
    } else {
      rho_scalar <- as.numeric(rho_raw)[1]
    }

    # For MFH2 the legacy variable name `varu2` appears below; keep it in scope.
    varu2 <- varu2_vec[1]
    rho   <- rho_scalar
    Unomenrho2_05 <- if (abs(rho_scalar) < 1) (1 - rho_scalar^2)^(-0.5) else 1

    # Prepare sampling error covariance matrices
    sigmaedts <- as.matrix(data[, vardir, drop = FALSE])
    storage.mode(sigmaedts) <- "double"
    Sigmaed <- array(0, c(nT, nT, nD))
    for (d in seq_len(nD)) {
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
      if (!matrixcalc::is.positive.definite(Sigmaed[, , d])) {
        Sigmaed[, , d] <- as.matrix(Matrix::nearPD(Sigmaed[, , d],
                                                     keepDiag = TRUE)$mat)
      }
    }

    # Accumulator for MSE + MCPE of benchmarked estimates
    n_mcpe_cols <- nT + nT * (nT - 1) / 2
    mcpedt_bench <- matrix(0, nrow = nD, ncol = n_mcpe_cols)

    # Pick the msae refit function for the bootstrap samples
    refit_fn <- switch(
      model_type,
      "MFH1" = eblupMFH1,
      "MFH2" = eblupMFH2,
      "MFH3" = eblupMFH3
    )

    b <- 1
    bench_attempts <- 0L
    bench_aborted  <- FALSE
    while (b <= nB) {
      bench_attempts <- bench_attempts + 1L
      if (bench_attempts > max_attempts) {
        warning(sprintf(
          "bench_regional_mfh(): hit max_attempts=%d after %d successful and %d failed bootstrap refits; aborting bootstrap MSE/MCPE for benchmarked estimates. Returning benchmarked EBLUPs with NULL MSE/MCPE.",
          max_attempts, b - 1L, countfail
        ))
        bench_aborted <- TRUE
        break
      }
      message(sprintf("Bench bootstrap %d / %d (%s)", b, nB, model_type))

      # --- Generate bootstrap random effects, DGP depends on model_type ---
      udt_b    <- rep(0, M)
      edt_b    <- rep(0, M)
      meandt_b <- rep(0, M)

      # For MFH2 (scalar varu2, scalar rho) we keep the original AR(1)
      # generator. For MFH3 the AR(1) structure still holds but with
      # time-varying innovation variance varu2_vec[t]. For MFH1 there is
      # no AR(1): we just draw u[d,t] ~ N(0, varu2_vec[t]) independently.
      if (model_type == "MFH1") {
        # Independent random effects across time (and across domains)
        udt_mat_gen <- matrix(
          rnorm(M, mean = 0, sd = rep(sqrt(varu2_vec), times = nD)),
          nrow = nD, ncol = nT, byrow = TRUE
        )
        # Store in row-major domain-then-time order for backward-compat
        for (d in seq_len(nD)) {
          idx_d <- (d - 1) * nT + seq_len(nT)
          udt_b[idx_d] <- udt_mat_gen[d, ]
        }
      } else {
        # MFH2 / MFH3: AR(1) on random effects with scalar rho.
        # For MFH3 innovation variance varies by time (varu2_vec[t]).
        adt_b <- rnorm(M, mean = 0,
                       sd = rep(sqrt(varu2_vec), times = nD))
        i <- 1
        for (d in seq_len(nD)) {
          # MFH2 is homoskedastic, so the stationary AR(1) initialization is
          # appropriate. MFH3 is heteroskedastic over time; use the period-1
          # random-effect variance directly rather than imposing stationarity.
          udt_b[i] <- if (model_type == "MFH2") Unomenrho2_05 * adt_b[i] else adt_b[i]
          for (tt in 2:nT) {
            i <- i + 1
            udt_b[i] <- rho_scalar * udt_b[i - 1] + adt_b[i]
          }
          i <- i + 1
        }
      }

      # Sampling errors + mean structure (same across variants)
      i <- 1
      for (d in seq_len(nD)) {
        edt_b[i:(i + nT - 1)] <- MASS::mvrnorm(1, mu = rep(0, nT),
                                                 Sigma = Sigmaed[, , d])
        for (tt in seq_len(nT)) {
          meandt_b[i + tt - 1] <- X_list[[tt]][d, ] %*% beta_list[[tt]]
        }
        i <- i + nT
      }

      mudt_b <- meandt_b + udt_b   # true area means
      ydt_b  <- mudt_b + edt_b     # observed (direct) data

      # Reshape to D x nT matrices
      mudt_mat <- matrix(0, nrow = nD, ncol = nT)
      ydt_mat  <- matrix(0, nrow = nD, ncol = nT)
      for (tt in seq_len(nT)) {
        mudt_mat[, tt] <- mudt_b[seq(from = tt, to = M, by = nT)]
        ydt_mat[, tt]  <- ydt_b[seq(from = tt, to = M, by = nT)]
      }

      # Refit MFH2 on bootstrap data
      ydt_df <- setNames(as.data.frame(ydt_mat), paste0("Y", seq_len(nT)))
      formula_b <- lapply(seq_len(nT), function(tt) {
        rhs <- paste(attr(terms(formula[[tt]]), "term.labels"), collapse = " + ")
        as.formula(paste0("Y", tt, " ~ ", rhs))
      })

      used_vars <- unique(unlist(lapply(formula, all.vars)))
      data_b <- cbind(ydt_df, data[, used_vars, drop = FALSE], sigmaedts)

      result_b <- tryCatch({
        refit_fn(formula = formula_b, vardir = vardir, data = data_b)
      }, error = function(e) NULL)

      if (is.null(result_b) || !result_b$fit$convergence) {
        countfail <- countfail + 1
        next
      }

      # Benchmark the bootstrap EBLUPs using the same direct data as
      # benchmarks (from the bootstrap world: ydt_mat is the "direct")
      bench_b <- apply_bench(result_b$eblup, ydt_mat, region_vec, Nd_mat,
                             regional_benchmark_mat = NULL)
      est_bench_b <- bench_b$bench

      # Use unbenchmarked true values for MSE computation
      # (Datta et al. 2011: MSE = E[(benchmarked_est - true)^2])
      true_bench_mat <- mudt_mat

      # Compute squared differences and cross-products
      dif <- est_bench_b - true_bench_mat
      dif_b <- matrix(0, nrow = nD, ncol = n_mcpe_cols)
      pos <- nT
      for (t1 in 1:(nT - 1)) {
        dif_b[, t1] <- dif[, t1]^2
        for (t2 in (t1 + 1):nT) {
          pos <- pos + 1
          dif_b[, pos] <- dif[, t1] * dif[, t2]
        }
      }
      dif_b[, nT] <- dif[, nT]^2

      mcpedt_bench <- mcpedt_bench + dif_b
      b <- b + 1
    }

    if (bench_aborted) {
      # Bootstrap aborted before any usable iterations: leave MSE/MCPE NULL.
      mse_bench  <- NULL
      mcpe_bench <- NULL
    } else {
      n_valid <- b - 1L
      if (n_valid <= 0L) {
        warning("bench_regional_mfh(): no successful bootstrap refits; returning benchmarked EBLUPs with NULL MSE/MCPE.")
        mse_bench <- NULL
        mcpe_bench <- NULL
      } else {
        mcpedt_avg <- mcpedt_bench / n_valid

        mse_bench  <- mcpedt_avg[, 1:nT, drop = FALSE]
        colnames(mse_bench) <- colnames(eblup_mat)

        if (nT >= 2) {
          mcpe_bench <- mcpedt_avg[, (nT + 1):n_mcpe_cols, drop = FALSE]
          colnames(mcpe_bench) <- apply(combn(nT, 2), 2, function(pair) {
            paste0("(", pair[1], ",", pair[2], ")")
          })
        }
      }
    }
  }

  list(
    eblup_bench = eblup_bench,
    mse_bench   = mse_bench,
    mcpe_bench  = mcpe_bench,
    lambda      = lambda_mat,
    fails       = countfail
  )
}
