# ====================================================================
# FGT indicator helpers (shared across UFH, MFH, Comparison QMDs)
# ====================================================================

#' Compute the Foster-Greer-Thorbecke poverty indicator
#'
#' @param welfare Numeric vector of welfare values (income/consumption).
#' @param povline Numeric vector (or scalar) of poverty lines.
#' @param alpha   Integer FGT order: 0 = headcount, 1 = gap, 2 = severity.
#' @return Numeric vector of FGT values in [0, 1].
compute_fgt <- function(welfare, povline, alpha = 0L) {
  gap <- pmax(0, (povline - welfare) / povline)
  if (alpha == 0L) as.numeric(welfare < povline) else gap^alpha
}

#' Human-readable labels for an FGT order
#'
#' @param alpha Integer FGT order (0, 1, or 2).
#' @return A named list with \code{short} (e.g. "Poverty Rate"),
#'   \code{noun} (e.g. "headcount"), and \code{fgt} (e.g. "FGT(0)").
fgt_label <- function(alpha = 0L) {
  switch(as.character(alpha),
    "0" = list(short = "Poverty Rate",     noun = "poverty headcount rate", fgt = "FGT(0)"),
    "1" = list(short = "Poverty Gap",      noun = "poverty gap",           fgt = "FGT(1)"),
    "2" = list(short = "Poverty Severity", noun = "poverty severity",      fgt = "FGT(2)"),
    stop("Unsupported FGT alpha: ", alpha)
  )
}

# ====================================================================

#' A function to perform variance smoothing
#' 
#' The variance smoothing function applies the methodology of You and Hiridoglou (2023)
#' which uses simply log linear regression to estimate direct variances for sample 
#' poverty rates which is useful for replacing poverty rates in areas with low sampling.
#' 
#' @param domain a vector of unique domain/target areas
#' @param direct_var the raw variances estimated from sample data
#' @param sampsize the sample size for each domain
#' 
#' @export

varsmoothie_king <- function(domain,
                             direct_var,
                             sampsize){
  
  dt <- data.table(Domain = domain,
                   var = direct_var,
                   n = sampsize)
  
  dt$log_n <- log(dt$n)
  dt$log_var <- log(dt$var)
  
  lm_model <- lm(formula = log_var ~ log_n,
                 data = dt[!(abs(dt$log_var) == Inf),])
  
  dt$pred_var <- predict(lm_model, newdata = dt)
  residual_var <-  summary(lm_model)$sigma^2
  dt$var_smooth <- exp(dt$pred_var) * exp(residual_var/2)
  
  return(dt[, c("Domain", "var_smooth"), with = F])
  
}




#' A function to perform stepwise variable selection based on selection criteria
#' 
#' @param dt data.frame, dataset containing the set of outcome and independent variables
#' @param xvars character vector, the set of x variables
#' @param y character, the name of the y variable
#' @param cor_thresh double, a correlation threshold between 0 and 1
#' @param criteria character, criteria that can be chosen are "AIC", "AICc", "AICb1", "AICb2", "BIC", "KIC", "KICc", "KICb1", or "KICb2". Defaults to "AIC". If transformation is set to "arcsin", only "AIC" and "BIC" can be chosen.
#' @param vardir character, name of the variable containing the domain-specific sampling variances of the direct estimates that are included in dt
#' @param transformation character, either "no" (default) or "arcsin".
#' @param eff_smpsize character, name of the variable containing the effective sample sizes that are included in dt. Required argument when the arcsin transformation is chosen. Defaults to NULL.
#' @param B When the information criteria by Marhuenda et al. (2014) should be computed (only available without transformation), B must be greater than 1. Defaults to 0. For practical applications, values larger than 200 are recommended.
#' 
#' @import data.table
#' @import caret
#' @importFrom emdi step fh

step_wrapper_fh <- function(dt, xvars, y, cor_thresh = 0.95, criteria = "AIC",
                         vardir, transformation = "no", backtransformation = "bc",
                         eff_smpsize, B = 0) {
  
  dt <- as.data.table(dt)
  
  # Drop columns that are entirely NA
  dt <- dt[, which(unlist(lapply(dt, function(x) !all(is.na(x))))), with = FALSE]
  
  xvars <- xvars[xvars %in% colnames(dt)]
  
  # Keep only complete cases
  dt <- dt[complete.cases(dt),] 
  
  # Step 1: Remove aliased (perfectly collinear) variables
  model_formula <- as.formula(paste(y, "~", paste(xvars, collapse = " + ")))
  lm_model <- lm(model_formula, data = dt)
  aliased <- is.na(coef(lm_model))
  if (any(aliased)) {
    xvars <- names(aliased)[!aliased & names(aliased) != "(Intercept)"]
  }
  
  # Step 2: Remove near-linear combinations
  xmat <- as.matrix(dt[, ..xvars])
  combo_check <- tryCatch(findLinearCombos(xmat), error = function(e) NULL)
  if (!is.null(combo_check) && length(combo_check$remove) > 0) {
    xvars <- xvars[-combo_check$remove]
    xmat <- as.matrix(dt[, ..xvars])
  }
  
  # Step 3: Drop highly correlated variables
  cor_mat <- abs(cor(xmat))
  diag(cor_mat) <- 0
  while (any(cor_mat > cor_thresh, na.rm = TRUE)) {
    cor_pairs <- which(cor_mat == max(cor_mat, na.rm = TRUE), arr.ind = TRUE)[1, ]
    var1 <- colnames(cor_mat)[cor_pairs[1]]
    var2 <- colnames(cor_mat)[cor_pairs[2]]
    # Drop the variable with higher mean correlation
    drop_var <- if (mean(cor_mat[var1, ]) > mean(cor_mat[var2, ])) var1 else var2
    xvars <- setdiff(xvars, drop_var)
    xmat <- as.matrix(dt[, ..xvars])
    cor_mat <- abs(cor(xmat))
    diag(cor_mat) <- 0
  }
  
  # Step 4: Warn if still ill-conditioned
  cond_number <- kappa(xmat, exact = TRUE)
  if (cond_number > 1e10) {
    warning("Design matrix is ill-conditioned (condition number > 1e10). Consider reviewing variable selection.")
  }
  
  # Final model fit
  model_formula <- as.formula(paste(y, "~", paste(xvars, collapse = " + ")))
  
  # Stepwise selection
  fh_args <- list(
    fixed = model_formula,
    vardir = vardir,
    combined_data = dt,
    method = "ml",
    MSE = FALSE
  )
  
  if (transformation == "arcsin") {
    fh_args$transformation <- "arcsin"
    fh_args$backtransformation <- backtransformation
    fh_args$eff_smpsize <- eff_smpsize
  } else {
    fh_args$transformation <- "no"
    fh_args$B <- c(0, B)
  }
  
  fh_model <- do.call(emdi::fh, fh_args)
  
  stepwise_model <- emdi::step(fh_model, criteria = criteria)
  
  return(stepwise_model)
  
}

#' A function to benchmark EBLUP and MSE estimates of a FH model.
#' 
#' @param model an object of type "fh".
#' @param benchmark a number determining the benchmark value.
#' @param share a vector containing the shares of the population size per area and the total population size (N_d/N).Values must be sorted like the domains in the fh object.
#' @param type character indicating the type of benchmarking. Types that can be chosen (i) Raking ("raking"), (ii) Ratio adjustment ("ratio"). Defaults to "raking".
#' @param overwrite if TRUE, the benchmarked FH estimates are added to the ind object of the emdi object and the MSE estimates are set to NULL since these are not benchmarked. Defaults to FALSE.
#' @param MSE if TRUE, MSE estimates are computed. Defaults to FALSE.
#' @param B a number defining the number of bootstrap iterations when MSE is set to TRUE. Defaults to 50. For practical applications, values larger than 200 are recommended.
#' @param seed an integer to set the seed for the random number generator. For the usage of random number generation see details. If seed is set to NULL, seed is chosen randomly. Defaults to 123.
#' 
#' @importFrom emdi fh benchmark
#' 
#' @export

# only raking and ratio
bench <- function(model, benchmark, share, type = "ratio", overwrite = FALSE,
                  MSE = FALSE, B = 50, seed = 123){
  
  check_benchmark_arguments(
    object = model, benchmark = benchmark,
    share = share, type = type, overwrite = overwrite
  )
  
  point_bench <- suppressMessages(emdi::benchmark(model,
                                                  benchmark = benchmark,
                                                  share = share, 
                                                  type = type,
                                                  overwrite = overwrite)) 
  
  if (!MSE) return(point_bench)
  
  if (!is.null(seed)) set.seed(seed)
  
  boot_bench <- function(model, sigmau2, vardir, combined_data, framework,
                         B, method = "reml",
                         transformation = "no", 
                         backtransformation = "naive",
                         eff_smpsize = NULL) {
    
    M <- framework$N_dom_smp + framework$N_dom_unobs
    m <- framework$N_dom_smp
    
    ### Bootstrap
    in_sample <- model$ind$Out == 0
    out_sample <- model$ind$Out == 1
    
    # Results matrices
    true_value_boot_bench <- matrix(NA, ncol = B, nrow = M)
    est_value_boot_bench <- matrix(NA, ncol = B, nrow = M)
    v_boot <- matrix(NA, ncol = B, nrow = M)
    res_boot <- matrix(NA, ncol = B, nrow = m)
    e_boot <- matrix(NA, ncol = B, nrow = m)
   
    # Get covariates for all domains
    pred_X <- model.matrix(update(model$fixed, helper ~ .), 
                           data = transform(framework$combined_data, 
                                            helper = rnorm(nrow(framework$combined_data))))
    # For nonparametric version
    # center and rescale vhat
    vhat <- model$model$random_effects[,1]
    vhat_cs <- ((vhat - mean(vhat)) / sqrt(sum( (vhat - mean(vhat))^2) / m) ) * sqrt(sigmau2)
    
    # center and rescale res
    res <- model$model$real_residuals#[,1]
    res_cs <- ((res - mean(res)) / sqrt(sum( (res - mean(res))^2) / m) )
    
    for (b in seq_len(B)) {
      tryCatch({
        
        # parametric version
       # v_boot <- rnorm(M, 0, sqrt(sigmau2))
       # e_boot <- rnorm(m, 0, sqrt(vardir)) 
        
        # nonparametric version
        v_boot[, b] <- sample(vhat_cs, M, replace = TRUE)
        res_boot[, b] <- sample(res_cs, m, replace = TRUE)
        e_boot[, b] <- res_boot[, b] * sqrt(vardir) 
        Xbeta_boot <- pred_X %*% model$model$coefficients$coefficients
        
        if (transformation == "no") {
          ## True Value for bootstraps
          true_value_boot_ <- Xbeta_boot + v_boot[, b]
          true_value_boot_bench[, b] <- true_value_boot_

          ystar <- rep(NA, M)
          ystar[in_sample] <- Xbeta_boot[in_sample] + v_boot[in_sample, b] + e_boot[, b]
          
          combined_data_star <- model$framework$combined_data
          combined_data_star$ystar <- ystar
          fh_star <- suppressMessages(
            update(model,
                   fixed = update.formula(formula(model), ystar ~ .),
                   combined_data = combined_data_star,
                   MSE = FALSE,
                   seed = NULL)
          )
          
          est_value_boot <- rep(NA, M)
          est_value_boot <- fh_star$ind$FH
          
          est_value_bench <- emdi::benchmark(fh_star,
                                             benchmark = benchmark,
                                             share = share, 
                                             type = type,
                                             overwrite = FALSE) 
          
          est_value_boot_bench[, b] <- est_value_bench$FH_Bench
          
          message("b =", b, "\n")
          
        }else if (transformation == "arcsin") {
          ## True Value for bootstraps
          ## Truncation
          true_value_boot_trans <- Xbeta_boot + v_boot[, b]
          true_value_boot_trans[true_value_boot_trans < 0] <- 0
          true_value_boot_trans[true_value_boot_trans > (pi / 2)] <- (pi / 2)
          
          ## Back-transformation
          ## true values without correction
          true_value_boot_ <- (sin(true_value_boot_trans))^2
          true_value_boot_bench[, b] <- true_value_boot_
          

          ystar_trans <- Xbeta_boot[in_sample] + v_boot[in_sample, b] + e_boot[, b]
         
          ## Estimation of sigmau2_boot on transformed scale
          framework2 <- framework
          framework2$direct <- ystar_trans
          
          framework2$model_X <- model.matrix(update(model$fixed, helper ~ .), 
                                             data = transform(framework$combined_data[in_sample, ], 
                                                              helper = rnorm(nrow(framework$combined_data[in_sample, ]))))
          sigmau2_boot <- wrapper_estsigmau2(
            framework = framework2, method = model$method$method,
            interval = c(0, var(framework2$direct)))
          
          x <- framework2$model_X
          ## Computation of the coefficients'estimator (Bstim)
          D <- diag(1, m)
          V <- sigmau2_boot * D %*% t(D) + diag(as.numeric(vardir))
          Vi <- solve(V)
          Q <- solve(t(x) %*% Vi %*% x)
          Beta.hat_boot <- Q %*% t(x) %*% Vi %*% ystar_trans
          
          ## Computation of the EBLUP
          res <- ystar_trans - c(x %*% Beta.hat_boot)
          Sigma.u <- sigmau2_boot * D
          u.hat <- Sigma.u %*% t(D) %*% Vi %*% res
          
          ## Estimated Small area mean on transformed scale for the out and in sample
          # values
          est_mean_boot_trans <- x %*% Beta.hat_boot + D %*% u.hat
          pred_out_boot_trans <- pred_X %*% Beta.hat_boot
          
          est_value_boot_trans <- rep(NA, M)
          est_value_boot_trans[in_sample] <- est_mean_boot_trans
          est_value_boot_trans[out_sample] <- pred_out_boot_trans[out_sample]
          
          gamma_trans <- as.numeric(vardir) / (sigmau2_boot + as.numeric(vardir))
          est_value_boot_trans_var <- sigmau2_boot * gamma_trans
          est_value_boot_trans_var_ <- rep(0, M)
          est_value_boot_trans_var_[in_sample] <- est_value_boot_trans_var
          
          
          # backtransformation
          if (backtransformation == "bc") {
            int_value <- NULL
            for (i in seq_len(M)) {
              if (in_sample[i] == T) {
                mu_dri <- est_value_boot_trans
                # Get value of first domain
                mu_dri <- mu_dri[i]
                
                Var_dri <- est_value_boot_trans_var_
                Var_dri <- as.numeric(Var_dri[i])
                
                integrand <- function(x, mean, sd) {
                  sin(x)^2 * dnorm(x, mean = mu_dri, sd = sqrt(Var_dri))
                }
                
                upper_bound <- min(
                  mean(framework$direct) + 10 * sd(framework$direct),
                  mu_dri + 100 * sqrt(Var_dri)
                )
                lower_bound <- max(
                  mean(framework$direct) - 10 * sd(framework$direct),
                  mu_dri - 100 * sqrt(Var_dri)
                )
                
                int_value <- c(int_value, integrate(integrand,
                                                    lower = 0, upper = pi / 2
                )$value)
              } else {
                int_value <- c(int_value, (sin(est_value_boot_trans[i]))^2)
              }
            }
          } else if (backtransformation == "naive") {
            int_value <- NULL
            for (i in seq_len(M)) {
              int_value <- c(int_value, (sin(est_value_boot_trans[i]))^2)
            }
          }
          est_value_boot <- int_value
          model_tmp <- model
          model_tmp$ind$FH <- est_value_boot
          est_value_bench <- emdi::benchmark(model_tmp,
                                             benchmark = benchmark,
                                             share = share, 
                                             type = type,
                                             overwrite = FALSE) 
          est_value_boot_bench[, b] <- est_value_bench$FH_Bench
        
          message("b =", b, "\n")
       } 

      }, error = function(e) {
        message("Bootstrap iteration ", b, " failed: ", e$message)
        # leave column as NA and continue
      }) # end tryCatch 
    } # End of bootstrap runs
    
    Quality_MSE <- function(estimator, TrueVal, B) {
      apply((estimator - TrueVal)^2, 1, mean, na.rm = TRUE)
    }
    mse_bench <- Quality_MSE(est_value_boot_bench, true_value_boot_bench, B)
    
    mse_data <- data.frame(Domain = framework$combined_data[[framework$domains]])
    mse_data$Direct <- NA
    mse_data$Direct[in_sample] <- framework$vardir
    
    # Small area MSE
    mse_data$FH <- model$MSE$FH
    mse_data$FH_Bench <- mse_bench
    mse_data$Out[in_sample] <- 0
    mse_data$Out[out_sample] <- 1
    
    return(mse_data)
  }
  MSE_bench <- boot_bench(model, sigmau2 = model$model$variance, 
                          vardir = model$framework$vardir, 
                          combined_data = model$framework$combined_data, 
                          framework = model$framework,
                          B = B, method = model$method$method,
                          transformation = model$transformation$transformation,
                          backtransformation = model$transformation$backtransformation,
                          eff_smpsize = model$call$eff_smpsize)
  
  if (overwrite == TRUE) {
    point_bench$MSE <- MSE_bench
  }else if (overwrite == FALSE) {
    point_bench$MSE <- MSE_bench$FH
    point_bench$MSE_Bench <- MSE_bench$FH_Bench
  }
  
  return(point_bench)
}

# Argument checking
check_benchmark_arguments <- function(object, benchmark, share, type,
                                      overwrite) {
  if (!inherits(object, "fh")) {
    stop("Object needs to be fh object.")
  }
  
  if ((any(is.na(object$ind$FH)))) {
    stop(strwrap(prefix = " ", initial = "",
                 "If no predictions for out-of-sample domains are available,
                the benchmarking algorithm does not work."))
  }
  if (is.null(type) || !(is.character(type)) || !(type == "raking" ||
                                                  type == "ratio" )) {
    stop(strwrap(prefix = " ", initial = "",
                 "Type must be a character. The two options for types are
                 ''raking'' and ''ratio''."))
  }
  if (is.null(benchmark) || !(is.numeric(benchmark) &&
                              length(benchmark) == 1)) {
    stop(strwrap(prefix = " ", initial = "",
                 "benchmark needs to be a single numeric value. See also
                 help(benchmark)."))
  }
  if (!is.vector(share) || length(share) != length(object$ind$Domain)) {
    stop(strwrap(prefix = " ", initial = "",
                 "share must be a vector with length equal to the number of
                 domains.."))
  }
  if (any(is.na(share))) {
    stop("share must not contain NAs.")
  }
  if (!is.logical(overwrite) || length(overwrite) != 1) {
    stop(strwrap(prefix = " ", initial = "",
                 "overwrite must be a logical value. Set overwrite to TRUE or
                 FALSE. The default is set to FALSE."))
  }
}

point_emdi <- function(object, indicator = "all") {
  if (is.null(object$ind)) {
    stop(strwrap(prefix = " ", initial = "",
                 "No estimates in object: method point not applicable"))
  }
  if ((ncol(object$ind) == 11) && any(indicator == "Custom" |
                                      indicator == "custom")) {
    stop(strwrap(prefix = " ", initial = "",
                 "No individual indicators are defined. Either select other
                 indicators or define custom indicators and generate a new emdi
                 object. See also help(direct) or help(ebp)."))
  }
  
  if (any(indicator == "Quantiles") || any(indicator == "quantiles")) {
    indicator <- c(
      indicator[!(indicator == "Quantiles" | indicator == "quantiles")],
      "Quantile_10", "Quantile_25", "Median", "Quantile_75", "Quantile_90"
    )
  }
  if (any(indicator == "poverty") || any(indicator == "Poverty")) {
    indicator <- c(
      indicator[!(indicator == "poverty" |
                    indicator == "Poverty")],
      "Head_Count", "Poverty_Gap"
    )
  }
  
  if (any(indicator == "inequality") || any(indicator == "Inequality")) {
    indicator <- c(
      indicator[!(indicator == "inequality" | indicator == "Inequality")],
      "Gini", "Quintile_Share"
    )
  }
  
  if (any(indicator == "custom") || any(indicator == "Custom")) {
    indicator <- c(
      indicator[!(indicator == "custom" | indicator == "Custom")],
      colnames(object$ind[-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)])
    )
  }
  
  if (inherits(object, "fh")) {
    object$ind["Out"] <- NULL
  }
  
  
  
  if (any(indicator == "all") || any(indicator == "All")) {
    ind <- object$ind
    ind_name <- "All indicators"
  } else if (any(indicator == "fh") || any(indicator == "FH")) {
    ind <- object$ind[, c("Domain", "FH")]
    ind_name <- "Fay-Herriot estimates"
  } else if (any(indicator == "fh_bench") || any(indicator == "FH_Bench")) {
    ind <- object$ind[, c("Domain", "FH_Bench")]
    ind_name <- "Benchmarked Fay-Herriot estimates"
  } else if (any(indicator == "Direct") || any(indicator == "direct")) {
    ind <- object$ind[, c("Domain", "Direct")]
    ind_name <- "Direct estimates used in Fay-Herriot approach"
  } else {
    selection <- colnames(object$ind[-1]) %in% indicator
    ind <- object$ind[, c(TRUE, selection)]
    ind_name <- paste(unique(indicator), collapse = ", ")
  }
  
  point_emdi <- list(ind = ind, ind_name = ind_name)
  class(point_emdi) <- "point.emdi"
  
  return(point_emdi)
}



mse_emdi <- function(object, indicator = "all", CV = FALSE) {
  if (is.null(object$MSE) && CV == TRUE) {
    stop(strwrap(prefix = " ", initial = "",
                 "No MSE estimates in emdi object: arguments MSE and CV have to
                 be FALSE or a new emdi object with variance/MSE needs to be
                 generated."))
  }
  if ((ncol(object$ind) == 11) && any(indicator == "Custom" |
                                      indicator == "custom")) {
    stop(strwrap(prefix = " ", initial = "",
                 "No individual indicators are defined. Either select other
                 indicators or define custom indicators and generate a new emdi
                 object. See also help(ebp)."))
  }
  
  # Calculation of CVs
  if (inherits(object, "fh")) {
    if ("FH_Bench" %in% colnames(object$MSE)) {
      object$MSE <- object$MSE[, c("Domain", "Direct", "FH", "FH_Bench")]
      object$ind <- object$ind[, c("Domain", "Direct", "FH", "FH_Bench")]
    } else {
      object$MSE <- object$MSE[, c("Domain", "Direct", "FH")]
      object$ind <- object$ind[, c("Domain", "Direct", "FH")]
    }
  }
  all_cv <- sqrt(object$MSE[, -1]) / object$ind[, -1]
  
  if (any(indicator == "Quantiles") || any(indicator == "quantiles")) {
    indicator <- c(
      indicator[!(indicator == "Quantiles" ||
                    indicator == "quantiles")],
      "Quantile_10", "Quantile_25", "Median",
      "Quantile_75", "Quantile_90"
    )
  }
  if (any(indicator == "poverty") || any(indicator == "Poverty")) {
    indicator <- c(
      indicator[!(indicator == "poverty" ||
                    indicator == "Poverty")],
      "Head_Count", "Poverty_Gap"
    )
  }
  if (any(indicator == "inequality") || any(indicator == "Inequality")) {
    indicator <- c(
      indicator[!(indicator == "inequality" ||
                    indicator == "Inequality")],
      "Gini", "Quintile_Share"
    )
  }
  if (any(indicator == "custom") || any(indicator == "Custom")) {
    indicator <- c(
      indicator[!(indicator == "custom" | indicator == "Custom")],
      colnames(object$ind[-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)])
    )
  }
  
  if (any(indicator == "all") || any(indicator == "All")) {
    ind <- object$MSE
    ind_cv <- cbind(Domain = object$MSE[, 1], all_cv)
    ind_name <- "All indicators"
  } else if (any(indicator == "fh") || any(indicator == "FH")) {
    ind <- object$MSE[, c("Domain", "FH")]
    ind_cv <- cbind(Domain = object$MSE[, 1], all_cv)
    ind_name <- "Fay-Herriot estimates"
  } else if (any(indicator == "fh_bench") || any(indicator == "FH_Bench")) {
    ind <- object$MSE[, c("Domain", "FH_Bench")]
    ind_cv <- cbind(Domain = object$MSE[, 1], all_cv)
    ind_name <- "Benchmarked Fay-Herriot estimates"
  } else if (any(indicator == "Direct") || any(indicator == "direct")) {
    ind <- object$MSE[, c("Domain", "Direct")]
    ind_cv <- cbind(Domain = object$MSE[, 1], all_cv)
    ind_name <- "Direct estimates used in Fay-Herriot approach"
  } else {
    selection <- colnames(object$MSE[-1]) %in% indicator
    ind <- object$MSE[, c(TRUE, selection)]
    ind_cv <- data.frame(Domain = object$MSE[, 1], all_cv[, selection])
    colnames(ind_cv) <- colnames(ind)
    ind_name <- paste(unique(indicator), collapse = ", ")
  }
  
  if (CV == FALSE) {
    mse_emdi <- list(ind = ind, ind_name = ind_name)
  } else {
    mse_emdi <- list(ind = ind, ind_cv = ind_cv, ind_name = ind_name)
  }
  
  class(mse_emdi) <- "mse.emdi"
  
  return(mse_emdi)
}

#' Shows Plots for the Comparison of Estimates
#'
#' Function \code{compare_plot} is a generic function used to produce plots
#' comparing point and existing MSE/CV estimates of direct and model-based
#' estimation for all indicators or a selection of indicators.
#' @param model a model object of type "emdi", either "ebp" or "fh",
#' representing point and MSE estimates.
#' @param direct an object of type "direct","emdi", representing point
#' and MSE estimates. If the input argument \code{model} is of type "ebp",
#' \code{direct} is required. If the input argument \code{model} is of type
#' "fh", the \code{direct} component is already included in the input
#' argument \code{model}.
#' @param indicator optional character vector that selects which indicators
#' shall be returned. Defaults to "all".
#' @param MSE optional logical. If \code{TRUE}, the MSE estimates of the direct
#' and model-based estimates are compared via suitable plots. Defaults to
#' \code{FALSE}.
#' @param CV optional logical. If \code{TRUE}, the coefficient of variation
#' estimates of the direct and model-based estimates are compared via suitable
#' plots. Defaults to \code{FALSE}.
#' @param label argument that enables to customize title and axis labels. There
#' are three options to label the evaluation plots: (i) original labels
#' ("orig"), (ii) axis labels but no title ("no_title"), (iii) neither axis
#' labels nor title ("blank").
#' @param color a vector with two elements determining color schemes in returned
#' plots.
#' @param shape a numeric vector with two elements determining the shape of
#' points in returned plots.
#' @param line_type a character vector with two elements determining the line
#' types in returned plots.
#' @param gg_theme \code{\link[ggplot2]{theme}} list from package \pkg{ggplot2}.
#' For using this argument, package \pkg{ggplot2} must be loaded via
#' \code{library(ggplot2)}.
#' @param ... further arguments passed to or from other methods.
#' @return Plots comparing direct and model-based estimators for each selected
#' indicator obtained by \code{\link[ggplot2]{ggplot}}.
#' @export
#' @name compare_plot

compare_plot <- function(model, direct, indicator = "all", MSE = FALSE,
                         CV = FALSE, label = "orig",
                         color = c("blue", "lightblue3"),
                         shape = c(16, 16), line_type = c("solid", "solid"),
                         gg_theme = NULL, ...) {
  UseMethod("compare_plot")
}



#' Shows Plots for the Comparison of Estimates
#'
#' Methods \code{compare_plot.direct}, \code{compare_plot.ebp} and
#' \code{compare_plot.fh} produce plots comparing point and existing
#' MSE/CV estimates of direct and model-based estimation for all indicators or a
#' selection of indicators for objects of type "emdi". The direct and
#' model-based point estimates are compared by a scatter plot and a line plot
#' for each selected indicator. If the input arguments MSE and CV are set to
#' TRUE, two extra plots are created, respectively: the MSE/CV estimates of the
#' direct and model-based estimates are compared by boxplots and scatter plots.
#' @param model a model object of type "emdi", either "ebp" or "fh",
#' representing point and MSE estimates.
#' @param direct an object of type "direct","emdi", representing point
#' and MSE estimates. If the input argument \code{model} is of type "ebp",
#' \code{direct} is required. If the input argument \code{model} is of type
#' "fh", the \code{direct} component is already included in the input
#' argument \code{model}.
#' @param indicator optional character vector that selects which indicators
#' shall be returned: (i) all calculated indicators ("all");
#' (ii) each indicator name: "Mean", "Quantile_10", "Quantile_25", "Median",
#' "Quantile_75", "Quantile_90", "Head_Count",
#' "Poverty_Gap", "Gini", "Quintile_Share" or the function name/s of
#' "custom_indicator/s"; (iii) groups of indicators: "Quantiles", "Poverty",
#' "Inequality" or "Custom". If two of these groups are selected, only the first
#' one is returned. Note, additional custom indicators can be defined as
#' argument for the EBP approaches (see also \code{\link{ebp}}) and do not
#' appear in groups of indicators even though these might belong to one of the
#' groups. If the \code{model} argument is of type "fh", indicator can be set
#' to "all", "Direct", FH", or "FH_Bench" (if emdi object is overwritten by
#' function \code{\link{benchmark}}). Defaults to "all".
#' @param MSE optional logical. If \code{TRUE}, the MSE estimates of the direct
#' and model-based estimates are compared via boxplots and scatter plots.
#' @param CV optional logical. If \code{TRUE}, the coefficient of variation
#' estimates of the direct and model-based estimates are compared via boxplots
#' and scatter plots.
#' @param label argument that enables to customize title and axis labels. There
#' are three options to label the evaluation plots: (i) original labels
#' ("orig"), (ii) axis labels but no title ("no_title"), (iii) neither axis
#' labels nor title ("blank").
#' @param color a vector with two elements. The first color determines
#' the color for the regression line in the scatter plot and the color for
#' the direct estimates in the remaining plots. The second color specifies the
#' color of the intersection line in the scatter plot and the color for the
#' model-based estimates in the remaining plots. Defaults to
#' c("blue", "lightblue3").
#' @param shape a numeric vector with two elements. The first shape determines
#' the shape of the points in the scatterplot and the shape of the points for
#' the direct estimates in the remaining plots. The second shape determines
#' the shape for the points for the model-based estimates. The options
#' are numbered from 0 to 25. Defaults to c(16, 16).
#' @param line_type a character vector with two elements. The first line type
#' determines the line type for the regression line in the scatter plot and the
#' line type for the direct estimates in the remaining plots. The second line
#' type specifies the line type of the intersection line in the scatter plot and
#' the line type for the model-based estimates in the remaining plots. The
#' options are: "twodash", "solid", "longdash", "dotted", "dotdash", "dashed"
#' and "blank". Defaults to  c("solid", "solid").
#' @param gg_theme \code{\link[ggplot2]{theme}} list from package \pkg{ggplot2}.
#' For using this argument, package \pkg{ggplot2} must be loaded via
#' \code{library(ggplot2)}. See also Example 2.
#' @param ... further arguments passed to or from other methods.
#' @return A scatter plot and a line plot comparing direct and model-based
#' estimators for each selected indicator obtained by
#' \code{\link[ggplot2]{ggplot}}. If the input arguments MSE and CV are set to
#' TRUE two extra plots are created, respectively: the MSE/CV estimates of the
#' direct and model-based estimates are compared by boxplots and scatter plots.
#' @details Since all of the comparisons need a direct estimator, the plots are
#' only created for in-sample domains.
#' @seealso \code{\link{emdiObject}}, \code{\link{direct}}, \code{\link{ebp}},
#' \code{\link{fh}}
#' @examples
#' \donttest{
#' # Examples for comparisons of direct estimates and models of type ebp
#'
#' # Loading data - population and sample data
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'
#' # Generation of two emdi objects
#' emdi_model <- ebp(
#'   fixed = eqIncome ~ gender + eqsize + cash +
#'     self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent +
#'     fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#'   pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#'   threshold = function(y) {
#'     0.6 * median(y)
#'   }, L = 50, MSE = TRUE,
#'   na.rm = TRUE, cpus = 1
#' )
#'
#' emdi_direct <- direct(
#'   y = "eqIncome", smp_data = eusilcA_smp,
#'   smp_domains = "district", weights = "weight", threshold = 11161.44,
#'   var = TRUE, boot_type = "naive", B = 50, seed = 123, na.rm = TRUE
#' )
#'
#' # Example 1: Receive first overview
#' compare_plot(model = emdi_model, direct = emdi_direct)
#'
#' # Example 2: Change plot theme
#' library(ggplot2)
#' compare_plot(emdi_model, emdi_direct,
#'   indicator = "Median",
#'   gg_theme = theme(
#'     axis.line = element_line(size = 3, colour = "grey80"),
#'     plot.background = element_rect(fill = "lightblue3"),
#'     legend.position = "none"
#'   )
#' )
#'
#' # Example for comparison of direct estimates and models of type fh
#'
#' # Loading data - population and sample data
#' data("eusilcA_popAgg")
#' data("eusilcA_smpAgg")
#'
#' # Combine sample and population data
#' combined_data <- combine_data(
#'   pop_data = eusilcA_popAgg,
#'   pop_domains = "Domain",
#'   smp_data = eusilcA_smpAgg,
#'   smp_domains = "Domain"
#' )
#'
#' # Generation of the emdi object
#' fh_std <- fh(
#'   fixed = Mean ~ cash + self_empl, vardir = "Var_Mean",
#'   combined_data = combined_data, domains = "Domain",
#'   method = "ml", MSE = TRUE
#' )
#' # Example 3: Receive first overview
#' compare_plot(fh_std)
#'
#' # Example 4: Compare also MSE and CV estimates
#' compare_plot(fh_std, MSE = TRUE, CV = TRUE)
#' }
#' @importFrom reshape2 melt
#' @importFrom ggplot2 geom_point geom_smooth geom_line geom_boxplot
#' @importFrom ggplot2 aes xlim ylim scale_shape_manual scale_linetype_manual
#' @importFrom ggplot2 scale_color_manual scale_fill_manual coord_flip
#' @name compare_plots_emdi
#' @rdname compare_plot
NULL



compare_plots <- function(object, type, selected_indicators, MSE, CV, label,
                          color, shape, line_type, gg_theme, ...) {
  Model_based <- NULL
  Direct <- NULL
  ID <- NULL
  value <- NULL
  Method <- NULL
  slope <- NULL
  intercept <- NULL
  area <- NULL
  
  
  if (MSE == FALSE && CV == FALSE) {
    plotList <- vector(mode = "list", length = length(selected_indicators) * 2)
    names(plotList) <- paste(rep(
      c("scatter", "line"),
      length(selected_indicators)
    ),
    rep(selected_indicators, each = 2),
    sep = "_"
    )
  } else if ((MSE == TRUE || CV == TRUE) && !(MSE == TRUE && CV == TRUE)) {
    plotList <- vector(mode = "list", length = length(selected_indicators) * 4)
    names(plotList) <- paste(rep(
      c("scatter", "line"),
      length(selected_indicators)
    ),
    rep(selected_indicators, each = 4),
    sep = "_"
    )
  } else if (MSE == TRUE && CV == TRUE) {
    plotList <- vector(mode = "list", length = length(selected_indicators) * 6)
    names(plotList) <- paste(rep(
      c("scatter", "line"),
      length(selected_indicators)
    ),
    rep(selected_indicators, each = 6),
    sep = "_"
    )
  }
  
  # scatter line
  for (ind in selected_indicators) {
    label_ind <- define_evallabel(type = type, label = label, indi = ind)
    
    if (is.null(object$smp_size)) {
      data_tmp <- data.frame(
        Direct = object[, paste0(ind, "_Direct")],
        Model_based = object[, paste0(ind, "_Model")]
      )
      label_ind$line["x_lab"] <- "Domains (unordered)"
    } else {
      data_tmp <- data.frame(
        Direct = object[, paste0(ind, "_Direct")],
        Model_based = object[, paste0(ind, "_Model")],
        smp_size = object$smp_size
      )
      data_tmp <- data_tmp[order(data_tmp$smp_size), ]
      data_tmp$smp_size <- NULL
    }
    
    data_tmp$ID <- seq_along(object$Domain)
    data_shaped <- reshape2::melt(data_tmp, id.vars = "ID")
    names(data_shaped) <- c("ID", "Method", "value")
    
    
    print((plotList[[paste("scatter", ind, sep = "_")]] <-
             ggplot(data_tmp, aes(x = Direct, y = Model_based)) +
             geom_point(shape = shape[1]) +
             geom_smooth(
               method = lm, formula = y ~ x, se = FALSE,
               inherit.aes = FALSE,
               lty = line_type[1],
               aes(colour = "Reg. line", x = Direct, y = Model_based)
             ) +
             geom_abline(
               mapping = aes(
                 colour = "Intersection",
                 slope = slope, intercept = intercept
               ),
               data.frame(slope = 1, intercept = 0),
               lty = line_type[2]
             ) +
             xlim(
               min(min(data_tmp$Direct), min(data_tmp$Model_based)),
               max(max(data_tmp$Direct), max(data_tmp$Model_based))
             ) +
             ylim(
               min(min(data_tmp$Direct), min(data_tmp$Model_based)),
               max(max(data_tmp$Direct), max(data_tmp$Model_based))
             ) +
             ggtitle(label_ind$scatter["title"]) +
             ylab(label_ind$scatter["y_lab"]) +
             xlab(label_ind$scatter["x_lab"]) +
             scale_color_manual(name = "", values = c(
               "Intersection" = color[2],
               "Reg. line" = color[1]
             )) +
             gg_theme))
    
    if (interactive() && dev.cur() == 1L) { cat("Press [enter] to continue"); readline() }
    
    
    print((plotList[[paste("line", ind, sep = "_")]] <-
             ggplot(data = data_shaped, aes(
               x = ID,
               y = value, group = Method,
               colour = Method
             )) +
             geom_line(aes(linetype = Method), linewidth = 0.7) +
             geom_point(aes(color = Method, shape = Method), size = 2) +
             scale_shape_manual(
               values = c(shape[1], shape[2]),
               breaks = c("Direct", "Model_based"),
               labels = c("Direct", "Model-based")
             ) +
             scale_linetype_manual(
               values = c(line_type[1], line_type[2]),
               breaks = c("Direct", "Model_based"),
               labels = c("Direct", "Model-based")
             ) +
             scale_color_manual(
               values = c(color[1], color[2]),
               breaks = c("Direct", "Model_based"),
               labels = c("Direct", "Model-based")
             ) +
             scale_fill_manual(
               name = "Method",
               breaks = c("Direct", "Model_based"),
               labels = c("Direct", "Model-based")
             ) +
             xlab(label_ind$line["x_lab"]) +
             ylab(label_ind$line["y_lab"]) +
             ggtitle(label_ind$line["title"]) +
             gg_theme))
    
    if (MSE == TRUE) {
      data_tmp2 <- data.frame(
        Direct = object[, paste0(ind, "_Direct_MSE")],
        Model_based = object[, paste0(ind, "_Model_MSE")],
        smp_size = object$smp_size
      )
      
      data_tmp2 <- data_tmp2[order(data_tmp2$smp_size, decreasing = TRUE), ]
      data_tmp2$smp_size <- NULL
      data_tmp2$ID <- seq_along(object$Domain)
      data_shaped <- reshape2::melt(data_tmp2, id.vars = "ID")
      names(data_shaped) <- c("ID", "Method", "value")
      data_shaped$area <- rep(seq_len(NROW(data_tmp2$Direct)), 2)
      
      if (interactive() && dev.cur() == 1L) { cat("Press [enter] to continue"); readline() }
      
      print((plotList[[paste("boxplot", "MSE", ind, sep = "_")]] <-
               ggplot(data_shaped, aes(x = Method, y = value, fill = Method)) +
               geom_boxplot() +
               coord_flip() +
               labs(
                 title = label_ind$boxplot_MSE["title"],
                 x = label_ind$boxplot_MSE["x_lab"],
                 y = label_ind$boxplot_MSE["y_lab"]
               ) +
               scale_fill_manual(
                 name = "Method",
                 values = color
               ) +
               gg_theme))
      
      if (interactive() && dev.cur() == 1L) { cat("Press [enter] to continue"); readline() }
      
      print((plotList[[paste("ordered", "MSE", ind, sep = "_")]] <-
               ggplot(data_shaped, aes(x = area, y = value, colour = Method)) +
               geom_point(aes(color = Method, shape = Method)) +
               labs(
                 title = label_ind$ordered_MSE["title"],
                 x = label_ind$ordered_MSE["x_lab"],
                 y = label_ind$ordered_MSE["y_lab"]
               ) +
               scale_color_manual(values = color)) +
              scale_shape_manual(values = c(shape[1], shape[2])) + gg_theme)
    }
    
    if (CV == TRUE) {
      data_tmp3 <- data.frame(
        Direct = object[, paste0(ind, "_Direct_CV")],
        Model_based = object[, paste0(ind, "_Model_CV")],
        smp_size = object$smp_size2
      )
      
      data_tmp3 <- data_tmp3[order(data_tmp3$smp_size, decreasing = TRUE), ]
      data_tmp3$smp_size <- NULL
      data_tmp3$ID <- seq_along(object$Domain)
      data_shaped <- reshape2::melt(data_tmp3, id.vars = "ID")
      names(data_shaped) <- c("ID", "Method", "value")
      data_shaped$area <- rep(seq_len(NROW(data_tmp3$Direct)), 2)
      
      if (interactive() && dev.cur() == 1L) { cat("Press [enter] to continue"); readline() }
      
      print((plotList[[paste("boxplot", "CV", ind, sep = "_")]] <-
               ggplot(data_shaped, aes(x = Method, y = value, fill = Method)) +
               geom_boxplot() +
               coord_flip() +
               labs(
                 title = label_ind$boxplot_CV["title"],
                 x = label_ind$boxplot_CV["x_lab"],
                 y = label_ind$boxplot_CV["y_lab"]
               ) +
               scale_fill_manual(
                 name = "Method",
                 values = color
               )) + gg_theme)
      
      if (interactive() && dev.cur() == 1L) { cat("Press [enter] to continue"); readline() }
      
      data_shaped
      
      print((plotList[[paste("ordered", "CV", ind, sep = "_")]] <-
               ggplot(data_shaped, aes(x = area, y = value, colour = Method)) +
               geom_point(aes(color = Method, shape = Method)) +
               labs(
                 title = label_ind$ordered_CV["title"],
                 x = label_ind$ordered_CV["x_lab"],
                 y = label_ind$ordered_CV["y_lab"]
               ) +
               scale_color_manual(values = color)) +
              scale_shape_manual(values = c(shape[1], shape[2])) + gg_theme)
    }
    
    
    if (!ind == tail(selected_indicators, 1)) {
      if (interactive() && dev.cur() == 1L) { cat("Press [enter] to continue"); readline() }
    }
  }
  invisible(plotList)
}

define_evallabel <- function(type, label, indi) {
  if (!inherits(label, "list")) {
    if (label == "orig") {
      if (type == "unit") {
        label <- list(
          scatter = c(
            title = indi,
            y_lab = "Model-based",
            x_lab = "Direct"
          ),
          line = c(
            title = indi,
            y_lab = "Value",
            x_lab = "Domain (ordered by sample size)"
          ),
          boxplot_MSE = c(
            title = indi,
            y_lab = "MSE",
            x_lab = ""
          ),
          ordered_MSE = c(
            title = indi,
            y_lab = "MSE",
            x_lab = "Domain (ordered by
                                      sample size)"
          ),
          boxplot_CV = c(
            title = indi,
            y_lab = "CV",
            x_lab = ""
          ),
          ordered_CV = c(
            title = indi,
            y_lab = "CV",
            x_lab = "Domain (ordered by sample size)"
          )
        )
      } else if (type == "area") {
        label <- list(
          scatter = c(
            title = indi,
            y_lab = "Model-based",
            x_lab = "Direct"
          ),
          line = c(
            title = indi,
            y_lab = "Value",
            x_lab = "Domain (ordered by decreasing MSE
                               of Direct)"
          ),
          boxplot_MSE = c(
            title = indi,
            y_lab = "",
            x_lab = "MSE"
          ),
          ordered_MSE = c(
            title = indi,
            y_lab = "MSE",
            x_lab = "Domain (ordered by increasing
                                      MSE of Direct)"
          ),
          boxplot_CV = c(
            title = indi,
            y_lab = "CV",
            x_lab = ""
          ),
          ordered_CV = c(
            title = indi,
            y_lab = "CV",
            x_lab = "Domain (ordered by increasing CV
                                     of Direct)"
          )
        )
      }
    } else if (label == "blank") {
      label <- list(
        scatter = c(
          title = "",
          y_lab = "",
          x_lab = ""
        ),
        line = c(
          title = "",
          y_lab = "",
          x_lab = ""
        ),
        boxplot_MSE = c(
          title = "",
          y_lab = "",
          x_lab = ""
        ),
        ordered_MSE = c(
          title = "",
          y_lab = "",
          x_lab = ""
        ),
        boxplot_CV = c(
          title = "",
          y_lab = "",
          x_lab = ""
        ),
        ordered_CV = c(
          title = "",
          y_lab = "",
          x_lab = ""
        )
      )
    } else if (label == "no_title") {
      if (type == "unit") {
        label <- list(
          scatter = c(
            title = "",
            y_lab = "Model-based",
            x_lab = "Direct"
          ),
          line = c(
            title = "",
            y_lab = "Value",
            x_lab = "Domain (ordered by sample size)"
          ),
          boxplot_MSE = c(
            title = "",
            y_lab = "MSE",
            x_lab = ""
          ),
          ordered_MSE = c(
            title = "",
            y_lab = "MSE",
            x_lab = "Domain (ordered by sample
                                      size)"
          ),
          boxplot_CV = c(
            title = "",
            y_lab = "CV",
            x_lab = ""
          ),
          ordered_CV = c(
            title = "",
            y_lab = "CV",
            x_lab = "Domain (ordered by sample size)"
          )
        )
      } else if (type == "area") {
        label <- list(
          scatter = c(
            title = "",
            y_lab = "Model-based",
            x_lab = "Direct"
          ),
          line = c(
            title = "",
            y_lab = "Value",
            x_lab = "Domain (ordered by decreasing MSE of
                               Direct)"
          ),
          boxplot_MSE = c(
            title = "",
            y_lab = "",
            x_lab = "MSE"
          ),
          ordered_MSE = c(
            title = "",
            y_lab = "MSE",
            x_lab = "Domain (ordered by increasing MSE
                                      of Direct)"
          ),
          boxplot_CV = c(
            title = "",
            y_lab = "CV",
            x_lab = ""
          ),
          ordered_CV = c(
            title = "",
            y_lab = "CV",
            x_lab = "Domain (ordered by increasing CV
                                     of Direct)"
          )
        )
      }
    }
  }
  return(label)
}


#' @rdname compare_plot
#' @export
compare_plot.fh <- function(model = NULL, direct = NULL, indicator = "all",
                            MSE = FALSE, CV = FALSE, label = "orig",
                            color = c("blue", "lightblue3"),
                            shape = c(16, 16), line_type = c(
                              "solid",
                              "solid"
                            ),
                            gg_theme = NULL, ...) {
  compare_plot_check(
    model = model, indicator = indicator,
    label = label, color = color, shape = shape,
    line_type = line_type, gg_theme = gg_theme
  )
  
  if (inherits(direct, "ebp")) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0("It is not possible to compare the point and MSE
                        estimates of a model of type 'fh', to the point and MSE
                        estimates of an 'ebp' object.")))
  }
  
  if (inherits(model, "fh") && inherits(direct, "direct")) {
    warning(strwrap(prefix = " ", initial = "",
                    paste0("fh models are only compared to their own inherrent
                           direct estimates. Hence, the argument direct is
                           ignored."
                    )))
  }
  
  compare_plot_fh(
    model = model, direct = model, indicator = indicator,
    MSE = MSE, CV = CV,
    label = label, color = color, shape = shape,
    line_type = line_type, gg_theme = gg_theme
  )
}


#' Shows plots for the comparison of estimates
#'
#' For all indicators or a selection of indicators two plots are returned. The
#' first plot is a scatter plot of estimates to compare and the second is a line
#' plot with these estimates.
#' @param model an object of type "emdi", either "ebp" or "fh", representing
#' point and MSE estimates.
#' @param direct an object of type "direct","emdi", representing point
#' and MSE estimates.
#' @param indicator optional character vector that selects which indicators
#' shall be returned: (i) all calculated indicators ("all");
#' (ii) each indicator name: "Mean", "Quantile_10", "Quantile_25", "Median",
#' "Quantile_75", "Quantile_90", "Head_Count",
#' "Poverty_Gap", "Gini", "Quintile_Share" or the function name/s of
#' "custom_indicator/s"; (iii) groups of indicators: "Quantiles", "Poverty",
#' "Inequality" or "Custom".If two of these groups are selected, only the first
#' one is returned. Defaults to "all". Note, additional custom indicators can be
#' defined as argument for model-based approaches (see also \code{\link{ebp}})
#' and do not appear in groups of indicators even though these might belong to
#' one of the groups.
#' @param MSE optional logical. If \code{TRUE}, the MSE estimates of the direct
#' and model-based estimates are compared via suitable plots. Defaults to
#' \code{FALSE}.
#' @param CV optional logical. If \code{TRUE}, the coefficient of variation
#' estimates of the direct and model-based estimates are compared via suitable
#' plots. Defaults to \code{FALSE}.
#' @param label argument that enables to customize title and axis labels. There
#' are three options to label the evaluation plots: (i) original labels
#' ("orig"), (ii) axis labels but no title ("no_title"), (iii) neither axis
#' labels nor title ("blank").
#' @param color a vector with two elements determining color schemes in returned
#' plots.
#' @param shape a numeric vector with two elements determining the shape of
#' points in returned plots.
#' @param line_type a character vector with two elements determining the line
#' types in returned plots.
#' @param gg_theme \code{\link[ggplot2]{theme}} list from package \pkg{ggplot2}.
#' For using this argument, package \pkg{ggplot2} must be loaded via
#' \code{library(ggplot2)}.
#' @param ... further arguments passed to or from other methods.
#' @return A scatter plot and a line plot comparing direct and model-based
#' estimators for each selected indicator obtained by
#' \code{\link[ggplot2]{ggplot}}. If the input arguments MSE and CV are set to
#' TRUE two extra plots are created, respectively: the MSE/CV estimates of the
#' direct and model-based estimates are compared by boxplots and scatter plots.
#' @noRd

compare_plot_fh <- function(model, direct, indicator = "all", MSE = FALSE,
                            CV = FALSE, label = "orig",
                            color = c("blue", "lightblue3"),
                            shape = c(16, 16), line_type = c("solid", "solid"),
                            gg_theme = NULL) {
  Model_based <- NULL
  Direct <- NULL
  ID <- NULL
  value <- NULL
  Method <- NULL
  
  Data <- point_emdi(object = model, indicator = "all")$ind
  Data <- Data[!is.na(Data$Direct), ]
  selected_indicators <- colnames(Data)[!(colnames(Data) %in% c(
    "Domain",
    "Direct"
  ))]
  colnames(Data) <- c(
    "Domain", "FH_Direct",
    paste0(
      colnames(Data)[!(colnames(Data) %in%
                         c("Domain", "Direct"))],
      "_Model"
    )
  )
  if ("FH_Bench" %in% selected_indicators) {
    Data$FH_Bench_Direct <- Data$FH_Direct
  }
  if ("FH_Bench" %in% indicator && !("FH_Bench" %in% selected_indicators)) {
    message(strwrap(prefix = " ", initial = "",
                    "emdi object does not contain benchmarked fh estimates.
                   Only FH estimates are compared with direct. See also
                   help(benchmark)."))
  }
  
  if (!(any(indicator == "all") || any(indicator == "direct") ||
        any(indicator == "Direct"))) {
    selected_indicators <- selected_indicators[selected_indicators %in%
                                                 indicator]
  }
  
  if (is.null(model$MSE)) {
    Data$smp_size <- NULL
  }
  
  if (MSE == TRUE || CV == TRUE) {
    all_precisions <- mse_emdi(object = model, indicator = "all", CV = TRUE)
    if ("FH_Bench" %in% colnames(all_precisions$ind)) {
      colnames(all_precisions$ind) <- c("Domain", paste0(c(
        "FH_Direct",
        "FH_Model",
        "FH_Bench_Model"
      ), "_MSE"))
      all_precisions$ind$FH_Bench_Direct_MSE <- all_precisions$ind$FH_Direct_MSE
      colnames(all_precisions$ind_cv) <- c("Domain", paste0(c(
        "FH_Direct",
        "FH_Model",
        "FH_Bench_Model"
      ), "_CV"))
      all_precisions$ind_cv$FH_Bench_Direct_CV <- all_precisions$ind_cv$FH_Direct_CV
    } else {
      colnames(all_precisions$ind) <- c("Domain", paste0(c(
        "FH_Direct",
        "FH_Model"
      ), "_MSE"))
      colnames(all_precisions$ind_cv) <- c("Domain", paste0(c(
        "FH_Direct",
        "FH_Model"
      ), "_CV")) 
    }
    combined <- merge(all_precisions$ind, all_precisions$ind_cv, id = "Domain")
    combined <- combined[!is.na(combined$FH_Direct_MSE), ]
    
    Data <- merge(Data, combined, id = "Domain")
    Data$smp_size <- -Data$FH_Direct_MSE
    Data$smp_size2 <- -Data$FH_Direct_CV
  }
  
  if (model$framework$N_dom_unobs > 0) {
    message(strwrap(prefix = " ", initial = "",
                    "Please note that since all of the comparisons need a direct
                   estimator, the plots are only created for in-sample
                   domains."))
  }
  
  compare_plots(
    object = Data, type = "area",
    selected_indicators = selected_indicators,
    MSE = MSE, CV = CV, label = label, color = color,
    shape = shape, line_type = line_type, gg_theme = gg_theme
  )
}

compare_plot_check <- function(model, indicator, label, color, shape,
                               line_type, gg_theme) {
  if (is.null(indicator) || !all(indicator == "all" | indicator == "Quantiles" |
                                 indicator == "quantiles" |
                                 indicator == "Poverty" |
                                 indicator == "poverty" |
                                 indicator == "Inequality" |
                                 indicator == "inequality" |
                                 indicator == "Custom" | indicator == "custom" |
                                 indicator %in% names(model$ind[-1]))) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0("The argument indicator is set to ", indicator, ". The
                        argument only allows to be set to all, a name of
                        estimated indicators or indicator groups as described
                        in help(estimators.emdi).")))
  }
  
  if (is.null(label) || (!(label == "orig" || label == "no_title" ||
                           label == "blank"))) {
    stop(strwrap(prefix = " ", initial = "",
                 "label can be one of the following characters 'orig',
                 'no_title' or 'blank'."))
  }
  if (length(color) != 2 || !is.vector(color)) {
    stop(strwrap(prefix = " ", initial = "",
                 "color needs to be a vector of length 2 defining the two
                 colors for the scatter and line plot. See also
                 help(compare_plot)."))
  }
  if (length(shape) != 2 || !is.vector(shape)) {
    stop(strwrap(prefix = " ", initial = "",
                 "shape needs to be a vector of length 2 defining the two
                 shapes for the estimates in the line plots. See also
                 help(compare_plot)."))
  }
  if (length(line_type) != 2 || !is.vector(shape)) {
    stop(strwrap(prefix = " ", initial = "",
                 "line_type needs to be a vector of length 2 defining the types
                 for the lines in the line plots. See also
                 help(compare_plot)."))
  }
  if (!all(line_type %in% c(
    "twodash", "solid", "longdash", "dotted", "dotdash",
    "dashed", "blank"
  ))) {
    stop(strwrap(prefix = " ", initial = "",
                 "An element in argument line_type is not a valid option.
                 See help(compare_plot) for valid options."))
  }
}

#' Presents Point, MSE and CV Estimates
#'
#' Function \code{estimators} is a generic function used to present point and
#' mean squared error (MSE) estimates and calculated coefficients of variation
#' (CV).
#' @param object an object for which point and/or MSE estimates and/or
#' calculated CV's are desired.
#' @param indicator optional character vector that selects which indicators
#' shall be returned.
#' @param MSE optional logical. If \code{TRUE}, MSE estimates for selected
#' indicators per domain are added to the data frame of point estimates.
#' Defaults to \code{FALSE}.
#' @param CV optional logical. If \code{TRUE}, coefficients of variation for
#' selected indicators per domain are added to the data frame of point
#' estimates. Defaults to \code{FALSE}.
#' @param ... arguments to be passed to or from other methods.
#' @return
#' The return of \code{estimators} depends on the class of its argument. The
#' documentation of particular methods gives detailed information about the
#' return of that method.
#' @export

estimators <- function(object, indicator, MSE, CV, ...) UseMethod("estimators")


#' Presents Point, MSE and/or CV Estimates of an emdiObject
#'
#' Method \code{estimators.emdi} presents point and MSE estimates for regional
#' disaggregated indicators. Coefficients of variation are calculated
#' using these estimators. This method enables to select for which indicators
#' the estimates shall be returned. The returned object is suitable for
#' printing with the \code{print.estimators.emdi} method.
#' @param object an object of type "emdi", representing point and,
#' if chosen, MSE estimates.
#' @param indicator optional character vector that selects which indicators
#' shall be returned: (i) all calculated indicators ("all");
#' (ii) each indicator name: "Mean", "Quantile_10", "Quantile_25", "Median",
#' "Quantile_75", "Quantile_90", "Head_Count",
#' "Poverty_Gap", "Gini", "Quintile_Share" or the function name/s of
#' "custom_indicator/s"; (iii) groups of indicators: "Quantiles", "Poverty",
#' "Inequality" or "Custom". If two of these groups are selected, only the first
#' one is returned. Note, additional custom indicators can be
#' defined as argument for model-based approaches (see also \code{\link{ebp}})
#' and do not appear in groups of indicators even though these might belong to
#' one of the groups. If the \code{model} argument is of type "fh",
#' indicator can be set to "all", "Direct", FH", or "FH_Bench" (if emdi
#' object is overwritten by function benchmark). Defaults to "all".
#' @param MSE optional logical. If \code{TRUE}, MSE estimates for selected
#' indicators per domain are added to the data frame of point estimates.
#' Defaults to \code{FALSE}.
#' @param CV optional logical. If \code{TRUE}, coefficients of variation for
#' selected indicators per domain are added to the data frame of point
#' estimates. Defaults to \code{FALSE}.
#' @param ... other parameters that can be passed to function \code{estimators}.
#' @return
#' The return of \code{estimators.emdi} is an object of type "estimators.emdi"
#' with point and/or MSE estimates and/or calculated CV's per domain obtained
#' from \code{emdiObject$ind} and, if chosen, \code{emdiObject$MSE}. These
#' objects contain two elements, one data frame \code{ind} and a character
#' naming the indicator or indicator group \code{ind_name}.
#' @details Objects of class "estimators.emdi" have methods for following
#' generic functions: \code{head} and \code{tail} (for default documentation,
#' see \code{\link[utils]{head}}),  \code{as.matrix} (for default documentation,
#' see \code{\link[base]{matrix}}), \code{as.data.frame} (for default
#' documentation, see \code{\link[base]{as.data.frame}}), \code{subset} (for
#' default documentation, see \code{\link[base]{subset}}).
#' @seealso \code{\link{emdiObject}}, \code{\link{direct}}, \code{\link{ebp}},
#' \code{\link{fh}}
#' @examples
#' \donttest{
#' # Loading data - population and sample data
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'
#' # Generate emdi object with additional indicators; here via function ebp()
#' emdi_model <- ebp(
#'   fixed = eqIncome ~ gender + eqsize + cash +
#'     self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent +
#'     fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#'   pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#'   threshold = 11064.82, transformation = "box.cox",
#'   L = 50, MSE = TRUE, B = 50, custom_indicator =
#'     list(
#'       my_max = function(y) {
#'         max(y)
#'       },
#'       my_min = function(y) {
#'         min(y)
#'       }
#'     ), na.rm = TRUE, cpus = 1
#' )
#'
#' # Example 1: Choose Gini coefficient, MSE and CV
#' gini <- estimators(emdi_model, indicator = "Gini", MSE = TRUE, CV = TRUE)
#' head(gini)
#' tail(gini)
#' as.data.frame(gini)
#' as.matrix(gini)
#' subset(gini, Domain = "Wien")
#'
#' # Example 2: Choose custom indicators without MSE and CV
#' estimators(emdi_model, indicator = "Custom")
#' }
#' @rdname estimators
#' @export

estimators.emdi <- function(object, indicator = "all", MSE = FALSE,
                            CV = FALSE, ...) {
  estimators_check(
    object = object, indicator = indicator,
    MSE = MSE, CV = CV
  )
  
  # Only point estimates
  all_ind <- point_emdi(object = object, indicator = indicator)
  selected <- colnames(all_ind$ind)[-1]
  
  if (MSE == TRUE || CV == TRUE) {
    all_precisions <- mse_emdi(
      object = object, indicator = indicator,
      CV = TRUE
    )
    colnames(all_precisions$ind) <- paste0(colnames(all_precisions$ind), "_MSE")
    colnames(all_precisions$ind_cv) <- paste0(
      colnames(all_precisions$ind_cv),
      "_CV"
    )
    combined <- data.frame(
      all_ind$ind, all_precisions$ind,
      all_precisions$ind_cv
    )
    endings <- c("", "_MSE", "_CV")[c(TRUE, MSE, CV)]
    
    combined <- combined[, c("Domain", paste0(rep(selected,
                                                  each =
                                                    length(endings)
    ), endings))]
  } else {
    combined <- all_ind$ind
  }
  
  estimators_emdi <- list(ind = combined, ind_name = all_ind$ind_name)
  
  class(estimators_emdi) <- "estimators.emdi"
  
  return(estimators_emdi)
}

# Prints estimators.emdi objects
#' @export

print.estimators.emdi <- function(x, ...) {
  cat(paste0("Indicator/s: ", x$ind_name, "\n"))
  print(x$ind)
}


# Tail/head functions ----------------------------------------------------------


#' @importFrom utils head
#' @export
# CV estimators

head.estimators.emdi <- function(x, n = 6L, addrownums = NULL, ...) {
  head(x$ind, n = n, addrownums = addrownums, ...)
}

#' @importFrom utils tail
#' @export

tail.estimators.emdi <- function(x, n = 6L, keepnums = TRUE,
                                 addrownums = NULL, ...) {
  tail(x$ind, n = n, keepnums = keepnums, ...)
}


# Transforms estimators.emdi objects into a matrix object
#' @export

as.matrix.estimators.emdi <- function(x, ...) {
  as.matrix(x$ind[, -1])
}

# Transforms estimators.emdi objects into a dataframe object
#' @export

as.data.frame.estimators.emdi <- function(x, ...) {
  as.data.frame(x$ind, ...)
}

# Subsets an estimators.emdi object
#' @export

subset.estimators.emdi <- function(x, ...) {
  x <- as.data.frame(x)
  subset(x = x, ...)
}

estimators_check <- function(object,
                             indicator,
                             MSE,
                             CV) {
  if (is.null(object$MSE) && (MSE == TRUE || CV == TRUE)) {
    stop(strwrap(prefix = " ", initial = "",
                 "No MSE estimates in emdi object: arguments MSE and CV have to
                 be FALSE or a new emdi object with variance/MSE needs to be
                 generated."))
  }
  if (!(inherits(MSE, "logical") && length(MSE) == 1)) {
    stop("MSE must be a logical value. Set MSE to TRUE or FALSE.")
  }
  if (!(inherits(CV, "logical") && length(CV) == 1)) {
    stop("CV must be a logical value. Set CV to TRUE or FALSE.")
  }
  if (inherits(object, "fh")) {
    if (is.null(indicator) || !all(indicator == "all" | indicator == "All" |
                                   indicator == "FH" |
                                   indicator == "FH_Bench" |
                                   indicator == "Direct")) {
      stop(strwrap(prefix = " ", initial = "",
                   paste0("The argument indicator is set to ", indicator, ".
                   The argument only allows to be set to all, FH, Direct or
                   FH_Bench (if benchmark function is used before).")))
    }
  } else {
    if (is.null(indicator) || !all(indicator == "all" | indicator == "All" |
                                   indicator == "Quantiles" |
                                   indicator == "quantiles" |
                                   indicator == "Poverty" |
                                   indicator == "poverty" |
                                   indicator == "Inequality" |
                                   indicator == "inequality" |
                                   indicator == "Custom" |
                                   indicator == "custom" |
                                   indicator %in% names(object$ind[-1]))) {
      stop(strwrap(prefix = " ", initial = "",
                   paste0("The argument indicator is set to ", indicator, ".
                          The argument only allows to be set to all, a name of
                          estimated indicators or indicator groups as described
                          in help(estimators.emdi).")))
    }
  }
}


#' Visualizes regional disaggregated estimates on a map
#'
#' Function \code{map_plot} creates spatial visualizations of the estimates
#' obtained by small area estimation methods.
#'
#' @param object an object of type emdi, containing the estimates to be
#' visualized.
#' @param indicator optional character vector that selects which indicators
#' shall be returned: (i) all calculated indicators ("all");
#' (ii) each indicator name: "Mean", "Quantile_10", "Quantile_25", "Median",
#' "Quantile_75", "Quantile_90", "Head_Count", "Poverty_Gap", "Gini",
#' "Quintile_Share" or the function name/s of "custom_indicator/s";
#' (iii) groups of indicators: "Quantiles", "Poverty" or
#' "Inequality". Note, additional custom indicators can be
#' defined as argument for model-based approaches (see also \code{\link{ebp}})
#' and do not appear in groups of indicators even though these might belong to
#' one of the groups. If the \code{model} argument is of type "fh",
#' indicator can be set to "all", "Direct", FH", or "FH_Bench" (if emdi
#' object is overwritten by function benchmark). Defaults to "all".
#' @param MSE optional logical. If \code{TRUE}, the MSE is also visualized.
#' Defaults to \code{FALSE}.
#' @param CV optional logical. If \code{TRUE}, the CV is also visualized.
#' Defaults to \code{FALSE}.
#' @param map_obj an \code{"sf", "data.frame"} object as defined by the
#' \pkg{sf} package on which the data should be visualized.
#' @param map_dom_id a character string containing the name of a variable in
#' \code{map_obj} that indicates the domains.
#' @param map_tab a \code{data.frame} object with two columns that match the
#' domain variable from the census data set (first column) with the domain
#' variable in the map_obj (second column). This should only be used if the IDs
#' in both objects differ.
#' @param color a \code{vector} of length 2 defining the lowest and highest
#' color in the plots.
#' @param scale_points a structure defining the lowest and the highest
#' value of the colorscale. If a numeric vector of length two is given, this
#' scale will be used for every plot.
#' @param guide character passed to
#' \code{scale_colour_gradient} from \pkg{ggplot2}.
#' Possible values are "none", "colourbar", and "legend".
#' @param return_data if set to \code{TRUE}, a fortified data frame including
#' the map data as well as the chosen indicators is returned. Customized maps
#' can easily be obtained from this data frame via the package \pkg{ggplot2}.
#' Defaults to \code{FALSE}.
#' @return Creates the plots demanded, and, if selected, a fortified data.frame
#' containing the mapdata and chosen indicators.
#' @seealso \code{\link{direct}}, \code{\link{ebp}}, \code{\link{fh}},
#' \code{\link{emdiObject}}, \code{\link[sf]{sf}}
#' @examples
#' \donttest{
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'
#' # Generate emdi object with additional indicators; here via function ebp()
#' emdi_model <- ebp(
#'   fixed = eqIncome ~ gender + eqsize + cash +
#'     self_empl + unempl_ben + age_ben + surv_ben + sick_ben +
#'     dis_ben + rent + fam_allow + house_allow + cap_inv +
#'     tax_adj, pop_data = eusilcA_pop,
#'   pop_domains = "district", smp_data = eusilcA_smp,
#'   smp_domains = "district", threshold = 11064.82,
#'   transformation = "box.cox", L = 50, MSE = TRUE, B = 50
#' )
#'
#' # Load shape file
#' load_shapeaustria()
#'
#' # Create map plot for mean indicator - point and MSE estimates but no CV
#' map_plot(
#'   object = emdi_model, MSE = TRUE, CV = FALSE,
#'   map_obj = shape_austria_dis, indicator = c("Mean"),
#'   map_dom_id = "PB"
#' )
#'
#' # Create a suitable mapping table to use numerical identifiers of the shape
#' # file
#'
#' # First find the right order
#' dom_ord <- match(shape_austria_dis$PB, emdi_model$ind$Domain)
#'
#' #Create the mapping table based on the order obtained above
#' map_tab <- data.frame(pop_data_id = emdi_model$ind$Domain[dom_ord],
#'                       shape_id = shape_austria_dis$BKZ)
#'
#' # Create map plot for mean indicator - point and CV estimates but no MSE
#' # using the numerical domain identifiers of the shape file
#'
#' map_plot(
#'   object = emdi_model, MSE = FALSE, CV = TRUE,
#'   map_obj = shape_austria_dis, indicator = c("Mean"),
#'   map_dom_id = "BKZ", map_tab = map_tab
#' )
#' }
#' @export
#' @importFrom reshape2 melt
#' @importFrom ggplot2 aes geom_polygon geom_sf facet_wrap coord_equal labs
#' @importFrom ggplot2 theme element_blank scale_fill_gradient ggplot ggtitle
#' @importFrom rlang .data

map_plot <- function(object,
                     indicator = "all",
                     MSE = FALSE,
                     CV = FALSE,
                     map_obj = NULL,
                     map_dom_id = NULL,
                     map_tab = NULL,
                     color = c("white", "red4"),
                     scale_points = NULL,
                     guide = "colourbar",
                     return_data = FALSE
) {
  
  if (is.null(map_obj)) {
    
    message("No Map Object has been provided. An artificial polygone is used for
             visualization")
    
    map_pseudo(object    = object,
               indicator = indicator,
               panelplot = FALSE,
               MSE       = MSE,
               CV        = CV
    )
  } else if (!inherits(map_obj, "sf")) {
    
    stop("map_obj is not of class sf from the sf package")
    
  } else {
    
    if (length(color) != 2 || !is.vector(color)) {
      stop(paste("col needs to be a vector of length 2 defining the starting,",
                 "mid and upper color of the map-plot"))
    }
    
    plot_real(object       = object,
              indicator    = indicator,
              MSE          = MSE,
              CV           = CV,
              map_obj      = map_obj,
              map_dom_id   = map_dom_id,
              map_tab      = map_tab,
              col          = color,
              scale_points = scale_points,
              return_data  = return_data,
              guide        = guide
    )
  }
}

map_pseudo <- function(object, indicator, panelplot, MSE, CV) {
  
  x <- y <- id <- value <- NULL
  
  values <-  estimators(object    = object,
                        indicator = indicator,
                        MSE       = MSE,
                        CV        = CV
  )$ind
  
  indicator <- colnames(values)[-1]
  
  tplot <- get_polygone(values = values)
  
  if (panelplot) {
    ggplot(tplot, aes(x = x, y = y)) +
      geom_polygon(aes(
        group = id,
        fill = value
      )) +
      facet_wrap(~variable,
                 ncol = ceiling(sqrt(length(unique(tplot$variable))))
      )
  } else {
    for (ind in indicator) {
      print(print(ggplot(tplot[tplot$variable == ind, ], aes(x = x, y = y)) +
                    ggtitle(paste0(ind)) +
                    geom_polygon(aes(
                      group = id,
                      fill = value
                    ))))
      if (interactive() && dev.cur() == 1L) { cat("Press [enter] to continue"); readline() }
    }
  }
}

plot_real <- function(object,
                      indicator = "all",
                      MSE = FALSE,
                      CV = FALSE,
                      map_obj = NULL,
                      map_dom_id = NULL,
                      map_tab = NULL,
                      col = col,
                      scale_points = NULL,
                      return_data = FALSE,
                      guide = NULL) {
  
  
  if (!is.null(map_obj) && is.null(map_dom_id)) {
    stop("No Domain ID for the map object is given")
  }
  
  long <- lat <- group <- NULL
  
  map_data <- estimators(object    = object,
                         indicator = indicator,
                         MSE       = MSE,
                         CV        = CV
  )$ind
  
  if (!is.null(map_tab)) {
    map_data <- merge(x    = map_data,
                      y    = map_tab,
                      by.x = "Domain",
                      by.y = names(map_tab)[1]
    )
    matcher <- match(map_obj[[map_dom_id]],
                     map_data[, names(map_tab)[2]])
    
    if (any(is.na(matcher))) {
      if (all(is.na(matcher))) {
        stop("Domains of map_tab and Map object do not match. Check map_tab")
      } else {
        warnings(paste("Not all Domains of map_tab and Map object could be",
                       "matched. Check map_tab"))
      }
    }
    
    map_data <- map_data[matcher, ]
    map_data <- map_data[, !colnames(map_data) %in%
                           c("Domain", map_dom_id), drop = F]
    map_data$Domain <- map_data[, colnames(map_data) %in% names(map_tab)]
  } else {
    matcher <- match(map_obj[[map_dom_id]], map_data[, "Domain"])
    
    if (any(is.na(matcher))) {
      if (all(is.na(matcher))) {
        stop(paste("Domain of emdi object and Map object do not match.",
                   "Try using map_tab"))
      } else {
        warnings(paste("Not all Domains of emdi object and Map object",
                       "could be matched. Try using map_tab"))
      }
    }
    map_data <- map_data[matcher, ]
  }
  
  map_obj.merged <- merge(map_obj, map_data, by.x = map_dom_id, by.y = "Domain")
  
  indicator <- colnames(map_data)
  indicator <- indicator[!(indicator %in% c("Domain", "shape_id"))]
  
  for (ind in indicator) {
    
    map_obj.merged[[ind]][!is.finite(map_obj.merged[[ind]])] <- NA
    
    scale_point <- get_scale_points(y            = map_obj.merged[[ind]],
                                    ind          = ind,
                                    scale_points = scale_points
    )
    
    print(ggplot(data = map_obj.merged,
                 aes(long, lat, group = group, fill = .data[[ind]])) +
            geom_sf(color = "azure3") +
            labs(x = "", y = "", fill = ind) +
            ggtitle(gsub(pattern = "_", replacement = " ", x = ind)) +
            scale_fill_gradient(low    = col[1],
                                high   = col[2],
                                limits = scale_point,
                                guide  = guide
            ) +
            theme(axis.ticks   = element_blank(),
                  axis.text    = element_blank(),
                  legend.title = element_blank()
            )
    )
    
    if (!ind == tail(indicator, 1)) {
      if (interactive() && dev.cur() == 1L) { cat("Press [enter] to continue"); readline() }
    }
  }
  if (return_data) {
    return(map_obj.merged)
  }
}

get_polygone <- function(values) {
  
  if (is.null(dim(values))) {
    values <- as.data.frame(values)
  }
  
  n <- nrow(values)
  cols <- ceiling(sqrt(n))
  n <- cols^2
  
  values["id"] <- seq_len(nrow(values))
  
  poly <- data.frame(id       = rep(seq_len(n), each = 4),
                     ordering = seq_len((n * 4)),
                     x        = c(0, 1, 1, 0) +
                       rep(0:(cols - 1), each = (cols * 4)),
                     y        = rep(c(0, 0, 1, 1) +
                                      rep(0:(cols - 1), each = 4), cols)
  )
  
  combo <- merge(poly, values, by = "id", all = TRUE, sort = FALSE)
  
  melt(data    = combo[order(combo$ordering), ],
       id.vars = c("id", "x", "y", "ordering")
  )
}

get_scale_points <- function(y, ind, scale_points) {
  
  result <- NULL
  
  if (!is.null(scale_points)) {
    if (inherits(scale_points, "numeric") && length(scale_points) == 2) {
      result <- scale_points
    }
  }
  if (is.null(result)) {
    rg <- range(y, na.rm = TRUE)
    result <- rg
  }
  return(result)
}


#' Function for estimating sigmau2 with REML.
#'
#' This function estimates sigmau2.
#'
#' @param interval interval for the algorithm.
#' @param direct direct estimator.
#' @param x matrix with explanatory variables.
#' @param vardir direct variance.
#' @param areanumber number of domains.
#' @return estimated sigmau2.
#' @noRd


Reml <- function(interval, direct, x, vardir, areanumber) {
  A.reml <- function(interval, direct, x, vardir, areanumber) {
    psi <- matrix(c(vardir), areanumber, 1)
    Y <- matrix(c(direct), areanumber, 1)
    X <- x
    Z.area <- diag(1, areanumber)
    sigma.u_log <- interval[1]
    I <- diag(1, areanumber)
    # V is the variance covariance matrix
    V <- sigma.u_log * Z.area %*% t(Z.area) + I * psi[, 1]
    Vi <- solve(V)
    Xt <- t(X)
    XVi <- Xt %*% Vi
    Q <- solve(XVi %*% X)
    P <- Vi - (Vi %*% X %*% Q %*% XVi)
    
    ee <- eigen(V)
    -(areanumber / 2) * log(2 * pi) - 0.5 * sum(log(ee$value)) - (0.5) *
      log(det(t(X) %*% Vi %*% X)) - (0.5) * t(Y) %*% P %*% Y
  }
  ottimo <- optimize(A.reml, interval,
                     maximum = TRUE,
                     vardir = vardir, areanumber = areanumber,
                     direct = direct, x = x
  )
  
  estsigma2u <- ottimo$maximum
  
  return(sigmau_reml = estsigma2u)
}


#' Function for estimating sigmau2 using maximum likelihood.
#'
#' This function estimates sigmau2 using the maximum profile
#' likelihood.
#'
#' @param interval interval for the algorithm.
#' @param direct direct estimator.
#' @param x matrix with explanatory variables.
#' @param vardir direct variance.
#' @param areanumber number of domains.
#' @return estimated sigmau2.
#' @noRd

MPL <- function(interval, direct, x, vardir, areanumber) {
  ML <- function(interval, direct, x, vardir, areanumber) {
    psi <- matrix(c(vardir), areanumber, 1)
    Y <- matrix(c(direct), areanumber, 1)
    X <- x
    Z.area <- diag(1, areanumber)
    sigma.u_log <- interval[1]
    I <- diag(1, areanumber)
    # V is the variance covariance matrix
    V <- sigma.u_log * Z.area %*% t(Z.area) + I * psi[, 1]
    Vi <- solve(V)
    Xt <- t(X)
    XVi <- Xt %*% Vi
    Q <- solve(XVi %*% X)
    P <- Vi - (Vi %*% X %*% Q %*% XVi)
    
    ee <- eigen(V)
    -(areanumber / 2) * log(2 * pi) - 0.5 * sum(log(ee$value)) -
      (0.5) * t(Y) %*% P %*% Y
  }
  
  ottimo <- optimize(ML, interval,
                     maximum = TRUE,
                     vardir = vardir, areanumber = areanumber,
                     direct = direct, x = x
  )
  
  estsigma2u <- ottimo$maximum
  
  return(sigmau_mpl = estsigma2u)
}

#' Wrapper function for the estmation of sigmau2
#'
#' This function wraps the different estimation methods for sigmau2.
#'
#' @param vardir direct variance.
#' @param tol precision criteria for the estimation of sigmau2.
#' @param maxit maximum of iterations for the estimation of sigmau2.
#' @param interval interval for the algorithm.
#' @param direct direct estimator.
#' @param x matrix with explanatory variables.
#' @param areanumber number of domains.
#' @return estimated sigmau2.
#' @noRd

wrapper_estsigmau2 <- function(framework, method, interval) {
  sigmau2 <- if (method == "reml") {
    Reml(
      interval = interval, vardir = framework$vardir, x = framework$model_X,
      direct = framework$direct, areanumber = framework$N_dom_smp
    )
  } else if (method == "ml") {
    MPL(
      interval = interval, vardir = framework$vardir,
      x = framework$model_X, direct = framework$direct,
      areanumber = framework$N_dom_smp
    )
  }
  return(sigmau2)
}


#' Regional ratio-adjustment benchmarking for Fay-Herriot estimates
#'
#' Benchmarks province-level FH point estimates to region-level poverty rates,
#' where each region's target is the population-weighted average of the direct
#' province estimates within that region:
#' \deqn{B_r = \frac{\sum_{d \in r} N_d\,\hat\theta_d^{\text{Dir}}}
#'                   {\sum_{d \in r} N_d}}
#' A separate ratio factor \eqn{\lambda_r = B_r / \bar\theta_r^{\text{FH}}}
#' is computed for each region and applied to every province in that region,
#' where \eqn{\bar\theta_r^{\text{FH}}} is the population-weighted mean of
#' the unbenchmarked FH estimates within region \eqn{r}.
#'
#' @param model     An object of class \code{"fh"} (output of \code{emdi::fh}
#'   with \code{MSE = TRUE}).
#' @param region_df A data.frame with one row per province and columns:
#'   \describe{
#'     \item{domain}{Province identifier matching \code{model$ind$Domain}.}
#'     \item{region}{Integer region code (1, 2, ...).}
#'     \item{Nd}{Province population size used as benchmark weight.}
#'     \item{direct_povrate}{Direct province poverty-rate estimate; may be
#'       \code{NA} for out-of-sample provinces (they still receive a
#'       benchmarked estimate via their region's ratio factor).}
#'   }
#'
#' @param MSE       Logical. If \code{TRUE}, compute benchmarked MSEs using
#'   a bootstrap similar to the one used in \code{fh(..., mse_type = "boot")}
#'   and store them in \code{model$MSE$FH_Bench}. Defaults to \code{FALSE}.
#' @param B         Number of bootstrap iterations for benchmarked MSE
#'   estimation when \code{MSE = TRUE}. Defaults to \code{200}.
#' @param seed      Optional integer seed for reproducibility.
#'
#' @return The \code{model} object with \code{FH_Bench} appended to both
#'   \code{model$ind} and \code{model$MSE}. If \code{MSE = FALSE}, benchmarked
#'   MSE is approximated by the unbenchmarked FH MSE.
#'
#' @export
bench_regional <- function(model, region_df, MSE = FALSE, B = 200,
                           seed = 123) {

  eps_denom <- 1e-8

  apply_regional_bench <- function(model, region_df) {
    ind <- model$ind   # columns: Domain, Direct, FH, Out, ...

    # Attach region metadata
    ind <- merge(
      ind,
      region_df[, c("domain", "region", "Nd", "direct_povrate")],
      by.x  = "Domain", by.y = "domain",
      all.x = TRUE, sort = FALSE
    )

    reg_ids  <- sort(unique(ind$region[!is.na(ind$region)]))
    B_r_vec  <- setNames(numeric(length(reg_ids)), as.character(reg_ids))
    Nd_sum_r <- setNames(numeric(length(reg_ids)), as.character(reg_ids))

    # Regional benchmark B_r and total population N_r
    for (r in reg_ids) {
      rows <- which(ind$region == r)
      Nd_sum_r[as.character(r)] <- sum(ind$Nd[rows], na.rm = TRUE)
      ok <- rows[!is.na(ind$direct_povrate[rows]) & !is.na(ind$Nd[rows])]
      B_r_vec[as.character(r)] <- if (length(ok) > 0) {
        weighted.mean(ind$direct_povrate[ok], ind$Nd[ok])
      } else {
        NA_real_
      }
    }

    ind$B_r      <- B_r_vec[as.character(ind$region)]
    ind$Nd_sum_r <- Nd_sum_r[as.character(ind$region)]
    ind$share_r  <- ind$Nd / ind$Nd_sum_r   # province share within its region

    # Weighted mean of FH estimates within each region
    FH_bar_r <- setNames(numeric(length(reg_ids)), as.character(reg_ids))
    for (r in reg_ids) {
      rows <- which(ind$region == r)
      FH_bar_r[as.character(r)] <-
        sum(ind$FH[rows] * ind$share_r[rows], na.rm = TRUE)
    }

    ind$FH_bar_r <- FH_bar_r[as.character(ind$region)]
    ind$lambda_r <- ifelse(
      is.na(ind$B_r) | is.na(ind$FH_bar_r) | abs(ind$FH_bar_r) < eps_denom,
      1,
      ind$B_r / ind$FH_bar_r
    )                                             # per-region ratio factor
    ind$FH_Bench <- ind$FH * ind$lambda_r          # benchmarked estimate

    model$ind$FH_Bench <- ind$FH_Bench[match(model$ind$Domain, ind$Domain)]
    model
  }

  boot_bench_regional <- function(model, region_df, B) {
    framework <- model$framework
    sigmau2 <- model$model$variance
    vardir <- framework$vardir
    in_sample <- model$ind$Out == 0
    out_sample <- model$ind$Out == 1
    m <- framework$N_dom_smp
    M <- framework$N_dom_smp + framework$N_dom_unobs

    transformation <- model$transformation$transformation
    backtransformation <- model$transformation$backtransformation

    # Design matrix for ALL domains (used for out-of-sample prediction)
    pred_X <- model.matrix(
      update(model$fixed, helper ~ .),
      data = transform(framework$combined_data,
                       helper = rnorm(nrow(framework$combined_data)))
    )
    Xbeta <- as.numeric(pred_X %*% model$model$coefficients$coefficients)

    # Nonparametric bootstrap: center and rescale random effects & residuals
    vhat <- model$model$random_effects[, 1]
    vhat_cs <- ((vhat - mean(vhat)) /
      sqrt(sum((vhat - mean(vhat))^2) / m)) * sqrt(sigmau2)

    res <- model$model$real_residuals
    res_cs <- (res - mean(res)) / sqrt(sum((res - mean(res))^2) / m)

    true_value_boot <- matrix(NA_real_, nrow = M, ncol = B)
    est_value_boot_bench <- matrix(NA_real_, nrow = M, ncol = B)

    # Regional metadata for benchmarking
    meta <- merge(
      data.frame(Domain = model$ind$Domain, stringsAsFactors = FALSE),
      region_df[, c("domain", "region", "Nd", "direct_povrate")],
      by.x = "Domain", by.y = "domain",
      all.x = TRUE, sort = FALSE
    )
    reg_ids <- sort(unique(meta$region[!is.na(meta$region)]))
    B_r_vec <- setNames(rep(NA_real_, length(reg_ids)), as.character(reg_ids))
    Nd_sum_r <- setNames(rep(NA_real_, length(reg_ids)), as.character(reg_ids))

    for (r in reg_ids) {
      rows <- which(meta$region == r)
      Nd_sum_r[as.character(r)] <- sum(meta$Nd[rows], na.rm = TRUE)
      ok <- rows[!is.na(meta$direct_povrate[rows]) & !is.na(meta$Nd[rows])]
      B_r_vec[as.character(r)] <- if (length(ok) > 0) {
        weighted.mean(meta$direct_povrate[ok], meta$Nd[ok])
      } else {
        NA_real_
      }
    }

    meta$B_r <- B_r_vec[as.character(meta$region)]
    meta$Nd_sum_r <- Nd_sum_r[as.character(meta$region)]
    meta$share_r <- meta$Nd / meta$Nd_sum_r

    # Helper: apply regional ratio-adjustment to a vector of FH estimates
    regional_bench_vec <- function(fh_vec) {
      FH_bar_r <- setNames(rep(NA_real_, length(reg_ids)), as.character(reg_ids))
      for (r in reg_ids) {
        rows <- which(meta$region == r)
        FH_bar_r[as.character(r)] <-
          sum(fh_vec[rows] * meta$share_r[rows], na.rm = TRUE)
      }
      lambda_r <- ifelse(
        is.na(meta$B_r) | is.na(FH_bar_r[as.character(meta$region)]) |
          abs(FH_bar_r[as.character(meta$region)]) < eps_denom,
        1,
        meta$B_r / FH_bar_r[as.character(meta$region)]
      )
      fh_vec * lambda_r
    }

    for (b in seq_len(B)) {
      tryCatch({
        v_boot <- sample(vhat_cs, M, replace = TRUE)
        res_boot <- sample(res_cs, m, replace = TRUE)
        e_boot <- res_boot * sqrt(vardir)

        if (transformation == "no") {
          # ==== No transformation: use update() safely ====

          # True values (original scale)
          true_value_boot[, b] <- Xbeta + v_boot

          # Bootstrap y*
          ystar <- rep(NA_real_, M)
          ystar[in_sample] <- Xbeta[in_sample] + v_boot[in_sample] + e_boot

          combined_data_star <- framework$combined_data
          combined_data_star$ystar <- ystar

          fh_star <- suppressMessages(
            update(
              model,
              fixed = update.formula(formula(model), ystar ~ .),
              combined_data = combined_data_star,
              MSE = FALSE,
              seed = NULL
            )
          )

          est_boot <- fh_star$ind$FH
          est_value_boot_bench[, b] <- regional_bench_vec(est_boot)

        } else if (transformation == "arcsin") {
          # ==== Arcsin transformation: manual EBLUP (avoids double-transformation) ====
          # Following the same approach as bench() -- do NOT call update() because
          # it would re-apply arcsin(sqrt(.)) to already-transformed y*.

          # ---- True values: truncate + naive back-transform ----
          true_trans <- Xbeta + v_boot
          true_trans[true_trans < 0] <- 0
          true_trans[true_trans > (pi / 2)] <- (pi / 2)
          true_value_boot[, b] <- (sin(true_trans))^2

          # ---- Bootstrap y* on the transformed scale ----
          ystar_trans <- Xbeta[in_sample] + v_boot[in_sample] + e_boot

          # ---- Re-estimate sigmau2 from bootstrap data ----
          framework2 <- framework
          framework2$direct <- ystar_trans
          framework2$model_X <- model.matrix(
            update(model$fixed, helper ~ .),
            data = transform(framework$combined_data[in_sample, ],
                             helper = rnorm(sum(in_sample)))
          )
          sigmau2_boot <- wrapper_estsigmau2(
            framework = framework2, method = model$method$method,
            interval = c(0, var(framework2$direct))
          )

          # ---- Manual GLS coefficient estimation ----
          x <- framework2$model_X                         # in-sample design matrix
          D <- diag(1, m)
          V <- sigmau2_boot * D %*% t(D) + diag(as.numeric(vardir))
          Vi <- solve(V)
          Q <- solve(t(x) %*% Vi %*% x)
          Beta.hat_boot <- Q %*% t(x) %*% Vi %*% ystar_trans

          # ---- Manual EBLUP on transformed scale ----
          res_star <- ystar_trans - c(x %*% Beta.hat_boot)
          Sigma.u <- sigmau2_boot * D
          u.hat <- Sigma.u %*% t(D) %*% Vi %*% res_star

          est_mean_boot_trans <- x %*% Beta.hat_boot + D %*% u.hat    # in-sample
          pred_out_boot_trans <- pred_X %*% Beta.hat_boot              # all domains

          est_value_boot_trans <- rep(NA_real_, M)
          est_value_boot_trans[in_sample]  <- est_mean_boot_trans
          est_value_boot_trans[out_sample] <- pred_out_boot_trans[out_sample]

          # ---- Conditional variance (needed for bias-corrected back-transform) ----
          gamma_trans <- as.numeric(vardir) / (sigmau2_boot + as.numeric(vardir))
          est_value_boot_trans_var <- sigmau2_boot * gamma_trans
          est_value_boot_trans_var_ <- rep(0, M)
          est_value_boot_trans_var_[in_sample] <- est_value_boot_trans_var

          # ---- Back-transformation ----
          if (backtransformation == "bc") {
            # Bias-corrected: integrate sin^2(x) * phi(x; mu, sigma^2) over [0, pi/2]
            int_value <- NULL
            for (i in seq_len(M)) {
              if (in_sample[i] == TRUE) {
                mu_dri  <- est_value_boot_trans[i]
                Var_dri <- as.numeric(est_value_boot_trans_var_[i])

                integrand <- function(x, mean, sd) {
                  sin(x)^2 * dnorm(x, mean = mu_dri, sd = sqrt(Var_dri))
                }

                int_value <- c(int_value, integrate(integrand,
                                                     lower = 0, upper = pi / 2)$value)
              } else {
                int_value <- c(int_value, (sin(est_value_boot_trans[i]))^2)
              }
            }
          } else if (backtransformation == "naive") {
            int_value <- NULL
            for (i in seq_len(M)) {
              int_value <- c(int_value, (sin(est_value_boot_trans[i]))^2)
            }
          }

          est_boot <- int_value
          est_value_boot_bench[, b] <- regional_bench_vec(est_boot)
        }

        message("Regional bootstrap b = ", b, "\n")
      }, error = function(e) {
        message("Regional bootstrap iteration ", b, " failed: ", e$message)
      })
    }

    # Use unbenchmarked true values (Datta et al. 2011: MSE = E[(bench_est - true)^2])
    apply((est_value_boot_bench - true_value_boot)^2, 1, mean, na.rm = TRUE)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  model <- apply_regional_bench(model, region_df)

  if (!is.null(model$MSE)) {
    if (MSE) {
      mse_bench <- boot_bench_regional(model, region_df = region_df, B = B)
      model$MSE$FH_Bench <- mse_bench[match(model$MSE$Domain, model$ind$Domain)]
    } else {
      model$MSE$FH_Bench <- model$MSE$FH
    }
  }

  return(model)
}
