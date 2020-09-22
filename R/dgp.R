#' Generates data from a linear model
#'
#' Generates nObs observations where the outcome (y) is the sum of nNonZero  out of
#' nCov uniform(-1, 1) random variables. The covariates with non-zero coefficients
#' are always the nNonZero first covariates.
#'
#' @param nObs Number of observations
#' @param nCov Number of covariates, excluding intercept
#' @param nNonZero Number of covariates with coefficients set to one, from left
#' to right
#' @param trueMean If TRUE, returns response/outcome without noise. Defaults to
#' false
#' @param beta_coefs Vector of length nCov containing beta coefficients. Defaults to a vector of ones.

dgp <- function(nObs, nCov, nNonZero, trueMean = FALSE,
                beta_coefs = rep(1, nCov)){

  X <- matrix(runif(nObs * nCov, min = -1, max = 1), nrow = nObs)
  y <- X[, 0:nNonZero, drop = FALSE]  %*% as.matrix(beta_coefs)
  if (trueMean == FALSE) {
    y <- y + rnorm(nObs, mean = 0, sd = 1)
  }
  df <- data.frame(y, X)
  return(df)
}