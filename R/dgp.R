#' Generates data from a linear model
#'
#' Generates data on where the outcome is the sum of nCov uniform(-1, 1) random
#' variables. All covariates are set to 0 by default, then a number corresponding
#' to nNonZero are changed to one.
#'
#' @param nObs Number of observations
#' @param nCov Number of covariates, excluding intercept
#' @param nNonZero Number of covariates with coefficients set to one, from left
#' to right

dgp <- function(nObs, nCov, nNonZero){
  X <-matrix(runif(nObs * nCov, min = -1, max = 1), nrow = nObs)
  y <- rowSums(X[, 0:nNonZero, drop = FALSE]) + rnorm(nObs, mean = 0, sd = 1)
  df <- data.frame(y, X)
  return(df)
}


