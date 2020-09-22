#' Generates data from a linear model
#'
#' Generates n_obs observations where the outcome (y) is the weighted sum, with
#' weights given by beta_coefs, of a number of independent uniform(-1, 1)
#' distributed random variables.
#'
#' @param n_obs Number of observations
#' @param beta_coefs Vector of beta coefficients. The length of the vector will
#' determine the number of covariates
#' @param trueMean If TRUE, returns response/outcome without noise. Defaults to
#' false

dgp <- function(n_obs, beta_coefs, true_mean = FALSE){

  n_cov <- length(beta_coefs)
  X <- matrix(runif(n_obs * n_cov, min = -1, max = 1), nrow = n_obs)
  y <- X  %*% as.matrix(beta_coefs)
  if (true_mean == FALSE) {
    y <- y + rnorm(n_obs, mean = 0, sd = 1)
  }
  df <- data.frame(y, X)
  return(df)
}