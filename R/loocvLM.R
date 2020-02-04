#' Calculates leave-one-out-cross-validation errors for a linear model
#'
#' @param data The name of the data set
#' @param model The name of the (linear) model


loocvLM <- function(data, model){
  errorCV <- 0
  for(i in 1: nrow(data)){
    test <- data[i, ]
    train <- data[-i, ]
    errorCV <- errorCV + (predict(lm(model, data = train), test) - test[1])^2
  }
  return(errorCV)
}
