#' Generates design matrices from a dataset
#'
#' Generates two design matrices, each with a specified complexity
#' (meaning number of covariates), and with a specific degree of 
#' shared complexity (meaning how many of their covariates
#' to they have in common).
#' 
#' The function builds the design matrices using up covariates from
#' the data set as needed starting from left to right. So for example,
#' if there is no shared complexity and each model has complexity 3 the
#' first design matrix will contain the three first covariates in the 
#' data set, and the second will contain covariates four through six. If 
#' there are more covariates than this in the data set these will be 
#' left unused. In the case where there is shared complexity, the covariates 
#' that are included in both models will be the first taken from left to 
#' right.
#' 
#' (Maybe a better way to do this would be to supply the function with 
#' two 0/1 vectors of which covariates to include.)
#'
#' @param complexity Number of covariates in each model
#' @param sharedComplexity Number of covariates in common between both models
#' @param dataSet The data set used, the first column contains the response and
#' the is no included intercept

generateDesignMatrices <- function(complexity,
                                   sharedComplexity,
                                   dataSet) {

    nRows <- nrow(dataSet)
    dataSet <- as.matrix(dataSet)

    XM2 <- XM1 <- matrix(NA, nrow = nRows, ncol = complexity + 1)
    XM2[, 1] <- XM1[, 1] <- rep(1, nRows)

    if (sharedComplexity > 0) {

        b1 <- sharedComplexity + 1 # b for boud, shorter lines...
        XM2[, 2:b1] <- XM1[, 2:b1] <- dataSet[, 2:b1]
        if (complexity > sharedComplexity) {
            b2 <- sharedComplexity + 2
            b3 <- complexity + 1
            b4 <- 2 * (complexity - sharedComplexity) + sharedComplexity + 1
            XM1[, b2:b3] <- dataSet[, b2:b3]
            XM2[, b2:b3] <- dataSet[, (b3 + 1):b4]
        }

    } else {

        b6 <- (complexity + 1)
        XM1[, 2:b6] <- dataSet[, 2:b6]
        XM2[, 2:b6] <- dataSet[, (b6 + 1):(2 * complexity + 1)]

    }
    return(list(XM1, XM2))
}