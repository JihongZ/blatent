#' @title Sets the distribution parameters for initializing all parameters
#'
#' @description All parameters are initialized with distributions using these parameters.
#' Used to quickly set priors for sets of parameters.
#'
#' @param normalMean Sets the initialization distribution mean for all parameters with
#' normal distributions. Defaults to \code{0}.
#'
#' @param normalVariance Sets the initialization distribution variance for all parameters with
#' normal distributions. Defaults to \code{10}.
#'
#' @param normalCovariance Sets the initialization distribution covariance for all parameters with
#' multivariate normal distributions. Defaults to \code{0}.
#'
#' @return A list containing named values for each argument in the function.
#'
#' @export
setDefaultInitializeParameters = function(normalMean = 0, normalVariance = 1, normalCovariance = 0){
  defaultInitializeParameters = list(
    normalMean = normalMean,
    normalVariance = normalVariance,
    normalCovariance = normalCovariance
  )
  return(defaultInitializeParameters)
}
