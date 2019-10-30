#' @title Simulates data using parameters from posterior distribution of blatent Markov chain
#'
#' @description Simulates data using parameters from posterior distribution of blatent Markov chain.
#'
#' @param model A blatent MCMC model object.
#'
#' @param nSamples The number of PPMC samples to be simulated.
#'
#' @param seed The random number seed. Defaults to the seed set in the blatent model object.
#'
#' @param type The type of statistic to generate. Defaults to c("covariance", "bivariate"), which produces covariances
#'             and bivariate Chi-Square statistics. Not all of these may be good for model fit.
#'
#' @export
blatentPPMC = function(model, nSamples, seed = model$options$seed, type = c("covariance", "bivariate")){

  # put full posterior into one easily readable matrix
  fullPosterior = list(variables = Reduce(
    f = "rbind",
    x = model$chain
    )
  )


  if (any(c("covariance", "bivariate") %in% type)){

    # check list of pairwise variables with data for correlation
    obsCov = cov(model$data[model$specs$observedVariables], use = "pairwise.complete.obs")

    # convert true zeros to NAs
    for (i in 1:nrow(obsCov)){
      if (!is.na(any(obsCov[i,]==0))){
        if (any(obsCov[i,]==0)) obsCov[i,which(obsCov[i,]==0)] = NA
      }

    }


    # remove NAs
    covEst = sapply(
      X = 1:(nrow(obsCov) - 1),
      FUN = function(x, obsCov)
        if (any(!is.na(obsCov[x, (x + 1):ncol(obsCov)])))
          return((which(!is.na(obsCov[x, (x + 1):ncol(obsCov)])) + x)),
      obsCov = obsCov
    )

    # convert list into paired numbers:
    estimableCovariances = Reduce("rbind", lapply(
      X = 1:(nrow(obsCov) - 1),
      FUN = function(x, covEst) {
        temp = cbind(rep(x, length(covEst[[x]])), covEst[[x]])
      },
      covEst = covEst
    ))

    # make list of all estimable covariances
    estimableCovariances = cbind(colnames(obsCov)[estimableCovariances[, 1]], colnames(obsCov)[estimableCovariances[, 2]])

  }

  # if (model$options$parallel){
  #
  #   #make cluster
  #   model$specs$cl = parallel::makeCluster(model$specs$nCores, outfile="")
  #
  #   #set random seed
  #   parallel::clusterSetRNGStream(cl = model$specs$cl, iseed = seed)
  #
  #   #export packages
  #   tempResult = parallel::clusterEvalQ(cl = model$specs$cl, library(blatent))
  #   parallel::clusterExport(cl = model$specs$cl, varlist = c("model", "fullPosterior", "estimableCovariances"), envir = environment())
  #   chain = parallel::parLapply(cl = model$specs$cl, X = 1:nSamples, fun=singlePPC, model = model, fullPosterior = fullPosterior,
  #                               estimableCovariances = estimableCovariances, type = type)
  #   if (model$options$parallel) parallel::stopCluster(cl = model$specs$cl)
  # } else {
    # set.seed(seed)
    chain = lapply(X = 1:nSamples, FUN = singlePPMC, model = model, fullPosterior = fullPosterior, estimableCovariances = estimableCovariances, type = type)

  # }
  chain = Reduce(f = "rbind", x = chain)
  return(chain)

}
