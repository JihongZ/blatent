singlePPMC = function(num, model, fullPosterior, estimableCovariances, type){
  print(num)
  # draw sample at random from posterior
  postDraw = sample(size = 1, x = 1:nrow(fullPosterior$variables))

  # move posterior values to variables
  model$variables = lapply(X = model$variables, FUN = function(x, fullPosterior, postDraw){
    x$beta = t(t(fullPosterior$variables[postDraw, which(colnames(fullPosterior$variables) %in% x$paramNames)][x$paramNames]))
    return(x)
  }, fullPosterior = fullPosterior, postDraw = postDraw)

  # generate latent variable values
  model$data$classnumber_ = NA

  var = 1
  for (var in 1:length(model$specs$latentOrder)){
    model$data[model$specs$latentOrder[var]] =
      generateDataFromModel(variables = model$variables[model$specs$latentOrder[var]], data = model$data)
  }

  # move posterior values to data
  # model$data$classnumber_ = t(fullPosterior$units[postDraw,])
  # if (model$specs$nCategoricalLatents > 1){
  #   model$data[model$specs$latentOrder] =
  #     vapply(
  #       X = colnames(fullPosterior$units),
  #       FUN = function(x, fullPosterior, postDraw, nAttributes) {
  #         return(dec2bin(
  #           fullPosterior$units[postDraw, which(colnames(fullPosterior$units) == x)],
  #           nattributes = nAttributes,
  #           basevector = rep(2, nAttributes)
  #         ))
  #       },
  #       fullPosterior = fullPosterior,
  #       postDraw = postDraw,
  #       nAttributes = model$specs$nCategoricalLatents,
  #       FUN.VALUE = numeric(model$specs$nCategoricalLatents)
  #     )
  # } else {
  #   model$data[model$specs$latentOrder] = t(fullPosterior$units[postDraw, ]) - 1
  # }

  simData = generateDataFromModel(variables = model$variables[model$specs$observedVariables], data = model$data)

  ppcdata = NULL

  proportion = NULL
  if ("mean" %in% type) {
    proportion =
      apply(
        X = simData,
        MARGIN = 2,
        FUN = mean,
        na.rm = TRUE
      )
    proportion = round(proportion, digits = 4)
  }
  ppcdata = c(ppcdata, proportion)

  univariate = NULL
  if ("univariate" %in% type) {

    result = unlist(lapply(
      X = model$specs$observedVariables,
      FUN = function(x)
        return(
          itemChiSquare(observed = model$data[,x], observedLevels = list(c(0,1)),
                        expected = simData[,x], expectedLevels = list(c(0,1)), minObs = 0, minExp = 0)
        )
    ))

    univariate = c(result, sum(result))
    names(univariate) = c(paste0(colnames(simData),"_univariate"), "totalUnivariate")
    univariate = round(univariate, digits = 4)
  }
  ppcdata = c(ppcdata, univariate)

  covariance = NULL
  if ("covariance" %in% type) {
    covEst = cov(simData, use = "pairwise.complete.obs")
    covariance = mapply(
      FUN = function(x, y, covEst)
        return(covEst[x, y]),
      estimableCovariances[, 1],
      estimableCovariances[, 2],
      MoreArgs = list(covEst = covEst)
    )
    names(covariance) = paste0(estimableCovariances[,1], "_", estimableCovariances[,2])
    covariance = round(covariance, digits = 4)
  }
  ppcdata = c(ppcdata, covariance)

  bivariate = NULL
  if ("bivariate" %in% type) {
    bivariate = mapply(
      FUN = function(x, y, data, simData){
        # return(c(x,y))
        return(itemChiSquare(observed = data[c(x,y)], observedLevels = list(c(0,1), c(0,1)),
                             expected = simData[,c(x,y)], expectedLevels = list(c(0,1), c(0,1)), minObs = 0, minExp = 0))
      }
      ,
      estimableCovariances[, 1],
      estimableCovariances[, 2],
      MoreArgs = list(data = model$data, simData = simData)
    )
    bivariate = c(bivariate, sum(bivariate))
    names(bivariate) = c(paste0(estimableCovariances[,1], "_", estimableCovariances[,2], "_bivariate"), "totalBivariate")

  }
  ppcdata = c(ppcdata, bivariate)

  return(data.frame(t(ppcdata)))
}
