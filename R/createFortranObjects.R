createFortranObjects = function(specs, options, data, variables, blankChain){
  # creates fortran specs object for writing to disk to transfer to Fortran estimation
  fspecs = c(
    specs$nUnits,
    specs$nObservedVariables,
    specs$nLatents,
    nrow(data),
    ncol(data),
    NA, #number of rows in model matrix
    NA, #number of cols in model matrix
    length(variables),
    length(specs$parameters),
    options$nChains,
    options$minTuneChains,
    options$maxTuneChains,
    options$nTuneIterations,
    options$nBurnin,
    options$nSampled,
    options$nThin,
    NA, #number of rows in priors,
    NA, #number of columns in priors
    NA, #number of rows in inits
    NA, #number of columns in inits
    NA, #number of rows in modelcolumns
    NA, #number of columns in modelcolumns
    NA, #number of rows in variable parameter chains
    NA,#number of columns in variable parameter chains
    NA, #number of rows in unit parameter chains
    NA,  #number of columns in unit parameter chains
    options$seed # initialization of seed for one chain (increment for additional chains)
  )


  # NEED: MODEL DATA WITH INTERCEPT
  fdata = cbind(matrix(data = 1, nrow = nrow(data), ncol = 1), data)
  names(fdata)[1] = "(Intercept)"

  # next: variables object
  fvariables = matrix(data = 0, nrow = length(variables), ncol = 5)
  var = 1
  for (var in 1:length(variables)){
    if (variables[[var]]$isLatent){
      fvariables[var,1] = 0
    } else {
      fvariables[var,1] = 1
    }

    fvariables[var,2] = which(names(fdata) == names(variables)[var])
    if (variables[[var]]$distributionSpecs$distribution == "bernoulli"){
      fvariables[var,3] = 1
    }
    if (variables[[var]]$distributionSpecs$link == "probit"){
      fvariables[var,4] = 1
    }

    if (!is.null(variables[[var]]$underlyingVariableName)){
      fvariables[var,5] = which(names(data) == variables[[var]]$underlyingVariableName)
    }
  }

  # NEED: PRIORS VECTOR
  fpriors = matrix(data = 0, nrow = 1, ncol = 5) # cols are variable #, parameter #, distribution, mean, variance
  fpriors[1,3] = 2 # normal prior
  fpriors[1,4] = options$defaultPriors$normalMean
  fpriors[1,5] = options$defaultPriors$normalVariance


  # NEED: Initial values matrix
  finit = matrix(data = 0, nrow = 1, ncol = 5) # cols are variable #, parameter #, distribution, mean, variance
  finit[1,3] = 2 # normal prior
  finit[1,4] = options$defaultInitializeParameters$normalMean
  finit[1,5] = options$defaultInitializeParameters$normalVariance

  # NEED: MODEL MATRIX
  fmodel = data.frame(matrix(data = 0, nrow = 1, ncol = ncol(fdata)))
  names(fmodel) = names(fdata)
  blankRow = fmodel

  modelColumns = list()
  # add intercept row first to get fmodel started
  fmodel$`(Intercept)`[1] = 1
  rownames(fmodel)[1] = "(Intercept)"
  var = 33
  for (var in 1:length(variables)){
    varName = names(variables)[var]
    varTerms = terms(variables[[var]]$formula)
    varIntercept = attr(x = varTerms, "intercept")
    varOrder = attr(x = varTerms, "order")
    varFactors = attr(x = varTerms, which = "factors")

    # intercept is present, so add column with intercept only
    if (varIntercept == 1){
      # check for intercept row
      if (!"(Intercept)" %in% rownames(fmodel)){
        fmodel = rbind(fmodel, matrix(data = 0, nrow = 1, ncol = ncol(fmodel)))
        fmodel[nrow(fmodel),"(Intercept)"] = 1
        rownames(fmodel)[nrow(fmodel)] = "(Intercept)"
        modelColumns[[varName]] = c(modelColumns[[varName]], which(rownames(fmodel) == "(Intercept)"))
      } else {
        modelColumns[[varName]] = c(modelColumns[[varName]], which(rownames(fmodel) == "(Intercept)"))
      }
    }

    if (length(varOrder)>0){
      term = 1
      for (term in 1:length(varOrder)){
        # check for model row with same name
        if (!colnames(varFactors)[term] %in% rownames(fmodel)){
          fmodel = rbind(fmodel, blankRow)

          # loop through all 1s in factor and put into fmodel
          fmodel[nrow(fmodel), names(which(varFactors[,term] == 1))] = 1
          rownames(fmodel)[nrow(fmodel)] = colnames(varFactors)[term]
          modelColumns[[varName]] = c(modelColumns[[varName]], which(rownames(fmodel) == colnames(varFactors)[term]))
        } else {
          modelColumns[[varName]] = c(modelColumns[[varName]], which(rownames(fmodel) == colnames(varFactors)[term]))
        }
      }
    }



  }

  # next, create a matrix that has the number and specific model rows in fmodel for each variable
  fmodelrows = matrix(data = 0, nrow = length(variables), ncol = max(unlist(lapply(X = modelColumns, FUN = length))) + 1)
  for (var in 1:length(variables)){
    fmodelrows[var, 1] = length(modelColumns[[var]])
    for (i in 1:length(modelColumns[[var]])){
      fmodelrows[var, i+1] = modelColumns[[var]][i]
    }
  }


  fspecs[6] = nrow(fmodel)
  fspecs[7] = ncol(fmodel)

  fspecs[17] = nrow(fpriors)
  fspecs[18] = ncol(fpriors)
  fspecs[19] = nrow(finit)
  fspecs[20] = ncol(finit)
  fspecs[21] = nrow(fmodelrows)
  fspecs[22] = ncol(fmodelrows)


  fvchain = NULL
  fuchain = NULL

  for (i in 1:options$nChains){
    fvchain = rbind(fvchain, blankChain$variables)
    fuchain = rbind(fuchain, blankChain$units)
  }

  fspecs[23] = nrow(blankChain$variables)
  fspecs[24] = ncol(blankChain$variables)
  fspecs[25] = nrow(blankChain$units)
  fspecs[26] = ncol(blankChain$units)

  return(
    list(
      fspecs = fspecs,
      fvariables = fvariables,
      fpriors = fpriors,
      fdata = fdata,
      finit = finit,
      fmodel = fmodel,
      fmodelrows = fmodelrows
    )
  )

}
