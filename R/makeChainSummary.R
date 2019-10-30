makeChainSummary = function(chain, HPDIval = .95){
  if (class(chain) == "mcmc.list"){
    stackedChain = coda::mcmc(do.call("rbind", lapply(
      X = chain,
      FUN = function(x)
        return(as.matrix(x))
    )))
    nChains = length(chain)
  } else {
    nChains = 1
  }


  # build massive matrix of parameters by statistics
  chainSummary = summary(chain)
  chainSummary = cbind(chainSummary$statistics, chainSummary$quantiles)

  # add HDPIs:
  HDPI = coda::HPDinterval(coda::mcmc(stackedChain), prob = HPDIval)
  colnames(HDPI) = c(paste0("lowerHDPI", HPDIval), paste0("upperHDPI95", HPDIval))
  chainSummary = cbind(chainSummary, HDPI)

  if (nChains > 1){
    convergenceDiagnostics = coda::gelman.diag(chain, multivariate = FALSE)
    colnames(convergenceDiagnostics$psrf) = c("PSRF", "PSRF Upper C.I.")
    chainSummary = cbind(chainSummary, convergenceDiagnostics$psrf)
  } else {
    convergenceDiagnostics = coda::heidel.diag(chain)
    temp = convergenceDiagnostics[[1]][1:nrow(convergenceDiagnostics[[1]]), c(3,4)]
    colnames(temp) = c("Heidel.Diag p-value", "Heidel.Diag Htest")
    chainSummary = cbind(chainSummary, temp)
  }

  return(chainSummary)
}
