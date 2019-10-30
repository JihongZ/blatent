#' @title Blatent estimation specifications
#'
#' @description Creates control specifics for estimation options for estimating Bayesian latent variable models.
#'
#' @param defaultPriors Sets priors for all parameters that are not specified in priorsList of
#'                      \code{\link{blatentEstimate}}. Defaults to list set by
#'                      \code{\link{setDefaultPriors}} function. Values in list currently allowed are
#'                      \itemize{
#'                         \item \code{normalMean} for the mean of a normal distribution (defaults to \code{0}).
#'                         \item \code{normalVariance} for the variance of a normal distribution (defaults to \code{1000}).
#'                         \item \code{normalCovariance} for the covariance of a multivariate normal distribution
#'                               (defaults to \code{0}).
#'                      }
#'
#' @param defaultInitializeParameters List of values that sets distributions used to initialize
#'                      parameters. Defaults to list set by \code{\link{setDefaultInitializeParameters}}
#'                      function. Values in list currently allowed are:
#'                      \itemize{
#'                         \item \code{normalMean} for the mean of a normal distribution (defaults to \code{0}).
#'                         \item \code{normalVariance} for the variance of a normal distribution (defaults to \code{1}).
#'                         \item \code{normalCovariance} for the covariance of a multivariate normal distribution
#'                               (defaults to \code{0}).
#'                      }
#' @param defaultPosteriorPredictiveChecks List of logical values for which built-in posterior predictive model
#'                                         checks should be run. Defaults to list set by
#'                                         \code{\link{setDefaultPosteriorPredictiveChecks}} function.
#'                                         Values in list currently allowed are:
#'                      \itemize{
#'                         \item \code{bivariate} for bivariate Chi-squares.
#'                         \item \code{tetrachoric} for bivariate tetrachoric correlations.
#'                      }
#'
#' @param estimator Sets the estimation algorithm to be used. Currently, only \code{"GPDCM"} (Gibbs Probit
#'                  Diagnostic Classification Model) is allowed, which is the default value.
#'
#' @param estimatorLocation Sets the path to the location of estimator executable, if \code{estimatorType} is
#'                          \code{"external"}. Currently set to only location available.
#'
#' @param estimatorType Sets location of estimator. Currently, only \code{"external"} (for estimation routines
#'                      external to R) is allowed, which is the default value.
#'
#'
#' @param fileSaveLocation Sets the path for output files used for exteneral estimation routines.
#'                         Only used when \code{estimatorType = "external"}.
#'
#' @param maxTuneChains Sets the maximum number of tuning chains for MCMC sampling algorithm, if needed. Currently,
#'                      no Metropolis steps exist in algorithm, so is unused. Defaults to \code{0}.
#'
#' @param minTuneChains Sets the minimum number of tuning chains for MCMC sampling algorithm, if needed.
#'                      Currently, no Metropolis steps exist in algorithm, so is unused. Defaults to \code{0}.
#'
#' @param nBurnin Sets the number of burnin iterations. Defaults to \code{1000}.
#'
#' @param nChains Sets the number of independent Markov chains run by the program. Defaults to \code{4}.
#'
#' @param nCores Sets the number of cores used in parallel processing if option \code{parallel} is
#'               \code{TRUE}. Defaults to \code{-1}. Values are semi-indicative of how many
#'               processors will be used:
#'               \itemize{
#'                  \item \code{-1} indicates that all but one available processor will be used.
#'                  \item \code{0} indicates that all available processors will be used.
#'                  \item \code{>0} indicates that specific number of processors will be used, if available.
#'               }
#'               Note: currently, parallel processing is unavailable, so this is unused.
#'
#' @param nSampled Sets the number of posterior draws to sample, per chain. Defaults to \code{1000}.
#'
#' @param nThin Sets the thinning interval, saving only the posterior draws that comes at this value.
#'              Defaults to \code{5}.
#'
#' @param nTuneIterations Sets the number of iterations per tuning chain, if needed. Currently,
#'                        no Metropolis steps exist in algorithm, so is unused. Defaults to \code{0}.
#'
#' @param parallel If \code{TRUE}, enables parallel processing of estimation and PPCM analyses.
#'                 Currently, parallel processing is unavailable, so this is unused. Defaults to \code{FALSE}.
#'
#' @param seed Sets the random number seed for the analysis. Defaults to \code{NULL}, which does not
#'             set the seed and uses current session value per each analysis.
#'
#' @return A list of values containing named entries for all arguments shown above.
#'
#' @export
blatentControl <-
  function(defaultPriors = setDefaultPriors(),
           defaultInitializeParameters = setDefaultInitializeParameters(),
           defaultPosteriorPredictiveChecks = setDefaultPosteriorPredictiveChecks(),
           estimator = "GPDCM",
           estimatorType = "external",
           estimatorLocation = "/Users/jonathantemplin/Documents/repos/blatent-main/gpdcm/",
           fileSaveLocation = paste0(getwd(), "/"),
           maxTuneChains = 0,
           minTuneChains = 0,
           nBurnin = 1000,
           nChains = 4,
           nCores = -1,
           nSampled = 1000,
           nThin = 5,
           nTuneIterations = 0,
           parallel = FALSE,
           seed = NULL) {

    return(
      list(
        defaultInitializeParameters = defaultInitializeParameters,
        defaultPriors = defaultPriors,
        estimator = estimator,
        estimatorType = estimatorType,
        estimatorLocation = estimatorLocation,
        fileSaveLocation = fileSaveLocation,
        maxTuneChains = maxTuneChains,
        minTuneChains = minTuneChains,
        nBurnin = nBurnin,
        nChains = nChains,
        nCores = nCores,
        nSampled = nSampled,
        nThin = nThin,
        nTuneIterations = nTuneIterations,
        parallel = parallel,
        posteriorPredictiveChecks = defaultPosteriorPredictiveChecks,
        seed = seed
      )
    )
  }
