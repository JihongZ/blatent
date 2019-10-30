
blatentModel <-
  R6::R6Class(
    classname = "blatentModel",
    public = list(
      chain = NULL,
      data = NULL,
      options = NULL,
      specs = NULL,
      parameterSummary = NULL,
      variables = NULL,
      initialize = function(data, specs, options, chain, variables){
        self$data = data
        self$specs = specs
        self$options = options
        self$chain = chain
        self$variables = variables
      },
      summary = function(numDigits = 3L, ...){
        # format for numeric values
        num.format  <- paste("%", max(8L, numDigits + 5L), ".", numDigits, "f", sep = "")
        char.format <- paste("%", max(8L, numDigits + 5L), "s", sep = "")

        cat("\nParameter Estimates:");
        if (self$options$nChains > 1){
          headings = paste0(sprintf(char.format, c("Mean", "SD", "LowHPDI", "UpHDPI", "PSRF")), collapse = "")
        } else if (self$options$nChains == 1){
          headings = paste0(sprintf(char.format, c("Mean", "SD", "LowHPDI", "UpHDPI", "HDPV")), collapse = "")
        }
        cat("\n")

        for (variable in self$specs$allVariables){
          cat(paste0(rep("-", 80), collapse = ""))
          cat("\n")

          preamble = paste0(variable, ":", collapse = "")
          buffer = paste0(rep(" ", 80-nchar(preamble) - nchar(headings)), collapse = "")

          cat(paste0(preamble, buffer, headings))
          cat("\n")
          for (param in self$variables[[variable]]$paramNames){

            csRow = which(rownames(self$parameterSummary) == param)

            preamble = paste0("  ", param, collapse = "")
            buffer = paste0(rep(" ", 80-nchar(preamble) - nchar(headings)), collapse = "")
            if (self$options$nChains > 1){
              paramVals = paste0(sprintf(num.format, self$parameterSummary[csRow, c("Mean", "SD", "lowerHDPI0.95", "upperHDPI950.95", "PSRF")]), collapse = "")
            } else if (self$options$nChains == 1){
              paramVals = paste0(sprintf(num.format, self$parameterSummary[csRow, c("Mean", "SD", "lowerHDPI0.95", "upperHDPI950.95", "Heidel.Diag p-value")]), collapse = "")
            }

            cat(paste0(preamble, buffer, paramVals, collapse = ""))
            cat("\n")
          }
        }

      }
    )
  )

# S3 dispatch functions:
summary.blatentModel <- function(object, ...){
  object$summary(...)

}

blatentSimulatedData <-
  R6::R6Class(
    classname = "blatentSimluatedData",
    public = list(
      data = NA,
      defaultSimulatedParameters = NA,
      details = NA,
      nObs = NA,
      simModel = NA,
      seed = NA,
      trueValues = NA,
      initialize = function(nObs, simModel, defaultSimulatedParameters, seed, details, data, trueValues){
        self$data = data
        self$defaultSimulatedParameters = defaultSimulatedParameters
        self$details = details
        self$nObs = nObs
        self$simModel = simModel
        self$seed = seed
        self$trueValues = trueValues
      },
      summary = function(){

      }
    )
  )


summary.blatentSimluatedData <- function(object, ...){
  object$summary()
}

# example pulled from Github:
#
# R6list <- R6::R6Class(
#   "R6list",
#   public = list(
#     orig = NULL,
#     initialize = function(x) {
#       self$orig <- x
#     },
#     as.list = function() {
#       self$orig
#     }
#   )
# )
#
# as.list.R6list <- function(x, ...) {
#   x$as.list()
# }
