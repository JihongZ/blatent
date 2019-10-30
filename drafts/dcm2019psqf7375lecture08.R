# blatent installation ---------------------------------------------------------------------
# use the devtools package so you can download blatent package from my github repository
if (!require(devtools)) install.packages("devtools")

# install the blatent package; force=TRUE makes sure you have the most current version
#install_github("jonathantemplin/blatent", force = TRUE)

# GPDCM installation -----------------------------------------------------------------------
# download the GPDCM program from my website
#system("wget https://jonathantemplin.com/GPDCMRunning")

# set the linux permissions for the GPDCM program to be able to execute the program
system(paste0("chmod a+x ",getwd(),"/GPDCMRunning"))
system(paste0("chmod a+w+x ",getwd()))


# initial example using simulated data -----------------------------------------------------

# use the blatent library
library(blatent)

# create the model to be used using the syntax for blatent
modelText = "

# Measurement Model

item1-item10 ~ A1
item11-item20 ~ A2
item21-item30 ~ A3

A1 ~ 1
A2 ~ A1
A3 ~ A1 + A2 + A1:A2

# Latent Variable Specifications:
A1 A2 A3 <- latent(unit = 'rows', distribution = 'bernoulli', structure = 'univariate', type = 'ordinal')

# Observed Variable Specifications:
item1-item30 <- observed(distribution = 'bernoulli', link = 'probit')
"

# create the data for the example
simulatedData = blatentSimulate(modelText = modelText, nObs = 1000,
                                defaultSimulatedParameters = setDefaultSimulatedParameters(
                                  observedIntercepts = "runif(n = 1, min = -2, max = 0)",
                                  observedMainEffects = "runif(n = 1, min = 1, max = 2)",
                                  observedInteractions = "runif(n = 1, min = 0, max = .5)",
                                  latentIntercepts = "runif(n = 1, min = 0, max = 0)",
                                  latentMainEffects  = "runif(n = 1, min = 0, max = 0)",
                                  latentInteractions = "runif(n = 1, min = 0, max = 0)"
                                ), seed = 110)

# estimate the model

model01 = blatentEstimate(
  dataMat = simulatedData$data,
  modelText = modelText,
  options = blatentControl(
    seed = 10172019,
    nChains = 4,
    nBurnin = 100,
    nSampled = 100,
    nThin = 1,
    estimatorLocation = paste0(getwd(),"/")
  )
)

# show the parameter summary from the analysis
model01$summary()

# do PPMC to check model fit

load("dcm2019psqf7375lecture08.RData")
# checking with covariances
model01PPMC = blatentPPMC(model = model01, nSamples = 10)
hist(model01PPMC$item1_item2)
cov(simulatedData$data$item1, simulatedData$data$item2)

# checking with bivariate fit
hist(model01PPMC$item1_item2_bivariate)
