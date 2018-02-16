# This is Ian rewriting Josh's code so that it makes sense to me.

library(dplyr)
library(mice)
library(data.table)
source("./src/synthpop_code/generalizedSynthPop/synthpopFunctions.R")

# Data elements. ACS_marginals is loaded in but doesn't seem to be used
# CLdata is corelogic housing data

income_marginal <- read.csv("./data/mitre/working/cleanedExampleData/income_marginal.csv")
CL_PUMS = read.csv("./data/mitre/working/cleanedExampleData/corelogicAndPUMS.csv", stringsAsFactors = FALSE)

# transform has to be a function name. Prob will remake this to the X1 X2 paradigm.

imputed_draws = imputeWithMICE(CL_PUMS, "HINCP", c("VALP", "TAXP2"), transform = sqrt, outName = "sqrtHINCP", imputations = 1000)

###
### END IMPUTATION STEP. NEXT FIND MARGINALS.
###

# The imputation variable is continuous, so it must be binned according to breaks. 

breaks <- c(-1,25000,50000,75000,100000,125000,150000,200000)
expCutoff = 2e5

# next, fit exponential tail for values > 200,000 using the MLE for an exponential distribution, 1/x-bar
CL_PUMS %>% filter(source == "PUMS" & HINCP > expCutoff) %>% dplyr::select(HINCP) -> highInc

mle_lambda = 1/(mean(unlist(highInc) - expCutoff))

marginDist = makeMarginalDensity(income_marginal, breaks = breaks, expParm = mle_lambda, expCutoff = expCutoff)
