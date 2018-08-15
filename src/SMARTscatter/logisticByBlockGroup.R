library(dplyr)
library(data.table)
library(stringr)

## Get blockgroup list from file

bgs = list.files("./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP/") %>%
  str_extract("\\d{7}") %>%
  na.omit %>%
  as.numeric
  
resamples = rbindlist(
  lapply(bgs, function(bg) {
    out = fread(sprintf("./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP/bg_%s/resamples.csv", bg))
    out = data.table(out, blockGroup = bg)
    return(out)
  })
)
assignments = fread("./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP/caseAssignments.csv")
