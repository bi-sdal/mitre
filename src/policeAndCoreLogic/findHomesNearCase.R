library(data.table)
library(dplyr)
library(ggplot2)

# Load data
# Police data
# Columns: Rep_Dist is a code describing what happened; Disposition???; Priority???; 
load("./data/mitre/working/PoliceData/cfsDOME2013.RData")
policeData = data.table(cfsDOME2013[,-1])
rm(cfsDOME2013)
# CL Data
CLdata = fread("./data/mitre/original/synthpop_data/Arlington_CL_2013_Data.csv")[,-1]


cbind(colnames(policeData), missingProp = sapply(1:ncol(policeData), function(x) mean(is.na(policeData[,x,with = FALSE]))))
