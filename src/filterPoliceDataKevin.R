library(gdata)
library(dplyr)

#
# Explore the police data a bit
#

dataDir = '/home/sdal/projects/arl/arlington911/data/original/police/aug082016/'

dispatch = read.xls(sprintf("%scfs2015.xls", dataDir))
dispatch = filter(dispatch, Original_Call == "DOME")
reportNumbers = na.omit(dispatch$Report_No)
head(dispatch)

inper = read.xls(sprintf("%sinmast-inper62015.xls", dataDir))
inper = filter(inper, Report_No %in% reportNumbers)
inper
