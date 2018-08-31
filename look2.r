# some code to read in the clean police data from Gizem's project
library(dplyr)
library(lubridate)
# find things...
system('ls ../../sdal/projects/arl/arlington911/data/final/police')
# previously 
p1 <- read.csv('../../sdal/projects/arl/arlington911/data/final/police/inmast_clean.csv')
c1 <- read.csv('../../sdal/projects/arl/arlington911/data/final/police/cfs_clean.csv')

# read in the inmast files to get an idea of how gettable victim and other info is by call
cfs3 <- read.csv('../../sdal/projects/arl/arlington911/data/working/police/aug082016/cfs2013.xlsx.csv')
dis3 <- read.csv('../../sdal/projects/arl/arlington911/data/working/police/aug082016/cfs-dispatched2013.xlsx.csv')
per3 <- read.csv('../../sdal/projects/arl/arlington911/data/working/police/aug082016/inmast-inper62013.xlsx.csv')
inm3 <- read.csv('../../sdal/projects/arl/arlington911/data/working/police/aug082016/inmast2013.xlsx.csv')
#inmast2013.xlsx.csv
#inmast-inibro2013.xlsx.csv
#inmast-inpar72013.xlsx.csv
#inmast-inper62013.xlsx.csv

# keep the DOME calls and see how many have a victim
cfs3 %>% filter(Original_Call=='DOME') -> cfs3D

per3 %>% filter(Report_No %in% cfs3D$Report_No) -> tmp
table(tmp$Involvement)

# 2057 DOME calls; 433 result in a report (all with a victim), 217 ARR, 187 SUS, 
