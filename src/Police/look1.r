# some code to read in the clean police data from Gizem's project
library(dplyr)
library(lubridate)
 # find things...
system('ls ../../sdal/projects/arl/arlington911/data/final/police')
 # previously 
p1 <- read.csv('../../sdal/projects/arl/arlington911/data/final/police/inmast_clean.csv')
c1 <- read.csv('../../sdal/projects/arl/arlington911/data/final/police/cfs_clean.csv')

 # codes that are domestic violence - from Gizem's slides

DomVi <- c('0901','0902',  # kill Family w/Gun 0901 or without 0902
           '1301',         # agg assault family gun
           '1302A',        # agg assault family knife
           '1302B',        # agg assault family other
           '1303',         # agg assault family hands, fist, feet
           '1313A',        # simp assault family
           '3802',         # cruelty towards child
           '3803',         # cruelty towards wife
           '3899',         # other family offense
           '9820',         # domestic disturbance
           '11D')          # FORCIBLE FONDLING (CHILD)

# 2013-2015 data > working > police > AC Police Data > 2013-2015
# seems to have arrests and charges
# Offenses (multiple) for a single arrest.
# Offense from 2015Apr07_2015Apr15  Offense 11D Forcible Fondling (child)
c1 %>% filter(Rep_Dist %in% DomVi) -> tmp
p1 %>% filter(Rep_Dist %in% DomVi) -> tmp
 # DOME calls
table(c1$Original_Call)
 # where they are located
c1 %>% filter(Original_Call=='DOME') %>% group_by(Location) %>% summarize(n=n()) %>%
  arrange(desc(n)) -> cDOMEloc
 # get the date format in c1
cdates <- as.Date(c1$Received_Date_Time) 
c1 %>% mutate(Received_Date_Time=as.POSIXct(Received_Date_Time)) -> tmp
 # number of calls by hour of day
tmp %>% filter(Original_Call=='DOME') %>%
  mutate(hour=hour(Received_Date_Time),day=day(Received_Date_Time)) %>%
  group_by(hour,day) %>% summarize(n=n()) -> tmp2
 # DOME calls by hour
plot(tmp2$hour,tmp2$n)

# figure out what are the incidents that are spawned by DOME calls
c1 %>% filter(Original_Call=='DOME') %>% 
  left_join(p1[,c('Report_No','Rep_Dist')],by='Report_No') -> tmp


## separate out DOME CFS calls for 2013 (our trial year)
library(ggplot2)
library(ggmap)
c1 %>% filter(Original_Call=='DOME',year(Received_Date_Time)==2013) -> cfsDOME2013
locDOME2013 <- cfsDOME2013$Location[1:30]
locDOME2013 <- paste(locDOME2013,'Arlington, VA')
nloc <- length(locDOME2013)
llmat <- matrix(NA,nrow=nloc,ncol=2)
for(k in 1:nloc){
  llmat[k,] <- (geocode(locDOME2013[k],output = "latlona", source = "google"))[1:2]
}
tmp <- geocode(locDOME2013,output = "latlona", source = "google")




