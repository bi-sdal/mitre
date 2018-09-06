# getting housing unit count by block group for Arlington.
# This will likely be needed to adjust the total number of housing units
# given in the current estimate from corelogic and ATRAC data.

# housing units
acs_housing <- read_csv("data/mitre/original/acs/housing/ACS_16_5YR_B25001_with_ann.csv",
                        col_types = cols(Id = col_character(),Id2 = col_character()), skip = 1)

# population
acs_population <- read_csv("data/mitre/original/acs/population/ACS_16_5YR_B01003_with_ann.csv", 
                        col_types = cols(Id = col_character(), Id2 = col_character()), skip = 1)

# a plot
x <- acs_housing[[4]]
y <- acs_population[[4]]
par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(4,4,1,1))
plot(x,y)
blockGroup <- substring(acs_housing[[2]],6,12)
housingACSbg <- data.frame(blockGroup=blockGroup,nunit=x,pop=y)

 # compare to other estimates from corelogic - use datHouseBG created in logisticByBlockGroup.R
datHouseBG %>% group_by(blockGroup) %>% summarize(nHouseCL=n()) -> tmp
tmp$blockGroup <- as.character(tmp$blockGroup)
housingACSbg$blockGroup <- as.character(housingACSbg$blockGroup)
housingACSbg %>% left_join(tmp,by=c("blockGroup"="blockGroup")) -> housingACSbg2
 # look at the sums
c(sum(housingACSbg2$nunit,na.rm=T),sum(housingACSbg2$nHouseCL,na.rm=T))
 # plot the difference
plot(housingACSbg2$nHouseCL,housingACSbg2$nunit)

 # write out to a file...
write.csv(housingACSbg2,'./data/mitre/working/MiscData/housingACSbg.csv',row.names=FALSE,quote=FALSE)
# try reading the file to make sure it wrote out ok
#tmp <- read.csv('./data/mitre/working/MiscData/housingACSbg.csv',stringsAsFactors = FALSE)
