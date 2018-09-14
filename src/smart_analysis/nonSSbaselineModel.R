library(data.table)


incRoomArl = fread("./data/mitre/working/incomeRoomsArlBg.csv")
dome = fread("./data/mitre/working/PoliceData/drugBGrate.csv")
housingACSbg <- read.csv('./data/mitre/working/MiscData/housingACSbg.csv',stringsAsFactors = FALSE)

yDat <- readRDS(here('data/mitre/final/logistic_regressions/fit_notSmart.RDS'))[[1]]$data


dat = merge(incRoomArl, dome, by.x = "blockGroup", by.y = "BlockGroup")[,nunit:= NULL]
dat = merge(dat, housingACSbg)
dat = merge(dat, yDat[,c("blockGroup", "cases")])

dat = dat[dat$nunit > 20 & blockGroup != 1017013,]
dat$rate = dat$rate*dat$n / dat$nunit

summary(glm(cbind(cases, nunit - cases) ~ (medianRooms) + (medianIncome) + rate, data = dat, family = binomial))
sapply(dat, summary, na.rm = TRUE)
