library(readr)
library(data.table)

count_missing <- function(col) {
    data_type <- class(col)
    print(data_type)
    if (is.numeric(col)) {
        print(is.numeric(col))
        return(sum(is.na(col)))
    } else if (is.character(col)) {
        return(table(col, useNA = 'always'))
    } else {
        warning('Non numeric or character type found. Returning NA')
        return(NA)
    }
}

pf_data_list <- list()

## PUMS ------------------------------------------------------------------------

PUMS = fread("./data/mitre/original/synthpop_data/ss14hva.csv")
PUMS = PUMS[PUMA10 %in% c(1301, 1302) & VALP < 1500000 & HINCP >= 0, .(PUMA10, HINCP, VALP, TAXP, RMSP)]

pf_pums<- lapply(PUMS, count_missing)

pf_pums <- tidyr::gather(data.frame(pf_pums), key = 'column_variable', value = 'num_missing')
pf_pums

pf_data_list$pums <- pf_pums



## marginal income -------------------------------------------------------------

marginalIncome = fread("./data/mitre/original/marginData/arlMarginalIncomeBG.csv", skip = 1)[,c(2,seq(6,37,by=2))]

marginalIncome$Id2 = as.numeric(substr(marginalIncome$Id2,6,12))

mi_sub <- marginalIncome[, 2:17]
head(mi_sub)

pf_marginal_income <- lapply(mi_sub, count_missing)

pf_marginal_income <- tidyr::gather(data.frame(pf_marginal_income), key = 'column_variable', value = 'num_missing')
pf_marginal_income

pf_data_list$marginal_income <- pf_marginal_income

## Group by 25k
marginalIncome = data.table(BlockGroup = marginalIncome$Id2,
                            '0to25' = rowSums(marginalIncome[,2:5]),
                            '25to50' = rowSums(marginalIncome[,6:10]),
                            '50to75' = rowSums(marginalIncome[,11:12]),
                            '75to100' = rowSums(marginalIncome[,13]),
                            '100to125' = rowSums(marginalIncome[,14]),
                            '125to150' = rowSums(marginalIncome[,15]),
                            '150to200' = rowSums(marginalIncome[,16]),
                            '200plus' = rowSums(marginalIncome[,17]))
head(marginalIncome)
lapply(marginalIncome, class)

## marginal room count ---------------------------------------------------------

marginalRoomCount = fread("./data/mitre/original/marginData/ACS_NO_ROOMS.csv")
marginalRoomCount$Id2 = as.numeric(substr(marginalRoomCount$Id2,6,12))

mrc_sub <-marginalRoomCount[, c(5:13)]
pf_mrc <- lapply(mrc_sub, count_missing)
pf_mrc <- tidyr::gather(data.frame(pf_mrc), key = 'column_variable', value = 'num_missing')
head(pf_mrc)

pf_data_list$marginal_room_count <- pf_mrc

marginalRoomCount = marginalRoomCount[,.(BlockGroup = marginalRoomCount$Id2,
                                         oneRoom = `Estimate; Total: - 1 room`,
                                         twoRooms = `Estimate; Total: - 2 rooms`,
                                         threeRooms = `Estimate; Total: - 3 rooms`,
                                         fourRooms = `Estimate; Total: - 4 rooms`,
                                         fiveRooms = `Estimate; Total: - 5 rooms`,
                                         sixRooms = `Estimate; Total: - 6 rooms`,
                                         sevenRooms = `Estimate; Total: - 7 rooms`,
                                         eightRooms = `Estimate; Total: - 8 rooms`,
                                         ninePlusRooms = `Estimate; Total: - 9 or more rooms`)]

## arlington core logic --------------------------------------------------------

CLdata = fread("./data/mitre/original/synthpop_data/Arlington_CL_2013_Data.csv")

CLdata = CLdata[!is.na(TOTAL.VALUE.CALCULATED) & !is.na(TAX.AMOUNT) & !is.na(BlockGroup_rec),
                .(BlockGroup=BlockGroup_rec, TOTAL.VALUE.CALCULATED,  TAX.AMOUNT, LATITUDE, LONGITUDE, UNITS.NUMBER, PropertyType)]

## pf_cl$UNITS.NUMBER / nrow(CLdata)
## [1] 0.6088685
## 61% missing
cl_sub <- CLdata[, c("TOTAL.VALUE.CALCULATED", "TAX.AMOUNT",
                     "LATITUDE", "LONGITUDE", "UNITS.NUMBER"), drop=FALSE]

pf_cl <- lapply(cl_sub, count_missing)
pf_cl <- tidyr::gather(data.frame(pf_cl), key = 'column_variable', value = 'num_missing')

pf_cl$num_obs <- nrow(cl_sub)
pf_cl$pct_missing <- pf_cl$num_missing / pf_cl$num_obs
pf_data_list$core_logic <- pf_cl

# Set all missing unit numbers to 1 except for multifamily households
CLdata$UNITS.NUMBER[CLdata$PropertyType != "Multifamily" & is.na(CLdata$UNITS.NUMBER)] = 1
CLdata = na.omit(unique(CLdata[,PropertyType := NULL]))
setkey(CLdata, BlockGroup)

## ATRACK ----------------------------------------------------------------------

rentalData = fread("./data/mitre/working/PoliceData/CoreLogic_ATRACK_joined.csv")
rentalData = rentalData[,.(BlockGroup=BlockGroup_rec, TOTAL.VALUE.CALCULATED,  TAX.AMOUNT, LATITUDE, LONGITUDE, UNITS.NUMBER = units)]

rd_sub <- rentalData[, c("TOTAL.VALUE.CALCULATED", "TAX.AMOUNT",
                         "LATITUDE", "LONGITUDE", "UNITS.NUMBER")]
head(rd_sub)

pf_rd <- lapply(rd_sub, count_missing)
pf_rd <- tidyr::gather(data.frame(pf_rd), key = 'column_variable', value = 'num_missing')

library(magrittr)
library(scales)

pf_rd$num_obs <- nrow(rd_sub)
pf_rd$pct_missing <- percent(pf_rd$num_missing / pf_rd$num_obs)
pf_rd

pf_data_list$atrack_rental_data <- pf_rd
