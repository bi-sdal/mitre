library(data.table)
library(tidyr)
library(dplyr)

#R.utils::sourceDirectory('R')
source('R/load_input_data.R')
source('R/profile.R')
ls()

pf_data_list <- list()

all_data <- load_all()

## PUMS ------------------------------------------------------------------------

pf_pums <- count_missing_tidy(all_data$pums)
pf_data_list$pums <- pf_pums

## marginal income -------------------------------------------------------------

mi_sub <- dplyr::select(all_data$acs_marginalIncome, -1)
pf_marginal_income <- count_missing_tidy(mi_sub)
pf_data_list$marginal_income <- pf_marginal_income

## marginal room count ---------------------------------------------------------

all_data$acs_marginalRoomCount

mrc_sub <- all_data$acs_marginalRoomCount[, c(5:13)]
pf_mrc <- count_missing_tidy(mrc_sub)
pf_data_list$marginal_room_count <- pf_mrc


## arlington core logic --------------------------------------------------------

cl_sub <- all_data$coreLogic[,
                             c("TOTAL.VALUE.CALCULATED", "TAX.AMOUNT",
                               "LATITUDE", "LONGITUDE", "UNITS.NUMBER"),
                             drop=FALSE]
pf_cl <- count_missing_tidy(cl_sub)
pf_data_list$core_logic <- pf_cl

cl_recode_sub <- all_data$coreLogic_recode[,
                             c("TOTAL.VALUE.CALCULATED", "TAX.AMOUNT",
                               "LATITUDE", "LONGITUDE", "UNITS.NUMBER"),
                             drop=FALSE]
pf_cl_recode <- count_missing_tidy(cl_recode_sub)
pf_data_list$core_logic_recode <- pf_cl_recode

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
