library(data.table)

load_pums <- function(path = "./data/mitre/original/synthpop_data/ss14hva.csv") {
    PUMS <- fread(path)
    PUMS <- PUMS[PUMA10 %in% c(1301, 1302) & VALP < 1500000 & HINCP >= 0, .(PUMA10, HINCP, VALP, TAXP, RMSP)]
    return(PUMS)
}

load_acs_marginalIncome <- function(path = "./data/mitre/original/marginData/arlMarginalIncomeBG.csv",
                                    skip = 1,
                                    cols = c(2, seq(6, 37, by=2)),
                                    id_substr_1 = 6,
                                    id_substr_2 = 12) {
    marginalIncome <- fread("./data/mitre/original/marginData/arlMarginalIncomeBG.csv", skip = 1)[,c(2,seq(6,37,by=2))]
    marginalIncome$Id2 = as.numeric(substr(marginalIncome$Id2,
                                           id_substr_1, id_substr_2))
    return(marginalIncome)
}

load_acs_marginalRoomCount <- function(path = "./data/mitre/original/marginData/ACS_NO_ROOMS.csv",
                                       id_substr_1 = 6,
                                       id_substr_2 = 12) {
    marginalRoomCount = fread(path)
    marginalRoomCount$Id2 = as.numeric(substr(marginalRoomCount$Id2,
                                              id_substr_1,
                                              id_substr_2))

    marginalRoomCount = marginalRoomCount[,
                                          .(BlockGroup = marginalRoomCount$Id2,
                                            oneRoom = `Estimate; Total: - 1 room`,
                                            twoRooms = `Estimate; Total: - 2 rooms`,
                                            threeRooms = `Estimate; Total: - 3 rooms`,
                                            fourRooms = `Estimate; Total: - 4 rooms`,
                                            fiveRooms = `Estimate; Total: - 5 rooms`,
                                            sixRooms = `Estimate; Total: - 6 rooms`,
                                            sevenRooms = `Estimate; Total: - 7 rooms`,
                                            eightRooms = `Estimate; Total: - 8 rooms`,
                                            ninePlusRooms = `Estimate; Total: - 9 or more rooms`)]
    return(marginalRoomCount)
}

load_coreLogic <- function(path = "./data/mitre/original/synthpop_data/Arlington_CL_2013_Data.csv") {
    CLdata = fread(path)
    CLdata = CLdata[!is.na(TOTAL.VALUE.CALCULATED) & !is.na(TAX.AMOUNT) & !is.na(BlockGroup_rec),
                    .(BlockGroup=BlockGroup_rec, TOTAL.VALUE.CALCULATED,  TAX.AMOUNT, LATITUDE, LONGITUDE, UNITS.NUMBER, PropertyType)]
    return(CLdata)
}

load_coreLogic_recode <- function() {
    coreLogic_recode <- load_coreLogic()
    # print(names(coreLogic_recode))
    # print(head(coreLogic_recode))
    coreLogic_recode$UNITS.NUMBER[coreLogic_recode$PropertyType != "Multifamily" & is.na(coreLogic_recode$UNITS.NUMBER)] <- 1
    # print('here')
    # print(head(coreLogic_recode))
    coreLogic_recode <- na.omit(unique(coreLogic_recode[, PropertyType := NULL]))
    # print(head(coreLogic_recode))
    setkey(coreLogic_recode, BlockGroup)
    return(coreLogic_recode)
}



load_some <- function(tbls) {
    if ('pums' %in% tbls){
    }
    if ('acs_marginalIncome' %in% tbls) {
    }
    if ('acs_marginalIncome_25k' %in% tbls) {
    }
}

load_all <- function() {
    pums <- load_pums()
    acs_marginalIncome <- load_acs_marginalIncome()
    acs_marginalRoomCount <- load_acs_marginalRoomCount()
    coreLogic <- load_coreLogic()

    acs_marginalIncome_25k <- data.table(BlockGroup = acs_marginalIncome$Id2,
                                         '0to25'    = rowSums(acs_marginalIncome[,2:5]),
                                         '25to50'   = rowSums(acs_marginalIncome[,6:10]),
                                         '50to75'   = rowSums(acs_marginalIncome[,11:12]),
                                         '75to100'  = rowSums(acs_marginalIncome[,13]),
                                         '100to125' = rowSums(acs_marginalIncome[,14]),
                                         '125to150' = rowSums(acs_marginalIncome[,15]),
                                         '150to200' = rowSums(acs_marginalIncome[,16]),
                                         '200plus'  = rowSums(acs_marginalIncome[,17]))

    coreLogic_recode <- load_coreLogic_recode()

    return(list(
        'pums' = pums,
        'acs_marginalIncome' = acs_marginalIncome,
        'acs_marginalIncome_25k' = acs_marginalIncome_25k,
        'acs_marginalRoomCount' = acs_marginalRoomCount,
        'coreLogic' = coreLogic,
        'coreLogic_recode' = coreLogic_recode
    ))
}
