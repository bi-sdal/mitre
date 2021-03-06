library(data.table)
library(dplyr)
library(ggplot2)
source("./R/00-simulateArlFunctions.R")
source("./src/SMARTscatter/parameterFiles/allDataFullRuns.R")
source("./src/SMARTscatter/01-prepareAndLoadData.R")
rm(marginalIncome, marginalRooms)

clPums = data.table(clAtrackPums)[source %in% c("CL", "PUMS")]
clPums$source = as.character(clPums$source)
path = "./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP_householdSize_singleParent_snKid_militaryService_unmarriedPartner_multiGenHouse/"
simulation = getResampleByIndex(path, 5)

plotDat = clPums[,.(VALP, sqrtHINCP, houseID, RMSP, source)] %>%
  merge(simulation[,.(houseID, impSqrtHINCP = sqrt(simulation$sqrtHINCP), RMSP)], by = "houseID", all.x = TRUE)
  
plotDat[,plotSqrtHINCP := ifelse(is.na(sqrtHINCP), impSqrtHINCP, sqrtHINCP)]
plotDat[,plotRMSP := ifelse(is.na(RMSP.x), RMSP.y, RMSP.x)]

hashMarks = data.table(x = plotDat[sample(1:nrow(plotDat), 500), VALP], y = -40)

pdf("./output/housevIncome.pdf", height = 4.5, width = 5)
ggplot(data = plotDat) + 
  geom_point(aes(x = VALP, y = plotSqrtHINCP, color = source)) + 
  geom_point(data = hashMarks, aes(x = x, y = y), shape = 3) + 
  scale_color_discrete("Source", labels = c("CoreLogic", "PUMS")) + 
  labs(x = "House Value (fixed)", y = "Sqrt Income (imputed)") + 
  guides(color = FALSE)
dev.off()

pdf("./output/roomsvIncome.pdf", height = 4.5, width = 5)
ggplot(data = plotDat[plotRMSP < 15]) + 
  geom_boxplot(aes(x = as.factor(plotRMSP), y = plotSqrtHINCP, color = source)) + 
  scale_color_discrete("Source", labels = c("Imputed", "PUMS")) + 
  labs(x = "Rooms Per House", y = "Sqrt Income")
dev.off()
#
# Household level plots
#

clAtrack = data.table(clAtrackPums[clAtrackPums$source != "PUMS",])
pHat = fread("./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP_householdSize_singleParent_snKid_militaryService_unmarriedPartner_multiGenHouse/logregFits.csv")

houseLevelData = clAtrack[,.(VALP, LATITUDE, LONGITUDE, houseID)][simulation, on = "houseID"][pHat[, .(houseID, pHat = resample1)], on = "houseID"]

arlFit <- readRDS('./data/mitre/final/smart_maps/fitted_prob_data.RDS')
# Probability plot
pdf("./output/houseDOMEProbPlot.pdf", height = 4.5, width = 5)

ggplot(arlFit) + 
  geom_polygon(aes(x=long,y=lat,group=group,), alpha=0,color="grey70",lwd=.5) +
  geom_point(data = houseLevelData, aes(x = LONGITUDE, y = LATITUDE, color = pHat), size = .4) +
  scale_color_gradient2("Fitted Probability",
                        breaks = round(unname(quantile(houseLevelData$pHat, prob = c(.00009, .0009, .009, .09, .9, .999999))), 3), 
                        low = 'blue', mid = 'white', high = 'red', midpoint = -3.5, trans = 'log') +
  coord_quickmap() + 
  theme_minimal() +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
        plot.caption = element_text(hjust=0)) + #labels
  labs(title="Fitted Probability of Domestic Abuse Call", x="", y="") 
dev.off()
# Same but zoomed in

pdf("./output/houseDOMEProbPlotZoomed.pdf", height = 4.5, width = 5)
ggplot(arlFit) + 
  geom_polygon(aes(x=long,y=lat,group=group,), alpha=0,color="grey70",lwd=.5) +
  geom_point(data = houseLevelData, aes(x = LONGITUDE, y = LATITUDE, color = pHat), size = 1.2) +
  scale_color_gradient2("Fitted Probability",breaks = unname(quantile(houseLevelData$pHat, prob = c(.00009, .0009, .009, .09, .9, .999999))), low = 'blue', mid = 'white', high = 'red', midpoint = -3.5, trans = 'log', guide = FALSE) +
  #coord_quickmap() + 
  theme_minimal() +
  coord_cartesian(xlim = c(-77.14, -77.1), ylim = c(38.85, 38.87)) +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.title = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
        plot.caption = element_text(hjust=0)) 
dev.off()




pdf("./output/houseVALPProbPlot.pdf", height = 4.5, width = 5)
ggplot(arlFit) + 
  geom_polygon(aes(x=long,y=lat,group=group), alpha=0,color="grey70",lwd=.5) +
  geom_point(data = na.omit(houseLevelData), aes(x = LONGITUDE, y = LATITUDE, color = VALP), size = .4) +
  scale_color_gradient2("Home Value", low = 'blue', mid = 'white', high = 'red', midpoint = median(houseLevelData$VALP, na.rm = TRUE), breaks = 1000 * c(250, 750, 1250)) +
  coord_quickmap() + 
  theme_minimal() +
  theme(#axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        #axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
        plot.caption = element_text(hjust=0)) + #labels
  labs(title="Home Values", x="", y="") 
dev.off()
pdf("./output/houseVALPProbPlotZoomed.pdf", height = 4.5, width = 5)
ggplot(arlFit) + 
  geom_polygon(aes(x=long,y=lat,group=group), alpha=0,color="grey70",lwd=.5) +
  geom_point(data = na.omit(houseLevelData), aes(x = LONGITUDE, y = LATITUDE, color = VALP), size = .4) +
  scale_color_gradient2("Home Value", low = 'blue', mid = 'white', high = 'red', midpoint = median(houseLevelData$VALP, na.rm = TRUE), guide = FALSE) +
  coord_quickmap() + 
  theme_minimal() +
  coord_cartesian(xlim = c(-77.06, -77.04), ylim = c(38.848, 38.86)) +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        axis.title = element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
        plot.caption = element_text(hjust=0)) 
dev.off()

# Plots for proportion of coefficients over/under 0

smart_dat <- data.table(readRDS('./data/mitre/final/logistic_regressions/smart_data_coefs.RDS'))

props = smart_dat[,
          .(propGrZero = mean(estimate_scaled > 0)),
          by = term]

pdf("./output/propOverZeroCoefs.pdf", height = 4.5, width = 5)
ggplot(props, aes(x = term, y = propGrZero)) +   # Fill column
  geom_bar(stat = "identity", width = .6, fill = 'maroon') + # Labels
  coord_flip() +  # Flip axes
  geom_hline(yintercept = c(.05, .95)) + 
  labs(title="Coefficients Across Runs", y = "Proportion of runs greater than zero, lines at 5% and 95%", x = "Features")
dev.off()




