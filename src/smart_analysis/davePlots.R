# some plots to show an ACS table
read.csv('./data/mitre/working/simulatedArlingtonData/marginalIncome.csv')->margIncACS


# make a plot of pums responses:
# read in the first 18 lines of posteriorPlots.R, producing the object plotDat
# then do this:

plotDat %>% filter(source=="PUMS") -> pdat

pdf("./output/housevIncomePUMS.pdf", height = 4.5, width = 5)
ggplot(data = pdat) + 
  geom_point(aes(x = VALP, y = plotSqrtHINCP, color = source)) + 
  #geom_point(data = hashMarks, aes(x = x, y = y), shape = 3) + 
  scale_color_discrete("Source", labels = c("CoreLogic", "PUMS")) + 
  labs(x = "House Value (fixed)", y = "Sqrt Income (imputed)") + 
  guides(color = FALSE)
dev.off()