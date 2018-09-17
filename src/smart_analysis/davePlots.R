# some plots to show an ACS table
read.csv('./data/mitre/working/simulatedArlingtonData/marginalIncome.csv')->margIncACS

plotACSinc <- function(ibg,...){
  incints <- c(0,0,25000,25000,50000,50000,75000,75000,100000,100000,125000,125000,
               150000,150000,200000,200000,300000,300000)
  incInt <- c(0,25,50,75,100,125,150,200,400)
  incWid <- diff(incInt)
  #
  #ibg <- match(bg,margIncACS$BlockGroup)
  incvals <- margIncACS[ibg,-1]
  incvals <- as.vector(margIncACS[ibg,-1])
  denvals <- incvals/incWid*25
  nbin <- length(incvals)
  xx <- as.vector(rbind(incInt,incInt))
  yy <- unlist(c(0,as.vector(rbind(denvals,denvals)),0*denvals[nbin]))
  plot(xx,yy,type='l',...)
  #browser()
} # plotACSinc('1023023')

 # make a couple of tables
pdf('acsInc66.pdf',width=3.5,height=2.1)
par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(4,4,.5,.5))
plotACSinc(117,xlab='income ($1000)',ylab='count per $25000',col='red')
dev.off()

# wtf - this environment makes is difficult to debug

# make a plot of pums responses:
# read in the first 18 lines of posteriorPlots.R, producing the object plotDat
# then do this:

plotDat %>% filter(source=="PUMS") -> pdat

pdf("./output/housevIncomePUMS.pdf", height = 4.5, width = 5)
ggplot(data = pdat) + 
  geom_point(aes(x = VALP, y = plotSqrtHINCP, color = source)) + 
  #geom_point(data = hashMarks, aes(x = x, y = y), shape = 3) + 
  scale_color_discrete("Source", labels = c("CoreLogic", "PUMS")) + 
  labs(x = "House Value", y = "Sqrt Income") + 
  guides(color = FALSE)
dev.off()