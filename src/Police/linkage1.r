library(dplyr)
library(data.table)
library(sf)
require(ggplot2)

# Load data
#Police data
policeData = fread("./data/mitre/working/PoliceData/policeData.csv")

# Simulated Arlington Data
incomeSims = fread("./data/mitre/working/simulatedArlingtonData/arlSimWithCaseCounts.csv")

# Load table linking case nos to houses, with distances. NOTE: the number in closestHouse corresponds to the appropriate row in simulated income data.
closestAddressToCase = fread("./data/mitre/working/simulatedArlingtonData/closestHouseToCall.csv")

# append the closest housing unit to each DOME case
policeData %>% left_join(closestAddressToCase,by=c("Call_No"="Call_No")) -> policeData
# a few facts here: 1 address had 53 calls - a large apt building
#                   42% of call locations are repeats

# make a table of police incident locations address lat and lon
policeData %>% group_by(LONGITUDE,LATITUDE,Location) %>%
  summarize(n=n()) %>% arrange(desc(n)) -> policeCount



# show distances from a single event and a collection of houses
PDF=FALSE
if(PDF) pdf('distv1.pdf',width=4,height=4)
par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(0,0,0,0))
plot(simA1$LONGITUDE,simA1$LATITUDE,pch='.',cex=.7,col='grey80')
points(simA1$LONGITUDE,simA1$LATITUDE,pch='.',cex=.7,col='grey80')
points(cfsDOME2013$lng,cfsDOME2013$lat,pch=19,cex=.4,col='black')
matlines(ym,xm,col='red')
if(PDF) dev.off()

# show the locations of housing units 
if(PDF) pdf('linkv2.pdf',width=4,height=4)
par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(0,0,0,0))
plot(incomeSims$LONGITUDE,incomeSims$LATITUDE,type='n')
iu2 <- incomeSims$UNITS.NUMBER >= 50; col2='green'; cex2=1.0;
points(incomeSims$LONGITUDE[iu2],incomeSims$LATITUDE[iu2],pch=19,cex=cex2,col=col2)
iu3 <- incomeSims$UNITS.NUMBER < 50 & incomeSims$UNITS.NUMBER >= 20; col3='cyan'; cex3=0.7;
points(incomeSims$LONGITUDE[iu3],incomeSims$LATITUDE[iu3],pch=19,cex=cex3,col=col3)
iu4 <- incomeSims$UNITS.NUMBER < 20 & incomeSims$UNITS.NUMBER >= 2; col4='yellow'; cex4=0.5;
points(incomeSims$LONGITUDE[iu4],incomeSims$LATITUDE[iu4],pch=19,cex=cex4,col=col4)
points(incomeSims$LONGITUDE,incomeSims$LATITUDE,pch='.',cex=.7,col='grey80')
points(policeData$LONGITUDE,policeData$LATITUDE,pch=16,cex=.5,col='black')
legend('bottomleft',pch=c(20,19,19,19,19,19),col=c('grey80','yellow','cyan','green','black'),
       legend=c('single unit','2-19 units','20-49 units','50+ units','police call'))
if(PDF) dev.off()

 # show counts of DOME locations
if(PDF) pdf('domeCountsv1.pdf',width=4,height=4)
par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(0,0,0,0))
plot(incomeSims$LONGITUDE,incomeSims$LATITUDE,type='n')
iu2 <- incomeSims$UNITS.NUMBER >= 50; col2='green'; cex2=1.0;
points(incomeSims$LONGITUDE[iu2],incomeSims$LATITUDE[iu2],pch=19,cex=cex2,col=col2)
iu3 <- incomeSims$UNITS.NUMBER < 50 & incomeSims$UNITS.NUMBER >= 20; col3='cyan'; cex3=0.7;
points(incomeSims$LONGITUDE[iu3],incomeSims$LATITUDE[iu3],pch=19,cex=cex3,col=col3)
iu4 <- incomeSims$UNITS.NUMBER < 20 & incomeSims$UNITS.NUMBER >= 2; col4='yellow'; cex4=0.5;
points(incomeSims$LONGITUDE[iu4],incomeSims$LATITUDE[iu4],pch=19,cex=cex4,col=col4)
points(incomeSims$LONGITUDE,incomeSims$LATITUDE,pch='.',cex=.7,col='grey80')
points(policeData$LONGITUDE,policeData$LATITUDE,pch=16,cex=.5,col='black')
legend('bottomleft',pch=c(20,19,19,19,19,19),col=c('grey80','yellow','cyan','green','black'),
       legend=c('single unit','2-19 units','20-49 units','50+ units','police call'))
ic=1:5
text(policeCount$LONGITUDE[ic],policeCount$LATITUDE[ic],
     as.character(policeCount$n[ic]),col='red',cex=.8)
points(policeCount$LONGITUDE[ic],policeCount$LATITUDE[ic],
     col='red',pch='.',cex=.2)
if(PDF) dev.off()

# show links on a plot
if(PDF) pdf('linkv2.pdf',width=4,height=4)
par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(0,0,0,0))
plot(incomeSims$LONGITUDE,incomeSims$LATITUDE,pch='.',col='grey')
#plot(simA1$LONGITUDE,simA1$LATITUDE,pch='.',col='grey')
points(policeData$LONGITUDE,policeData$LATITUDE,pch=16,cex=.5,col='black')
points(incomeSims$LONGITUDE[policeData$closestHouse],
       incomeSims$LATITUDE[policeData$closestHouse],pch=16,col='cyan',cex=.3)
if(PDF) dev.off()

# now compute distance to a couple of select points
# plot the housing units and cases
if(PDF) pdf('localRegions.pdf',width=4,height=4)
par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(0,0,0,0))
plot(simA1$LONGITUDE,simA1$LATITUDE,pch='.',col='grey')
points(cfsDOME2013$lng,cfsDOME2013$lat,pch=16,cex=.5,col='black')
#localPoints <- locator(5)
# now find all units that are within 500m to each point
isNear <- function(lon0,lat0,LAT=incomeSims$LATITUDE,LON=incomeSims$LONGITUDE,dist=500){
  # a function that returns TRUE if the LAT,LON value is within dist
  # of point
  dvals <- distHaversine(c(lon0,lat0),cbind(LON,LAT))
  return(dvals <= dist)
}
# call isNear to get a vector of T/F indicators of nearness
okmat <- cbind(isNear(localPoints$x[1],localPoints$y[1],dist=1000),
               isNear(localPoints$x[2],localPoints$y[2],dist=1000),
               isNear(localPoints$x[3],localPoints$y[3],dist=1000),
               isNear(localPoints$x[4],localPoints$y[4],dist=1000),
               isNear(localPoints$x[5],localPoints$y[5],dist=1000))
for(k in 1:5) points(incomeSims$LONGITUDE[okmat[,k]],incomeSims$LATITUDE[okmat[,k]],pch='.',col=k+1)
text(localPoints,labels=as.character(1:5),col='yellow')
if(PDF) dev.off()
 # using the closest housing unit, link cases to housing units
tabEvent <- table(policeData$closestHouse)
ihu <- as.numeric(names(tabEvent))
ylink <- rep(0,nrow(incomeSims))
ylink[ihu] <- tabEvent

 # rate by region
for(k in 1:5){
  print(sum(yevent[okmat[,k]])/sum(incomeSims$UNITS.NUMBER[okmat[,k]]))
}
# rates by region 0.001046645 0.000293443 0.01084636 0.01544944 0.002644714

 # show a first plot of income by ylink
par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(4,4,1,1))
plot(sqrt(incomeSims$incomeDraw1),jitter(ylink),pch='.')
plot(ksmooth(incomeSims$incomeDraw1,ylink))
plot(ylink,incomeSims$eventsAtAddress)
yevent <- ifelse(ylink <= incomeSims$UNITS.NUMBER,ylink,incomeSims$UNITS.NUMBER)

 # fit a logistic regression
plotLR <- function(isubset,kreal,PLOT=TRUE){
  # plots plots a collection of fits of logistic regression to a region
  # of data
  nreal = length(kreal)
  coefs <- matrix(0,nrow=2,ncol=nreal)
  for(k in 1:nreal){
    colnm <- paste('incomeDraw',kreal[k],sep='')
    icol <- match(colnm,names(incomeSims))
    xval <- incomeSims[[icol]]
    coefs[,k] <- coef(glm(cbind(yevent,incomeSims$UNITS.NUMBER-yevent) ~ sqrt(xval),family = binomial(),subset=okmat[,isubset]))
  }
  r1 <- seq(10000,200000,length=100); r <- sqrt(r1)
  pmat <- matrix(NA,nrow=100,ncol=nreal)
  for(k in 1:nreal) pmat[,k] <- exp(coefs[1,k]+r*coefs[2,k])/(1+exp(coefs[1,k]+r*coefs[2,k]))
  if(PLOT){
    matplot(r1,pmat,type='l',ylim=c(0,.03),xlab='income',ylab='p(Call)')
    points(incomeSims$incomeDraw1[okmat[,isubset]],jitter(.03*ylink[okmat[,isubset]]),pch='.')
  }
  return(pmat)
}

 # make one plot for each of the regions
r1 <- seq(10000,200000,length=100)
yl <- c(0,.03)
p1 <-plotLR(1,1:100); p2 <-plotLR(2,1:100)  
p3 <-plotLR(3,1:100); p4 <-plotLR(4,1:100) 
p5 <-plotLR(5,1:100)

if(PDF) pdf('lrs.pdf',width=5.5,height=5)
par(mfrow=c(3,2),oma=c(4,4,1,2),mar=c(0,0,0,0))
matplot(r1,p1,type='l',ylim=yl,axes=F,xlab='',ylab=''); box(); 
axis(2)
text(90000,.025,1,cex=2)
matplot(r1,p2,type='l',ylim=yl,axes=F,xlab='',ylab=''); box();
text(90000,.025,2,cex=2)
matplot(r1,p3,type='l',ylim=yl,axes=F,xlab='',ylab=''); box();
text(90000,.025,3,cex=2)
matplot(r1,p4,type='l',ylim=yl,axes=F,xlab='',ylab=''); box();
axis(4); axis(1)
text(90000,.025,4,cex=2)
matplot(r1,p5,type='l',ylim=yl,axes=F,xlab='',ylab=''); box();
text(90000,.025,5,cex=2)
axis(2); axis(1)
mtext('Pr(DOME call)',side=2,line=2.7,outer=T)
mtext('household income',side=1,line=2.7,outer=F)
if(PDF) dev.off()
  
  