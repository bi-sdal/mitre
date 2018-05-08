# link housing locations to incident locations - a first attempt

# read in the 20 realizations for test cases
simA1 <- read.csv('~/sdal/projects/mitre/working/simulatedArlingtonData/simulatedArlingtonIncome.csv')

# load the dataframe holding the 2013 DOME calls in the police data
load('~/sdal/projects/mitre/working/PoliceData/cfsDOME2013.RData')

 # most locations have a singel housing unit, others have as many as 330.
simA1 %>% group_by(LATITUDE,LONGITUDE) %>% summarize(n=n()) -> uloc
table(uloc$n)

 # get distances from each event to each housing unit
library(geosphere)
tmp <- dist(cfsDOME2013[1,c('lat','lng')],simA1[1:3,c('LATITUDE','LONGITUDE')])
ic=sample(1:nrow(cfsDOME2013),1); ia=sample(1:nrow(simA1),100)
distHaversine(cfsDOME2013[ic,c('lat','lng')],simA1[ia,c('LATITUDE','LONGITUDE')])

xm <- rbind(cfsDOME2013[ic,c('lat')],simA1[ia,c('LATITUDE')])
ym <- rbind(cfsDOME2013[ic,c('lng')],simA1[ia,c('LONGITUDE')])

PDF=FALSE
if(PDF) pdf('distv1.pdf',width=4,height=4)
par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(0,0,0,0))
plot(simA1$LONGITUDE,simA1$LATITUDE,pch='.',col='grey')
points(cfsDOME2013$lng,cfsDOME2013$lat,pch=16,cex=.5,col='black')
matlines(ym,xm,col='red')
if(PDF) dev.off()

 # make a loop that gets distances for each incident
library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores=detectCores() # there are 40 cores available
# Aaron says use 20 or less.
cores=20
cl <- makeCluster(cores) #not to overload your computer
registerDoParallel(cl)

finalDist <- foreach(i=1:nrow(cfsDOME2013), .combine=cbind,.packages='geosphere') %dopar% {
  tempDist = distHaversine(cfsDOME2013[i,c('lat','lng')],
                          simA1[,c('LATITUDE','LONGITUDE')])
  tempDist #Equivalent to finalMatrix = cbind(finalMatrix, tempMatrix)
}
#stop cluster
stopCluster(cl)

 # runs pretty fast
 # now, finalDist is a 44642 x 2043 matrix.  For now, grab the closest
 # housing unit location to the police event
iclosest <- apply(finalDist,2,which.min)
iclosest <- rep(NA,nrow(cfsDOME2013))
for(k in 1:nrow(cfsDOME2013)){
  print(k)
  if(all(is.na(finalDist[,k]))) { iclosest[k] <- NA} else
  {iclosest[k] = which.min(finalDist[,k])}
}
 # check this on a plot
if(PDF) pdf('linkv1.pdf',width=4,height=4)
par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(0,0,0,0))
plot(simA1$LONGITUDE,simA1$LATITUDE,pch='.',col='grey')
points(cfsDOME2013$lng,cfsDOME2013$lat,pch=16,cex=.5,col='black')
points(simA1$LONGITUDE[iclosest],simA1$LATITUDE[iclosest],pch=16,col='cyan',cex=.3)
if(PDF) dev.off()

# now compute distance to a couple of select points
 # plot the housing units and cases
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
okmat <- cbind(isNear(localPoints$x[1],localPoints$y[1]),
               isNear(localPoints$x[2],localPoints$y[2]),
               isNear(localPoints$x[3],localPoints$y[3]),
               isNear(localPoints$x[4],localPoints$y[4]),
               isNear(localPoints$x[5],localPoints$y[5]))
for(k in 1:5) points(incomeSims$LONGITUDE[okmat[,k]],incomeSims$LATITUDE[okmat[,k]],pch='.',col=k+1)

 
