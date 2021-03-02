# inferring a population proportion from a simple random sample
# Here let's try fixing the actual choices in the sample, leaving the
# rest of the population as uncertain.
# Dumb example of Dave's trying to understand how mcmc (or other sampling-based
# approaches) can be used to produce posterior realizations of a population
# given different kinds of samples.  Here we take a simple random sample of
# size n from a population of size N

# something simple and binary
# set small=T if we want a small problem, otherwise F for a big one
small=F
if(small){
# use small population size so that the "finite population size" correction
# is relevant here (not that we use it, it's automatically accounted for in
# the Bayesian inference).
 N = 10
 # a smaller sample 
 y = c(1,0,1,0,1,0,1); n = length(y)
} else{
 # a setting where the clt should kick in
 N = 100
 # a larger sample 
 y = c(1,0,1,0,1,0,1,0,1,1,1,0,0,0,0,0,0,0,1,1,1,1); n = length(y)
}


# Prior 1:   All configurations of 10 1's or 0's are
# equally likely, for now, order counts.  So this prior gives more probability
# to populations with half 1's, and half 0's. Here, 
#  > sample(c(0,1),size=N,replace=T)
# will produce a prior draw.  

# Prior 2: An (better) alternative is to give any value of true
# p equal weight, so that a population with x 1's has prior mass 1/choose(N,x).
# This gives a uniform distribution for p, which is likely more sensible.

# posterior for the population Y given the sample y; use Prior 2
logpost = function(Y,y){
 # log of the posterior for the population with a srs of y, the outcomes
 # are either 0 or 1.
 N = length(Y); n=length(y)
 X = sum(Y); x = sum(y)
  # reject any y which is impossible
 if(x > X) return(-9e9)
 if(n-x > N-X) return(-9e9)
 llike = dhyper(x,X,N-X,n,log=T) # prob of this sample with x 1's | Y
 lprior = -lchoose(N,X)
 return(llike+lprior)
}

# now let's try it

 # # of mcmc iterations
Niter = 10000
Ysamp = matrix(0,ncol=Niter,nrow=N)
 # initial draw of the population
#Ysamp[,1]=sample(c(0,1),size=N,replace=T)
 # initial log posterior density
lp0 = logpost(Ysamp[,1],y)

# carry out (antithetic) Metropolis sampling
for(i in 2:Niter){
  Ycur = Ysamp[,i-1]
  for(k in 1:N){
    can = Ycur; can[k] = ifelse(Ycur[k]==1,0,1)
    lpcan <- logpost(can,y)
    u <- runif(1)
    if(log(u) < (lpcan - lp0)){
       # accept the proposal
      Ycur <- can
      lp0 <- lpcan # update the posterior density
    } else{
      Ycur <- Ycur # set the new value to the old value; leave logpost as is.
    }
  }
  Ysamp[,i] = Ycur
}
 # look at the 1st 5 iterations of the mcmcm output
Ysamp[,1:5]
 # see the estimated posterior distribution for p
par(mfrow=c(2,2),oma=c(0,0,0,0),mar=c(4.5,4.5,1,1))
plot(apply(Ysamp,2,mean),type='l',xlab='iteration',ylab='pop mean')
image(1:20,1:N,t(Ysamp[,1:20]),xlab='iteration',ylab='population')
hist(apply(Ysamp,2,sum),breaks=seq(0-.25,N+.25,by=.5),col='grey70',
  xlab="population number of 1s",main='estimated posterior')
hist(apply(Ysamp,2,sum)/N,breaks=seq(0-.25,N+.25,by=.5)/N,col='grey70',
  xlab="population proportion of 1s",main='estimated posterior')


