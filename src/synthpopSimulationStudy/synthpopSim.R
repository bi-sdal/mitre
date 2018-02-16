n = 1000
sigma2 = 5
mu = rbeta(n, 2, 5) * 100
hist(mu)
x = rnorm(n, mu, sqrt(sigma2))
nbreaks = 10

breaks = (0:nbreaks) * 100/nbreaks

margin = unname(table(cut(mu, breaks, include.lowest = TRUE)))
probs = margin/sum(margin)
probs

ntrial = 1000
nmcmc = 20

mseNoResample = numeric(nmcmc)
mseResample = numeric(nmcmc)

# bootstrap sample the data nmcmc times
bsMat = matrix(NA, n, nmcmc)
resampleMat = matrix(NA, n, nmcmc)
resampleVec = numeric(n)
sigmaTrial = 8

for(i in 1:nmcmc) bsMat[,i] = rnorm(n, x, sigmaTrial)

for(j in 1:n){
  #resampleMat[j,] = bsMat[j, sample(1:nmcmc, nmcmc, replace = TRUE, prob = probs[as.numeric(cut(bsMat[1,], breaks))])]
  probsVector = probs[as.numeric(cut(bsMat[1,], breaks))]
  probsVector = probsVector/sum(probsVector)
  resampleVec[j] = sum(bsMat[j,] * probsVector)
}

mseNoResample = (rowMeans(bsMat) - mu)^2
t.test((resampleVec - mu)^2, mseNoResample, paired = T)
mean((resampleVec - mu)^2)
mean(mseNoResample)

