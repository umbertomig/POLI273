## Code Class 07 - From Gerber and Green
#install.packages('tidyverse')
#install.packages('ri')
packageurl <- "https://cran.r-project.org/src/contrib/Archive/ri/ri_0.9.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
#install.packages('ggpubr')
require(tidyverse)
require(ri)
require(ggpubr)

dt <- data.frame(x = sample(seq(0, 1, 0.01), size = 100, replace = T),
                 y = sample(seq(0, 1, 0.01), size = 100, replace = T))
dt2 <- data.frame(x = rep(seq(0, 1, 0.1),10))
y = numeric()
for (i in 1:10) {
  y = c(y,rep((i-1)/10, 11))
}
dt2$y <- y

fig1 <- ggplot(dt, aes(x = x, y = y)) + 
  geom_point(colour="black") + ylim(0,0.9) + xlim(0,0.9)
fig2 <- ggplot(dt2, aes(x = x, y = y)) + 
  geom_point(colour="black") + ylim(0,0.9) + xlim(0,0.9)

ggarrange(fig1, fig2, ncol = 2, nrow = 1)

dt <- data.frame(Vila = as.character(1:10),
                 Yi0 = round(rnorm(10), 1), Yi1 = 0.5+round(rnorm(10), 1))

ggplot(dt, aes(x = Vila, y = Yi0)) + 
  geom_point(colour="black") +geom_point(aes(x=Vila, y=Yi1), colour="red") + ylim(-4,4) + ylab('Potential Outcomes (red = tr)')

dt

combn(10,5)

dt2 <- dt
dt2$Yi0[11-combn(10,5)[,1]] = NA
dt2$Yi1[combn(10,5)[,1]] = NA
dt2

meanYi0 <- numeric()
meanYi1 <- numeric()
cbn <- combn(10,5)
for (i in 1:choose(10,5)) {
  meanYi0[i] <- mean(dt$Yi0[cbn[,i]])
  meanYi1[i] <- mean(dt$Yi1[11-cbn[,i]])
}
hist(meanYi1-meanYi0)

Y0 <- c(0,1,2,4,4,6,6,9,14,15,16,16,17,18)
Y1 <- c(0,0,1,2,0,0,2,3,12,9,8,15,5,17)

Z <- c(1,1,0,0,0,0,0,0,0,0,0,0,1,1)

# generate all permutations of Z under _complete_ 
# random assignment
# note that default is to do every possible 
# permutation if less than 10,000 permutations

compperms <- genperms(Z)
numperms <- ncol(compperms)

# create empty vector
compmeans <- rep(NA,numperms)

# loop to create average treatment effect 
# estimates for each randomization
for (i in 1:numperms) 
  compmeans[i] <- mean(Y1[compperms[,i]==1]) - mean(Y0[compperms[,i]==0])

# randomize within blocks
block <- c(1,1,1,1,1,1,1,1,2,2,2,2,2,2)

# generate all permutations of Z under block random assignment

blockperms <- genperms(Z,block)
numperms <- ncol(blockperms)

# create empty vector
blockmeans <- rep(NA,numperms)

# loop to create average treatment effect 
# estimates for each randomization
for (i in 1:numperms) 
  blockmeans[i] <- weighted.mean(Y1[blockperms[,i]==1],c(8/2,8/2,6/2,6/2)) - weighted.mean(Y0[blockperms[,i]==0],c(8/6,8/6,8/6,8/6,8/6,8/6,6/4,6/4,6/4,6/4))

save(compmeans,blockmeans,file="figure3.1.Rdata")

# Draw histograms for Figure 3.1	

par(mfrow=c(2,1))
hist(compmeans,main="Sampling Distribution under Complete Randomization",xlim=c(-15,10),xlab="ATE Estimates",freq=FALSE,ylim=c(0,.3))
hist(blockmeans,main="Sampling Distribution under Blocked Randomization",xlim=c(-15,10),xlab="ATE Estimates",freq=FALSE,ylim=c(0,.3))
par(mfrow=c(1,1))

# calculate the proportion of esitmates that are above zero

length(compmeans[compmeans > 0])
length(compmeans[compmeans > 0])/length(compmeans)

length(blockmeans[blockmeans > 0])
length(blockmeans[blockmeans > 0])/length(blockmeans)