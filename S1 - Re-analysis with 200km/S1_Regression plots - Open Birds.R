## load up stuff
library(tidyverse)
library(cmdstanr)
library(shinystan)
library(rstan)
library(bayesplot)
library(scales)
library(rethinking)
library(pdftools)
library(rstantools)
library(posterior)

rm(list = ls())
gc()


### RICHNESS! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
d <- read.csv("FINAL_REVISED_DATASET_RO_200km.csv")

d_space <- d %>% filter(space.time == 2)
d_time <- d %>% filter(space.time == 1)

d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

load("Output_FINAL_REVISED_RO_200km.RData")
draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "retrans_noise", "observer", "first" ))
a <- summary(stanfit, pars = "a")

# Assign average parameter values
avS_B <- mean(draws$b_space)
avT_B <- mean(draws$b_time)
avS_A <- mean(draws$a[,,2])
avT_A <- mean(draws$a[,,1])
avS_O <- mean(draws$observer[,,2])
avT_O <- mean(draws$observer[,,1])
avS_F <- mean(draws$first[,2])
avT_F <- mean(draws$first[,1])


png("ALL_Slopes_RO_200km_ALL.png",width=40,height=30,units="cm",res=600)

par(mfrow=c(1,2),
    omi=c(0.5, 0.5, 0.5, 0.5),
    mar=c(4,5,2,2))

plot(d_space$Forest.cover, d_space$Richness, #d_space$TA,
     col = alpha("#2c7bb6", 0), 
     pch = 3,
     ylim = c(0, 40),
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Species richness")

axis(2, cex.axis = 1.5)

print("Starting TIME plots...")
## TIME
for(i in 1:13){
  print(paste0("Progress: ", round(i/13*100, 0), "% finished."))
  x.seq <- seq( from=min(d_time$Forest.cover), to = max(d_time$Forest.cover), length.out = 30 )
  
  x.scaled <- (x.seq-mean(d_time$Forest.cover))/sd(d_time$Forest.cover)
  mu.time <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                      mean(draws$observer[, , 1]) + mean(draws$first[,1]) +
                                                      draws$retrans_noise )) )
  
  ci.time <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                    mean(draws$observer[, , 1]) + mean(draws$first[,1]) +
                                                    draws$retrans_noise )) )
  
  
  
  shade( ci.time, x.scaled, col = alpha("#f792b0", 0.02))
  lines( x.scaled, mu.time, col = alpha("#f792b0", 0.5), lwd = 1.5 )
}

av_mu.time <- sapply(x.scaled, function(x) exp(avT_A + avT_B * x + avT_O + avT_F + mean(draws$retrans_noise)))
lines(x.scaled, av_mu.time, col = "#f792b0", lwd = 5)


plot(d_space$Forest.cover, d_space$Richness,
     col = alpha("#ED432D", 0), 
     pch = 3,
     ylim = c(0, 40),
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Species richness")

axis(2, cex.axis = 1.5)

print("Finished TIME plots!")
print("Starting SPACE plots...")
## SPACE
for(i in 1:13){
  print(paste0("Progress: ", round(i/13*100, 0), "% finished."))
  x.seq <- seq( from=min(d_space$Forest.cover), to = max(d_space$Forest.cover), length.out = 30 )
  
  x.scaled <- (x.seq-mean(d_space$Forest.cover))/sd(d_space$Forest.cover)
  mu.space <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 2] + draws$b_space[, i] * x + 
                                                       mean(draws$observer[, , 2]) + mean(draws$first[,2]) +
                                                       draws$retrans_noise )) )
  
  ci.space <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 2] + draws$b_space[, i] * x + 
                                                     mean(draws$observer[, , 2]) + mean(draws$first[,2]) +
                                                     draws$retrans_noise )) )
  
  shade( ci.space, x.scaled, col = alpha("#f792b0", 0.02))
  lines( x.scaled, mu.space, col = alpha("#f792b0", 0.5), lwd = 1.5 )
}

av_mu.space <- sapply(x.scaled, function(x) exp(avS_A + avS_B * x + avS_O + avS_F + mean(draws$retrans_noise)))
lines(x.scaled, av_mu.space, col = "#f792b0", lwd = 5)
print("Finished SPACE plots! :-)")
dev.off()


#### ABUNDANCE! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
gc()

d <- read.csv("FINAL_REVISED_DATASET_TO_200km.csv")
d_space <- d %>% filter(space.time == 2)
d_time <- d %>% filter(space.time == 1)

d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

load("Output_FINAL_REVISED_TO_200km.RData")
# Extract draws and parameter summaries
draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "retrans_noise", "observer", "first" ))
a <- summary(stanfit, pars = "a")

# Assign average parameter values
avS_B <- mean(draws$b_space)
avT_B <- mean(draws$b_time)
avS_A <- mean(draws$a[,,2])
avT_A <- mean(draws$a[,,1])
avS_O <- mean(draws$observer[,,2])
avT_O <- mean(draws$observer[,,1])
avS_F <- mean(draws$first[,2])
avT_F <- mean(draws$first[,1])


png("ALL_Slopes_TO_200km_ALL.png",width=40,height=30,units="cm",res=600)

par(mfrow=c(1,2),
    omi=c(0.5, 0.5, 0.5, 0.5),
    mar=c(4,5,2,2))

plot(d_time$Forest.cover, d_time$TA,
     col = alpha("#2c7bb6", 0), 
     ylim = c(0, 40),
     pch = 3,
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Total abundance")

axis(2, cex.axis = 1.5)

print("Starting TIME plots...")
## TIME
for(i in 1:13){
  print(paste0("Progress: ", round(i/13*100, 0), "% finished."))
  x.seq <- seq( from=min(d_time$Forest.cover), to = max(d_time$Forest.cover), length.out = 30 )
  
  x.scaled <- (x.seq-mean(d_time$Forest.cover))/sd(d_time$Forest.cover)
  mu.time <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                      mean(draws$observer[, , 1]) + mean(draws$first[,1]) +
                                                      draws$retrans_noise )) )
  
  ci.time <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                    mean(draws$observer[, , 1]) + mean(draws$first[,1]) +
                                                    draws$retrans_noise )) )
  
  
  
  shade( ci.time, x.scaled, col = alpha("#f7b456", 0.005))
  lines( x.scaled, mu.time, col = alpha("#f7b456", 0.5), lwd = 1.5 )
}

av_mu.time <- sapply(x.scaled, function(x) exp(avT_A + avT_B * x + avT_O + avT_F + mean(draws$retrans_noise)))
lines(x.scaled, av_mu.time, col = "#f7b456", lwd = 5)


plot(d_space$Forest.cover, d_space$TA,
     col = alpha("#ED432D", 0), 
     ylim = c(0, 40),
     pch = 3,
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Total abundance")

axis(2, cex.axis = 1.5)

print("Finished TIME plots!")
print("Starting SPACE plots...")
## SPACE
for(i in 1:13){
  print(paste0("Progress: ", round(i/13*100, 0), "% finished."))
  x.seq <- seq( from=min(d_space$Forest.cover), to = max(d_space$Forest.cover), length.out = 30 )
  
  x.scaled <- (x.seq-mean(d_space$Forest.cover))/sd(d_space$Forest.cover)
  mu.space <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 2] + draws$b_space[, i] * x + 
                                                       mean(draws$observer[, , 2]) + mean(draws$first[,2]) +
                                                       draws$retrans_noise )) )
  
  ci.space <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 2] + draws$b_space[, i] * x + 
                                                     mean(draws$observer[, , 2]) + mean(draws$first[,2]) +
                                                     draws$retrans_noise )) )
  
  shade( ci.space, x.scaled, col = alpha("#f7b456", 0.005))
  lines( x.scaled, mu.space, col = alpha("#f7b456", 0.5), lwd = 1.5 )
}

av_mu.space <- sapply(x.scaled, function(x) exp(avS_A + avS_B * x + avS_O + avS_F + mean(draws$retrans_noise)))
lines(x.scaled, av_mu.space, col = "#f7b456", lwd = 5)
print("Finished SPACE plots! :-)")
dev.off()