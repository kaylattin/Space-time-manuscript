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
d <- read.csv("FinalDataset_RF_200km_ALL.csv")

d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

load("Output_RF_200km_ALL.RData")
draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "retrans_noise", "observer", "first" ))
avg_space <- summary(stanfit, pars = "avg_b_space")
avg_time <- summary(stanfit, pars = "avg_b_time")
a_space <- summary(stanfit, pars = "avg_a_space")
a_time <- summary(stanfit, pars = "avg_a_time")
obs_space <- summary(stanfit, pars = "avg_obs_space")
obs_time <- summary(stanfit, pars = "avg_obs_time")
avg_first <- summary(stanfit, pars = "avg_first_obs")

avS_B <- avg_space$summary[,1]
avT_B <- avg_time$summary[,1]
avS_A <- a_space$summary[,1]
avT_A <- a_time$summary[,1]
avS_O <- obs_space$summary[,1]
avT_O <- obs_time$summary[,1]
avF <- avg_first$summary[,1]

d_space <- d %>% filter(space.time == 2)
d_time <- d %>% filter(space.time == 1)

png("ALL_Slopes_RF_200km_ALL.png",width=40,height=30,units="cm",res=600)

par(mfrow=c(1,2),
    omi=c(0.5, 0.5, 0.5, 0.5),
    mar=c(4,5,2,2))

plot(d_space$Forest.cover, d_space$Richness, #d_space$TA,
     col = alpha("#2c7bb6", 0), 
     pch = 3,
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Species richness")

axis(2, cex.axis = 1.5)

print("Starting TIME plots...")
## TIME
for(i in 1:13){
  print(paste0("Progress: ", round(i/30*100, 0), "% finished."))
  x.seq <- seq( from=min(d_time$Forest.cover), to = max(d_time$Forest.cover), length.out = 30 )
  
  x.scaled <- (x.seq-mean(d_time$Forest.cover))/sd(d_time$Forest.cover)
  mu.time <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                      mean(draws$observer[, , 1]) + mean(draws$first[,1]) +
                                                      draws$retrans_noise )) )
  
  ci.time <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                    mean(draws$observer[, , 1]) + mean(draws$first[,1]) +
                                                    draws$retrans_noise )) )
  
  
  
  shade( ci.time, x.scaled, col = alpha("#2c7bb6", 0.005))
  lines( x.scaled, mu.time, col = alpha("#2c7bb6", 0.5), lwd = 1.5 )
}

av_mu.time <- sapply(x.scaled, function(x) exp(avT_A + avT_B * x + avT_O + avF + mean(draws$retrans_noise)))
lines(x.scaled, av_mu.time, col = "#2c7bb6", lwd = 5)


plot(d_space$Forest.cover, d_space$Richness,
     col = alpha("#ED432D", 0), 
     pch = 3,
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Species richness")

axis(2, cex.axis = 1.5)

print("Finished TIME plots!")
print("Starting SPACE plots...")
## SPACE
for(i in 1:13){
  print(paste0("Progress: ", round(i/30*100, 0), "% finished."))
  x.seq <- seq( from=min(d_space$Forest.cover), to = max(d_space$Forest.cover), length.out = 30 )
  
  x.scaled <- (x.seq-mean(d_space$Forest.cover))/sd(d_space$Forest.cover)
  mu.space <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 2] + draws$b_space[, i] * x + 
                                                       mean(draws$observer[, , 2]) + mean(draws$first[,2]) +
                                                       draws$retrans_noise )) )
  
  ci.space <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 2] + draws$b_space[, i] * x + 
                                                     mean(draws$observer[, , 2]) + mean(draws$first[,2]) +
                                                     draws$retrans_noise )) )
  
  shade( ci.space, x.scaled, col = alpha("#ED432D", 0.005))
  lines( x.scaled, mu.space, col = alpha("#ED432D", 0.5), lwd = 1.5 )
}

av_mu.space <- sapply(x.scaled, function(x) exp(avS_A + avS_B * x + avS_O + avF + mean(draws$retrans_noise)))
lines(x.scaled, av_mu.space, col = "#ED432D", lwd = 5)
print("Finished SPACE plots! :-)")
dev.off()


#### ABUNDANCE! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
gc()

d <- read.csv("FinalDataset_TF_200km_ALL.csv")

d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

load("Output_TF_200km_ALL.RData")
draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "retrans_noise", "observer", "first" ))
avg_space <- summary(stanfit, pars = "avg_b_space")
avg_time <- summary(stanfit, pars = "avg_b_time")
a_space <- summary(stanfit, pars = "avg_a_space")
a_time <- summary(stanfit, pars = "avg_a_time")
obs_space <- summary(stanfit, pars = "avg_obs_space")
obs_time <- summary(stanfit, pars = "avg_obs_time")
avg_first <- summary(stanfit, pars = "avg_first_obs")

avS_B <- avg_space$summary[,1]
avT_B <- avg_time$summary[,1]
avS_A <- a_space$summary[,1]
avT_A <- a_time$summary[,1]
avS_O <- obs_space$summary[,1]
avT_O <- obs_time$summary[,1]
avF <- avg_first$summary[,1]

d_space <- d %>% filter(space.time == 2)
d_time <- d %>% filter(space.time == 1)

png("ALL_Slopes_TF_200km.png",width=40,height=30,units="cm",res=600)

par(mfrow=c(1,2),
    omi=c(0.5, 0.5, 0.5, 0.5),
    mar=c(4,5,2,2))

plot(d_time$Forest.cover, d_time$TA,
     col = alpha("#2c7bb6", 0), 
     #ylim = c(0, 60),
     pch = 3,
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Total abundance")

axis(2, cex.axis = 1.5)

print("Starting TIME plots...")
## TIME
for(i in 1:13){
  print(paste0("Progress: ", round(i/30*100, 0), "% finished."))
  x.seq <- seq( from=min(d_time$Forest.cover), to = max(d_time$Forest.cover), length.out = 30 )
  
  x.scaled <- (x.seq-mean(d_time$Forest.cover))/sd(d_time$Forest.cover)
  mu.time <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                      mean(draws$observer[, , 1]) + mean(draws$first[,1:2]) +
                                                      draws$retrans_noise )) )
  
  ci.time <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                    mean(draws$observer[, , 1]) + mean(draws$first[,1:2]) +
                                                    draws$retrans_noise )) )
  
  
  
  shade( ci.time, x.scaled, col = alpha("#2c7bb6", 0.005))
  lines( x.scaled, mu.time, col = alpha("#2c7bb6", 0.5), lwd = 1.5 )
}

av_mu.time <- sapply(x.scaled, function(x) exp(avT_A + avT_B * x + avT_O + avF + mean(draws$retrans_noise)))
lines(x.scaled, av_mu.time, col = "#2c7bb6", lwd = 5)


plot(d_space$Forest.cover, d_space$TA,
     col = alpha("#ED432D", 0), 
     #ylim = c(0, 60),
     pch = 3,
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Total abundance")

axis(2, cex.axis = 1.5)

print("Finished TIME plots!")
print("Starting SPACE plots...")
## SPACE
for(i in 1:13){
  print(paste0("Progress: ", round(i/30*100, 0), "% finished."))
  x.seq <- seq( from=min(d_space$Forest.cover), to = max(d_space$Forest.cover), length.out = 30 )
  
  x.scaled <- (x.seq-mean(d_space$Forest.cover))/sd(d_space$Forest.cover)
  mu.space <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 1] + draws$b_space[, i] * x + 
                                                       mean(draws$observer[, , 1]) + mean(draws$first[,1:2]) +
                                                       draws$retrans_noise )) )
  
  ci.space <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 1] + draws$b_space[, i] * x + 
                                                     mean(draws$observer[, , 1]) + mean(draws$first[,1:2]) +
                                                     draws$retrans_noise )) )
  
  shade( ci.space, x.scaled, col = alpha("#ED432D", 0.005))
  lines( x.scaled, mu.space, col = alpha("#ED432D", 0.5), lwd = 1.5 )
}

av_mu.space <- sapply(x.scaled, function(x) exp(avS_A + avS_B * x + avS_O + avF + mean(draws$retrans_noise)))
lines(x.scaled, av_mu.space, col = "#ED432D", lwd = 5)
print("Finished SPACE plots! :-)")
dev.off()