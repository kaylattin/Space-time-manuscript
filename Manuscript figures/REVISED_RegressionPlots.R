## load up stuff
library(tidyverse)
library(cmdstanr)
library(rstan)
library(scales)
library(rethinking)
library(Cairo)
rm(list = ls())
gc()

setwd("~/space-for-time")

### RICHNESS! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
d <- read.csv("~/space-for-time/FinalDataset_RFsub.csv")

d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

load("~/space-for-time/Output_RFsub_FINAL.RData")
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

#png("REVISED_Slopes_RF.png",width=40,height=30,units="cm",res=600)

Cairo(file="REVISED_Slopes_RF_bg.png", 
      type="png",
      units="cm", 
      width=40, 
      height=30, 
      gamma = getOption("gamma"),
      bg = "white",
      dpi=600)

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
for(i in 1:30){
  print(paste0("Progress: ", round(i/30*100, 0), "% finished."))
  x.seq <- seq( from=min(d_time$Forest.cover), to = max(d_time$Forest.cover), length.out = 200 )
  
  x.scaled <- (x.seq-mean(d_time$Forest.cover))/sd(d_time$Forest.cover)
  mu.time <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                      mean(draws$observer[, , 1]) + mean(draws$first[,1:2]) +
                                                      draws$retrans_noise )) )
  
  ci.time <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                    mean(draws$observer[, , 1]) + mean(draws$first[,1:2]) +
                                                    draws$retrans_noise )) )
  
  
  
  shade( ci.time, x.scaled, col = "#2c7bb603")
  lines( x.scaled, mu.time, col = "#2c7bb666", lwd = 1.5 )
}



plot(d_space$Forest.cover, d_space$Richness,
     col = alpha("#ED432D", 0), 
     pch = 3,
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Species richness")

axis(2, cex.axis = 1.5)

print("Finished TIME plots!")
print("Starting SPACE plots...")
## SPACE
for(i in 1:30){
  print(paste0("Progress: ", round(i/30*100, 0), "% finished."))
  x.seq <- seq( from=min(d_space$Forest.cover), to = max(d_space$Forest.cover), length.out = 200 )
  
  x.scaled <- (x.seq-mean(d_space$Forest.cover))/sd(d_space$Forest.cover)
  mu.space <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 1] + draws$b_space[, i] * x + 
                                                      mean(draws$observer[, , 1]) + mean(draws$first[,1:2]) +
                                                      draws$retrans_noise )) )
  
  ci.space <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 1] + draws$b_space[, i] * x + 
                                                    mean(draws$observer[, , 1]) + mean(draws$first[,1:2]) +
                                                    draws$retrans_noise )) )
  
  shade( ci.space, x.scaled, col = "#ED432D03")
  lines( x.scaled, mu.space, col = "#ED432D66", lwd = 1.5 )
}
print("Finished SPACE plots! :-)")
dev.off()

Cairo(file="REVISED_Slopes_RF_fg.png", 
      type="png",
      units="cm", 
      width=40, 
      height=30, 
      gamma = getOption("gamma"),
      bg = "transparent",
      dpi=600)

par(mfrow=c(1,2),
    omi=c(0.5, 0.5, 0.5, 0.5),
    mar=c(4,5,2,2))

plot(d_time$Forest.cover, d_time$Richness, 
     col = alpha("#2c7bb6", 0), 
     pch = 3,
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Species richness")

axis(2, cex.axis = 1.5)

av_mu.time <- sapply(x.scaled, function(x) exp(avT_A + avT_B * x + avT_O + avF + mean(draws$retrans_noise)))
lines(x.scaled, av_mu.time, col = "#2c7bb6", lwd = 5)

plot(d_space$Forest.cover, d_space$Richness, 
     col = alpha("#2c7bb6", 0), 
     pch = 3,
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Species richness")

axis(2, cex.axis = 1.5)
av_mu.space <- sapply(x.scaled, function(x) exp(avS_A + avS_B * x + avS_O + avF + mean(draws$retrans_noise)))
lines(x.scaled, av_mu.space, col = "#ED432D", lwd = 5)

dev.off()

#### ABUNDANCE! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
gc()

d <- read.csv("~/space-for-time/FinalDataset_TFsub.csv")

d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

load("~/space-for-time/Output_TFsub_FINAL.RData")
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

Cairo(file="REVISED_Slopes_TF_bg.png", 
      type="png",
      units="cm", 
      width=40, 
      bg = "white",
      height=30, 
      dpi=600)

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
for(i in 1:30){
  print(paste0("Progress: ", round(i/30*100, 0), "% finished."))
  x.seq <- seq( from=min(d_time$Forest.cover), to = max(d_time$Forest.cover), length.out = 200 )
  
  x.scaled <- (x.seq-mean(d_time$Forest.cover))/sd(d_time$Forest.cover)
  mu.time <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                      mean(draws$observer[, , 1]) + mean(draws$first[,1:2]) +
                                                      draws$retrans_noise )) )
  
  ci.time <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                    mean(draws$observer[, , 1]) + mean(draws$first[,1:2]) +
                                                    draws$retrans_noise )) )
  
  
  
  shade( ci.time, x.scaled, col = "#2c7bb603")
  lines( x.scaled, mu.time, col = "#2c7bb680", lwd = 1.5 )
}


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
for(i in 1:30){
  print(paste0("Progress: ", round(i/30*100, 0), "% finished."))
  x.seq <- seq( from=min(d_space$Forest.cover), to = max(d_space$Forest.cover), length.out = 200 )
  
  x.scaled <- (x.seq-mean(d_space$Forest.cover))/sd(d_space$Forest.cover)
  mu.space <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 1] + draws$b_space[, i] * x + 
                                                       mean(draws$observer[, , 1]) + mean(draws$first[,1:2]) +
                                                       draws$retrans_noise )) )
  
  ci.space <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 1] + draws$b_space[, i] * x + 
                                                     mean(draws$observer[, , 1]) + mean(draws$first[,1:2]) +
                                                     draws$retrans_noise )) )
  
  shade( ci.space, x.scaled, col = "#ED432D03")
  lines( x.scaled, mu.space, col = "#ED432D80", lwd = 1.5 )
}

print("Finished SPACE plots! :-)")
dev.off()

Cairo(file="REVISED_Slopes_TF_fg.png", 
      type="png",
      units="cm", 
      width=40, 
      height=30, 
      gamma = getOption("gamma"),
      bg = "transparent",
      dpi=600)

par(mfrow=c(1,2),
    omi=c(0.5, 0.5, 0.5, 0.5),
    mar=c(4,5,2,2))

plot(d_time$Forest.cover, d_time$TA,
     col = alpha("#2c7bb6", 0), 
     pch = 3,
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Total abundance")

axis(2, cex.axis = 1.5)

av_mu.time <- sapply(x.scaled, function(x) exp(avT_A + avT_B * x + avT_O + avF + mean(draws$retrans_noise)))
lines(x.scaled, av_mu.time, col = "#2c7bb6", lwd = 5)

plot(d_space$Forest.cover, d_space$TA,
     col = alpha("#2c7bb6", 0), 
     pch = 3,
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Total abundance")

axis(2, cex.axis = 1.5)
av_mu.space <- sapply(x.scaled, function(x) exp(avS_A + avS_B * x + avS_O + avF + mean(draws$retrans_noise)))
lines(x.scaled, av_mu.space, col = "#ED432D", lwd = 5)

dev.off()