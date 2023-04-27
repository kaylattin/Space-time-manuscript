##################################################################################
##################################################################################
###                                                                            ###
###  R CODE: BIRD-FOREST COVER REGRESSION PLOTS ACROSS SPACE-TIME COMPARISONS  ###
###                                                                            ###
##################################################################################
##################################################################################

## Load in libaries
library(tidyverse)
library(cmdstanr)
library(rstan)
library(scales)
library(rethinking)
library(Cairo)

rm(list = ls()) # Clean environment
gc() # Clean garbage

setwd("~/Space-time-manuscript")

##################################################################
##                       SPECIES RICHNESS                       ##
##################################################################

d <- read.csv("~/Space-time-manuscript/FinalDataset_ROsub_ALL.csv") # Load in dataset
d_space <- d %>% filter(space.time == 2) # Space
d_time <- d %>% filter(space.time == 1) # Time

d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

load("~/Space-time-manuscript/Output_ROsub_ALL.RData") #  Load in RStan object

# Extract draws and parameter summaries
draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "retrans_noise", "observer", "first"))
avg_space <- summary(stanfit, pars = "avg_b_space")
avg_time <- summary(stanfit, pars = "avg_b_time")
a_space <- summary(stanfit, pars = "avg_a_space")
a_time <- summary(stanfit, pars = "avg_a_time")
obs_space <- summary(stanfit, pars = "avg_obs_space")
obs_time <- summary(stanfit, pars = "avg_obs_time")
avg_first <- summary(stanfit, pars = "avg_first_obs")
a <- summary(stanfit, pars = "a")

# Assign average parameter values
avS_B <- avg_space$summary[,1]
avT_B <- avg_time$summary[,1]
avS_A <- a_space$summary[,1]
avT_A <- a_time$summary[,1]
avS_O <- obs_space$summary[,1]
avT_O <- obs_time$summary[,1]
avF <- avg_first$summary[,1]

# Residuals
y <- d$Richness_ALL # observed y
y_rep <- as.data.frame(stanfit, pars = "y_rep") # y predictions

resids <- vector("list")
r <- vector()

for(i in 1:1089){
  resids[[i]] <- as.data.frame(mean(y[i] - y_rep[,i])) # calculate residuals for each y_rep draw
}

avg_resids <- mean(unlist(resids)) # average residuals

##------------------------
##  Plotting Figure in R  
##------------------------

Cairo(file="ALL_Slopes_ROsub.png", 
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

# Initialize plot space for time
plot(d_time$Forest.cover, d_time$Richness_ALL,
     col = alpha("#2c7bb6", 0), 
     pch = 3,
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     ylim = c(0,100),
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Species richness")

axis(2, cex.axis = 1.5)

# Plot estimated mean regression lines using fake input data (similar method to McElreath, 2020)

print("Starting TIME plots...")

## TIME
for(i in 1:30){
  print(paste0("Progress: ", round(i/30*100, 0), "% finished."))
  x.seq <- seq( from=min(d_time$Forest.cover), to = max(d_time$Forest.cover), length.out = 200 )
  
  x.scaled <- (x.seq-mean(d_time$Forest.cover))/sd(d_time$Forest.cover)
  mu.time <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                      mean(draws$observer[, , 1]) + mean(draws$first[,1]) +
                                                      draws$retrans_noise )*exp(avg_resids)) )
  
  ci.time <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                    mean(draws$observer[, , 1]) + mean(draws$first[,1]) +
                                                    draws$retrans_noise )*exp(avg_resids)) )
  
  
  
  shade( ci.time, x.scaled, col = "#25349403")
  lines( x.scaled, mu.time, col = "#25349466", lwd = 1.5 )
}

print("Finished TIME plots!")

print("Starting SPACE plots...")

# Initialize plot space for space
plot(d_space$Forest.cover, d_space$Richness_ALL,
     col = alpha("#ED432D", 0), 
     pch = 3,
     ylim = c(0,100),
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Species richness")

axis(2, cex.axis = 1.5)

for(i in 1:30){
  
  print(paste0("Progress: ", round(i/30*100, 0), "% finished."))
  x.seq <- seq( from=min(d_space$Forest.cover), to = max(d_space$Forest.cover), length.out = 200 )
  
  x.scaled <- (x.seq-mean(d_space$Forest.cover))/sd(d_space$Forest.cover)
  mu.space <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 2] + draws$b_space[, i] * x + 
                                                      mean(draws$observer[, , 2]) + mean(draws$first[,2]) +
                                                      draws$retrans_noise )*exp(avg_resids)) )
  
  ci.space <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 2] + draws$b_space[, i] * x + 
                                                    mean(draws$observer[, , 2]) + mean(draws$first[,2]) +
                                                    draws$retrans_noise )*exp(avg_resids)) )
  
  shade( ci.space, x.scaled, col = "#25349403")
  lines( x.scaled, mu.space, col = "#25349466", lwd = 1.5 )
  
}

print("Finished SPACE plots! :-)")

dev.off()

## Plot mean relationship summarized across all space-time comparisons, separately in foreground
## For further overlaying and image processing in ClipStudio...

png(file="ALL_Slopes_RO_fg.png", 
    type="cairo",
    units="cm", 
    width=40, 
    height=30, 
    bg = "transparent",
    res=600)

par(mfrow=c(1,2),
    omi=c(0.5, 0.5, 0.5, 0.5),
    mar=c(4,5,2,2))

## Temporal mean slope
plot(d_time$Forest.cover, d_time$Richness_ALL, 
     col = alpha("#2c7bb6", 0), 
     pch = 3,
     ylim = c(0,100),
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Species richness")

axis(2, cex.axis = 1.5)

av_mu.time <- sapply(x.scaled, function(x) exp(avT_A + avT_B * x + avT_O + avF + mean(draws$retrans_noise))) # Average bird-forest cover relationship averaged across all 30 comparisons ... 
lines(x.scaled, av_mu.time, col = "#253494", lwd = 5)

## Spatial mean slope
plot(d_space$Forest.cover, d_space$Richness_ALL, 
     col = alpha("#2c7bb6", 0), 
     pch = 3,
     ylim = c(0,100),
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Species richness")

axis(2, cex.axis = 1.5)

av_mu.space <- sapply(x.scaled, function(x) exp(avS_A + avS_B * x + avS_O + avF + mean(draws$retrans_noise))) # Average bird-forest cover relationship averaged across all 30 comparisons ... 
lines(x.scaled, av_mu.space, col = "#253494", lwd = 5, lty = 2)

dev.off()

### ----------------------------------------------------------------------------

#################################################################
##                       TOTAL ABUNDANCE                       ##
#################################################################

rm(list = ls())
gc()

d <- read.csv("~/Space-time-manuscript/FinalDataset_TOsub_ALL.csv")

d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

load("~/Space-time-manuscript/Output_TOsub_ALL.RData")
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

# Residuals
y <- d$TA_ALL # observed y
y_rep <- as.data.frame(stanfit, pars = "y_rep") # y predictions

resids <- vector("list")
r <- vector()

for(i in 1:1089){
  
  resids[[i]] <- as.data.frame(mean(y[i] - y_rep[,i])) # calculate residuals for each y_rep draw
  
}

avg_resids <- mean(unlist(resids)) # average residuals

Cairo(file="ALL_Slopes_TOsub.png", 
      type="png",
      units="cm", 
      width=40, 
      bg = "white",
      height=30, 
      dpi=600)

par(mfrow=c(1,2),
    omi=c(0.5, 0.5, 0.5, 0.5),
    mar=c(4,5,2,2))

plot(d_time$Forest.cover, d_time$TA_ALL,
     col = alpha("#2c7bb6", 0), 
     ylim = c(0,150),
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
                                                      mean(draws$observer[, , 1]) + mean(draws$first[,1]) +
                                                      draws$retrans_noise )*exp(avg_resids)) )
  
  ci.time <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                    mean(draws$observer[, , 1]) + mean(draws$first[,1]) +
                                                    draws$retrans_noise )*exp(avg_resids)) )
  
  
  
  shade( ci.time, x.scaled, col = "#31a35403")
  lines( x.scaled, mu.time, col = "#31a35480", lwd = 1.5 )
}

print("Finished TIME plots!")
print("Starting SPACE plots...")

plot(d_space$Forest.cover, d_space$TA_ALL,
     col = alpha("#ED432D", 0), 
     ylim = c(0,150),
     pch = 3,
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Total abundance")

axis(2, cex.axis = 1.5)

## SPACE
for(i in 1:30){
  print(paste0("Progress: ", round(i/30*100, 0), "% finished."))
  x.seq <- seq( from=min(d_space$Forest.cover), to = max(d_space$Forest.cover), length.out = 200 )
  
  x.scaled <- (x.seq-mean(d_space$Forest.cover))/sd(d_space$Forest.cover)
  mu.space <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 2] + draws$b_space[, i] * x + 
                                                       mean(draws$observer[, , 2]) + mean(draws$first[,2]) +
                                                       draws$retrans_noise )*exp(avg_resids)) )
  
  ci.space <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 2] + draws$b_space[, i] * x + 
                                                     mean(draws$observer[, , 2]) + mean(draws$first[,2]) +
                                                     draws$retrans_noise )*exp(avg_resids)) )
  
  shade( ci.space, x.scaled, col = "#31a35403")
  lines( x.scaled, mu.space, col = "#31a35480", lwd = 1.5 )
}

print("Finished SPACE plots! :-)")
dev.off()

png(file="ALL_Slopes_TO_fg.png", 
      type="cairo",
      units="cm", 
      width=40, 
      height=30, 
      bg = "transparent",
      res=600)

par(mfrow=c(1,2),
    omi=c(0.5, 0.5, 0.5, 0.5),
    mar=c(4,5,2,2))

plot(d_time$Forest.cover, d_time$TA_ALL,
     col = alpha("#2c7bb6", 0),
     ylim = c(0,150),
     pch = 3,
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Total abundance")

axis(2, cex.axis = 1.5)

av_mu.time <- sapply(x.scaled, function(x) exp(avT_A + avT_B * x + avT_O + avF + mean(draws$retrans_noise)))
lines(x.scaled, av_mu.time, col = "#31a354", lwd = 5)

plot(d_space$Forest.cover, d_space$TA_ALL,
     col = alpha("#2c7bb6", 0), 
     ylim = c(0,150),
     pch = 3,
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Total abundance")

axis(2, cex.axis = 1.5)
av_mu.space <- sapply(x.scaled, function(x) exp(avS_A + avS_B * x + avS_O + avF + mean(draws$retrans_noise)))
lines(x.scaled, av_mu.space, col = "#31a354", lwd = 5, lty=2)

dev.off()

### END OF CODE ---------------------------------------------------------------


