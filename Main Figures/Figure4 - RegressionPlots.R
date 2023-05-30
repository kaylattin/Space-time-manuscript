##################################################################################
##################################################################################
###                                                                            ###
###  R CODE: BIRD-FOREST COVER REGRESSION PLOTS ACROSS SPACE-TIME COMPARISONS  ###
###                                                                            ###
##################################################################################
##################################################################################

## Load in libraries
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

d <- read.csv("~/Space-time-manuscript/FINAL_REVISED_DATASET_RF.csv") # Load in dataset
d_space <- d %>% filter(space.time == 2) # Space
d_time <- d %>% filter(space.time == 1) # Time

d$Region <- as.integer(as.factor(d$ref)) # Create integer categorical for each space-time comparison

d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

load("~/Space-time-manuscript/Output_FINAL_REVISED_RF.RData") #  Load in RStan object

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

##------------------------
##  Plotting Figure in R  
##------------------------

Cairo(file="ALL_Slopes_FINAL_RF.png", 
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
     ylim = c(0,40),
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Species richness")

axis(2, cex.axis = 1.5)

# Plot estimated mean regression lines using fake input data (similar method to McElreath, 2020)

print("Starting TIME plots...")

## TIME
for(i in 1:25){
  print(paste0("Progress: ", round(i/30*100, 0), "% finished."))
  x.seq <- seq( from=min(d_time$Forest.cover), to = max(d_time$Forest.cover), length.out = 200 )
  
  x.scaled <- (x.seq-mean(d_time$Forest.cover))/sd(d_time$Forest.cover)
  mu.time <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                      mean(draws$observer[, , 1]) + mean(draws$first[,1]) +
                                                      draws$retrans_noise )) )
  
  ci.time <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                    mean(draws$observer[, , 1]) + mean(draws$first[,1]) +
                                                    draws$retrans_noise )) )
  
  
  
  shade( ci.time, x.scaled, col = "#25349403")
  lines( x.scaled, mu.time, col = "#25349466", lwd = 1.5 )
}

print("Finished TIME plots!")

print("Starting SPACE plots...")

# Initialize plot space for space
plot(d_space$Forest.cover, d_space$Richness_ALL,
     col = alpha("#ED432D", 0), 
     pch = 3,
     ylim = c(0,40),
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Species richness")

axis(2, cex.axis = 1.5)

for(i in 1:25){
  
  print(paste0("Progress: ", round(i/30*100, 0), "% finished."))
  x.seq <- seq( from=min(d_space$Forest.cover), to = max(d_space$Forest.cover), length.out = 200 )
  
  x.scaled <- (x.seq-mean(d_space$Forest.cover))/sd(d_space$Forest.cover)
  mu.space <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 2] + draws$b_space[, i] * x + 
                                                      mean(draws$observer[, , 2]) + mean(draws$first[,2]) +
                                                      draws$retrans_noise )) )
  
  ci.space <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 2] + draws$b_space[, i] * x + 
                                                    mean(draws$observer[, , 2]) + mean(draws$first[,2]) +
                                                    draws$retrans_noise )) )
  
  shade( ci.space, x.scaled, col = "#25349403")
  lines( x.scaled, mu.space, col = "#25349466", lwd = 1.5 )
  
}

print("Finished SPACE plots! :-)")

dev.off()

## Plot mean relationship summarized across all space-time comparisons, separately in foreground
## For further overlaying and image processing in ClipStudio...

png(file="ALL_Slopes_fg_RF.png", 
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
     ylim = c(0,40),
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Species richness")

axis(2, cex.axis = 1.5)

av_mu.time <- sapply(x.scaled, function(x) exp(avT_A + avT_B * x + avT_O + avT_F + mean(draws$retrans_noise))) # Average bird-forest cover relationship averaged across all 30 comparisons ... 
lines(x.scaled, av_mu.time, col = "#253494", lwd = 5)

## Spatial mean slope
plot(d_space$Forest.cover, d_space$Richness_ALL, 
     col = alpha("#2c7bb6", 0), 
     pch = 3,
     ylim = c(0,40),
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Species richness")

axis(2, cex.axis = 1.5)

av_mu.space <- sapply(x.scaled, function(x) exp(avS_A + avS_B * x + avS_O + avS_F + mean(draws$retrans_noise))) # Average bird-forest cover relationship averaged across all 30 comparisons ... 
lines(x.scaled, av_mu.space, col = "#253494", lwd = 5, lty = 2)

dev.off()

### ----------------------------------------------------------------------------

#################################################################
##                       TOTAL ABUNDANCE                       ##
#################################################################

rm(list = ls())
gc()

d <- read.csv("~/Space-time-manuscript/FINAL_REVISED_DATASET_TF.csv")
d_space <- d %>% filter(space.time == 2) # Space
d_time <- d %>% filter(space.time == 1) # Time

d$Region <- as.integer(as.factor(d$ref)) # Create integer categorical for each space-time comparison
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

load("~/Space-time-manuscript/Output_FINAL_REVISED_TF.RData")
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

Cairo(file="ALL_Slopes_FINAL_TF.png", 
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
     ylim = c(0,120),
     pch = 3,
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Total abundance")

axis(2, cex.axis = 1.5)

print("Starting TIME plots...")
## TIME
for(i in 1:25){
  print(paste0("Progress: ", round(i/30*100, 0), "% finished."))
  x.seq <- seq( from=min(d_time$Forest.cover), to = max(d_time$Forest.cover), length.out = 200 )
  
  x.scaled <- (x.seq-mean(d_time$Forest.cover))/sd(d_time$Forest.cover)
  mu.time <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                      mean(draws$observer[, , 1]) + mean(draws$first[,1]) +
                                                      draws$retrans_noise )) )
  
  ci.time <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                    mean(draws$observer[, , 1]) + mean(draws$first[,1]) +
                                                    draws$retrans_noise )) )
  
  
  
  shade( ci.time, x.scaled, col = "#31a35403")
  lines( x.scaled, mu.time, col = "#31a35480", lwd = 1.5 )
}

print("Finished TIME plots!")
print("Starting SPACE plots...")

plot(d_space$Forest.cover, d_space$TA_ALL,
     col = alpha("#ED432D", 0), 
     ylim = c(0,120),
     pch = 3,
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Total abundance")

axis(2, cex.axis = 1.5)

## SPACE
for(i in 1:25){
  print(paste0("Progress: ", round(i/30*100, 0), "% finished."))
  x.seq <- seq( from=min(d_space$Forest.cover), to = max(d_space$Forest.cover), length.out = 200 )
  
  x.scaled <- (x.seq-mean(d_space$Forest.cover))/sd(d_space$Forest.cover)
  mu.space <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 2] + draws$b_space[, i] * x + 
                                                       mean(draws$observer[, , 2]) + mean(draws$first[,2]) +
                                                       draws$retrans_noise )) )
  
  ci.space <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 2] + draws$b_space[, i] * x + 
                                                     mean(draws$observer[, , 2]) + mean(draws$first[,2]) +
                                                     draws$retrans_noise )) )
  
  shade( ci.space, x.scaled, col = "#31a35403")
  lines( x.scaled, mu.space, col = "#31a35480", lwd = 1.5 )
}

print("Finished SPACE plots! :-)")
dev.off()

png(file="ALL_Slopes_fg_TF.png", 
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
     ylim = c(0,120),
     pch = 3,
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Total abundance")

axis(2, cex.axis = 1.5)

av_mu.time <- sapply(x.scaled, function(x) exp(avT_A + avT_B * x + avT_O + avT_F + mean(draws$retrans_noise)))
lines(x.scaled, av_mu.time, col = "#31a354", lwd = 5)

plot(d_space$Forest.cover, d_space$TA_ALL,
     col = alpha("#2c7bb6", 0), 
     ylim = c(0,120),
     pch = 3,
     yaxt = "n", cex.axis = 1.5, cex.lab = 2, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Total abundance")

axis(2, cex.axis = 1.5)
av_mu.space <- sapply(x.scaled, function(x) exp(avS_A + avS_B * x + avS_O + avS_F + mean(draws$retrans_noise)))
lines(x.scaled, av_mu.space, col = "#31a354", lwd = 5, lty=2)

dev.off()

### END OF CODE ---------------------------------------------------------------


