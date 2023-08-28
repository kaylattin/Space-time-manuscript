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

##################################################################
##                       SPECIES RICHNESS                       ##
##################################################################

d <- read.csv("FINAL_REVISED_DATASET_RF.csv") # Load in dataset
d_space <- d %>% filter(space.time == 2) # Space
d_time <- d %>% filter(space.time == 1) # Time

d$Region <- as.integer(as.factor(d$ref)) # Create integer categorical for each space-time comparison

d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

load("Output_FINAL_REVISED_RF.RData") #  Load in RStan object

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

tiff(file="FIGURE 4_FINAL.tiff",
      units="px", 
      width=5600, 
      height=4000,
      res = 600,
     compression = "lzw")

par(mfrow=c(2,4),
    omi=c(0.1, 0.1, 0.1, 0.1),
    mar=c(5,4.5,2.5,1), 
    xpd=FALSE)

# Initialize plot space for time
plot(d_time$Forest.cover, d_time$Richness_ALL,
     col = alpha("#2c7bb6", 0), 
     pch = 3,
     yaxt = "n", cex.axis = 1.3, cex.lab = 1.5, 
     ylim = c(0,40),
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Species richness")

title("A - Forest birds", cex.main = 2, font.main = 2, adj = 0, line= 1)
axis(2, cex.axis = 1.3)

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
  lines( x.scaled, mu.time, col = "#25349440", lwd = 1.5 )
}

print("Finished TIME plots!")

av_mu.time <- sapply(x.scaled, function(x) exp(avT_A + avT_B * x + avT_O + avT_F + mean(draws$retrans_noise))) # Average bird-forest cover relationship averaged across all 30 comparisons ... 
lines(x.scaled, av_mu.time, col = "#253494", lwd = 3)

print("Starting SPACE plots...")

# Initialize plot space for space
plot(d_space$Forest.cover, d_space$Richness_ALL,
     col = alpha("#ED432D", 0), 
     pch = 3,
     ylim = c(0,40),
     yaxt = "n", cex.axis = 1.3, cex.lab = 1.5, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Species richness")

axis(2, cex.axis = 1.3)

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
  lines( x.scaled, mu.space, col = "#25349430", lwd = 1.5 )
  
}

print("Finished SPACE plots! :-)")

av_mu.space <- sapply(x.scaled, function(x) exp(avS_A + avS_B * x + avS_O + avS_F + mean(draws$retrans_noise))) # Average bird-forest cover relationship averaged across all 30 comparisons ... 
lines(x.scaled, av_mu.space, col = "#253494", lwd = 3, lty = 3)

#################################################################
##                       TOTAL ABUNDANCE                       ##
#################################################################

rm(list = ls())
gc()

d <- read.csv("FINAL_REVISED_DATASET_TF.csv")
d_space <- d %>% filter(space.time == 2) # Space
d_time <- d %>% filter(space.time == 1) # Time

d$Region <- as.integer(as.factor(d$ref)) # Create integer categorical for each space-time comparison
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

load("Output_FINAL_REVISED_TF.RData")
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

plot(d_time$Forest.cover, d_time$TA_ALL,
     col = alpha("#2c7bb6", 0), 
     ylim = c(0,120),
     pch = 3,
     yaxt = "n", cex.axis = 1.3, cex.lab = 1.5, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Total abundance")

axis(2, cex.axis = 1.3)

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
  lines( x.scaled, mu.time, col = "#31a35430", lwd = 1.5 )
}

print("Finished TIME plots!")

av_mu.time <- sapply(x.scaled, function(x) exp(avT_A + avT_B * x + avT_O + avT_F + mean(draws$retrans_noise)))
lines(x.scaled, av_mu.time, col = "#31a354", lwd = 3)

print("Starting SPACE plots...")

plot(d_space$Forest.cover, d_space$TA_ALL,
     col = alpha("#ED432D", 0), 
     ylim = c(0,120),
     pch = 3,
     yaxt = "n", cex.axis = 1.3, cex.lab = 1.5, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Total abundance")

axis(2, cex.axis = 1.3)

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
  lines( x.scaled, mu.space, col = "#31a35430", lwd = 1.5 )
}

print("Finished SPACE plots! :-)")

av_mu.space <- sapply(x.scaled, function(x) exp(avS_A + avS_B * x + avS_O + avS_F + mean(draws$retrans_noise)))
lines(x.scaled, av_mu.space, col = "#31a354", lwd = 3, lty=3)


### ----------------------------------------------------------------------------


##################################################################
##                       SPECIES RICHNESS                       ##
##################################################################


d <- read.csv("FINAL_REVISED_DATASET_RO.csv") # Load in dataset
d_space <- d %>% filter(space.time == 2) # Space
d_time <- d %>% filter(space.time == 1) # Time

d$Region <- as.integer(as.factor(d$ref)) # Create integer categorical for each space-time comparison

d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

load("Output_FINAL_REVISED_RO.RData") #  Load in RStan object

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

# Initialize plot space for time
plot(d_time$Forest.cover, d_time$Richness_ALL,
     col = alpha("#2c7bb6", 0), 
     pch = 3,
     yaxt = "n", cex.axis = 1.3, cex.lab = 1.5, 
     ylim = c(0,20),
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Species richness")

title("B - Open-habitat birds", cex.main = 2, font.main = 2, adj = 0, line= 1)
axis(2, cex.axis = 1.3)

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
  
  
  
  shade( ci.time, x.scaled, col = "#41b6c403")
  lines( x.scaled, mu.time, col = "#41b6c430", lwd = 1.5 )
}

print("Finished TIME plots!")

av_mu.time <- sapply(x.scaled, function(x) exp(avT_A + avT_B * x + avT_O + avT_F + mean(draws$retrans_noise))) # Average bird-forest cover relationship averaged across all 30 comparisons ... 
lines(x.scaled, av_mu.time, col = "#41b6c4", lwd = 3)

print("Starting SPACE plots...")

# Initialize plot space for space
plot(d_space$Forest.cover, d_space$Richness_ALL,
     col = alpha("#ED432D", 0), 
     pch = 3,
     ylim = c(0,20),
     yaxt = "n", cex.axis = 1.3, cex.lab = 1.5, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Species richness")

axis(2, cex.axis = 1.3)

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
  
  shade( ci.space, x.scaled, col = "#41b6c403")
  lines( x.scaled, mu.space, col = "#41b6c430", lwd = 1.5 )
  
}

print("Finished SPACE plots! :-)")

av_mu.space <- sapply(x.scaled, function(x) exp(avS_A + avS_B * x + avS_O + avS_F + mean(draws$retrans_noise))) # Average bird-forest cover relationship averaged across all 30 comparisons ... 
lines(x.scaled, av_mu.space, col = "#41b6c4", lwd = 3, lty = 3)



# ----------------------------------------------------

#################################################################
##                       TOTAL ABUNDANCE                       ##
#################################################################

d <- read.csv("FINAL_REVISED_DATASET_TO.csv")
d_space <- d %>% filter(space.time == 2) # Space
d_time <- d %>% filter(space.time == 1) # Time

d$Region <- as.integer(as.factor(d$ref)) # Create integer categorical for each space-time comparison
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

load("Output_FINAL_REVISED_TO.RData")
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


plot(d_time$Forest.cover, d_time$TA_ALL,
     col = alpha("#2c7bb6", 0), 
     ylim = c(0,50),
     pch = 3,
     yaxt = "n", cex.axis = 1.3, cex.lab = 1.5, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Total abundance")

axis(2, cex.axis = 1.3)

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
  
  
  
  shade( ci.time, x.scaled, col = "#addd8e03")
  lines( x.scaled, mu.time, col = "#addd8e30", lwd = 1.5 )
}

print("Finished TIME plots!")


av_mu.time <- sapply(x.scaled, function(x) exp(avT_A + avT_B * x + avT_O + avT_F + mean(draws$retrans_noise)))
lines(x.scaled, av_mu.time, col = "#addd8e", lwd = 3)

print("Starting SPACE plots...")

plot(d_space$Forest.cover, d_space$TA_ALL,
     col = alpha("#ED432D", 0), 
     ylim = c(0,50),
     pch = 3,
     yaxt = "n", cex.axis = 1.3, cex.lab = 1.5, 
     cex.main = 2, main = "", xlab = "Percent forest cover", ylab = "Total abundance")

axis(2, cex.axis = 1.3)

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
  
  shade( ci.space, x.scaled, col = "#addd8e03")
  lines( x.scaled, mu.space, col = "#addd8e30", lwd = 1.5 )
}

print("Finished SPACE plots! :-)")

av_mu.space <- sapply(x.scaled, function(x) exp(avS_A + avS_B * x + avS_O + avS_F + mean(draws$retrans_noise)))
lines(x.scaled, av_mu.space, col = "#addd8e", lwd = 3, lty=3)

dev.off()


### END OF CODE ---------------------------------------------------------------


