############################################################################
############################################################################
###                                                                      ###
###       R CODE: EVALUATING NON-INDEPENDENCE OF SPATIAL COMPONENT       ###
###                                                                      ###
############################################################################
############################################################################

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

# Load in dataset
d <- read.csv("~/Space-time-manuscript/FinalDataset_RFsub_ALL.csv")
d$Region <- as.integer(as.factor(d$ref)) # Create integer categorical for 'region' or space-time comparison
d_space <- d %>% filter(space.time == 2)

##-----------------------------------------------
##  Pair-wise frequency of shared spatial sites  
##-----------------------------------------------

# For each pair of spatial region i, find the corresponding list of spatial sites (list of lists)
sites <- vector("list") # Initialize list

for(i in 1:30){
  
  d1 <- as.vector(d_space %>% filter(Region == i))
  sites[[i]] <- as.character(d1[,9])
  
}

# Rename lists so we're able to feed in to next step (stack())
names(sites) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
                  "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30")

# Cross-comparison
crossprod(table(stack(sites)))

listIntersect <- function(inList) {
  X <- crossprod(table(stack(inList)))
  X[lower.tri(X)] <- NA
  diag(X) <- NA
  out <- na.omit(data.frame(as.table(X)))
  out[order(out$ind),]
}

# Frequency of matched sites
pairs <- listIntersect(sites)
pairs$ind <- as.numeric(pairs$ind)
pairs$ind.1 <- as.numeric(pairs$ind.1)


##-----------------------------------------------
##  Pair-wise difference in mean spatial slopes  
##-----------------------------------------------

# Extract spatial slopes
load("Output_RFsub_ALL.RData")
b_space <- summary(stanfit, pars = "b_space") # spatial slopes
b <- b_space$summary[,1]

# For each combination pair, calculate difference in avg. spatial slopes
pairs$slope_diff <- NA # Initialize new column

# Initialize list
slope_diff <- as.numeric(vector("list"))

for(i in 1:nrow(pairs)){
  
  k = pairs[i, 2] # Get spatial site 1
  n = pairs[i, 1] # Get spatial site 2
  
  b1 <- b[k]
  b2 <- b[n]
  
  slope_diff[i] = as.numeric(b1-b2)
  
}

pairs$slope_diff <- as.numeric(paste(slope_diff))

##--------------------------------------------------------------------
##  No. shared spatial sites vs. difference in spatial slopes : Plot  
##--------------------------------------------------------------------

Cairo(file="Spatial_site_non-independence.png", 
      type="png",
      units="cm", 
      width=40, 
      height=40, 
      gamma = getOption("gamma"),
      bg = "white",
      dpi=600)

par(mar=c(6,7,4,3) + 0.0001)

plot(pairs$slope_diff, pairs$Freq,
     ylab = "Number of shared spatial sites between\n pair of space-time comparisons",
     xlab = "Pair-wise difference in mean spatial slopes",
     col = alpha("#ED432D", 0.40),
     pch = 19,
     cex = 3,
     cex.axis = 1.5, cex.lab = 2, 
      )

dev.off()
