############################################################################
############################################################################
###                                                                      ###
###     R CODE: SPATIAL FOREST COVER CHANGE & BIRD RESPONSE METRICS     ###
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
library(ggpubr)

rm(list = ls()) # Clean environment
gc() # Clean garbage

setwd("~/Space-time-manuscript")

### CHECK FOREST COVER CHANGE OVER SPACE -----------------------------------------------
d <- read.csv("~/Space-time-manuscript/FINAL_REVISED_DATASET_RF.csv")
bbs <- read.csv("~/Space-time-manuscript/Derived data products/BaseDataset_RF.csv")

bbs$rte_id <- as.integer(as.factor(bbs$RouteNumber)) # Set up index for routes
rtes <- bbs %>% distinct(RouteNumber, rte_id) # Get a dataframe for all distinct bbs routes
nrtes <- length(unique(bbs$rte_id)) # Get list of unique bbs routes

# Find the new time ranges from 2000 to 2019 for routes after accounting for years with BBS data. Some routes don't have BBS data for every year in the range.
# Then, find the corresponding forest change that occurred in that new time range
for(i in 1:nrtes) {
  print(paste0("Progress: ", round(i/nrtes*100, 2), "% finished."))
  r <- bbs[which(bbs$rte_id == i),]
  n <- nrow(r)
  # Find minimum and maximum years, i.e. time range for that route
  min <- min(r$Year)
  max <- max(r$Year)
  
  for(n in 1:n) {
    # If the year on iteration n is the minimum (earliest) year for that route, then copy over its % forest cover
    if(r$Year[n] == min) {
      rtes$firstyear[i] <- r$Year[n]
      rtes$firstcover[i] <- r$Forest.cover[n]
    }
    # If the year on iteration n is the maximum (latest) year for that route, then copy over its % forest cover
    if(r$Year[n] == max){
      rtes$lastyear[i] <- r$Year[n]
      rtes$lastcover[i] <- r$Forest.cover[n]
    }
  }
}

# Find the change in % forest between first and last years with bbs data
rtes$change <- rtes$lastcover - rtes$firstcover

d_space <- d %>% filter(space.time == 2)

s <- unique(d_space$RouteNumber)

f <- rtes %>% filter(RouteNumber %in% s)


### BIRD METRICS ------------------------------------------------------------------
d <- read.csv("~/Space-time-manuscript/FINAL_REVISED_DATASET_TO.csv")

all <- unique(d$RouteNumber)

f <- read.csv("~/Space-time-manuscript/Derived data products/d_long_forest.csv")
o <- read.csv("~/Space-time-manuscript/Derived data products/d_long_open.csv")

fsp <- f %>% filter(RouteNumber %in% all)
fsp <- unique(fsp$English_Common_Name)

osp <- o %>% filter(RouteNumber %in% all)
osp <- unique(osp$English_Common_Name)

d_space <- d %>% filter(space.time == 2)
d_time <- d %>% filter(space.time == 1)

# Richness
min(d_space$Richness_ALL)
max(d_space$Richness_ALL)
mean(d_space$Richness_ALL)
median(d_space$Richness_ALL)

min(d_time$Richness_ALL)
max(d_time$Richness_ALL)
mean(d_time$Richness_ALL)
median(d_time$Richness_ALL)

# Abundance
min(d_space$TA_ALL)
max(d_space$TA_ALL)
mean(d_space$TA_ALL)
median(d_space$TA_ALL)

min(d_time$TA_ALL)
max(d_time$TA_ALL)
mean(d_time$TA_ALL)
median(d_time$TA_ALL)

