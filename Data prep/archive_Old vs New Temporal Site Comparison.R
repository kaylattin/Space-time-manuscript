

# Load in libraries
library(sp)
library(sf)
library(rgdal)
library(tidyverse)
library(rgeos)
library(openxlsx)

# Note - run through this code twice: once for forest bird richness, and another for open bird richness
# Then proceed to code 05 to compile the final datasets and match total abundance datasets accordingly
setwd("~/space-for-time/Derived data products")

# Load in data
bbs <- read.csv("BaseDataset_RF.csv", header = T) # Loading in base dataset for forest bird richness
bbs$Transect <- paste(bbs$RouteNumber, bbs$Year, sep = ".") # Re-make Transect id column

bbs$rte_id <- as.integer(as.factor(bbs$RouteNumber)) # Set up index for routes
rtes <- bbs %>% distinct(RouteNumber, rte_id) # Get a dataframe for all distinct bbs routes
nrtes <- length(unique(bbs$rte_id)) # Get list of unique bbs routes


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

# Update temporal sites with forest cover information
t <- read.csv("300km_TemporalSites.csv")
temporal <- rtes %>% filter(RouteNumber %in% t$RouteNumber)
write.csv(temporal, "300km_TemporalSites.csv")

### Trying the automatic way
d <- read.csv("~/space-for-time/FinalDataset_RF_300km.csv")
spatial <- d %>% filter(space.time == 2) %>% distinct(RouteNumber)
subset.list <- vector("list")

#filter for spatial site
#filter for uniquie ref/temporal sites to get an x long dataframe where x = num of temporal sites matched 
#then rng from 1 to x
# select rng row from the df, extract it, place it into a list
#repeat for all spatial sites
# then rbind the list?
for(i in spatial$RouteNumber){
  df <- d %>% filter(RouteNumber == i)
  num <- sample(1:nrow(df), 1)
  
  subset.list[[paste(i)]] <- df[num,]
  
}

spatial_subset <- do.call("rbind", subset.list)
