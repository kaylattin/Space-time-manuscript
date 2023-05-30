# Load in library
library(tidyverse)
library(sp)
library(sf)

setwd("~/Space-time-manuscript/Derived data products")

# Load in initial files
Species <- read.csv("BBS_SpeciesList.csv")
Species <- Species %>% select(AOU, English_Common_Name)
Obs <- read.csv("BBS_Weather.csv", header=T)

# -----------------------------------------------------------------------------------------------------------
# --------------------------#
#      PREP RAW BBS DATA    | 
# -------------------------#

# Load in raw 2019 BBS data
d1 <- read.csv("fifty1.csv", header = T)
d2 <- read.csv("fifty2.csv", header = T)
d3 <- read.csv("fifty3.csv", header = T)
d4 <- read.csv("fifty4.csv", header = T)
d5 <- read.csv("fifty5.csv", header = T)
d6 <- read.csv("fifty6.csv", header = T)
d7 <- read.csv("fifty7.csv", header = T)
d8 <- read.csv("fifty8.csv", header = T)
d9 <- read.csv("fifty9.csv", header = T)
d10  <- read.csv("fifty10.csv",header = T)

# Rename column in d8 so column names match across dataframes
names(d8)[names(d8)=="statenum"] <- "StateNum"

# Create one dataframe
d <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10)

# Create RouteNumber field, combining 'Route' and 'StateNum' fields
d$Placeholder <- paste(d$StateNum, d$Route, sep=".") # create unique placeholder ID for statenum + route
r <- distinct(d, StateNum, Route, .keep_all = TRUE) # Create another df to get a list of unique Route & StateNum's

# Convert Route to a RouteNum that combines StateNum and the individual Route within the state
# i.e. Statenum = 04 and Route = 001 becomes RouteNumber = 4001
# StateNum goes up to 2 digits and Route goes up to 3 digits
nrows <- length(r$Route)
  
for(i in 1:nrows) {
  print(paste0("Progress: ", round(i/nrows*100, 2), "% finished."))
  if(r$Route[i] < 100 & r$Route[i] >= 10) {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep = "0")
  } 
  else if(r$Route[i] < 10) {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep = "00")
  }
  else {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep = "")
  }
}
  
r <- r %>% select(Placeholder, RouteNumber) # Select out the new field
d <- merge(d, r, by = "Placeholder") # Merge with original df
d$Transect <- paste(d$RouteNumber, d$Year, sep=".") # Create unique transect column using RouteNumber
d <- select(d, c(CountryNum, StateNum, Route, Year, AOU, RouteNumber, Transect, Stop1, Stop2, Stop3, Stop4, Stop5, Stop6, Stop7, Stop8, Stop9, Stop10, Stop11)) 
d <- merge(d, Species, by = "AOU") # Match common bird names by AOU

write.csv(d, "rawdata.csv")

# -----------------------------------------------------------------------------------------------------------
# -------------------------#
#    CLEAN UP SPECIES     | 
# ------------------------#

d <- read.csv("rawdata.csv")

# Merge subspecies and hybrids into 1 species category for numerous species 
d$English_Common_Name <- gsub(".+? Yellow-rumped Warbler", "Yellow-rumped Warbler", d$English_Common_Name)
d$English_Common_Name <- gsub(".+? Northern Flicker", "Northern Flicker", d$English_Common_Name)
d$English_Common_Name <- gsub(".+? Dark-eyed Junco", "Dark-eyed Junco", d$English_Common_Name)
d$English_Common_Name <- gsub("African Collared Dove .+?", "African Collared Dove", d$English_Common_Name)
d$English_Common_Name <- gsub(".+? Great Blue Heron", "Great Blue Heron", d$English_Common_Name)
d$English_Common_Name<- gsub("Black Brant", "Brant", d$English_Common_Name)

# Remove unid. observations and hybrids
d <- d %>%  filter(!grepl('unid.|hybrid', English_Common_Name))

# Move into long format
d_long <- reshape(d, v.names = "Count", varying = 9:19, timevar = "Stop", times = names(d)[9:19], direction='long')

# Get rid of extra text string in the Stop field
d_long$Stop <- str_remove(d_long$Stop, "Stop")


write.csv(d_long, "d_long.csv")

# -----------------------------------------------------------------------------------------------------------
# ---------------------------#
#   FOREST vs OPEN BIRDS    | 
# --------------------------#
# Repeat the following code twice: once to get a base dataset for forest birds, and another to get a base dataset for open habitat birds


# Load csv file back in
d_long <- read.csv("d_long.csv")


# Read in forest bird guild designations
forestcodes <- read.csv("ForestCodes.csv", header = T)
forestcodes <- forestcodes %>% dplyr::select(English_Common_Name, new.status) %>% distinct(English_Common_Name, new.status)

# Merge forest codes into dd_long
df <- merge(d_long, forestcodes, by = "English_Common_Name")

## Select for either forest or open birds
#df <- df %>% filter(new.status == "F") ## if doing forest birds at forest stops
df <- df %>% filter(new.status == c("O")) ## if doing open birds at open stops

# Write to csv file to save progress
write.csv(df, "d_long_open.csv")


# -----------------------------------------------------------------------------------------------------------
# ---------------------------#
#   SPECIES RESPONSES        | 
# --------------------------#

df <- read.csv("d_long_open.csv")
df$Transect <- paste(df$RouteNumber, df$Year, sep=".")
# Because of renaming of duplicate species (as subspecies or hybrids) above, counts for the same species can appear twice but not in the same record
# e.g., Northern Flicker, Yellow-Rumped Warbler, and Dark-eyed Junco might have more than 1 record per stop

df1 <- df %>% group_by(Transect, RouteNumber, Year, CountryNum, English_Common_Name) %>% summarize(Count = sum(Count))

# Calculate different arrays for richness and abundance
richness <- df1 %>% group_by(Transect, RouteNumber, Year, CountryNum) %>% summarize(Richness_ALL = n_distinct(which(Count >= 1)))
TA <- df1 %>% group_by(Transect, RouteNumber, Year, CountryNum) %>% summarize(TA_ALL = sum(Count))


write.csv(richness, "d_RO.csv")
write.csv(TA, "d_TO.csv")

# -----------------------------------------------------------------------------------------------------------
# -----------------------------#
#    OBSERVERS & WEATHER       | 
# ----------------------------#

# Create RouteNumber field, combining 'Route' and 'StateNum' fields
Obs$Placeholder <- paste(Obs$StateNum, Obs$Route, sep=".")  # Create unique Placeholder ID for statenum + route
r <- distinct(Obs, StateNum, Route, .keep_all = TRUE) # Create another df to get a list of unique Route & StateNum's
r <- r %>% select (StateNum, Route, Placeholder) # Select columns of interest

# Convert Route to a RouteNum that combines State/Province number and the individual Route within the state
# i.e. Statenum = 04 and Route = 001 becomes RouteNumber = 4001
# StateNum goes up to 2 digits and Route goes up to 3 digits
nrows <- length(r$Route)

for(i in 1:nrows) {
  print(paste0("Progress: ", round(i/nrows*100, 2), "% finished."))
  if(r$Route[i] < 100 & r$Route[i] >= 10) {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep="0")
  } 
  else if(r$Route[i] < 10) {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep="00")
  }
  else {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep="")
  }
  
}

Obs <- merge(Obs, r, by = "Placeholder", all.x = FALSE)
Obs$Transect <- paste(Obs$RouteNumber, Obs$Year, sep=".")


## Identify whether a route was an observer's first ever route in the bbs
Obs_id <- as.vector(unlist(Obs %>% distinct(ObsN)))
Obs_list <- vector("list") # Initialize list

for( i in Obs_id ){
  # Filter for all observations done by an observer in the whole history of bbs
  o <- Obs %>% filter(ObsN == i )
  nrow <- nrow(o)
  min <- min(o$Year)
  max <- max(o$Year)
  
  for( n in 1:nrow ){
    # Find the first year the observer surveyed for bbs and mark it as a first observation
    if( o$Year[n] == min ){
      o$FirstObs[n] = 2
    }else{
      o$FirstObs[n] = 1
    }
    
  }
  Obs_list[[paste(i)]] <- o
}


Obs <- do.call("rbind", Obs_list) # Rbind into a dataframe
Obs_clean <- Obs %>% dplyr::select(c(Transect, ObsN, StartWind, RunType, FirstObs)) # Select out columns of interest
Obs_clean <- Obs_clean[!duplicated(Obs_clean$Transect), ]  # Remove duplicated transects - some had shared observers, I'll just go with 1


richness$Transect <- paste(richness$RouteNumber, richness$Year, sep=".") # Re-make transect column of base dataset, sometimes it gets messed up
TA$Transect <- paste(TA$RouteNumber, TA$Year, sep=".") 

richness <- merge(richness, Obs_clean, by = "Transect", all.x = FALSE)   # Merge together
richness <- richness %>% filter(Year >= 2000) # Select years >= 2000


TA <- merge(TA, Obs_clean, by = "Transect", all.x = FALSE)   # Merge together
TA <- TA %>% filter(Year >= 2000) # Select years >= 2000

# -----------------------------------------------------------------------------------------------------------
# -----------------------------#
#        FOREST COVER          | ------------------------------------------------------------------------------------------
# ----------------------------#
#forest <- read.csv("ForestCover.csv")

#change <- forest %>% select(rte, change)
#cover <- select(forest, -c(change))

# reformat to long
#forest_long <- reshape(cover, v.names="Forest cover", varying = 2:21, timevar="year", times=names(cover)[2:21],direction='long')
#write.csv(forest_long, "ForestCoverLong.csv")

forest <- read.csv("ForestCoverLong.csv")

forest$Transect <- paste(forest$rte, forest$year, sep=".")
forest <- select(forest, -c(X, rte, year, id))

richness_final<- merge(richness, forest, by = "Transect", all.x = FALSE) # Merge forest data for each route into base dataset
TA_final<- merge(TA, forest, by = "Transect", all.x = FALSE) # Merge forest data for each route into base dataset

# Save final base dataset (repeat 4 times for richness-forest bird, richness-open bird, abundance-forest bird, abundance-open bird)
write.csv(richness_final, "BaseDataset_RF.csv")
write.csv(TA_final, "BaseDataset_TF.csv")


