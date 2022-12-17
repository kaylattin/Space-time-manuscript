# Load in libraries
library(gfcanalysis)
library(rgdal)
library(openxlsx)
library(tidyverse)
library(sf)
library(sp)
library(raster)


setwd("~/space-for-time/Shapefiles")


d <- st_read("buffer_NA_dataset.shp") # dataset where bbs coords <= 1 km from line
d <- as(d, "Spatial")
d <- d[!duplicated(d$rteno),] # do only if using routes, not stops

elev <- raster("3DEP_Elevation_Projected.tif")
res(elev)
plot(elev)
plot(d)

crs(elev)
crs(d)

rasterOptions(progress = 'window', timer = FALSE)
exElev <- raster::extract(elev, d, fun = mean, method = "bilinear")

dElev <- cbind(d@data$rteno, exElev)
write.csv(dElev, "~/space-for-time/Derived data products/Elevation-NA_Dataset.csv")

# Match elevation to existing 300km dataset
setwd("~/space-for-time")
dElev <- read.csv("~/space-for-time/Derived data products/Elevation-NA_Dataset.csv")
d1 <- read.csv("FinalDataset_TO_200km.csv")

supp <- merge(d1, dElev, by = "RouteNumber", all.x = TRUE)
write.csv(supp, "~/space-for-time/FinalDataset_TO_200km.csv")
