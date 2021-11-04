## Meta-information ##########----------
## Author: Hardika Dayalani (dayalani@rand.org)
## Creation: 2018-01-05 for Criminal Justice Data Analysis Course
## Description: Cleanup database for Vacant Lot Cleanups in Philadelphia

## Environment Setup ##########----------
remove( list= objects() )
options( stringsAsFactors= FALSE)

## Load libraries
library(rgdal)
library(rgeos)
library(sp)

## Extracting & Wrangling ##########---------- 

## Read in cleanup data
cleanups <- read.csv("data/Vacant Properties/vacant_lot_cleanups.csv")
summary(cleanups)

## extracting location
cleanups$location <- as.character(cleanups$location)
cleanups$lat <- as.numeric(gsub("\\((.*)\\, .*", "\\1", cleanups$location))
cleanups$lon <- as.numeric(gsub(".*\\, (.*)\\)$", "\\1", cleanups$location))

## extracting year
cleanups$year <- as.numeric(substr(cleanups$abate_date,7,10))

## aggregating cleanups by year
cleanups$cleanup_num <- 1#this is just a dummy variable to help aggregate the cleanups
cleanup_agg <- aggregate(cleanups$cleanup_num, by = list(cleanups$year), FUN = sum, na.rm = TRUE)
#this shows us, all the recorded cleanups are in 2012 and 2013
cleanup_agg

## subsetting and cleaning data
cleanups <- cleanups[,c("lat","lon","address","zip","counc_dist","year","cleanup_num")]

## Aggregating cleanups by PSA boundaries ##########---------- 

## reading in map with PSA boundaries and plotting it
PPDmap <- readOGR("data/Boundaries_PSA/Boundaries_PSA.shp",
                  "Boundaries_PSA")
PPDmap <- spTransform(PPDmap,CRSobj = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
plot(PPDmap)

## creating spatial points object containing all vacant lots cleanup locations
i <- which(!is.na(cleanups$lon))
a <- SpatialPoints(cleanups[i,c("lon","lat")],
                   proj4string=PPDmap@proj4string)

## match each polygon in PPDmap to each point
PSAlookup <- over(a, PPDmap)
PSAlookup[1:3,]
cleanups$psa <- NA
cleanups$psa[i] <- as.character(PSAlookup$PSA_NUM)

## print cleanups aggregated by psa number in descending order
rev(sort(table(cleanups$psa)))

## creating spatial points dataframe containing all vacant lots cleanup locations
cleanups <- SpatialPointsDataFrame(
  cleanups[,c("lon", "lat")],   # the coordinates
  cleanups[,c("address","zip","counc_dist","psa","year","cleanup_num")])
# set the projection to be longlat since coordinates are lon/lat
proj4string(cleanups) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

save(cleanups,file = "Vacant Lot Cleanup 2012-13.Rdata")
