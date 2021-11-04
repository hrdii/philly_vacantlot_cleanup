## Meta-information ##########----------
## Author: Hardika Dayalani (dayalani@rand.org)
## Creation: 2018-01-05 for Criminal Justice Data Analysis Course
## Description: Cleanup database for Crime Incidents in Philadelphia

## Environment Setup ##########----------
remove( list= objects() )
options( stringsAsFactors= FALSE)

## Load libraries 
library(sqldf)
library(lubridate)
library(rgdal)
library(rgeos)
library(sp)

## Extracting & Wrangling ##########---------- 

## Scan first 5 lines of crime incident database
scan(what="",file="data/Crime/incidents_part1_part2.csv",nlines=5,sep=",")


infile  <- file("data/Crime/incidents_part1_part2.csv","r")
outfile <- file("data/Crime/incidents_part1_part2_clean.csv", 'w')
#   pull in all the lines from the file
b <- readLines(infile)
#   close the file
close(infile)

## Find rows with commas in text that might throw off .csv format
c <- sapply(gregexpr(",",b),length)
table(c) ## we see most lines have 16 commas but some have 17

## cleaning commas
i <- which(c==17)
b[1]
b[i[1:6]]
b[i] <- gsub("(.*[,\"].*),(.*[\"].*)","\\1 \\2",b[i])

## Write database
writeLines(b, con=outfile)
flush(outfile)
close(outfile)

## Building database ##########---------- 

## reading in cleaned Philly Crime Incidents data 
d <- read.table("data/Crime/incidents_part1_part2_clean.csv",sep=",",nrows=5,header=TRUE)

## need to connect to the database one time per session
con <- dbConnect(SQLite(), dbname="phillycrime.db")
variabletypes <- dbDataType(con, d)

sure.you.want.to.rebuild.database <- FALSE
if(sure.you.want.to.rebuild.database){ # run once to set up the database
  # connect to or create a new SQlite database
  con <- dbConnect(SQLite(), dbname="phillycrime.db")
  # remove a crime table if it already exists
  if(dbExistsTable(con, "crime")) dbRemoveTable(con, "crime")
  # import the cleaned data file into RSQLite
  dbWriteTable(con, "crime",
               "data/Crime/incidents_part1_part2_clean.csv",
               row.names=FALSE,
               header=TRUE,
               field.types=variabletypes,
               sep=",") #" RSQLite doesn't handle commas in quotes
  dbListFields(con,"crime")
  dbDisconnect(con)
}

## Fixing dates ##########---------- 

## dummy query
res <- dbSendQuery(con, "
                   SELECT *
                   FROM crime")
fetch(res, n = 10) # just the first 10 rows
dbClearResult(res)

## List of variables
dbListFields(con,"crime")

#extracting date data
res <- dbSendQuery(con, "SELECT objectid, dispatch_date 
                   FROM crime")
data <- fetch(res, n = -1)
dbClearResult(res)

## cleaning dates
head(data)
data$dispatch_date <- ymd(data$dispatch_date)
data$year <- year(data$dispatch_date)
table(data$year)
data$dispatch_date <- as.character(data$dispatch_date)

#creating a database with reformated dates
if(dbExistsTable(con,"DateFix")) dbRemoveTable(con, "DateFix")
# save a table with ID and the properly formatted date
dbWriteTable(con, "DateFix", data, row.names=FALSE)
dbListTables(con)

#renaming crime database
res <- dbSendQuery(con, "
                   ALTER TABLE crime RENAME TO crime_old")
dbClearResult(res)

dbListFields(con,"crime_old")
dbListFields(con,"datefix")

#creating new crime database using the existing crime table and fixed dates
res <- dbSendQuery(con, "
                   CREATE TABLE crime AS
                   SELECT crime_old.objectid,
                   DateFix.year,
                   DateFix.dispatch_date AS date,
                   crime_old.dc_dist,
                   crime_old.psa,
                   crime_old.ucr_general,
                   crime_old.text_general_code,
                   crime_old.location_block,
                   crime_old.lat,
                   crime_old.lng AS lon
                   FROM crime_old,DateFix
                   WHERE crime_old.objectid=DateFix.objectid")

dbClearResult(res)
dbListTables(con)

## Extracting data ##########---------- 

#subsetting crime to a year before and after the period of cleanups
res <- dbSendQuery(con, "SELECT * 
                   FROM crime
                   WHERE (year = '2011' OR 
                          year = '2012' OR
                          year = '2013' OR
                          year = '2014')")

data <- fetch(res, n = -1)
dbClearResult(res)
dbDisconnect(con)

names(data)

## Merging spatial data ##########---------- 

#reading in map with PSA boundaries 
PPDmap <- readOGR("data/Boundaries_PSA/Boundaries_PSA.shp",
                  "Boundaries_PSA")
PPDmap <- spTransform(PPDmap, CRSobj = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

#creating spatial points object containing all crime locations
i <- which(!is.na(data$lon))
e <- SpatialPoints(data[i,c("lon","lat")],
                   proj4string=PPDmap@proj4string)

## matching each polygon in PPDmap to each point
PSAlookup <- over(e, PPDmap)
PSAlookup[1:3,]
data$psa <- NA
data$psa[i] <- as.character(PSAlookup$PSA_NUM)

data$crime_num <- 1 ##dummy variable for aggregating later

## creating spatial points dataframe containing all vacant lots cleanup locations
crime_data <- SpatialPointsDataFrame(
  data[,c("lon", "lat")],   # the coordinates
  data[,c("objectid","year","date","dc_dist","psa","ucr_general","text_general_code","location_block","crime_num")])

# set the projection to be longlat since coordinates are lon/lat
proj4string(crime_data) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

# now reproject to match safety zone map projection
save(crime_data, file = "Philly Crime Incidents 2011-2014.Rdata")
