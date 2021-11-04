## Meta-information ##########----------
## Author: Hardika Dayalani (dayalani@rand.org)
## Creation: 2018-01-05 for Criminal Justice Data Analysis Course
## Description: Mapping data aggregated at PSA level

## Environment Setup ##########----------
remove( list= objects() )
options( stringsAsFactors= FALSE)

## Load libraries 
library(rgdal)
library(rgeos)
library(sp)
library(leaflet)
library(RColorBrewer)

## Merging with Map Data ##########----------

## Load aggregated data
load("Crime and Vacant Lot Cleanup.Rdata")

## reading in map with PSA boundaries
PPDmap <- readOGR("data/Boundaries_PSA/Boundaries_PSA.shp",
                  "Boundaries_PSA")
PPDmap <- spTransform(PPDmap,CRSobj = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

## Merging data
PPDmap@data <- data.frame(PPDmap@data, df[match(PPDmap@data$PSA_NUM, df$psa),])

## Create Leaflet Map ##########----------
## Color Palettes
pal1 <- colorQuantile(palette = "RdYlBu", domain = PPDmap$cleanup_count, n = 5)
pal2 <- colorQuantile(palette = "RdYlBu", domain = PPDmap$Part_II_crime_count, n = 5, reverse = TRUE)
pal3 <- colorQuantile(palette = "RdYlBu", domain = PPDmap$Property_crime_count, n = 5, reverse = TRUE)
pal4 <- colorQuantile(palette = "RdYlBu", domain = PPDmap$Violent_crime_count, n = 5, reverse = TRUE)
pal5 <- colorQuantile(palette = "RdYlBu", domain = PPDmap$crime_count, n = 5, reverse = TRUE)

leaflet(PPDmap) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -75.15536, lat = 39.99228, zoom = 11) %>%
  addPolygons(stroke = TRUE, 
              smoothFactor = 0.2, 
              fillOpacity = 1,
              fillColor = ~pal5(crime_count),
              color = "#BDBDC3",
              weight = 1, 
              popup = paste0("<strong>PSA: </strong>", 
                             PPDmap$psa, 
                             "<br> <strong>Change in Total Crime Count from 2011 to 2014: </strong>", 
                             PPDmap$crime_count), 
              group = "Total Crime") %>%
  addPolygons(stroke = TRUE, 
              smoothFactor = 0.2, 
              fillOpacity = 1,
              fillColor = ~pal4(Violent_crime_count),
              color = "#BDBDC3",
              weight = 1, 
              popup = paste0("<strong>PSA: </strong>", 
                             PPDmap$psa, 
                             "<br> <strong>Change in Violent Crime Count from 2011 to 2014: </strong>",
                             PPDmap$Violent_crime_count), 
              group = "Violent Crime") %>%
  addPolygons(stroke = TRUE, 
              smoothFactor = 0.2, 
              fillOpacity = 1,
              fillColor = ~pal3(Property_crime_count),
              color = "#BDBDC3",
              weight = 1, 
              popup = paste0("<strong>PSA: </strong>", 
                             PPDmap$psa, 
                             "<br> <strong>Change in Property Crime Count from 2011 to 2014: </strong>", 
                             PPDmap$Property_crime_count), 
              group = "Property Crime") %>%
  addPolygons(stroke = TRUE, 
              smoothFactor = 0.2, 
              fillOpacity = 1,
              fillColor = ~pal2(Part_II_crime_count),
              color = "#BDBDC3",
              weight = 1, 
              popup = paste0("<strong>PSA: </strong>", 
                             PPDmap$psa, 
                             "<br> <strong>Change in Part II Crime Count from 2011 to 2014: </strong>", 
                             PPDmap$Part_II_crime_count), 
              group = "Part II Crime") %>%
  addPolygons(stroke = TRUE, 
              smoothFactor = 0.2, 
              fillOpacity = 1,
              fillColor = ~pal1(cleanup_count),
              color = "#BDBDC3",
              weight = 1, 
              popup = paste0("<strong>PSA: </strong>", 
                             PPDmap$psa, 
                             "<br> <strong>Vacant Lot Cleanup Counts: </strong>", 
                             PPDmap$cleanup_count), 
              group = "Vacant Lot Cleanups") %>%
  addLegend("bottomright", 
            colors = c(brewer.pal(5, "RdYlBu"), "#808080"),
            labels = c("Undesirable", " ", "Morderate", " ","Desireable","Data Not Available"),  
            opacity = 1) %>%
  addLayersControl(
    baseGroups = c("Total Crime", "Violent Crime", "Property Crime","Part II Crime","Vacant Lot Cleanups"),
    position = "topright",
    options = layersControlOptions(collapsed = FALSE))
