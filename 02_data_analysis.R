## Meta-information ##########----------
## Author: Hardika Dayalani (dayalani@rand.org)
## Creation: 2018-01-05 for Criminal Justice Data Analysis Course
## Description: Analyzing data aggregated at PSA level

## Environment Setup ##########----------
remove( list= objects() )
options( stringsAsFactors= FALSE)

## Load libraries 

## Wranging Crime Data ##########----------

## Load data
load("Philly Crime Incidents 2011-2014.Rdata")

## Define crime types
violent_crime <- c("Aggravated Assault", "Rape", "Homicide","Robbery")
property_crime <- c("Arson","Burglary","Theft","Motor Vehicle Theft")
part2_crime <- c("Narcotic","Vandalism/Criminal Mischief","Public Drunkenness","Disorderly Conduct")

## Adding crime categories
crime_data$Cat <- NA
crime_data$Cat[grep(paste(violent_crime, collapse = "|"),crime_data$text_general_code)] <- "Violent Crime"
crime_data$Cat[grep(paste(property_crime, collapse = "|"),crime_data$text_general_code)] <- "Property Crime"
crime_data$Cat[grep(paste(part2_crime, collapse = "|"),crime_data$text_general_code)] <- "Part II Crime"

## subsetting crime data by year
crime_data_2011 <- subset(crime_data, crime_data$year==2011)
crime_data_2014 <- subset(crime_data, crime_data$year==2014)

## tabulating crime by PSAs by category
crime_tab_2011 <- data.frame(table(crime_data_2011$psa,crime_data_2011$Cat))
crime_tab_2014 <- data.frame(table(crime_data_2014$psa,crime_data_2014$Cat))

## Reshaping aggregated data
crime_tab_2011 <- reshape(crime_tab_2011,timevar = "Var2", idvar = "Var1",direction = "wide")
colnames(crime_tab_2011) <- c("psa","Part_II_Crime", "Property_Crime", "Violent_Crime")

crime_tab_2014 <- reshape(crime_tab_2014,timevar = "Var2", idvar = "Var1",direction = "wide")
colnames(crime_tab_2014) <- c("psa","Part_II_Crime", "Property_Crime", "Violent_Crime")

## Merging aggregated data
i <- match(crime_tab_2011$psa,crime_tab_2014$psa)
crime_tab <- data.frame(
  psa = crime_tab_2011$psa,
  Part_II_crime_2011 = crime_tab_2011$Part_II_Crime,
  Property_crime_2011 = crime_tab_2011$Property_Crime,
  Violent_crime_2011 = crime_tab_2011$Violent_Crime
)

crime_tab$Part_II_crime_2014 <- crime_tab_2014$Part_II_Crime[i]
crime_tab$Property_crime_2014 <- crime_tab_2014$Property_Crime[i]
crime_tab$Violent_crime_2014 <- crime_tab_2014$Violent_Crime[i]

## tabulating total crime by PSAs
crime_tab_2011 <- data.frame(table(crime_data_2011$psa))
crime_tab_2014 <- data.frame(table(crime_data_2014$psa))

## Adding total crime 
i <- match(crime_tab_2011$Var1,crime_tab$psa)
crime_tab$Total_crime_2011 <- crime_tab_2011$Freq[i]

i <- match(crime_tab_2014$Var1,crime_tab$psa)
crime_tab$Total_crime_2014 <- crime_tab_2014$Freq[i]

## Calculating change in crimes 
crime_tab$Part_II_crime_diff <- (crime_tab$Part_II_crime_2014 - crime_tab$Part_II_crime_2011)
crime_tab$Property_crime_diff <- (crime_tab$Property_crime_2014 - crime_tab$Property_crime_2011)
crime_tab$Violent_crime_diff <- (crime_tab$Violent_crime_2014 - crime_tab$Violent_crime_2011)
crime_tab$Total_crime_diff <- (crime_tab$Total_crime_2014 - crime_tab$Total_crime_2011)

## Merging with Cleanup Data ##########----------

## Load data
load("Vacant Lot Cleanup 2012-13.Rdata")

#tabulating vacant lot cleanups by PSAs
cleanup_tab <- data.frame(table(cleanups$psa))

## Merging crime and cleanup data

i <- match(cleanup_tab$Var1,crime_tab$psa)
df <- data.frame(
  psa = cleanup_tab$Var1,
  cleanup_count = cleanup_tab$Freq
)

df$Part_II_crime_count <- crime_tab$Part_II_crime_diff[i]
df$Property_crime_count <- crime_tab$Property_crime_diff[i]
df$Violent_crime_count <- crime_tab$Violent_crime_diff[i]
df$crime_count <- crime_tab$Total_crime_diff[i]

save(df, file = "Crime and Vacant Lot Cleanup.Rdata")
