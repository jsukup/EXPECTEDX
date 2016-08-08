##ANALYSIS OF CHSI DATA - COMMUNITY HEALTH DATA INITIATIVE
##CHSI OBTAINED FROM: https://catalog.data.gov/dataset/community-health-status-indicators-chsi-to-combat-obesity-heart-disease-and-cancer
##CHR OBTAINED FROM: http://www.countyhealthrankings.org/rankings/data
##CHR Codebook: http://www.countyhealthrankings.org/sites/default/files/2015%20CHR%20SAS%20Analytic%20Data%20Documentation.pdf
##NHE OBTAINED FROM: https://www.cms.gov/research-statistics-data-and-systems/statistics-trends-and-reports/nationalhealthexpenddata/nationalhealthaccountshistorical.html
##HCAHPS OBTAINED FROM (8/1/16): https://data.medicare.gov/Hospital-Compare/Patient-survey-HCAHPS-Hospital/dgck-syfz
##
##OBTAINED ON: 7/22/16

##Load libraries
library(stringr)
library(dplyr)
library(ggmap)

##Read in CHSI and CHR data files
setwd('c:/program files (x86)/git/projects/expectedx/data') #Temporary working directory
files <- list.files(pattern = '*.csv')
for (i in 1:length(files))
    assign(files[i], read.csv(files[i]))

HCAHPS.csv$latlong <- str_extract(HCAHPS.csv$Location,'\\(\\d*\\.\\d*\\, ?-\\d*\\.\\d*\\)') #Extract latitude/longitude between parenthesis
HCAHPS.csv$latlong <- gsub('\\(','',HCAHPS.csv$latlong) #Remove leading parenthesis
HCAHPS.csv$latlong <- gsub('\\)','',HCAHPS.csv$latlong) #Remove trailing parenthesis

split <- strsplit(as.character(HCAHPS.csv$latlong), ",", fixed = TRUE) #Split by comma

HCAHPS.csv$latitude <- sapply(split, '[', 1) #Extract latitude
HCAHPS.csv$longitude <- sapply(split, '[', 2) #Extract longitude

HCAHPS.csv <- filter(HCAHPS.csv, State != 'PR', State != 'GU', State != 'MP', State != 'VI') #Remove territories 

nolatlong <- filter(HCAHPS.csv, is.na(latlong)) #Extract hospital addresses with no coordinates
nolatlong <- select(nolatlong, Provider.ID, Location) #Select address variable
nolatlong[,2] <- gsub(',', '', nolatlong[,2]) #Remove commas
nolatlong[,2] <- gsub('#', '', nolatlong[,2]) #Remove number sign
nolatlong <- cbind(unique(nolatlong)[,1], geocode(unique(nolatlong[,2]))) #Geocode addresses with latitude/longitude
names(nolatlong) <- c('Provider.ID','longitude','latitude') #Match names to HCAHPS.csv data frame

HCAHPS.csv <- merge(HCAHPS.csv, nolatlong, by = 'Provider.ID', all = TRUE) #Merge geocoded addresses with original

HCAHPS.csv$latitude.x <- ifelse(is.na(HCAHPS.csv$latitude.x), HCAHPS.csv$latitude.y, HCAHPS.csv$latitude.x)
HCAHPS.csv$longitude.x <- ifelse(is.na(HCAHPS.csv$longitude.x), HCAHPS.csv$longitude.y, HCAHPS.csv$longitude.x)

setwd('c:/program files (x86)/git/projects/expectedx') #Reset working directory





