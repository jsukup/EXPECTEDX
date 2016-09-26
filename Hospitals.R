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
setwd('D:/work/ExpX/r/data/HCAHPS/hospitals')
hospital.data <- read.csv("hospitals.csv")

hospital.data$latlong <- str_extract(hospital.data$Location,'\\(\\d*\\.\\d*\\, ?-\\d*\\.\\d*\\)') #Extract latitude/longitude between parenthesis
hospital.data$latlong <- gsub('\\(','',hospital.data$latlong) #Remove leading parenthesis
hospital.data$latlong <- gsub('\\)','',hospital.data$latlong) #Remove trailing parenthesis

split <- strsplit(as.character(hospital.data$latlong), ",", fixed = TRUE) #Split by comma

hospital.data$latitude <- sapply(split, '[', 1) #Extract latitude
hospital.data$longitude <- sapply(split, '[', 2) #Extract longitude

hospital.data <- filter(hospital.data, State != 'PR', State != 'GU', State != 'MP', State != 'VI') #Remove territories 

nolatlong <- filter(hospital.data, is.na(latlong)) #Extract hospital addresses with no coordinates
nolatlong <- select(nolatlong, Provider.ID, Location) #Select address variable
nolatlong[,2] <- gsub(',', '', nolatlong[,2]) #Remove commas
nolatlong[,2] <- gsub('#', '', nolatlong[,2]) #Remove number sign
nolatlong <- cbind(unique(nolatlong)[,1], geocode(unique(nolatlong[,2]))) #Geocode addresses with latitude/longitude
names(nolatlong) <- c('Provider.ID','longitude','latitude') #Match names to hospital.data data frame

hospital.data <- merge(hospital.data, nolatlong, by = 'Provider.ID', all = TRUE) #Merge geocoded addresses with original

hospital.data$latitude.x <- ifelse(is.na(hospital.data$latitude.x), hospital.data$latitude.y, hospital.data$latitude.x)
hospital.data$longitude.x <- ifelse(is.na(hospital.data$longitude.x), hospital.data$longitude.y, hospital.data$longitude.x)

hospital.data <- select(hospital.data, -latlong, -longitude.y, -latitude.y)
colnames(hospital.data) <- c("Provider.ID", "Name", "City", "State", "County", "Address", "lat", "long")

hospital.data <- filter(hospital.data, Provider.ID != "171312")
hospital.data <- filter(hospital.data, Provider.ID != "171324")
hospital.data <- filter(hospital.data, Provider.ID != "390111")
hospital.data <- filter(hospital.data, Provider.ID != "393303")
hospital.data <- filter(hospital.data, Provider.ID != "510001")
hospital.data <- filter(hospital.data, Provider.ID != "371311")
hospital.data <- filter(hospital.data, Provider.ID != "450214")

setwd("D:/work/ExpX/r/data/export")
write.csv(hospital.data, "Hospitals.csv", row.names = FALSE)
