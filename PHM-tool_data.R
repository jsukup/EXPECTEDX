###Script to merge all datasets
###CHR: County Health Rankings
###CHSI: Community Health Status Indicators
###CHR Obtained From: http://www.countyhealthrankings.org/rankings/data
###CHSI Obtained From: https://catalog.data.gov/dataset/community-health-status-indicators-chsi-to-combat-obesity-heart-disease-and-cancer
###HCAHPS OBTAINED FROM (8/1/16): https://data.medicare.gov/Hospital-Compare/Patient-survey-HCAHPS-Hospital/dgck-syfz
###Data Obtained On: 8/16/2016

##load libraries
library(car)
library(dplyr)
library(stringr)
library(ggmap)

##directories
dir.data.CHSI <- '/mnt/common/work/ExpX/r/data/CHSI'
dir.data.CHR <- '/mnt/common/work/ExpX/r/data/CHR'
dir.data.HCAHPS <- '/mnt/common/work/ExpX/r/data/HCAHPS'
dir.export <- '/mnt/common/work/ExpX/r/data/export'

##switches
#set to true to export .csv
export.CHSI <- FALSE
export.CHR <- FALSE
export.CHSI.CSR <- FALSE
export.HCAHPS <- FALSE

##functions
#for reading in data and putting into a list
readData <- function(dir, classes=NULL) {
  setwd(dir)
  files <- list.files(pattern = '*.csv')
  if(!is.null(classes))
    data <- lapply(files, read.csv, colClasses = classes)
  else
    data <- lapply(files, read.csv)
  names(data) <- gsub("\\.csv$", "", files) #clean up names
  return(data)
}

#for exporting to csv
exportData <- function(dir, data, fileName) {
  setwd(dir)
  write.csv(data, file = fileName)
}

##CHSI DATA
#read in data
CHSI.data <- readData(dir.data.CHSI, c("State_FIPS_Code"="factor", "County_FIPS_Code"="factor"))

#check for unneccesary datasets and remove
if(length(CHSI.data) > 1)
  CHSI.data <- CHSI.data[which(lapply(CHSI.data, nrow) == 3141)]

##merge CHSI data
#NOTE: LEADINGCAUSESFODEATH is messed up a bit.  The FIPS codes were converted to ints
#when the govt DBA dumpted the data.
CHSI.data$LEADINGCAUSESOFDEATH[1:2] <- list(NULL) #remove FIPS columns.
CHSI.data$LEADINGCAUSESOFDEATH <- merge(CHSI.data$LEADINGCAUSESOFDEATH, CHSI.data$DEMOGRAPHICS[1:4]) #add back in FIPS
CHSI.data.all <- Reduce(merge, CHSI.data)

##recode CHSI data
CHSI.replaceData <- c(-9999, -2222, -2222.2, -2, -1111.1, -1111, -1)
CHSI.data.all <- as.data.frame(lapply(CHSI.data.all, Recode, "CHSI.replaceData=NA"))

#export to .csv
if(export.CHSI)
  exportData(dir.export, CHSI.data.all, "CHSI_data_all.csv")

##CHR DATA
#read in data
CHR.data <- readData(dir.data.CHR, c("STATECODE"="factor", "COUNTYCODE"="factor"))

#check for unneccesary datasets and remove
if(length(CHR.data) > 1)
  CHR.data <- CHR.data[which(lapply(CHR.data, nrow) == 3141)]

##rename Code Columns and remove state roll-up
names(CHR.data[[1]])[names(CHR.data[[1]])=="STATECODE"] <- "State_FIPS_Code"
names(CHR.data[[1]])[names(CHR.data[[1]])=="COUNTYCODE"] <- "County_FIPS_Code"
CHR.data[[1]] <- subset(CHR.data[[1]], County_FIPS_Code!="000")

##merge CHR data (made to be expandable)
CHR.data.all <- Reduce(merge, CHR.data)

##export to .csv
if(export.CHR)
  exportData(dir.export, CHR.data.all, "CHR_data_all.csv")

##merge CHSI and CHR
CHSI.CHR.data.all <- merge(CHSI.data.all, CHR.data.all)

##export data to .csv
if(export.CHSI.CSR)
  exportData(dir.export, CHSI.CHR.data.all, "CHSI_CHR_data_all.csv")

##HCAHPS DATA
#read in data
HCAHPS.data <- readData(dir.data.HCAHPS)

##merge CHR data (made to be expandable)
HCAHPS.data.all <- Reduce(merge, HCAHPS.data)

HCAHPS.data.all$latlong <- str_extract(HCAHPS.data.all$Location,'\\(\\d*\\.\\d*\\, ?-\\d*\\.\\d*\\)') #Extract latitude/longitude between parenthesis
HCAHPS.data.all$latlong <- gsub('\\(','',HCAHPS.data.all$latlong) #Remove leading parenthesis
HCAHPS.data.all$latlong <- gsub('\\)','',HCAHPS.data.all$latlong) #Remove trailing parenthesis

split <- strsplit(as.character(HCAHPS.data.all$latlong), ",", fixed = TRUE) #Split by comma

HCAHPS.data.all$latitude <- sapply(split, '[', 1) #Extract latitude
HCAHPS.data.all$longitude <- sapply(split, '[', 2) #Extract longitude

HCAHPS.data.all <- filter(HCAHPS.data.all, State != 'PR', State != 'GU', State != 'MP', State != 'VI') #Remove territories 

nolatlong <- filter(HCAHPS.data.all, is.na(latlong)) #Extract hospital addresses with no coordinates
nolatlong <- select(nolatlong, Provider.ID, Location) #Select address variable
nolatlong[,2] <- gsub(',', '', nolatlong[,2]) #Remove commas
nolatlong[,2] <- gsub('#', '', nolatlong[,2]) #Remove number sign
nolatlong <- cbind(unique(nolatlong)[,1], geocode(unique(nolatlong[,2]))) #Geocode addresses with latitude/longitude
names(nolatlong) <- c('Provider.ID','longitude','latitude') #Match names to HCAHPS.data.all data frame

HCAHPS.data.all <- merge(HCAHPS.data.all, nolatlong, by = 'Provider.ID', all = TRUE) #Merge geocoded addresses with original

HCAHPS.data.all$latitude.x <- ifelse(is.na(HCAHPS.data.all$latitude.x), HCAHPS.data.all$latitude.y, HCAHPS.data.all$latitude.x)
HCAHPS.data.all$longitude.x <- ifelse(is.na(HCAHPS.data.all$longitude.x), HCAHPS.data.all$longitude.y, HCAHPS.data.all$longitude.x)

##clean up data
HCAHPS.data.all <- as.data.frame(lapply(HCAHPS.data.all, Recode, "'Not Applicable'=NA; ''=NA"))  #replace with NA
HCAHPS.data.all$longitude.y <- NULL  #drop extra lat/lon columns
HCAHPS.data.all$latitude.y <- NULL

#save copy
HCAHPS.copy <- HCAHPS.data.all
HCAHPS.data.all <- HCAHPS.copy

##Code below here will process everything into a "nice" form.
##TODO: Optimize.  For loop was used, but there is probably a more efficent way.
##Runtime on my PC is about a minute or so with i5 2.6  Not too bad
hospitals <- unique(HCAHPS.data.all$Provider.ID)
subCols <- c("Provider.ID", "HCAHPS.Measure.ID", "HCAHPS.Question", "HCAHPS.Answer.Description"
             , "Patient.Survey.Star.Rating", "Patient.Survey.Star.Rating.Footnote"
             , "HCAHPS.Answer.Percent", "HCAHPS.Answer.Percent.Footnote", "HCAHPS.Linear.Mean.Value")
subData <- HCAHPS.data.all[subCols]

testdat <- (lapply(subdata[subdata$Provider.ID == 10001, ], paste, collapse=";"))
testdat2 <- (lapply(subdata[subdata$Provider.ID == 10005, ], paste, collapse=";"))

testdat3 <- rbind(testdat, testdat2)

test5 <- lapply(hospitals, function(x) subData[subData$Provider.ID == x, ])

test5 <- lapply(hospitals, function(x) filter(subData, Provider.ID == x))
test6 <- as.data.frame(t(apply(test5[[1]], 2, paste, collapse=";")))

test6 <- lapply(as.data.frame(test5), paste, collapse=";")
test6 <- lapply(filter(subData, Provider.ID == x), paste, collapse=";")

test6 <- as.data.frame(t(paste(test5[[1]], collapse=";")), col.names=colnames(subdata))
test6$Provider.ID = subdata$Provider.ID[1]

test8 <- as.data.frame(lapply(lapply(hospitals, function(x) filter(subdata, Provider.ID == x), paste, collapse=";")))

test <- lapply(lapply(hospitals, function(x) subData[subdata$Provider.ID == x, ]), paste, sep=";")
test2 <- bind_rows(as.data.frame(test))
str(test)

test3 <- lapply(subdata[subdata$Provider.ID == 10001, ], paste, collapse=";")
test4 <- lapply(subdata[subdata$Provider.ID == 10005, ], paste, collapse=";")
testb <- as.data.frame(rbind(test3, test4))

test2 <- as.data.frame(do.call(
  rbind, lapply(
    lapply(
      hospitals, function(x) HCAHPS.data.all[HCAHPS.data.all$Provider.ID == x, ]), paste, collapse=";")))

test$Provider.ID <- 10001
hospitals[1]
dataList <- as.data.frame(lapply(lapply(hospitals, function(x) HCAHPS.data.all[HCAHPS.data.all$Provider.ID == x, ] ), paste, collapse=";"))

hospitals <- c(unique(HCAHPS.data.all$Provider.ID))

subData <- select(HCAHPS.data.all, Provider.ID, HCAHPS.Measure.ID, HCAHPS.Question, HCAHPS.Answer.Description
                  , Patient.Survey.Star.Rating, Patient.Survey.Star.Rating.Footnote
                  , HCAHPS.Answer.Percent, HCAHPS.Answer.Percent.Footnote, HCAHPS.Linear.Mean.Value)

dataList <- as.data.frame(lapply(filter(subData, Provider.ID == 10001), paste, collapse=";"))
dataList$Provider.ID = as.integer(dataList$Provider.ID)
dataList[1,1] <- hospitals[1]
for(i in 2:length(hospitals)) {
  dataList <- rbind(dataList, as.data.frame(lapply(filter(subData, Provider.ID == hospitals[i]), paste, collapse=";")))
  dataList$Provider.ID = as.integer(dataList$Provider.ID)
  dataList[i,1] <- hospitals[i]
}

HCAHPS.data.all <- filter(HCAHPS.data.all, HCAHPS.Measure.ID ==  "H_COMP_3_LINEAR_SCORE")
HCAHPS.data.all <- select(HCAHPS.data.all, -HCAHPS.Measure.ID, -HCAHPS.Question, -HCAHPS.Answer.Description
                          , -Patient.Survey.Star.Rating, -Patient.Survey.Star.Rating.Footnote
                          , -HCAHPS.Answer.Percent, -HCAHPS.Answer.Percent.Footnote, -HCAHPS.Linear.Mean.Value)


HCAHPS.data.all <- merge(HCAHPS.data.all, dataList)

if(export.HCAHPS)
  exportData(dir.export, HCAHPS.data.all, "HCAHPS_data_all.csv")