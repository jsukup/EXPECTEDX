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
library(jsonlite)
library(RCurl)

##directories
dir.data.CHSI <- '/mnt/common/work/ExpX/r/data/CHSI'
dir.data.CHR <- '/mnt/common/work/ExpX/r/data/CHR'
dir.data.HCAHPS <- '/mnt/common/work/ExpX/r/data/HCAHPS'
dir.data.FIPS <- '/mnt/common/work/ExpX/r/data/Census'
dir.export <- '/mnt/common/work/ExpX/r/data/export'

##urls
url.broadband.pt1 <- 'http://www.broadbandmap.gov/broadbandmap/almanac/jun2014/rankby/state/'
url.broadband.pt2 <- '/population/downloadSpeedGreaterThan10000k/county?format=json&order=asc'

##switches
#set to true to export .csv
export.CHSI <- TRUE
export.CHR <- TRUE
export.CHSI.CSR <- TRUE
export.HCAHPS <- TRUE

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

##keep only needed rows
CHSI.data.all <- select(CHSI.data.all, State_FIPS_Code, County_FIPS_Code
                        , A_Wh_Comp, A_Bl_Comp, A_Ot_Comp, A_Hi_Comp, B_Wh_Cancer
                        , B_Bl_Cancer, B_Ot_Cancer, B_Hi_Cancer, C_Wh_Suicide, C_Bl_Suicide
                        , C_Ot_Suicide, C_Hi_Suicide, C_Wh_Cancer, C_Bl_Cancer, C_Ot_Cancer
                        , C_Hi_Cancer, D_Wh_Cancer, D_Bl_Cancer, D_Ot_Cancer, D_Hi_Cancer
                        , D_Wh_HeartDis, D_Bl_HeartDis, D_Ot_HeartDis, D_Hi_HeartDis, D_Wh_Suicide
                        , D_Bl_Suicide, D_Ot_Suicide, D_Hi_Suicide, E_Wh_Cancer, E_Bl_Cancer
                        , E_Ot_Cancer, E_Hi_Cancer, E_Wh_HeartDis, E_Bl_HeartDis, E_Ot_HeartDis
                        , E_Hi_HeartDis, F_Wh_HeartDis, F_Bl_HeartDis, F_Ot_HeartDis, F_Hi_HeartDis
                        , F_Wh_Cancer, F_Bl_Cancer, F_Ot_Cancer, F_Hi_Cancer)

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

##keep only needed rows
CHR.data.all <- select(CHR.data.all, State_FIPS_Code, County_FIPS_Code
                       , Premature.death.Value
                       , Poor.or.fair.health.Value
                       , Poor.physical.health.days.Value
                       , Poor.mental.health.days.Value
                       , Low.birthweight.Value
                       , Adult.smoking.Value
                       , Adult.obesity.Value
                       , Food.environment.index.Value
                       , Physical.inactivity.Value
                       , Access.to.exercise.opportunities.Value
                       , Excessive.drinking.Value
                       , Alcohol.impaired.driving.deaths.Value
                       , Sexually.transmitted.infections.Value
                       , Teen.births.Value
                       , Primary.care.physicians.Value
                       , Mental.health.providers.Value
                       , Preventable.hospital.stays.Value
                       , Population.estimate.Value
                       , Percent.of.population.below.18.years.of.age
                       , Percent.of.population.aged.65.years.and.older
                       , Percent.of.population.that.is.non.Hispanic.African.American
                       , Percent.of.population.that.is.American.Indian.or.Alaskan.Native
                       , Percent.of.population.that.is.Asian
                       , Percent.of.population.that.is.Native.Hawaiian.or.Other.Pacific.Islander
                       , Percent.of.population.that.is.Hispanic
                       , Percent.of.population.that.is.non.Hispanic.White
                       , Population.that.is.not.proficient.in.English.Value
                       , Percent.of.population.that.is.female
                       , Population.living.in.a.rural.area.Value
                       , Diabetes.Value
                       , Other.primary.care.providers.Value)
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

##filter out unneeded rows
HCAHPS.data$`Patient_survey__HCAHPS__-_Hospital` <- filter(HCAHPS.data$`Patient_survey__HCAHPS__-_Hospital`
                                                           , HCAHPS.Measure.ID == 'H_COMP_1_LINEAR_SCORE'
                                                           | HCAHPS.Measure.ID == 'H_COMP_2_LINEAR_SCORE'
                                                           | HCAHPS.Measure.ID == 'H_COMP_3_LINEAR_SCORE'
                                                           | HCAHPS.Measure.ID == 'H_COMP_5_LINEAR_SCORE'
                                                           | HCAHPS.Measure.ID == 'H_COMP_6_LINEAR_SCORE'
                                                           | HCAHPS.Measure.ID == 'H_COMP_7_LINEAR_SCORE'
                                                           | HCAHPS.Measure.ID == 'H_HSP_RATING_LINEAR_SCORE'
                                                           | HCAHPS.Measure.ID == 'H_RECMND_LINEAR_SCORE')

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

##Code below here will process everything into a "nice" form.
HCAHPS.hospitals <- unique(HCAHPS.data.all$Provider.ID) #Extract provider ID list
HCAHPS.subCols <- c("Provider.ID", "HCAHPS.Measure.ID", "HCAHPS.Question", "HCAHPS.Answer.Description"
             , "Patient.Survey.Star.Rating", "Patient.Survey.Star.Rating.Footnote"
             , "HCAHPS.Answer.Percent", "HCAHPS.Answer.Percent.Footnote", "HCAHPS.Linear.Mean.Value") #List of columns for subdata
HCAHPS.subData <- HCAHPS.data.all[HCAHPS.subCols] #select only subset of data as defined above

HCAHPS.subData <- lapply(HCAHPS.hospitals, function(x) filter(HCAHPS.subData, Provider.ID == x)) #create a list of df grouped by prov.id
HCAHPS.subData <- bind_rows(lapply(HCAHPS.subData, function(x) as.data.frame(t(apply(x, 2, paste, collapse=";"))))) #concatenate cols into one row
HCAHPS.subData$Provider.ID <- gsub('.*;', '', HCAHPS.subData$Provider.ID) #fix provider ID names

HCAHPS.data.all <- filter(HCAHPS.data.all, HCAHPS.Measure.ID ==  "H_COMP_1_LINEAR_SCORE") #select only one row for each provider ID
HCAHPS.data.all <- select(HCAHPS.data.all, -HCAHPS.Measure.ID, -HCAHPS.Question, -HCAHPS.Answer.Description #drop cols to be replaced
                          , -Patient.Survey.Star.Rating, -Patient.Survey.Star.Rating.Footnote
                          , -HCAHPS.Answer.Percent, -HCAHPS.Answer.Percent.Footnote, -HCAHPS.Linear.Mean.Value)


HCAHPS.data.all <- merge(HCAHPS.data.all, HCAHPS.subData) #replace cols

if(export.HCAHPS)
  exportData(dir.export, HCAHPS.data.all, "HCAHPS_data_all.csv")

##BROADBAND DATA
states <- c("01","02","03","04","05","06","07","08","09", 10:50)
BB.data <- lapply(states, function(x) getURL(paste(paste(url.broadband.pt1, x, sep=""), url.broadband.pt2, sep="")))  #pull data

BB.data.all <- lapply(BB.data, fromJSON)  #put into list
BB.data.all <- lapply(BB.data.all, function(x) Reduce(rbind, x[4]))  #extract dfs
BB.data.all <- bind_rows(Reduce(rbind, BB.data.all))  #row bind dfs

FIPS.lookup <- read.csv(paste(dir.data.FIPS, "/national_county.txt", sep="")
                        , header=FALSE
                        , colClasses=c("character", "character", "character", "character", "character"))
colnames(FIPS.lookup) <- c("State", "stateFips", "County_FIPS_Code","geographyName", "Donno")
FIPS.lookup$geographyName <- gsub("*County", "", FIPS.lookup$geographyName)
FIPS.lookup$Donno <- NULL
FIPS.lookup$State <- NULL
