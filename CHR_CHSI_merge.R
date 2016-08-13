###Script to merge CHR and CHSI data
###CHR: County Health Rankings
###CHSI: Community Health Status Indicators
###CHR Obtained From: http://www.countyhealthrankings.org/rankings/data
###CHSI Obtained From: https://catalog.data.gov/dataset/community-health-status-indicators-chsi-to-combat-obesity-heart-disease-and-cancer
###Data Obtained On: 8/16/2016

##load libraries
library(car)

##directories
dir.data.CHSI <- '/mnt/common/work/ExpX/r/data/CHSI'
dir.data.CHR <- '/mnt/common/work/ExpX/r/data/CHR'
dir.export <- '/mnt/common/work/ExpX/r/data/export'

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

##read in CHSI data
CHSI.data <- readData(dir.data.CHSI, c("State_FIPS_Code"="factor", "County_FIPS_Code"="factor"))

#check for unneccesary datasets and remove
if(length(CHSI.data) > 1)
  CHSI.data <- CHSI.data[-which(lapply(CHSI.data, nrow) != 3141)]

##merge CHSI data
#NOTE: LEADINGCAUSESFODEATH is messed up a bit.  The FIPS codes were converted to ints
#when the govt DBA dumpted the data.
CHSI.data$LEADINGCAUSESOFDEATH[1:2] <- list(NULL) #remove FIPS columns.
CHSI.data$LEADINGCAUSESOFDEATH <- merge(CHSI.data$LEADINGCAUSESOFDEATH, CHSI.data$DEMOGRAPHICS[1:4]
                                        , by=c("CHSI_County_Name", "CHSI_State_Name")) #add back in FIPS
CHSI.data.all <- Reduce(merge, CHSI.data)

##recode CHSI data
CHSI.replaceData <- c(-9999, -2222, -2222.2, -2, -1111.1, -1111, -1)
CHSI.data.all <- recode(CHSI.data.all, 'CHSI.replaceData = NA')

##read in CHR data
CHR.data <- readData(dir.data.CHR, c("STATECODE"="factor", "COUNTYCODE"="factor"))

#check for unneccesary datasets and remove
if(length(CHR.data) > 1)
  CHR.data <- CHR.data[-which(lapply(CHR.data, nrow) != 3141)]

##rename Code Columns and remove state roll-up
names(CHR.data[[1]])[names(CHR.data[[1]])=="STATECODE"] <- "State_FIPS_Code"
names(CHR.data[[1]])[names(CHR.data[[1]])=="COUNTYCODE"] <- "County_FIPS_Code"
CHR.data[[1]] <- subset(CHR.data[[1]], County_FIPS_Code!="000")

##merge CHR data (made to be expandable)
CHR.data.all <- Reduce(merge, CHR.data)

##merge CHSI and CHR
CHSI.CHR.data.all <- merge(CHSI.data.all, CHR.data.all)

##export data to .csv
setwd(dir.export)
write.csv(CHSI.data.all, file = "CHSI_data_all.csv")
write.csv(CHR.data.all, file = "CHR_data_all.csv")
write.csv(CHSI.CHR.data.all, file = "CHSI_CSR_data_all.csv")