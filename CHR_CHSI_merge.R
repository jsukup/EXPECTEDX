###Script to merge CHR and CHSI data
###CHR: County Health Rankings
###CHSI: Community Health Status Indicators
###CHR Obtained From: http://www.countyhealthrankings.org/rankings/data
###CHSI Obtained From: https://catalog.data.gov/dataset/community-health-status-indicators-chsi-to-combat-obesity-heart-disease-and-cancer
###Data Obtained On: 8/10/2016

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
CHSI.data <- readData('/mnt/common/work/ExpX/r/data/CHSI/datafiles'
                      , c("State_FIPS_Code"="factor", "County_FIPS_Code"="factor"))

##merge CHSI data
#NOTE: LEADINGCAUSESFODEATH is messed up a bit.  The FIPS codes were converted to ints
#when the govt DBA dumpted the data.
CHSI.data$LEADINGCAUSESOFDEATH[1:2] <- list(NULL) #remove FIPS columns.
CHSI.data$LEADINGCAUSESOFDEATH <- merge(CHSI.data$LEADINGCAUSESOFDEATH, CHSI.data$DEMOGRAPHICS[1:4]
                                        , by=c("CHSI_County_Name", "CHSI_State_Name")) #add back in FIPS
#CHSI.data.all <- Reduce(function(...) merge(..., by="County_FIPS_Code"), CHSI.data) #this is the "proper" way to do it, but crashes my R
CHSI.data.all <- Reduce(merge, CHSI.data) #seems good enough

##read in CHR data
CHR.data <- readData('/mnt/common/work/ExpX/r/data/CHR'
                     , c("STATECODE"="factor", "COUNTYCODE"="factor"))

##rename Code Columns and remove state roll-up
names(CHR.data[[1]])[names(CHR.data[[1]])=="STATECODE"] <- "State_FIPS_Code"
names(CHR.data[[1]])[names(CHR.data[[1]])=="COUNTYCODE"] <- "County_FIPS_Code"
CHR.data[[1]] <- subset(CHR.data[[1]], County_FIPS_Code!="000")

##merge CHR data (made to be expandable)
CHR.data.all <- Reduce(merge, CHR.data)

##merge CHSI and CHR
CHSI.CHR.data.all <- merge(CHSI.data.all, CHR.data.all)
