###Script to merge all datasets
###CHR: County Health Rankings
###CHSI: Community Health Status Indicators
###CHR Obtained From: http://www.countyhealthrankings.org/rankings/data
###CHSI Obtained From: https://catalog.data.gov/dataset/community-health-status-indicators-chsi-to-combat-obesity-heart-disease-and-cancer
###HCAHPS OBTAINED FROM (8/1/16): https://data.medicare.gov/Hospital-Compare/Patient-survey-HCAHPS-Hospital/dgck-syfz
###FIPS Lookup table obtained from: https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt
###Data Obtained On: 8/16/2016

##load libraries
library(car)
library(dplyr)
library(stringr)
library(jsonlite)
library(RCurl)
library(data.table)
library(rgeos)
library(rgdal)

##directories
#dir.data.CHSI <- '/mnt/common/work/ExpX/r/data/CHSI'
#dir.data.CHR <- '/mnt/common/work/ExpX/r/data/CHR'
#dir.data.HCAHPS <- '/mnt/common/work/ExpX/r/data/HCAHPS'
#dir.data.FIPS <- '/mnt/common/work/ExpX/r/data/Census'
#dir.data.HPSA <- '/mnt/common/work/ExpX/r/data/HPSA'
#dir.data.GIS <- '/mnt/common/work/ExpX/r/rdata/GIS'
#dir.export <- '/mnt/common/work/ExpX/r/data/export'

dir.data.CHSI <- 'D:/work/ExpX/r/data/CHSI'
dir.data.CHR <- 'D:/work/ExpX/r/data/CHR'
dir.data.HCAHPS <- 'D:/work/ExpX/r/data/HCAHPS'
dir.data.FIPS <- 'D:/work/ExpX/r/data/Census'
dir.data.HPSA <- 'D:/work/ExpX/r/data/HPSA'
dir.data.GIS <- 'D:/work/ExpX/r/data/GIS/us.geojson'
dir.export <- 'D:/work/ExpX/r/data/export'

##urls
url.broadband.pt1 <- 'http://www.broadbandmap.gov/broadbandmap/almanac/jun2014/rankby/state/'
url.broadband.pt2 <- '/population/downloadSpeedGreaterThan10000k/county?format=json&order=asc'

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

#for correcting case
fixCase <- function(myCol) {
  temp <- tolower(myCol)
  temp <- paste0(toupper(substr(temp, 1, 1)), substr(temp, 2, nchar(temp)))
  return(temp)
}

#for computing sums
naSum <- function(dat) {
  if(sum(!is.na(dat)) != 0)
    return(sum(dat, na.rm = TRUE))
  else
    return(NA)
}

normalizedScore <- function(data, raw, scope, name, method = NULL) {
  if(is.null(method))
    method <- "uniform"
  
  if(scope == "nation") {
    temp <- aggregate(data[[raw]], by = list(data[["FIPS"]])
                      , function(x) {
                        if(sum(is.na(x)) == length(x))
                          return(NA)
                        else
                          sum(x, na.rm = TRUE)
                      })
    colnames(temp) <- c("FIPS", name)
    
    if(method == "uniform")
      temp[[name]] <- ecdf(temp[[name]])(temp[[name]])
    else if(method == "normal") {
      temp[[name]] <- pnorm(temp[[name]], mean = mean(temp[[name]], na.rm = TRUE)
                            , sd = sd(temp[[name]], na.rm = TRUE))
    }


  } else if(scope == "state") {
    temp <- lapply(unique(data[["State"]]), function(x) filter(data, State == x))  #change to a list of df by state
    temp <- lapply(temp, function(df) aggregate(df[[raw]], by = list(df[["FIPS"]])
                                                , function(x) {
                                                  if(sum(is.na(x)) == length(x))
                                                    return(NA)
                                                  else
                                                    sum(x, na.rm=TRUE)
                                                }))  #compute total score by county
    
    if(method == "uniform") {
      temp <- lapply(temp, function(df) cbind(df, tryCatch(ecdf(df[, 2])(df[, 2])
                                                           , error = function(conds)
                                                             return(rep(NA, times = length(df[, 2]))))))
    } else if(method == "normal") {
      temp <- lapply(temp, function(df) tryCatch(cbind(df, pnorm(df[, 2]
                                                                 , mean = mean(df[, 2], na.rm = TRUE)
                                                                 , sd = sd(df[, 2], na.rm = TRUE)))
                                                 , error = function(conds)
                                                   return(rep(NA, times = length(df[, 2])))))
    }

    
    
    temp <- bind_rows(temp)
    temp[, 2] <- temp[, 3]
    temp[, 3] <- NULL
    colnames(temp) <- c("FIPS", name)
  }
  return(temp)
}

countySmooth <- function(df, gis, smoothFactor)
{
  temp <- as.data.frame(gIntersects(gis, gis, byid = TRUE))
  colnames(temp) <- gis@data$GEOID
  temp.names <- colnames(temp)
  val.list <- lapply(colnames(temp), function(fips) df$Telehealth.Score.Raw[df$FIPS==fips])
  temp <- as.data.frame(Map(function(x, fips) Recode(x, paste0("0=NA;TRUE=", fips)), temp, val.list))
  #temp <- cbind(temp.names, rowMeans(temp, na.rm = TRUE))
  
  temp <- apply(temp, 1, function(x) if(sum(!is.na(x)) == 0)
    return(NA)
    else
      return(mean(x, na.rm = TRUE) * exp(-x * (-log(smoothFactor) / max(x, na.rm = TRUE)))))
  
  temp <- diag(as.matrix(as.data.frame(temp)))
  
  temp <- cbind(temp.names, temp)
  colnames(temp) <- c("FIPS", "Score.adj")
  temp <- as.data.frame(temp)
  temp <- transform(temp, FIPS = as.character(FIPS)
                    , Score.adj = as.numeric(as.character(Score.adj)))
  return(temp)
}

##FIPS lookup table to fix stuff
FIPS.colClasses <- "character" #to avoid factors
FIPS.lookup <- read.csv(paste(dir.data.FIPS, "/national_county.txt", sep=""), header=FALSE, colClasses=FIPS.colClasses)
colnames(FIPS.lookup) <- c("State", "stateFips", "County_FIPS_Code","County", "Donno")
FIPS.lookup$County <- gsub("*\\sCounty", "", FIPS.lookup$County)
FIPS.lookup$Donno <- NULL

FIPS.lookup <- filter(FIPS.lookup, State != "AS"
                      , State != "FM"
                      , State != "GU"
                      , State != "MH"
                      , State != "MP"
                      , State != "PW"
                      , State != "PR"
                      , State != "VI"
                      , State != "UM")

#add two missing counties to FIPS lookup in Alaska
missing <- as.data.frame(list(c("AK", "AK", "AK")
                              , c("02", "02", "02")
                              , c("201", "232", "280")
                              , c("Prince of Wales-Outer Ketchikan"
                                  , "Skagway-Hoonah-Angoon"
                                  , "Wrangell-Petersburg")))
colnames(missing) <- colnames(FIPS.lookup)
FIPS.lookup <- rbind(FIPS.lookup, missing)
FIPS.lookup$FIPS <- paste(FIPS.lookup$stateFips, FIPS.lookup$County_FIPS_Code, sep="")
rm(missing)

##CHSI DATA
#read in data
CHSI.data <- readData(dir.data.CHSI, "character")

#check for unneccesary datasets and remove
if(length(CHSI.data) > 1)
  CHSI.data <- CHSI.data[which(lapply(CHSI.data, nrow) == 3141)]

##merge CHSI data
#NOTE: LEADINGCAUSESFODEATH is messed up a bit.  The FIPS codes were converted to ints
#when the govt DBA dumpted the data.
CHSI.data$LEADINGCAUSESOFDEATH[1:2] <- list(NULL) #remove FIPS columns.
CHSI.data$LEADINGCAUSESOFDEATH <- merge(CHSI.data$DEMOGRAPHICS[1:4], CHSI.data$LEADINGCAUSESOFDEATH) #add back in FIPS
CHSI.data.all <- Reduce(merge, CHSI.data)

##keep only needed rows
CHSI.data.all <- select(CHSI.data.all
                        , State_FIPS_Code, County_FIPS_Code, CHSI_County_Name
                        , CHSI_State_Name, CHSI_State_Abbr
                        , B_Wh_Cancer, B_Bl_Cancer, B_Ot_Cancer, B_Hi_Cancer
                        , C_Wh_Cancer, C_Bl_Cancer, C_Ot_Cancer, C_Hi_Cancer
                        , D_Wh_Cancer, D_Bl_Cancer, D_Ot_Cancer, D_Hi_Cancer
                        , D_Wh_HeartDis, D_Bl_HeartDis, D_Ot_HeartDis, D_Hi_HeartDis
                        , E_Wh_Cancer, E_Bl_Cancer, E_Ot_Cancer, E_Hi_Cancer
                        , E_Wh_HeartDis, E_Bl_HeartDis, E_Ot_HeartDis, E_Hi_HeartDis
                        , F_Wh_HeartDis, F_Bl_HeartDis, F_Ot_HeartDis, F_Hi_HeartDis
                        , F_Wh_Cancer, F_Bl_Cancer, F_Ot_Cancer, F_Hi_Cancer
                        , County_FIPS_Code, CHSI_State_Abbr
                        , B_Wh_Homicide, B_Bl_Homicide, B_Ot_Homicide, B_Hi_Homicide
                        , C_Wh_Injury, C_Bl_Injury, C_Ot_Injury, C_Hi_Injury
)

##recode CHSI data
CHSI.replaceData <- c(-9999, -2222, -2222.2, -2, -1111.1, -1111, -1)
CHSI.data.all <- cbind(CHSI.data.all[1:5]
                       , lapply(CHSI.data.all[6:ncol(CHSI.data.all)], Recode, "CHSI.replaceData=NA"))
#compute sums
temp <- CHSI.data.all[, 6:45]

temp <- as.data.frame(t(apply(temp, 1, function(r)
  sapply(seq(from = 1, to = 37, by = 4), function(n)
    if(sum(is.na(r[n:(n+3)])) == length(temp[, n:(n+3)]))
      return(NA)
    else
      return(sum(r[n:(n+3)], na.rm = TRUE))
    ))))

temp$Cancer <- apply(select(temp, 1, 2, 3, 5, 8), 1, naSum)
temp$HeartDis <- apply(select(temp, 4, 6, 7), 1, naSum)
temp <- select(temp, Cancer, HeartDis, 10, 9)

colnames(temp) <- c("Cancer", "HeartDis", "Injury", "Homicide")


CHSI.data.all <- cbind(CHSI.data.all[1:5], temp)
rm(temp)

#export to .csv
if(export.CHSI)
  exportData(dir.export, CHSI.data.all, "CHSI_data_all.csv")

##CHR DATA
#read in data
CHR.data <- readData(dir.data.CHR, "character")

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
                       , Poor.or.fair.health.Value
                       , Population.estimate.Value
                       , Diabetes.Value)

colnames(CHR.data.all) <- c("State_FIPS_Code", "County_FIPS_Code", "Poor.Health", "Population", "Diabetes")
##export to .csv
if(export.CHR)
  exportData(dir.export, CHR.data.all, "CHR_data_all.csv")

##merge CHSI and CHR
CHSI.CHR.data.all <- merge(CHSI.data.all, CHR.data.all)

##export data to .csv
if(export.CHSI.CSR)
  exportData(dir.export, CHSI.CHR.data.all, "CHSI_CHR_data_all.csv")

##Compute Scores on CHSI and CHR data
CHSI.CHR.data.all$FIPS <- paste0(CHSI.CHR.data.all$State_FIPS_Code, CHSI.CHR.data.all$County_FIPS_Code)
CHSI.CHR.data.all <- select(CHSI.CHR.data.all, -State_FIPS_Code, -County_FIPS_Code, CHSI_County_Name
                            , -CHSI_State_Name, -CHSI_State_Abbr, -CHSI_County_Name)
CHSI.CHR.data.all <- cbind(CHSI.CHR.data.all[8], CHSI.CHR.data.all[6], CHSI.CHR.data.all[1:4], CHSI.CHR.data.all[5], CHSI.CHR.data.all[7])
CHSI.CHR.data.all <- transform(CHSI.CHR.data.all, Population = as.numeric(gsub(',', '', Population))
                               , Poor.Health = as.numeric(Poor.Health)
                               , Diabetes = as.numeric(Diabetes))

#national percentile
CHSI.CHR.data.national <- lapply(colnames(CHSI.CHR.data.all[3:8]), function(n)
  normalizedScore(CHSI.CHR.data.all, n, "nation", n))
CHSI.CHR.data.national <- Reduce(merge, CHSI.CHR.data.national)
CHSI.CHR.data.national <- merge(select(FIPS.lookup, FIPS, State, County)
                                , CHSI.CHR.data.national, by = "FIPS", all = TRUE)

#state percentile
temp <- merge(select(FIPS.lookup, FIPS, State), CHSI.CHR.data.all, by = "FIPS")
CHSI.CHR.data.state <- lapply(colnames(CHSI.CHR.data.all[3:8]), function(n)
  normalizedScore(temp, n, "state", n))
CHSI.CHR.data.state <- Reduce(merge, CHSI.CHR.data.state)
CHSI.CHR.data.state <- merge(select(FIPS.lookup, FIPS, State, County)
                                , CHSI.CHR.data.state, by = "FIPS", all = TRUE)
rm(temp)

##HCAHPS DATA
#read in data
HCAHPS.data <- readData(dir.data.HCAHPS, "character")

#remove uneeded rows.  This will speed up the grouping process
HCAHPS.data$`Readmissions_and_Deaths_-_Hospital` <- select(HCAHPS.data$`Readmissions_and_Deaths_-_Hospital`
                                                           , Provider.ID, Hospital.Name, State, County.Name, ZIP.Code
                                                           , Measure.Name, Measure.ID, Score)
HCAHPS.data$`Readmissions_and_Deaths_-_Hospital` <- filter(HCAHPS.data$`Readmissions_and_Deaths_-_Hospital`,
                                                           Measure.ID == "READM_30_HOSP_WIDE")

HCAHPS.data$`Structural_Measures_-_Hospital` <- select(HCAHPS.data$`Structural_Measures_-_Hospital`
                                                       , Provider.ID, Hospital.Name, State, County.Name, ZIP.Code
                                                       , Measure.Name, Measure.ID, Measure.Response)
HCAHPS.data$`Structural_Measures_-_Hospital` <- filter(HCAHPS.data$`Structural_Measures_-_Hospital`
                                                       , Measure.ID == "OP_12" | Measure.ID == "OP_17")

##clean up data
#HCAHPS.data$`Readmissions_and_Deaths_-_Hospital`$County.Name <- fixCase(HCAHPS.data$`Readmissions_and_Deaths_-_Hospital`$County.Name)
#HCAHPS.data$`Structural_Measures_-_Hospital`$County.Name <- fixCase(HCAHPS.data$`Structural_Measures_-_Hospital`$County.Name)

HCAHPS.data$`Structural_Measures_-_Hospital`$Measure.Response <- Recode(HCAHPS.data$`Structural_Measures_-_Hospital`$Measure.Response
,"'Not Available'=NA; ''=NA; 'Yes'='1'; 'No'='0'")  #replace with NA

HCAHPS.data$`Readmissions_and_Deaths_-_Hospital`$Score <- Recode(HCAHPS.data$`Readmissions_and_Deaths_-_Hospital`$Score
                                                                        ,"'Not Available'=NA")  #replace with NA

colnames(HCAHPS.data$`Readmissions_and_Deaths_-_Hospital`)[4] <- "County"
colnames(HCAHPS.data$`Structural_Measures_-_Hospital`)[4] <- "County"

#attach FIPS columns
FIPS.lookup.HCAHPS <- FIPS.lookup
FIPS.lookup.HCAHPS$County <- toupper(FIPS.lookup.HCAHPS$County)

#Fix messed up County data
HCAHPS.data$`Readmissions_and_Deaths_-_Hospital`$County <- gsub('SAINT','ST.'
                                                                , HCAHPS.data$`Readmissions_and_Deaths_-_Hospital`$County)
HCAHPS.data$`Structural_Measures_-_Hospital`$County <- gsub('SAINT','ST.'
                                                            , HCAHPS.data$`Structural_Measures_-_Hospital`$County)

HCAHPS.fix.bad <- c("'ST JOSEPH", "LA PORTE", "DE WITT", "LA SALLE", "DE KALB", "DE SOTO")
HCAHPS.fix.good <- c("ST. JOSEPH", "LAPORTE", "DEWITT", "LASALLE", "DEKALB", "DESOTO'")
HCAHPS.data$`Readmissions_and_Deaths_-_Hospital`$County <- Recode(HCAHPS.data$`Readmissions_and_Deaths_-_Hospital`$County
                                                                  , paste(HCAHPS.fix.bad, HCAHPS.fix.good, sep = "'='", collapse = "';'"))
HCAHPS.data$`Structural_Measures_-_Hospital`$County <- Recode(HCAHPS.data$`Structural_Measures_-_Hospital`$County
                                                                  , paste(HCAHPS.fix.bad, HCAHPS.fix.good, sep = "'='", collapse = "';'"))

FIPS.lookup.HCAHPS$County <- gsub("'", "", FIPS.lookup.HCAHPS$County)
FIPS.lookup.HCAHPS$County <- gsub("\\sPARISH", "", FIPS.lookup.HCAHPS$County)

FIPS.fix.bad <- c("'ANCHORAGE MUNICIPALITY", "MATANUSKA-SUSITNA BOROUGH", "JUNEAU CITY AND BOROUGH"
                  , "FAIRBANKS NORTH STAR BOROUGH", "KODIAK ISLAND BOROUGH", "SITKA CITY AND BOROUGH"
                  , "KETCHIKAN GATEWAY BOROUGH", "SALEM CITY", "LA SALLE", "DE WITT", "DE SOTO"
                  , "STE. GENEVIEVE", "BETHEL CENSUS AREA", "DILLINGHAM CENSUS AREA")
FIPS.fix.good <- c("ANCHORAGE", "MATANUSKA SUSITNA", "JUNEAU", "FAIRBANKS NORTH STAR", "KODIAK ISLAND"
                   , "SITKA", "KETCHIKAN GATEWAY", "SALEM", "LASALLE", "DEWITT", "DESOTO", "ST.E GENEVIEVE"
                   , "BETHEL", "DILLINGHAM'")
FIPS.lookup.HCAHPS$County <- Recode(FIPS.lookup.HCAHPS$County
                                    , paste(FIPS.fix.bad, FIPS.fix.good, sep = "'='", collapse = "';'"))


HCAHPS.data$`Structural_Measures_-_Hospital`$County[HCAHPS.data$`Structural_Measures_-_Hospital`$Provider.ID=="020024"] <- "KENAI PENINSULA BOROUGH"
HCAHPS.data$`Structural_Measures_-_Hospital`$County[HCAHPS.data$`Structural_Measures_-_Hospital`$Provider.ID=="021313"] <- "KENAI PENINSULA BOROUGH"

HCAHPS.fix.index <- c("021301", "021304", "021307", "020024", "021312", "021302", "021305", "021313", "021308", "430081")
HCAHPS.fix.index <- unlist(lapply(HCAHPS.fix.index, function(x) which(HCAHPS.data$`Readmissions_and_Deaths_-_Hospital`$Provider.ID == x)))
HCAHPS.fix.good <- c("VALDEZ-CORDOVA CENSUS AREA", "PETERSBURG CENSUS AREA", "VALDEZ-CORDOVA CENSUS AREA", "KENAI PENINSULA BOROUGH"
                     , "NORTH SLOPE BOROUGH", "KENAI PENINSULA BOROUGH", "WRANGELL CITY AND BOROUGH", "KENAI PENINSULA BOROUGH"
                     , "NOME CENSUS AREA", "BENNETT")
HCAHPS.data$`Readmissions_and_Deaths_-_Hospital`$County <- replace(HCAHPS.data$`Readmissions_and_Deaths_-_Hospital`$County, HCAHPS.fix.index, HCAHPS.fix.good)
HCAHPS.data <- lapply(HCAHPS.data, function(df) merge(FIPS.lookup.HCAHPS, df, by = c("State", "County")))

#compute stats to be used
#By National percentile
HCAHPS.data.national <- normalizedScore(HCAHPS.data$`Structural_Measures_-_Hospital`, "Measure.Response", "nation", "Structural.Score")
HCAHPS.data.national <- merge(select(FIPS.lookup, FIPS, State, County), HCAHPS.data.national, by = "FIPS", all = TRUE)
HCAHPS.data.national <- merge(HCAHPS.data.national
                              , normalizedScore(HCAHPS.data$`Readmissions_and_Deaths_-_Hospital`
                                                , "Score", "nation", "RaD.Score")
                              , by = "FIPS", all = TRUE)

#By State percentile
HCAHPS.data.state <- normalizedScore(HCAHPS.data$`Structural_Measures_-_Hospital`, "Measure.Response", "state", "Structural.Score")
HCAHPS.data.state <- merge(select(FIPS.lookup, FIPS, State, County), HCAHPS.data.state, by = "FIPS", all = TRUE)
HCAHPS.data.state <- merge(HCAHPS.data.state
                              , normalizedScore(HCAHPS.data$`Readmissions_and_Deaths_-_Hospital`
                                                , "Score", "state", "RaD.Score")
                              , by = "FIPS", all = TRUE)

if(export.HCAHPS)
  exportData(dir.export, HCAHPS.data.all, "HCAHPS_data_all.csv")

##BROADBAND DATA
states <- c("01","02","03","04","05","06","07","08","09", 10:50)
BB.data <- lapply(states, function(x) getURL(paste(paste(url.broadband.pt1, x, sep=""), url.broadband.pt2, sep="")))  #pull data

BB.data.all <- lapply(BB.data, fromJSON)  #put into list
BB.data.all <- lapply(BB.data.all, function(x) Reduce(rbind, x[4]))  #extract dfs
BB.data.all <- bind_rows(Reduce(rbind, BB.data.all))  #row bind dfs

BB.data.all <- select(BB.data.all, geographyId, downloadSpeedGreaterThan10000k)
colnames(BB.data.all) <- c("FIPS", "BroadBand.Score")

BB.data.all <- merge(select(FIPS.lookup, FIPS, State, County), BB.data.all, by = "FIPS", all = TRUE)

#By national percentile
BB.data.national <- normalizedScore(BB.data.all, "BroadBand.Score", "nation", "BroadBand.Score")
BB.data.national <- merge(select(FIPS.lookup, FIPS, State, County), BB.data.national, by = "FIPS", all = TRUE)

#By state percentile
BB.data.state <- normalizedScore(BB.data.all, "BroadBand.Score", "state", "BroadBand.Score")
BB.data.state <- merge(select(FIPS.lookup, FIPS, State, County), BB.data.state, by = "FIPS", all = TRUE)

##HPSA DATA
HPSA.data <- readData(dir.data.HPSA, "character")

HPSA.data <- lapply(HPSA.data, filter, HPSA.Type.Code == "Hpsa Geo", HPSA.Status.Description != "Withdrawn")  #filter all but geo
HPSA.data <- lapply(HPSA.data, filter
                    , HPSA.State.Abbreviation != "AS"
                    , HPSA.State.Abbreviation != "FM"
                    , HPSA.State.Abbreviation != "GU"
                    , HPSA.State.Abbreviation != "MH"
                    , HPSA.State.Abbreviation != "MP"
                    , HPSA.State.Abbreviation != "PW"
                    , HPSA.State.Abbreviation != "PR"
                    , HPSA.State.Abbreviation != "VI"
                    , HPSA.State.Abbreviation != "UM")  #filter out unneeded states
HPSA.data <- lapply(HPSA.data, select, State.and.County.Federal.Information.Processing.Standard.Code, HPSA.Source.Name, HPSA.Score, HPSA.Shortage)
HPSA.data <- lapply(HPSA.data, setNames, c("FIPS", "HPSA.Source.Name", "HPSA.Score", "HPSA.Shortage"))

#extract list of unique FIPS and calculate average
HPSA.FIPS <- lapply(HPSA.data, function(x) unique(x$FIPS))

HPSA.data <- Map(function(x,y) lapply(x, function(z) filter(y, FIPS == z)), HPSA.FIPS, HPSA.data)  #create dataframe of dataframes grouped by FIPS
HPSA.data <- lapply(HPSA.data, function(x) lapply(x, function(y) cbind(y$FIPS[1]
                                                                               , mean(as.numeric(as.data.table(y)[, .SD[which.max(HPSA.Score)], by = HPSA.Source.Name]$HPSA.Score))
                                                                               , mean(as.numeric(as.data.table(y)[, .SD[which.max(HPSA.Shortage)], by = HPSA.Source.Name]$HPSA.Shortage)))))  #compute average
HPSA.data <- lapply(HPSA.data, function(...) do.call(rbind.data.frame, ...))  #merge together

#fix cols
#TODO: cleanup
HPSA.data <- lapply(HPSA.data, setNames, c("FIPS", "HPSA.Score", "HPSA.Shortage"))
HPSA.data <- lapply(HPSA.data, function(df) transform(df
                                                      , FIPS = as.character(FIPS)
                                                      , HPSA.Score = as.numeric(as.character(HPSA.Score))
                                                      , HPSA.Shortage = as.numeric(as.character(HPSA.Shortage))))

#merge with FIPS lookup
HPSA.data <- lapply(HPSA.data, function(df) merge(select(FIPS.lookup, FIPS, State, County), df, by = "FIPS", all = TRUE))

#By national percentile ONLY USING PHYSICIANS
HPSA.data.national <- normalizedScore(HPSA.data$BCD_HPSA_FCT_DET_PC, "HPSA.Score", "nation", "PhysicianShortage.Score")
HPSA.data.national <- merge(select(FIPS.lookup, FIPS, State, County), HPSA.data.national, by = "FIPS", all = TRUE)

#Bt state percentile ONLY USING PHYSICIANS
HPSA.data.state <- normalizedScore(HPSA.data$BCD_HPSA_FCT_DET_PC, "HPSA.Score", "state", "PhysicianShortage.Score")
HPSA.data.state <- merge(select(FIPS.lookup, FIPS, State, County), HPSA.data.state, by = "FIPS", all = TRUE)

##MERGE ALL NATIONAL AND STATE TABLES
#TODO: Put national and state tables into a list beforehand
national.all <- merge(CHSI.CHR.data.national, HPSA.data.national)
national.all <- merge(national.all, BB.data.national)
national.all <- merge(national.all, HCAHPS.data.national)

state.all <- merge(CHSI.CHR.data.state, HPSA.data.state)
state.all <- merge(state.all, BB.data.state)
state.all <- merge(state.all, HCAHPS.data.state)


##COMPUTE TELEHEALTH IMPACT SCORE
national.all <- cbind(national.all, apply(national.all[4:13], 1, function(r)
  if(sum(is.na(r)) == length(r))
    return(NA)
  else
    return(sum(r, na.rm = TRUE))))
colnames(national.all)[14] <- "Telehealth.Score.Raw"

state.all <- cbind(state.all, apply(state.all[4:13], 1, function(r)
  if(sum(is.na(r)) == length(r))
    return(NA)
  else
    return(sum(r, na.rm = TRUE))))
colnames(state.all)[14] <- "Telehealth.Score.Raw"

##SMOOTHING
GIS.data <- readOGR(dir.data.GIS, "OGRGeoJSON")

temp <- countySmooth(national.all, GIS.data, 0.25)
national.all <- merge(national.all, temp, by = "FIPS")
national.all$Telehealth.Score.Raw.Adj <- rowSums(national.all[14:15], na.rm = TRUE)

temp <- countySmooth(state.all, GIS.data, 0.25)
state.all <- merge(state.all, temp, by = "FIPS")
state.all$Telehealth.Score.Raw.Adj <- rowSums(state.all[14:15], na.rm = TRUE)

##renormalize
temp <- normalizedScore(national.all, "Telehealth.Score.Raw.Adj", "nation", "Telehealth.Score.Final", method = "uniform")
colnames(temp)[1] <- "FIPS"
national.all <- merge(national.all, temp, by = "FIPS")

temp <- normalizedScore(state.all, "Telehealth.Score.Raw.Adj", "state", "Telehealth.Score.Final", method = "uniform")
colnames(temp)[1] <- "FIPS"
state.all <- merge(state.all, temp, by = "FIPS")

rm(temp)

#compute CHSI score and adjust units
national.all <- cbind(national.all, apply(national.all[4:7], 1, function(r)
  if(sum(is.na(r)) == length(r))
    return(NA)
  else
    return(sum(r, na.rm = TRUE))))
colnames(national.all)[18] <- "CHSI.Score"
national.all[4:18] <- national.all[4:18]*10
national.all$Telehealth.Score.Final <- national.all$Telehealth.Score.Final*10

state.all <- cbind(state.all, apply(state.all[4:7], 1, function(r)
  if(sum(is.na(r)) == length(r))
    return(NA)
  else
    return(sum(r, na.rm = TRUE))))
colnames(state.all)[18] <- "CHSI.Score"
state.all[4:18] <- state.all[4:18]*10
state.all$Telehealth.Score.Final <- state.all$Telehealth.Score.Final*10

#export data
setwd(dir.export)
write.csv(national.all, "telehealth-score_national.csv", row.names = FALSE)
write.csv(state.all, "telehealth-score_state.csv", row.names = FALSE)
