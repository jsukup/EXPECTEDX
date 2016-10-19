###Script to analyze final Telehealth Impact Score (TIS) study

##Load libraries
library(dplyr)
library(psych)

##Load scoring data
nat <- read.csv('./Data/National.csv', header = TRUE)
st <- read.csv('./Data/State.csv', header = TRUE)
hosp <- read.csv('./Data/Hospitals.csv', header = TRUE)
met <- read.csv('./Data/metrolist.csv', header = FALSE)

st.met <- left_join(st, met[,c('V3','V4')], by = c('FIPS' = 'V3')) #Merge by FIPS
met.only <- subset(st.met, !is.na(st.met$V4)) #Keep only counties within MSAs

best.in.state <- unique(met.only[which(met.only$Telehealth.Score.Final == 100), 'V4']) #Extract MSA counties in each state with TIS of 100 'perfect score"

##Calculate mean scores by state
st.good <- st[,c(2,4,5,8:18)] #Subset data set to remove FIPS and county name
means <- aggregate(st.good[,colnames(st.good[2:14])],st.good['State'], mean, na.rm = TRUE) #Calculate column means

desc.by.state <- describeBy(st.good[,2:14], st.good$State, na.rm = TRUE)

