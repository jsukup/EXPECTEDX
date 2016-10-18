###Script to analyze final Telehealth Impact Score (TIS) study

##Load libraries
library(dplyr)

##Load scoring data
nat <- read.csv('./Data/National.csv', header = TRUE)
st <- read.csv('./Data/State.csv', header = TRUE)
hosp <- read.csv('./Data/Hospitals.csv', header = TRUE)
met <- read.csv('./Data/metrolist.csv', header = FALSE)

st.met <- left_join(st, met[,c('V3','V4')], by = c('FIPS' = 'V3')) #Merge by FIPS
met.only <- subset(st.met, !is.na(st.met$V4)) #Keep only counties within MSAs

best.in.state <- unique(met.only[which(met.only$Telehealth.Score.Final == 100), 'V4']) #Extract MSA counties in each state with TIS of 100 'perfect score"

 
