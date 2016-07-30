##ANALYSIS OF CHSI DATA - COMMUNITY HEALTH DATA INITIATIVE
##CHSI OBTAINED FROM: https://catalog.data.gov/dataset/community-health-status-indicators-chsi-to-combat-obesity-heart-disease-and-cancer
##CHR OBTAINED FROM:
##NHE OBTAINED FROM:

##OBTAINED ON: 7/22/16

##Read in CHSI and CHR data files
setwd('c:/program files (x86)/git/projects/expectedx/data') #Temporary working directory
files <- list.files(pattern="*.csv")
for (i in 4:length(files)) 
    assign(files[i], read.csv(files[i]))

chr2015 <- read.csv('chr2015.csv', header = TRUE, stringsAsFactors = FALSE)
setwd('c:/program files (x86)/git/projects/expectedx')


