##BSA Project for Sales Benchmark Index client Aerotech Rocketry
##Predict next year's 'jobtype' using historical data

library(dplyr)
library(lubridate)
library(BTYD)
library(RMySQL)
library(plotly)
library(markovchain)
set.seed(734)

#Connect to SQL DB
# con <- dbConnect(RMySQL::MySQL(),
#                  user='johnsukup',
#                  password='Mj!v29Tp#b04',
#                  dbname='SBI_db',
#                  host='52.89.113.86')
#
# nos <- dbGetQuery(con, 'CALL Query_Number_of_Sales()')

con <- dbConnect(RMySQL::MySQL(),
                 user='johnsukup',
                 password='Mj!v29Tp#b04',
                 dbname='SBI_db',
                 host='52.89.113.86'
)

sr <- dbGetQuery(con, 'CALL Query_Sales_Rank()')
dbDisconnect(con)

#Cleaning
sr <- select(sr, c(1:5, 7, 18, 20:25))
names(sr)[8] <- 'Lookup2'
sr$Customer_Number <- as.numeric(sr$Customer_Number)
sr$Customer_Type <- factor(sr$Customer_Type)
levels(sr$Customer_Type)[1] <- NA
sr$Mrkt_Grp <- factor(sr$Mrkt_Grp)
sr$Territory <- factor(sr$Territory)
sr$Industry_Lookup <- factor(sr$Industry_Lookup)
levels(sr$Industry_Lookup)[1] <- NA
sr$Lookup2 <- factor(sr$Lookup2)
levels(sr$Lookup2)[1] <- NA
sr$Job_Type <- factor(sr$Job_Type, levels = c('A JOB TYPE','B JOB TYPE','C JOB TYPE','D JOB TYPE'), labels = c('A','B','C','D'))
sr$Last_Booking_Date <- as.Date(sr$Last_Booking_Date)

#BTYD Analysis
btyd.vs <- select(sr, Customer_Number, Last_Booking_Date, Extended_Price)
names(btyd.vs) <- c('cust','date','sales')

btyd.freq <- dc.ElogToCbsCbt(btyd.vs,
                             per = 'month',
                             statistic = 'freq',
                             T.cal = as.Date('2014-12-31')) #Purchase log to CBT and CBS

btyd.reach <- dc.ElogToCbsCbt(btyd.vs,
                             per = 'month',
                             statistic = 'reach',
                             T.cal = as.Date('2014-12-31')) #Purchase log to CBT and CBS

btyd.tospend <- dc.ElogToCbsCbt(btyd.vs,
                             per = 'month',
                             statistic = 'total.spend',
                             T.cal = as.Date('2014-12-31')) #Purchase log to CBT and CBS

btyd.avspend <- dc.ElogToCbsCbt(btyd.vs,
                             per = 'month',
                             statistic = 'average.spend',
                             T.cal = as.Date('2014-12-31')) #Purchase log to CBT and CBS
