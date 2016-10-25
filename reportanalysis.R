###Script to analyze final Telehealth Impact Score (TIS) study

##Load libraries
library(dplyr)
library(psych)
library(ggplot2)
library(extrafont)

##Load scoring data
nat <- read.csv('./Data/National.csv', header = TRUE)
st <- read.csv('./Data/State.csv', header = TRUE)
met <- read.csv('./Data/metrolist.csv', header = FALSE)

st.met <- left_join(st, met[,c('V3','V4')], by = c('FIPS' = 'V3')) #Merge by FIPS
met.only <- subset(st.met, !is.na(st.met$V4)) #Keep only counties within MSAs

best.in.state <- unique(met.only[which(met.only$Telehealth.Score.Final == 100), 'V4']) #Extract MSA counties in each state with TIS of 100 'perfect score"

##Calculate mean scores by state
st.good <- st[,c(2,4,5,8:18)] #Subset data set to remove FIPS and county name
means <- aggregate(st.good[,colnames(st.good[2:14])],st.good['State'], mean, na.rm = TRUE) #Calculate column means

desc.by.state <- describeBy(st.good[,2:14], st.good$State, mat = TRUE, digits = 2, na.rm = TRUE) #Descriptive statistics by state
desc.by.state$metric <- gsub('[0-9]','', x = rownames(desc.by.state)) #Create metric label
rownames(desc.by.state) <- NULL #Drop rownames

##Deep dive: IL and CA
il.counties <- st[st$State == 'IL',] #Subset counties by IL
ca.counties <- st[st$State == 'CA',] #Subset counties by CA

font_import()
loadfonts(device = 'win') #Load fonts

##Histogram/density plot for TIS in IL
png(filename = './Data/iltis.png', width = 950, height = 600)
ggplot(il.counties, aes(Telehealth.Score.Raw.Adj)) +
    geom_histogram(aes(y = ..density..),
                   binwidth = 5,
                   color = 'Black', fill = '#2D4471') +
    geom_density(alpha = .2,
                 fill = '#AAA838') +
    geom_vline(aes(xintercept = median(Telehealth.Score.Raw.Adj, na.rm = T)),
               color = '#A7373D',
               linetype = 4,
               size = 2) +
    labs(title = 'Distribution of Adjusted, Raw TIS: IL',
         y = 'Density',
         x = 'TIS') +
    annotate('text',
             x = 58,
             y = .04,
             label = 'Median IL TIS:\n 52.7',
             fontface = 'bold',
             family = 'Calibri') +
    annotate('segment',
             x = 111,
             xend = 111,
             y = .01,
             yend = .005,
             colour = "#2D4471",
             size = 1,
             arrow = arrow()) +
    annotate('text',
             x = 111,
             y = .012,
             label = c('Cook County:\n 111 Raw TIS'),
             fontface = 'bold',
             family = 'Calibri') +
    theme(plot.title = element_text(family = 'Calibri', face = 'bold', size = 20),
          axis.title = element_text(family = 'Calibri', face = 'bold', size = 16))
dev.off()

##Histogram/density plot for TIS in CA
png(filename = './Data/catis.png', width = 950, height = 600)
ggplot(ca.counties, aes(Telehealth.Score.Raw.Adj)) +
    geom_histogram(aes(y = ..density..),
                   binwidth = 5,
                   color = 'Black', fill = '#2D4471') +
    geom_density(alpha = .2,
                 fill = '#AAA838') +
    geom_vline(aes(xintercept = median(Telehealth.Score.Raw.Adj, na.rm = T)),
               color = '#A7373D',
               linetype = 4,
               size = 2) +
    labs(title = 'Distribution of Adjusted, Raw TIS: CA',
         y = 'Density',
         x = 'TIS') +
    annotate('text',
             x = 64,
             y = .04,
             label = 'Median CA TIS:\n 59.2',
             fontface = 'bold',
             family = 'Calibri') +
    annotate('segment',
             x = 99,
             xend = 99,
             y = .015,
             yend = .01,
             colour = "#2D4471",
             size = 1,
             arrow = arrow()) +
    annotate('text',
             x = 99,
             y = .017,
             label = 'St. Clair County:\n 99 Raw TIS',
             fontface = 'bold',
             family = 'Calibri') +
    theme(plot.title = element_text(family = 'Calibri', face = 'bold', size = 20),
          axis.title = element_text(family = 'Calibri', face = 'bold', size = 16))
dev.off()
