library(readr)
library(tidyverse)
library(scales)
library(lubridate)
library(stringr)

hv <- read_csv("C:/analyticsdev/Projects/EXPECTEDX/hawaiitourismtracker/data/historicvisitors.csv", skip = 1, n_max = 13)[-1,]

hv_new <- hv %>% 
    gather(arrival, ppl, Total:International_25) %>% 
    mutate(year = rep(1990:2015, each = 36)) %>% 
    rename(month = X1) %>% 
    mutate(date = paste(month, year, sep = '-')) 
    

hv_new$arrival <- factor(str_replace(hv_new$arrival, '_[0-9]+',''), levels = c('Total','Domestic','International'))
hv_new$month <- factor(hv_new$month, levels = month.abb, labels = month.abb)
hv_new$date <- parse_datetime(hv_new$date, format = '%b-%Y')

ggplot(hv_new, aes(x = as.Date(date), y = ppl, group = arrival, color = arrival)) +
    geom_line() +
    geom_smooth(se = FALSE, 
                lty = 1, 
                cex = 1.25) +
    scale_y_continuous(labels = comma) +
    scale_x_date(date_breaks = '1 year', 
                 date_labels = '%Y',
                 expand = c(0,0)) +
    scale_color_brewer(palette = 'Dark2') +
    labs(title = '1990 - 2015 Hawai\'i Tourist Visitors',
         x = 'Year',
         y = 'Arrivals',
         color = 'Source') +
    theme(axis.text.x = element_text(angle = 90, hjust = 0))

ggplot(hv_new, aes(x = month, y = ppl, group = interaction(month, arrival), color = arrival)) +
    geom_boxplot() +
    scale_y_continuous(labels = comma) +
    scale_color_brewer(palette = 'Dark2') +
    labs(title = '1990 - 2015 Hawai\'i Tourist Visitors',
         x = 'Month',
         y = 'Arrivals',
         color = 'Source')

hv_mth <- aggregate(ppl ~ month + arrival, data = hv_new, mean)

ggplot(hv_mth, aes(x = month, y = ppl, group = arrival, color = arrival)) +
    geom_line(lty = 1, 
              cex = 1.25) + 
    scale_y_continuous(labels = comma) +
    scale_color_brewer(palette = 'Dark2') +
    labs(title = '1990 - 2015 Hawai\'i Tourist Visitors (Monthly Average)',
         x = 'Month',
         y = 'Arrivals',
         color = 'Source')