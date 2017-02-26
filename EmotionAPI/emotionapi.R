##Microsoft Emotion API - Video Ad Testing

##Load libraries
library(httr)
library(magrittr)
library(purrr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)

apiUrl <- 'https://westus.api.cognitive.microsoft.com/emotion/v1.0/recognizeinvideo?outputStyle=perFrame' #API endpoint URL

key <- '935271b2f2ae41e7b4298ce3c7216ec1' #Auth key

orig_url <- 'https://1drv.ms/v/s!As_lzte5x5PDaPyuv0F3AN91nHo' #Video URL

##Rename video link for direct download
urlVideo <- RCurl::base64(orig_url) %>% 
    gsub('\\+', '\\-', .) %>% 
    gsub('\\/', '\\_', .) %>% 
    sub('\\=$', '', .) %>% 
    paste0('https://api.onedrive.com/v1.0/shares/u!', ., '/root/content')

mybody <- list(url = urlVideo) #Put URL in list for API POST

##Pass video to Emotion API for processing
faceEMO <- httr::POST(
    url = apiUrl,
    httr::content_type('application/json'),
    httr::add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key)),
    body = mybody,
    encode = 'json'
)

operationLocation <- httr::headers(faceEMO)[['operation-location']] #Extract link of video processing location for status updates

##Monitor video processing
while(TRUE){
    ret <- httr::GET(operationLocation,
                     httr::add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key)))
    con <- httr::content(ret)
    if(is.null(con$status)){
        warning('Connection Error, retry after 1 minute')
        Sys.sleep(60)
    } else if (con$status == 'Running' | con$status == 'Uploading'){
        cat(paste0('status ', con$status, '\n'))
        cat(paste0('progress ', con$progress, '\n'))
        Sys.sleep(60)
    } else {
        cat(paste0('status ', con$status, '\n'))
        break()
    }
}

data <- (con$processingResult %>% jsonlite::fromJSON())$fragments #Extract JSON of processed video

##Convert JSON to data frame
data$events <- purrr::map(data$events, function(events){
    events %>% purrr::map(function(event){
        jsonlite::flatten(event)
    }) %>% dplyr::bind_rows()
})

data <- unnest(data,events) #Unnest events variable into individual emotion scores
data <- data[-1831,] #Remove final row to make rows divisable by 61 seconds (total test video length 1:01)
data$seconds <- rep(1:61, each = 30) #Add time stamp in seconds

data <- aggregate(data[,9:16], by = list(seconds = data$seconds), mean) #Aggregate scores by seconds
