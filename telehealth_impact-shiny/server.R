
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)
library(rgdal)
library(rgeos)
library(magrittr)
library(dplyr)
library(RColorBrewer)
library(data.table)
library(maptools)

shinyServer(function(input, output, session) {

# Show progress bar while loading everything ------------------------------

  progress <- shiny::Progress$new()
  progress$set(message="Loading maps/data", value=0)

# Read US in map data
  us <- readOGR("data/us.geojson", "OGRGeoJSON")
  us <- us[!us$STATEFP %in% c("72"),]

  #us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
  #us_aea <- spTransform(us, CRS("+proj=ortho +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
  #us_aea <- spTransform(us, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  us_aea <- spTransform(us, CRS("+proj=robin +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

  proj.4.string <- "+proj=robin +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  proj.4.string.raw <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  #proj.4.string.google <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=45 +lon_0=-100 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
  #proj.4.string <- "+proj=ortho +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
  #proj.4.string  <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  us_aea <- spTransform(us, CRS(proj.4.string))
  
  map <- ggplot2::fortify(us_aea, region="GEOID")

  # Read in data
  files <- list.files("data", pattern = '*.csv')
  dataColcasses <- c("FIPS" = "character"
                     , "State" = "character"
                     , "County" = "character")
  data.all <- lapply(paste0('data/', files), read.csv, colClasses = dataColcasses)
  names(data.all) <- gsub("\\.csv$", "", files)

  data.names <- colnames(data.all$National)
  data.names <- c("id", data.names[2:7], "Poor/Fair Heath", data.names[9], "HPSA"
                  , "Broadband", "Hospital IT Readiness", "Readmissions", data.names[14:15]
                  , "Telemedicine Score Raw", "Score", "CHSI Score")

  data.all <- lapply(data.all, setNames, data.names) %>%
      lapply(function(df) cbind(df[ , c(1:3)], lapply(df[ , -c(1:3)], function(x) round(x, 2))))

  map_d <- lapply(data.all, function(df) merge(map, select(df, id, State, Score), all.x = TRUE))

  ramp <- colorRampPalette(c("white", brewer.pal(n=9, name="Blues")), space="Lab")

  map_d.names <- c(colnames(map_d$National), "fill_col")
  map_d <- lapply(map_d, function(df) cbind(df, cut(df$Score, seq(0,100,10), include.lowest=TRUE, labels=ramp(10))))
  map_d <- lapply(map_d, setNames, map_d.names)
  map_d <- lapply(map_d, function(df) transform(df, fill_col = as.character(fill_col)))
  map_d <- lapply(map_d, function(df) cbind(df, ifelse(is.na(df$fill_col), "#FFFFFF", df$fill_col)))
  map_d <- lapply(map_d, function(df) transform(df, fill_col = as.character(fill_col)))
  map_d <- lapply(map_d, function(df) select(df, -fill_col))
  map_d <- lapply(map_d, setNames, map_d.names)

  display.data <- reactive(data.all[[input$level]])

  map.width <- reactive({
    if(input$view == "all")
      map.width = 1100
    else {
      #map.width = (range(display.map()$lat)[2] - range(display.map()$lat)[1])/(range(map_d$lat)[2] - range(map_d$lat)[1])
      map.width = 650
    }
  })
  
  # Read in hospital data
  hospitals <- read.csv('data/hospital/Hospitals.csv', colClasses = "character")
  hospitals <- transform(hospitals, lat = as.numeric(lat), long = as.numeric(long))
  hospitals <- na.omit(hospitals)
  coordinates(hospitals) <- ~long+lat
  proj4string(hospitals) <- CRS(proj.4.string.raw)
  hospitals <- spTransform(hospitals, CRS(proj.4.string))
  hospitals <- cbind(hospitals@data, hospitals@coords)
  
  display.data <- reactive(data.all[[input$level]])
  
  display.hospitals <- reactive({
    if(input$view == "all") 
      display.hospitals <- select(filter(hospitals, State != 'AK', State != 'HI'), Name, long, lat)
    else
      display.hospitals <- select(filter(hospitals, State == input$view), Name, long, lat)
    return(display.hospitals)
    })
  
  map.width <- reactive({
    if(input$view == "all")
      map.width = 900
    else 
      map.width = 500
    return(map.width)
  })

  display.map <- reactive({
    if(input$view == "all")
      display.map = filter(map_d[[input$level]], State != 'AK', State != 'HI')
    else
      display.map = filter(map_d[[input$level]], State == input$view)
    return(display.map)
  })

  tooltip.data <- reactive({
    if(input$view == "all")
      tooltip.data <- c(2, 3, 17)
    else
      tooltip.data <- c(3, 8:13, 18, 16, 17)
    return(tooltip.data)
  })

  tooltips <- function(x) {
    if(is.null(x))
      return(NULL)
    else if("id" %in% colnames(x))
      y <- display.data() %>% filter(id==x$id) %>% select(tooltip.data())
    else if("Name" %in% colnames(x))
      y <- display.hospitals() %>% filter(Name == x$Name) %>% select(Name)
    sprintf("<table width='100%%'>%s</table>",
            paste0("<tr><td style='text-align:left'>", names(y),
                   ":</td><td style='text-align:right'>", format(y), collapse="</td></tr>"))
  }

  vis.map <- reactive({
    display.map() %>%
      group_by(group, id) %>%
      ggvis(x = ~long, y = ~lat) %>%
      layer_paths(fill := ~fill_col, strokeOpacity := 0.75, strokeWidth := 0.5) %>%
      add_tooltip(tooltips, "hover") %>%
      hide_legend("fill") %>%
      hide_axis("x") %>% hide_axis("y") %>%
      set_options(width=map.width(), height=650, keep_aspect = FALSE, resizable = TRUE)
})
  
  vis <- reactive({
    if(input$hospital) {
      vis <- vis.map() %>%
        layer_points(data = display.hospitals(), key := ~Name, x = ~long, y = ~lat, size := 10)
    }
    else
      vis <- vis.map()
    return(vis)
  })
  
  vis %>% bind_shiny('map')
# Turn off progress bar ---------------------------------------------------

  progress$close()

})