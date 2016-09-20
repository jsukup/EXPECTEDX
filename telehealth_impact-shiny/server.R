
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
  us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
  #us_aea <- spTransform(us, CRS("+proj=ortho +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
  #us_aea <- spTransform(us, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

  map <- ggplot2::fortify(us_aea, region="GEOID")

  # Read in data
  files <- list.files("data", pattern = '*.csv')
  dataColcasses <- c("FIPS" = "character"
                     , "State" = "character"
                     , "County" = "character")
  data.all <- lapply(paste0('data/', files), read.csv, colClasses = dataColcasses)
  names(data.all) <- gsub("\\.csv$", "", files)
  
  data.names <- colnames(data.all$National)
  data.names <- c("id", data.names[2:13], "Poor Heath", "Diabetes", "Physician"
                  , "Broadband", "Structural", "RaD", data.names[20:21]
                  , "Telehealth Score Raw", "Score", "CHSI Score")
  
  data.all <- lapply(data.all, setNames, data.names)
  
  map_d <- lapply(data.all, function(df) merge(map, select(df, id, State, Score), all.x = TRUE))

  ramp <- colorRampPalette(c("white", brewer.pal(n=9, name="Purples")), space="Lab")

  map_d.names <- c(colnames(map_d$National), "fill_col")
  map_d <- lapply(map_d, function(df) cbind(df, cut(df$Score, seq(0,100,10), include.lowest=TRUE, labels=ramp(10))))
  map_d <- lapply(map_d, setNames, map_d.names)
  map_d <- lapply(map_d, function(df) transform(df, fill_col = as.character(fill_col)))
  map_d <- lapply(map_d, function(df) cbind(df, ifelse(is.na(df$fill_col), "#FFFFFF", df$fill_col)))
  map_d <- lapply(map_d, function(df) transform(df, fill_col = as.character(fill_col)))
  map_d <- lapply(map_d, function(df) select(df, -fill_col))
  map_d <- lapply(map_d, setNames, map_d.names)
  
  display.data <- reactive(data.all[[input$level]])
  
  display.map <- reactive({
    if(input$view == "all")
      display.map = filter(map_d[[input$level]], State != 'AK', State != 'HI')
    else
        display.map = filter(map_d[[input$level]], State == input$view)
    return(display.map)
  })
  
  tooltip.data <- reactive({
    if(input$view == "all") 
      tooltip.data <- c(2, 3, 23)
    else
      tooltip.data <- c(3, 14:19, 24, 20, 23)
    return(tooltip.data)
  })
  
  tooltips <- function(x) {
    if(is.null(x) | !(x$id %in% display.data()$id)) return(NULL)
    y <- display.data() %>% filter(id==x$id) %>% select(tooltip.data())
    sprintf("<table width='100%%'>%s</table>",
            paste0("<tr><td style='text-align:left'>", names(y),
                  ":</td><td style='text-align:right'>", format(y), collapse="</td></tr>"))
    }
  
    display.map %>%
      group_by(group, id) %>%
      ggvis(~long, ~lat) %>%
      layer_paths(fill:=~fill_col, strokeOpacity := 0.75, strokeWidth := 0.5) %>%
      add_tooltip(tooltips, "hover") %>%
      hide_legend("fill") %>%
      hide_axis("x") %>% hide_axis("y") %>%
      set_options(width=900, height=600, keep_aspect=TRUE) %>%
      bind_shiny("map")

# Turn off progress bar ---------------------------------------------------

  progress$close()

})