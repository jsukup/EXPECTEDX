
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

# National Level

  us <- readOGR("data/us.geojson", "OGRGeoJSON")
  us <- us[!us$STATEFP %in% c("02", "15", "72"),]

  us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

  map <- ggplot2::fortify(us_aea, region="GEOID")

  telehealth.national <- read.csv("data/telehealth-score_national.csv"
                                  , colClasses = c("FIPS" = "character"
                                                   , "State" = "character"
                                                   , "County" = "character"))
  colnames(telehealth.national)[c(1, 23)] <- c("id", "Score")

  map_d.national <- merge(map, select(telehealth.national, id, Score), all.x=TRUE)

  ramp <- colorRampPalette(c("white", brewer.pal(n=9, name="Purples")), space="Lab")

  map_d.national$fill_col <- as.character(cut(map_d.national$Score, seq(0,100,10), include.lowest=TRUE, labels=ramp(10)))
  map_d.national$fill_col <- ifelse(is.na(map_d.national$fill_col), "#FFFFFF", map_d.national$fill_col)

  telehealth_values.national <- function(x) {
    if(is.null(x) | !(x$id %in% telehealth.national$id)) return(NULL)
    y <- telehealth.national %>% filter(id==x$id) %>% select(2, 3, 23)
    sprintf("<table width='100%%'>%s</table>",
            paste0("<tr><td style='text-align:left'>", names(y),
                   ":</td><td style='text-align:right'>", format(y), collapse="</td></tr>"))
  }

  map_d.national %>%
    group_by(group, id) %>%
    ggvis(~long, ~lat) %>%
    layer_paths(fill:=~fill_col, strokeOpacity := 0.5, strokeWidth := 0.25) %>%
    add_tooltip(telehealth_values.national, "hover") %>%
    hide_legend("fill") %>%
    hide_axis("x") %>% hide_axis("y") %>%
    set_options(width=900, height=600, keep_aspect=TRUE) %>%
    bind_shiny("telehealth_national")


  # State Level

  #us <- readOGR("data/us.geojson", "OGRGeoJSON")
  #us <- us[!us$STATEFP %in% c("02", "15", "72"),]
  
  #us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
  
  #map <- ggplot2::fortify(us_aea, region="GEOID")
  
  telehealth.state <- read.csv("data/telehealth-score_state.csv"
                               , colClasses = c("FIPS" = "character"
                                                , "State" = "character"
                                                , "County" = "character"))
  colnames(telehealth.state)[c(1, 23)] <- c("id", "Score")
  
  map_d.state <- merge(map, select(telehealth.state, id, Score), all.x=TRUE)
  
  ramp <- colorRampPalette(c("white", brewer.pal(n=9, name="Purples")), space="Lab")
  
  map_d.state$fill_col <- as.character(cut(map_d.state$Score, seq(0,100,10), include.lowest=TRUE, labels=ramp(10)))
  map_d.state$fill_col <- ifelse(is.na(map_d.state$fill_col), "#FFFFFF", map_d.state$fill_col)
  
  telehealth_values.state <- function(x) {
    if(is.null(x) | !(x$id %in% telehealth.state$id)) return(NULL)
    y <- telehealth.state %>% filter(id==x$id) %>% select(2, 3, 23)
    sprintf("<table width='100%%'>%s</table>",
            paste0("<tr><td style='text-align:left'>", names(y),
                   ":</td><td style='text-align:right'>", format(y), collapse="</td></tr>"))
  }
  
  map_d.state %>%
    group_by(group, id) %>%
    ggvis(~long, ~lat) %>%
    layer_paths(fill:=~fill_col, strokeOpacity := 0.5, strokeWidth := 0.25) %>%
    add_tooltip(telehealth_values.state, "hover") %>%
    hide_legend("fill") %>%
    hide_axis("x") %>% hide_axis("y") %>%
    set_options(width=900, height=600, keep_aspect=TRUE) %>%
    bind_shiny("telehealth_state")

# Turn off progress bar ---------------------------------------------------

  progress$close()

})