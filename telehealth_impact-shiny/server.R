
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

  telehealth <- read.csv("data/telehealth-score_national.csv")
  telehealth$id <- sprintf("%05d", as.numeric(as.character(telehealth$FIPS)))
  telehealth$total <- with(telehealth, Telehealth.Score.Final)

  map_d <- merge(map, telehealth, all.x=TRUE)

  ramp <- colorRampPalette(c("white", brewer.pal(n=9, name="YlOrRd")), space="Lab")

  map_d$fill_col <- as.character(cut(map_d$total, seq(0,100,10), include.lowest=TRUE, labels=ramp(10)))
  map_d$fill_col <- ifelse(is.na(map_d$fill_col), "#FFFFFF", map_d$fill_col)

  telehealth_values <- function(x) {
    if(is.null(x) | !(x$id %in% telehealth$id)) return(NULL)
    y <- telehealth %>% filter(id==x$id) %>% select(2, 3, 21)
    sprintf("<table width='100%%'>%s</table>",
            paste0("<tr><td style='text-align:left'>", names(y),
                   ":</td><td style='text-align:right'>", format(y), collapse="</td></tr>"))
  }

  map_d %>%
    group_by(group, id) %>%
    ggvis(~long, ~lat) %>%
    layer_paths(fill:=~fill_col, strokeOpacity := 0.5, strokeWidth := 0.25) %>%
    add_tooltip(telehealth_values, "hover") %>%
    hide_legend("fill") %>%
    hide_axis("x") %>% hide_axis("y") %>%
    set_options(width=900, height=600, keep_aspect=TRUE) %>%
    bind_shiny("telehealth_national")


  # State Level

  us <- readOGR("data/us.geojson", "OGRGeoJSON")
  us <- us[!us$STATEFP %in% c("02", "15", "72"),]

  us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

  map <- ggplot2::fortify(us_aea, region="GEOID")

  telehealth <- read.csv("data/telehealth-score_state.csv")
  telehealth$id <- sprintf("%05d", as.numeric(as.character(telehealth$FIPS)))
  telehealth$total <- with(telehealth, Telehealth.Score.Final)

  map_d <- merge(map, telehealth, all.x=TRUE)

  ramp <- colorRampPalette(c("white", brewer.pal(n=9, name="YlOrRd")), space="Lab")

  map_d$fill_col <- as.character(cut(map_d$total, seq(0,100,10), include.lowest=TRUE, labels=ramp(10)))
  map_d$fill_col <- ifelse(is.na(map_d$fill_col), "#FFFFFF", map_d$fill_col)

  telehealth_values <- function(x) {
    if(is.null(x) | !(x$id %in% telehealth$id)) return(NULL)
    y <- telehealth %>% filter(id==x$id) %>% select(2, 3, 21)
    sprintf("<table width='100%%'>%s</table>",
            paste0("<tr><td style='text-align:left'>", names(y),
                   ":</td><td style='text-align:right'>", format(y), collapse="</td></tr>"))
  }

  map_d %>%
    group_by(group, id) %>%
    ggvis(~long, ~lat) %>%
    layer_paths(fill:=~fill_col, strokeOpacity := 0.5, strokeWidth := 0.25) %>%
    add_tooltip(telehealth_values, "hover") %>%
    hide_legend("fill") %>%
    hide_axis("x") %>% hide_axis("y") %>%
    set_options(width=900, height=600, keep_aspect=TRUE) %>%
    bind_shiny("telehealth_state")

# Turn off progress bar ---------------------------------------------------

  progress$close()

})