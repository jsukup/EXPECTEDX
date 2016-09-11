
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)


shinyUI(fluidPage(
  titlePanel("Telehealth Impact"),
    mainPanel(
      tabsetPanel(
        tabPanel("National Level"
                 , wellPanel(p("A National Map of all US Counties colored by Telehealth Score.
                               Scores were normalized by dividing each county score by the largest national score.
                               In other words, a score of 100 represents the county in the entire United States that would benefit the most from telehealth."
                               ))
                 , ggvisOutput("telehealth_national")),
        tabPanel("State Level"
                 , wellPanel(p("A National Map of all US Counties colored by Telehealth Score.
                               Scores were normalized by dividing each county score by the largest score within the state.
                               In other words, a score of 100 represents the county in the entire state that would benefit the most from telehealth."
                               ))
                 , ggvisOutput("telehealth_state"))
      ),
      width=9
      )
  ))