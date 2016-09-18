
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)

# import state data

shinyUI(fluidPage(
  titlePanel("Telehealth Impact"),
  sidebarPanel(p(strong("NOTE: "), "This involves many objects, so it may take a minute or two to load 
                 espectially when viewing all counties."),
               p(strong("Scoring Level: "), "Controls whether scores are to be calculated at the state or national level.  
                  A score of 100 at the national level for example, represents the county that would benefit the most 
                  from telehealth in the entire US whereas a score of 100 at the state level would represent the county 
                  that would benefit the most within the state.."),
               p(strong("View: "), "Controls which map to draw.  Available options are each state or the entire continental US."),
               p(strong("Tooltips: "), "More data is displayed when view is set to a particular state. 
                 Each score is out of a possible 10 and the final score is out of 100.")
  ),
  mainPanel(
    radioButtons("level", "Scoring Level"
                 , c("National", "State"))
              , selectInput("view", "View"
                            , c('All' = 'all'
                                , 'Alabama'='AL', 'Alaska'='AK', 'Arizona'='AZ'
                                , 'Arkansas'='AR', 'California'='CA', 'Colorado'='CO'
                                , 'Connecticut'='CT', 'Delaware'='DE', 'Florida'='FL'
                                , 'Georgia'='GA', 'Hawaii'='HI', 'Idaho'='ID'
                                , 'Illinois'='IL', 'Indiana'='IN', 'Iowa'='IA'
                                , 'Kansas'='KS', 'Kentucky'='KY', 'Louisiana'='LA'
                                , 'Maine'='ME', 'Maryland'='MD', 'Massachusetts'='MA'
                                , 'Michigan'='MI', 'Minnesota'='MN', 'Mississippi'='MS'
                                , 'Missouri'='MO', 'Montana'='MT', 'Nebraska'='NE'
                                , 'Nevada'='NV', 'New Hampshire'='NH', 'New Jersey'='NJ'
                                , 'New Mexico'='NM', 'New York'='NY', 'North Carolina'='NC'
                                , 'North Dakota'='ND', 'Ohio'='OH', 'Oklahoma'='OK'
                                , 'Oregon'='OR', 'Pennsylvania'='PA', 'Rhode Island'='RI'
                                , 'South Carolina'='SC', 'South Dakota'='SD', 'Tennessee'='TN'
                                , 'Texas'='TX', 'Utah'='UT', 'Vermont'='VT'
                                , 'Virginia'='VA', 'Washington'='WA', 'West Virginia'='WV'
                                , 'Wisconsin'='WI', 'Wyoming'='WY'))
              , ggvisOutput("map")
      )
  ))