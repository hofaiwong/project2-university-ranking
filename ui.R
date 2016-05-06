library(shiny)
library(shinydashboard)
library(googleVis)
library(leaflet)
library(shinythemes)

## ui.R ##
shinyUI(
  navbarPage(
    title = "World University Rankings",
    id = "nav",
    theme = shinytheme("united"),

    tabPanel("Countries",
  
              fluidRow(
                column(width = 3,
                       h4("Ranking organization"),
                    radioButtons("sourceCountry",
                                 "Select organization",
                                 choices = list("Shanghai Ranking" = 1,
                                                "Times World University Ranking" = 2,
                                                "Center for World University Rankings" = 3),
                                 selected = 1),
                    
                    conditionalPanel(
                      condition = "input.sourceCountry == 1", #Shanghai
                      h4("Shanghai ranking metrics"),
                      
                      radioButtons("selectedStat_shanghai",
                                   "Select metric",
                                   choices = list("Top rank" = 1, 
                                                  "Median rank" = 2,
                                                  "Count of ranked universities" = 3,
                                                  "Mean total score" = 4,
                                                  "Mean alumni score" = 5,
                                                  "Mean award score" = 6,
                                                  "Mean HiCi score" = 7,
                                                  "Mean N&S score" = 8,
                                                  "Mean PUB score" = 9,
                                                  "Mean PCP score" = 10),
                                   selected = 1)
                    ),
                    
                    conditionalPanel(
                      condition = "input.sourceCountry == 2", #Times
                      h4("Times ranking metrics"),
                      
                      radioButtons("selectedStat_times",
                                   "Select metric",
                                   choices = list("Top rank" = 1, 
                                                  "Median rank" = 2,
                                                  "Count of ranked universities" = 3,
                                                  "Mean total score" = 4,
                                                  "Mean teaching score" = 5,
                                                  "Mean international score" = 6,
                                                  "Mean research score" = 7,
                                                  "Mean citations score" = 8,
                                                  "Mean income score" = 9),
                                   selected = 1)
                    ),
                    
                    conditionalPanel(
                      condition = "input.sourceCountry == 3", #CWUR
                      h4("CWUR ranking metrics"),
                      
                      radioButtons("selectedStat_cwur",
                                   "Select metric",
                                   choices = list("Top rank" = 1, 
                                                  "Median rank" = 2,
                                                  "Count of ranked universities" = 3,
                                                  "Mean total score" = 4,
                                                  "Mean education score" = 5,
                                                  "Mean alumni employment score" = 6,
                                                  "Mean faculty score" = 7,
                                                  "Mean publications score" = 8,
                                                  "Mean influence score" = 9,
                                                  "Mean citations score" = 10,
                                                  "Mean broad impact score" = 11,
                                                  "Mean patents score" = 12),
                                   selected = 1)
                    )
                    
                    ),
                
                column(width = 9,
                    tabsetPanel(
                      tabPanel("Map",
                      htmlOutput("country.map")
                      ),
                      tabPanel("Data",
                        div(dataTableOutput("country.table"), style = "font-size:80%")
                        )
                      )
                    )
              )
             ),

      
      tabPanel("Universities",
              fluidRow(
                column(3,
                       h4("Ranking organization"),
                       radioButtons("sourceUni",
                                    "Select source",
                                    choices = list("Shanghai Ranking" = 1,
                                                   "Times World University Ranking" = 2,
                                                   "Center for World University Rankings" = 3),
                                    selected = 1),
                       
                       conditionalPanel(
                         condition = "input.sourceCountry == 1", #Shanghai
                         sliderInput("s.sh.rank", "Shanghai rank:", min=0, max=500, value=100),
                         sliderInput("s.sh.tscore", "Total score:", min=0, max=100, value=100),
                         sliderInput("s.sh.alumni", "Alumni score:", min=0, max=100, value=100),
                         sliderInput("s.sh.award", "award score:", min=0, max=100, value=100),
                         sliderInput("s.sh.hici", "hici score:", min=0, max=100, value=100),
                         sliderInput("s.sh.ns", "ns score:", min=0, max=100, value=100),
                         sliderInput("s.sh.pub", "pub score:", min=0, max=100, value=100),
                         sliderInput("s.sh.pcp", "pcp score:", min=0, max=100, value=100)
                       )
                ),
                column(width = 9,
                       tabsetPanel(
                         tabPanel("Map",
                                  leafletOutput("unimap")
                         ),
                         tabPanel("Data",
                                  div(dataTableOutput("uni.table"), style = "font-size:80%")
                         )
                       )
                )
              )
      ),

      tabPanel("Compare",
              fluidRow(
                column(3,
                       h4("Select universities for comparison"),
                    selectizeInput("uni1",
                                   "Select university 1",
                                   universities),
                    selectizeInput("uni2",
                                   "Select university 2",
                                   universities),
                    selectizeInput("uni3",
                                   "Select university 3",
                                   universities)
                    ),
                column(9,
                    tabsetPanel(
                      tabPanel("Shanghai Scores",
                               htmlOutput("compare.shanghai")),
                      tabPanel("Times Scores",
                               htmlOutput("compare.times")),
                      tabPanel("CWUR Rankings",
                               htmlOutput("compare.cwur"))
                      )
                    )
                )
              )
    )
  )