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
                box(title = "Select Item", status = "info", solidHeader = TRUE,
                    collapsible = F, width = 4,
                    radioButtons("sourceCountry",
                                 "Select source",
                                 choices = list("Shanghai Ranking" = 1,
                                                "Times World University Ranking" = 2,
                                                "Center for World University Rankings" = 3),
                                 selected = 1),
                    radioButtons("selectedStat",
                                 "Select metric",
                                 choices = list("Top rank" = 1, 
                                                "Median rank" = 2,
                                                "Count" = 3),
                                 selected = 1)
                    ),
                box(width = 8,
                    htmlOutput("country.map")
                    )
              ),
              
              fluidRow(
                box(width = 12,
                    dataTableOutput("country.table")
                    )
              )
      ),
      
      tabPanel("Universities",
              fluidRow(
                # box(title = "Select Item", status = "info", solidHeader = TRUE,
                #     collapsible = F, width = 4,
                #     radioButtons("selectedSource",
                #                  "Select source",
                #                  choices = list("Shanghai Ranking" = 1,
                #                                 "Times World University Ranking" = 2,
                #                                 "Center for World University Rankings" = 3)),
                #     sliderInput("range", # choose the range 
                #                 label = "Range of interest:",
                #                 min = 0, max = 1000, value = c(0, 200))
                # ),
                box(width = 12,
                    leafletOutput("unimap")
                )
              ),
              
              fluidRow(
                box(width = 12,
                    dataTableOutput("uni.table")
                )
              )
      ),

      tabPanel("Compare",
              fluidRow(
                column(title = "Selections", status = "info", solidHeader = TRUE,
                    collapsible = F, width = 3,
                    selectizeInput("uni1",
                                   "Select university 1",
                                   sort(universities$new_name)),
                    selectizeInput("uni2",
                                   "Select university 2",
                                   sort(universities$new_name)),
                    selectizeInput("uni3",
                                   "Select university 3",
                                   sort(universities$new_name))
                    ),
                column(title='Comparisons ', status = "success", solidHeader = TRUE,
                    collapsible = F, width = 9,
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