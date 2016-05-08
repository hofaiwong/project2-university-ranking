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
                                   choices = list("Shanghai Rankings" = 1,
                                                  "Times World University Rankings" = 2,
                                                  "Center for World University Rankings" = 3),
                                   selected = 1),
                      br(),
                      conditionalPanel(
                        condition = "input.sourceCountry == 1", #Shanghai
                        h4("Shanghai ranking metrics"),
                        
                        radioButtons("selectedStat_shanghai",
                                     "Select metric",
                                     choices = list("Top rank" = 1, 
                                                    "Median rank" = 2,
                                                    "Count of ranked universities" = 3,
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
                                 column(width = 12,
                                        htmlOutput("country.map"))),
                        tabPanel("Bar Chart",
                                 column(width = 12,
                                        htmlOutput("country.bar")))
                      )
               ))),
    
    
    
    tabPanel("Universities",
             fluidRow(
               column(width = 12,
                      selectInput('selectUni',
                                  'Select university',
                                  universities,
                                  selected = 'New York University',
                                  selectize=TRUE),
                      dataTableOutput("unirank.table")
             )),
             hr(),
             fluidRow(width=12,
                      box(width = 4,
                          h4("Shanghai Scores"),
                          htmlOutput("bar.shanghai")
                      ),
                      box(width = 4,
                          h4("Times Scores"),
                          htmlOutput("bar.times")),
                      box(width = 4,
                          h4("CWUR Scores*"),
                          htmlOutput("bar.cwur"))
             )
    ),    
    
    
    
    tabPanel("Data",
             tabsetPanel(
               tabPanel("Country Data",
                        fluidRow(
                          column(12,
                                 selectizeInput('selectCountry',
                                                'Select country',
                                                countries,
                                                multiple = TRUE,
                                                selected='United States of America'),
                                 hr(),
                                 h4("Shanghai Rankings Data:"),
                                 div(dataTableOutput("selectcountry.shanghai.table"), style = "font-size:80%"),
                                 h4("Times Rankings Data:"),
                                 div(dataTableOutput("selectcountry.times.table"), style = "font-size:80%"),
                                 h4("CWUR Rankings Data:"),
                                 div(dataTableOutput("selectcountry.cwur.table"), style = "font-size:80%"))
                        )
               ),
               tabPanel("University Data",
                        fluidRow(
                          column(12,
                                 div(dataTableOutput("country.table"), style = "font-size:80%")
                          )
                        )
               )
             )
    ),
    
    tabPanel("Reference",
             fluidRow(
               column(12,
                      h4('Data Sources'),
                      h4('Explanation of scoring criteria'),
                      h4('Other assumptions')
               )
             )
    )
  )
)